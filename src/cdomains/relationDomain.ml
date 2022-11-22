(** Interfaces/implementations that generalize the apronDomain and affineEqualityDomain. *)

open Prelude
open GoblintCil

(** Abstracts the extended apron Var. *)
module type Var =
sig
  type t
  val compare : t -> t -> int
  val of_string : string -> t
  val to_string : t -> string
  val hash : t -> int
  val equal : t -> t -> bool
end

module type VarMetadata =
sig
  type t
  val var_name: t -> string
end

module VarMetadataTbl (VM: VarMetadata) (Var: Var) =
struct
  module VH = Hashtbl.Make (Var)

  let vh = VH.create 113

  let make_var ?name metadata =
    let name = Option.default_delayed (fun () -> VM.var_name metadata) name in
    let var = Var.of_string name in
    VH.replace vh var metadata;
    var

  let find_metadata (var: Var.t) =
    VH.find_option vh var
end

(* TODO: Why renamed VM -> RelVM? *)
module RelVM =
struct
  type t =
    | Local of varinfo (** Var for function local variable (or formal argument). *)
    | Arg of varinfo (** Var for function formal argument entry value. *)
    | Return (** Var for function return value. *)
    | Global of varinfo

  let var_name = function
    | Local x ->
      (* Used to distinguish locals of different functions that share the same name, not needed for base, as we use varinfos directly there *)
      x.vname ^ "#" ^ string_of_int x.vid
    | Arg x -> x.vname ^ "#" ^ string_of_int x.vid ^ "#arg"
    | Return -> "#ret"
    | Global g -> g.vname
end

module type RV =
sig
  type t
  type vartable

  val vh: vartable
  val make_var: ?name:string -> RelVM.t -> t
  val find_metadata: t -> RelVM.t Option.t
  val local: varinfo -> t
  val arg: varinfo -> t
  val return: t
  val global: varinfo -> t
  val to_cil_varinfo: t -> varinfo Option.t
end

module V (Var: Var): (RV with type t = Var.t and type vartable = RelVM.t VarMetadataTbl (RelVM) (Var).VH.t) =
struct
  type t = Var.t
  module VMT = VarMetadataTbl (RelVM) (Var)
  include VMT
  open RelVM

  type vartable = RelVM.t VMT.VH.t

  let local x = make_var (Local x)
  let arg x = make_var (Arg x)
  let return = make_var Return
  let global g = make_var (Global g)

  let to_cil_varinfo v =
    match find_metadata v with
    | Some (Global v | Local v | Arg v) -> Some v
    | _ -> None
end

module type LinCons =
sig
  type t
  val num_vars: t -> int
end

module type Tracked =
sig
  val type_tracked: typ -> bool
  val varinfo_tracked: varinfo -> bool
end

module type S2 =
sig
  type t
  type var
  type marshal

  module Tracked: Tracked

  include Lattice.S with type t:= t
  val is_bot_env : t -> bool
  val vars : t -> var list
  val add_vars : t -> var list -> t
  val remove_vars : t -> var list -> t

  (** Some of the functions end with the suffix _pt_with which indicates that the function could have potential side effects.
      In that case, the functions are supposed to return their modified input. *)

  val remove_vars_pt_with : t -> var list -> t
  val remove_filter : t -> (var -> bool) -> t
  val remove_filter_pt_with: t -> (var -> bool) -> t
  val copy_pt: t -> t
  val keep_vars : t -> var list -> t
  val keep_filter : t -> (var -> bool) -> t
  val forget_vars : t -> var list -> t

  (** Lazy bool ov parameter has been added to functions where functions of the Convert module are used.
      This is to also to make used of the improved overflow handling. *)

  val assign_exp : t -> var -> exp -> bool Lazy.t -> t
  val assign_var : t -> var -> var -> t
  val assign_var_parallel_pt_with : t -> (var * var) list -> t
  val assign_var_parallel' : t -> var list -> var list -> t
  val substitute_exp : t -> var -> exp -> bool Lazy.t -> t
  val unify: t -> t -> t
  val marshal: t -> marshal
  val unmarshal: marshal -> t
  val mem_var: t -> var -> bool
  val assert_inv : t -> exp -> bool -> bool Lazy.t -> t
  val eval_int : t -> exp -> bool Lazy.t -> Queries.ID.t
end

module type S3 =
sig
  include S2

  val cil_exp_of_lincons1: Apron.Lincons1.t -> exp option
  val invariant: t -> Apron.Lincons1.t list
end

type ('a, 'b) relcomponents_t = {
  rel: 'a;
  priv: 'b;
} [@@deriving eq, ord, hash, to_yojson]

module RelComponents (D3: S3) (PrivD: Lattice.S):
sig
  module RD: S3
  include Lattice.S with type t = (D3.t, PrivD.t) relcomponents_t
end =
struct
  module RD = D3
  type t = (RD.t, PrivD.t) relcomponents_t [@@deriving eq, ord, hash, to_yojson]

  include Printable.Std
  open Pretty

  let show r =
    let first  = RD.show r.rel in
    let third  = PrivD.show r.priv in
    "(" ^ first ^ ", " ^ third  ^ ")"

  let pretty () r =
    text "(" ++
    RD.pretty () r.rel
    ++ text ", " ++
    PrivD.pretty () r.priv
    ++ text ")"

  let printXml f r =
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (Goblintutil.escape (RD.name ())) RD.printXml r.rel (Goblintutil.escape (PrivD.name ())) PrivD.printXml r.priv

  let name () = RD.name () ^ " * " ^ PrivD.name ()

  let of_tuple(rel, priv):t = {rel; priv}
  let to_tuple r = (r.rel, r.priv)

  let arbitrary () =
    let tr = QCheck.pair (RD.arbitrary ()) (PrivD.arbitrary ()) in
    QCheck.map ~rev:to_tuple of_tuple tr

  let bot () = {rel = RD.bot (); priv = PrivD.bot ()}
  let is_bot {rel; priv} = RD.is_bot rel && PrivD.is_bot priv
  let top () = {rel = RD.top (); priv = PrivD.bot ()}
  let is_top {rel; priv} = RD.is_top rel && PrivD.is_top priv

  let leq {rel=x1; priv=x3 } {rel=y1; priv=y3} =
    RD.leq x1 y1 && PrivD.leq x3 y3

  let pretty_diff () (({rel=x1; priv=x3}:t),({rel=y1; priv=y3}:t)): Pretty.doc =
    if not (RD.leq x1 y1) then
      RD.pretty_diff () (x1,y1)
    else
      PrivD.pretty_diff () (x3,y3)

  let op_scheme op1 op3 {rel=x1; priv=x3} {rel=y1; priv=y3}: t =
    {rel = op1 x1 y1; priv = op3 x3 y3 }
  let join = op_scheme RD.join PrivD.join
  let meet = op_scheme RD.meet PrivD.meet
  let widen = op_scheme RD.widen PrivD.widen
  let narrow = op_scheme RD.narrow PrivD.narrow
end


module type RD =
sig
  module Var : Var
  module V : module type of struct include V(Var) end
  include S3 with type var = Var.t
end
