(** Signatures for relational value domains.

    See {!ApronDomain} and {!AffineEqualityDomain}. *)

open GobApron
open Batteries
open GoblintCil

module type VarMetadata =
sig
  type t
  val var_name: t -> string
end

module VM =
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

module VarMetadataTbl (VM: VarMetadata) =
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

module type RV =
sig
  type t = Var.t
  type vartable = VM.t VarMetadataTbl (VM).VH.t

  val vh: vartable
  val make_var: ?name:string -> VM.t -> t
  val find_metadata: t -> VM.t Option.t
  val local: varinfo -> t
  val arg: varinfo -> t
  val return: t
  val global: varinfo -> t
  val to_cil_varinfo: t -> varinfo Option.t
end

module V: RV =
struct
  open VM

  type t = Var.t
  module VMT = VarMetadataTbl (VM)
  include VMT

  type vartable = VM.t VMT.VH.t

  let local x = make_var (Local x)
  let arg x = make_var (Arg x)
  let return = make_var Return
  let global g = make_var (Global g)

  let to_cil_varinfo v =
    match find_metadata v with
    | Some (Global v | Local v | Arg v) -> Some v
    | _ -> None
end

module type Tracked =
sig
  val type_tracked: typ -> bool
  val varinfo_tracked: varinfo -> bool
end

module type S2 =
sig
  type t
  type var = Var.t
  type marshal

  module Tracked: Tracked

  include Lattice.S with type t:= t
  val is_bot_env : t -> bool
  val vars : t -> var list
  val add_vars : t -> var list -> t
  val remove_vars : t -> var list -> t

  val remove_vars_with : t -> var list -> unit
  (** Remove variables {e in-place}.
      This avoids an extra copy like {!remove_vars} if the input relation is unshared. *)

  val remove_filter : t -> (var -> bool) -> t

  val remove_filter_with: t -> (var -> bool) -> unit
  (** Filter variables {e in-place}.
      This avoids an extra copy like {!remove_filter} if the input relation is unshared. *)

  val copy: t -> t
  val keep_vars : t -> var list -> t
  val keep_filter : t -> (var -> bool) -> t
  val forget_vars : t -> var list -> t

  (** Lazy bool no_ov parameter has been added to functions where functions of the Convert module are used.
      This is to also to make used of the improved overflow handling. *)

  val assign_exp : Queries.ask -> t -> var -> exp -> bool Lazy.t -> t
  val assign_var : t -> var -> var -> t

  val assign_var_parallel_with : t -> (var * var) list -> unit
  (** Assign variables in parallel {e in-place}.
      This avoids an extra copy like {!assign_var_parallel'} if the input relation is unshared. *)

  val assign_var_parallel' : t -> var list -> var list -> t
  val substitute_exp : Queries.ask -> t -> var -> exp -> bool Lazy.t -> t
  val unify: t -> t -> t
  val marshal: t -> marshal
  val unmarshal: marshal -> t
  val mem_var: t -> var -> bool
  val assert_inv : Queries.ask -> t -> exp -> bool -> bool Lazy.t -> t
  val eval_int : Queries.ask -> t -> exp -> bool Lazy.t -> Queries.ID.t
end

module type S3 =
sig
  include S2
  val cil_exp_of_lincons1: Lincons1.t -> exp option
  val invariant: t -> Lincons1.t list
end

type ('a, 'b) relcomponents_t = {
  rel: 'a;
  priv: 'b;
} [@@deriving eq, ord, hash, to_yojson, relift, lattice]

module RelComponents (D3: S3) (PrivD: Lattice.S):
sig
  module RD: S3
  include Lattice.S with type t = (D3.t, PrivD.t) relcomponents_t
end =
struct
  module RD = D3
  type t = (RD.t, PrivD.t) relcomponents_t [@@deriving eq, ord, hash, to_yojson, relift, lattice]

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
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (XmlUtil.escape (RD.name ())) RD.printXml r.rel (XmlUtil.escape (PrivD.name ())) PrivD.printXml r.priv

  let name () = RD.name () ^ " * " ^ PrivD.name ()

  let arbitrary () =
    let to_tuple r = (r.rel, r.priv) in
    let of_tuple (rel, priv) = {rel; priv} in
    let tr = QCheck.pair (RD.arbitrary ()) (PrivD.arbitrary ()) in
    QCheck.map ~rev:to_tuple of_tuple tr

  let pretty_diff () (({rel=x1; priv=x3}:t),({rel=y1; priv=y3}:t)): Pretty.doc =
    if not (RD.leq x1 y1) then
      RD.pretty_diff () (x1,y1)
    else
      PrivD.pretty_diff () (x3,y3)
end


module type RD =
sig
  module V : RV
  include S3
end
