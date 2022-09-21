open Prelude
open Cil

module type Var =
sig
  type t
  val compare : t -> t -> int
  val of_string : string -> t
  val to_string : t -> string
  val  hash : t -> int
  val equal : t -> t -> bool
end

module RelVM =
struct
  type t =
    | Local (** Var for function local variable (or formal argument). *) (* No varinfo because local Var with the same name may be in multiple functions. *)
    | Arg (** Var for function formal argument entry value. *) (* No varinfo because argument Var with the same name may be in multiple functions. *)
    | Return (** Var for function return value. *)
    | Global of varinfo

  let var_name = function
    | Local -> failwith "var_name of Local"
    | Arg -> failwith "var_name of Arg"
    | Return -> "#ret"
    | Global g -> g.vname
end

module type VarMetadata =
sig
  type t
  val var_name: t -> string
end


module VarMetadataTbl =
functor (VM: VarMetadata) ->
  functor (Var: Var) ->
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
  type t
  type vartable

  val vh: vartable
  val make_var: ?name:string -> RelVM.t -> t
  val find_metadata: t -> RelVM.t Option.t
  val local_name: varinfo -> string
  val local: varinfo -> t
  val arg: varinfo -> t
  val return: t
  val global: varinfo -> t
  val to_cil_varinfo: fundec -> t -> varinfo Option.t
end

module V (Var: Var): (RV with type t = Var.t and type vartable = RelVM.t VarMetadataTbl (RelVM) (Var).VH.t) =
struct
   type t = Var.t
  module VMT = VarMetadataTbl (RelVM) (Var)
  include VMT
  open RelVM

  type vartable = RelVM.t VMT.VH.t

  (* Used to distinguish locals of different functions that share the same name, not needed for base, as we use varinfos directly there *)
  let local_name x = x.vname ^ "#" ^ string_of_int(x.vid)
  let local x = make_var ~name:(local_name x) Local
  let arg x = make_var ~name:(x.vname ^ "'") Arg (* TODO: better suffix, like #arg *)
  let return = make_var Return
  let global g = make_var (Global g)

  let to_cil_varinfo fundec v =
    match find_metadata v with
    | Some (Global v) -> Some v
    | Some (Local) ->
      let vname = Var.to_string v in
      List.find_opt (fun v -> local_name v = vname) (fundec.sformals @ fundec.slocals)
    | _ -> None
end

module type LinCons =
sig
  type t
  val get_size: t -> int
  val to_cil_exp: t -> exp Option.t
end

module type RelS  =
sig
  type t
  type var
  type marshal

  include Lattice.S with type t:= t
  val is_bot_env : t -> bool
  val vars : t -> var list
  val add_vars : t -> var list -> t
  val remove_vars : t -> var list -> t
  val remove_vars_pt_with : t -> var list -> t
  val remove_filter : t -> (var -> bool) -> t
  val remove_filter_pt_with: t -> (var -> bool) -> t
  val copy_pt: t -> t
  val keep_vars : t -> var list -> t
  val keep_filter : t -> (var -> bool) -> t
  val forget_vars : t -> var list -> t
  val assign_exp : t -> var -> exp -> bool Lazy.t -> t
  val assign_var : t -> var -> var -> t
  val assign_var_parallel_pt_with : t -> (var * var) list -> t
  val assign_var_parallel' : t -> var list -> var list -> t
  val substitute_exp : t -> var -> exp -> bool Lazy.t -> t
  val unify: t -> t -> t
  val marshal: t -> marshal
  val unmarshal: marshal -> t
  val mem_var: t -> var -> bool
end

module Tracked =
struct
  let type_tracked typ =
    isIntegralType typ

  let varinfo_tracked vi =
    (* no vglob check here, because globals are allowed in apron, but just have to be handled separately *)
    type_tracked vi.vtype && not vi.vaddrof
end

module type RelS2 =
sig
  include RelS

  val type_tracked : typ -> bool
  val varinfo_tracked : varinfo -> bool
  val assert_inv : t -> exp -> bool -> bool Lazy.t -> t
  val eval_int : t -> exp -> bool Lazy.t -> Queries.ID.t
end

module type RelS3 =
sig
  include RelS2

  type consSet

  val get_cons_size: consSet -> int

  val cons_to_cil_exp:  scope:Cil.fundec -> consSet -> exp Option.t

  val invariant: scope:Cil.fundec -> t -> consSet list
end

module NoInvariantRelD2 (D2: RelS2) : RelS3 with type var = D2.var =
struct
  include D2

  type consSet

  let get_cons_size _ = 0

  let cons_to_cil_exp ~scope _ = None

  let invariant ~scope _ = []
end

type ('a, 'b) relcomponents_t = {
  rel: 'a;
  priv: 'b;
} [@@deriving eq, ord, hash, to_yojson]

module RelComponents (D3: RelS3) (PrivD: Lattice.S):
sig
  module AD: RelS3
  include Lattice.S with type t = (D3.t, PrivD.t) relcomponents_t
end =
struct
  module AD = D3
  type t = (AD.t, PrivD.t) relcomponents_t [@@deriving eq, ord, hash, to_yojson]

  include Printable.Std
  open Pretty

  let show r =
    let first  = AD.show r.rel in
    let third  = PrivD.show r.priv in
    "(" ^ first ^ ", " ^ third  ^ ")"

  let pretty () r =
    text "(" ++
    AD.pretty () r.rel
    ++ text ", " ++
    PrivD.pretty () r.priv
    ++ text ")"

  let printXml f r =
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (Goblintutil.escape (AD.name ())) AD.printXml r.rel (Goblintutil.escape (PrivD.name ())) PrivD.printXml r.priv

  let name () = AD.name () ^ " * " ^ PrivD.name ()

  let of_tuple(rel, priv):t = {rel; priv}
  let to_tuple r = (r.rel, r.priv)

  let arbitrary () =
    let tr = QCheck.pair (AD.arbitrary ()) (PrivD.arbitrary ()) in
    QCheck.map ~rev:to_tuple of_tuple tr

  let bot () = {rel = AD.bot (); priv = PrivD.bot ()}
  let is_bot {rel; priv} = AD.is_bot rel && PrivD.is_bot priv
  let top () = {rel = AD.top (); priv = PrivD.bot ()}
  let is_top {rel; priv} = AD.is_top rel && PrivD.is_top priv

  let leq {rel=x1; priv=x3 } {rel=y1; priv=y3} =
    AD.leq x1 y1 && PrivD.leq x3 y3

  let pretty_diff () (({rel=x1; priv=x3}:t),({rel=y1; priv=y3}:t)): Pretty.doc =
    if not (AD.leq x1 y1) then
      AD.pretty_diff () (x1,y1)
    else
      PrivD.pretty_diff () (x3,y3)

  let op_scheme op1 op3 {rel=x1; priv=x3} {rel=y1; priv=y3}: t =
    {rel = op1 x1 y1; priv = op3 x3 y3 }
  let join = op_scheme AD.join PrivD.join
  let meet = op_scheme AD.meet PrivD.meet
  let widen = op_scheme AD.widen PrivD.widen
  let narrow = op_scheme AD.narrow PrivD.narrow
end


module type RD =
sig
  module Var : Var
  module V : module type of struct include V(Var) end
  include RelS3 with type var = Var.t
end
