open Prelude
open Pretty
open Cil
open PrecCompareUtil

module type RelVar =
sig
type t
  val compare : t -> t -> int
  val of_string : string -> t
  val to_string : t -> string
  val  hash : t -> int
  val equal : t -> t -> bool
end

module type RelD2  =
sig
  type var
  type t
  type marshal

  val name : unit -> string
  val is_bot_env : t -> bool
  val equal : t -> t -> bool
  val hash : t -> int
  val compare : t -> t -> int
  val show : t -> string
  val pretty : unit -> t -> doc
  val printXml : 'a BatInnerIO.output -> t -> unit
  val name : unit -> string
  val to_yojson : t -> Printable.json
  val invariant : Invariant.context -> t -> Invariant.t
  val arbitrary : unit -> t QCheck.arbitrary
  val leq : t -> t -> bool
  val join : t -> t -> t
  val meet : t -> t -> t
  val widen : t -> t -> t
  val narrow : t -> t -> t
  val pretty_diff : unit -> t * t -> doc
  val bot : unit -> t
  val is_bot : t -> bool
  val top : unit -> t
  val is_top : t -> bool
  val vars : t -> var list
  val add_vars : t -> var list -> t
  val remove_vars : t -> var list -> t
  val remove_filter : t -> (var -> bool) -> t
  val keep_vars : t -> var list -> t
  val keep_filter : t -> (var -> bool) -> t
  val forget_vars : t -> var list -> t
  val assign_exp : t -> var -> exp -> bool -> t
  val assign_var : t -> var -> var -> t
  val assign_var_parallel : t -> (var * var) list -> t
  val assign_var_parallel' : t -> var list -> var list -> t
  val substitute_exp : t -> var -> exp -> bool -> t
  val type_tracked : typ -> bool
  val varinfo_tracked : varinfo -> bool
  val assert_inv : t -> exp -> bool -> bool -> t
  val eval_int : t -> exp -> IntDomain.IntDomTuple.t
  val unify: t -> t -> t
  val tag: t -> int
  val relift: t -> t

  val marshal: t -> marshal

  val unmarshal: marshal -> t
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


module VarMetadataTbl (VM: VarMetadata) =
  functor (Var: RelVar) ->
struct
  module VH = Hashtbl.Make (Var)

  let vh = VH.create 113

  let make_var ?name metadata =
    let name = Option.default_delayed (fun () -> VM.var_name metadata) name in
    let var = Var.of_string name in
    VH.replace vh var metadata;
    var

  let find_metadata var =
    VH.find_option vh var
end

module V (Var: RelVar)=
struct
  include VarMetadataTbl (RelVM) (Var)
  open RelVM
  let local x = make_var ~name:x.vname Local
  let arg x = make_var ~name:(x.vname ^ "'") Arg (* TODO: better suffix, like #arg *)
  let return = make_var Return
  let global g = make_var (Global g)
end

type ('a, 'b) relcomponents_t = {
  apr: 'b;
  priv: 'a;
} [@@deriving eq, ord, to_yojson]

module RelComponent (D2: RelD2) =
  functor (PrivD: Lattice.S) ->
  struct
    type t = (PrivD.t, D2.t) relcomponents_t [@@deriving eq, ord, to_yojson]

    include Printable.Std
    open Pretty
    let hash r  = D2.hash r.apr + PrivD.hash r.priv * 33

    let show r =
      let first  = D2.show r.apr in
      let third  = PrivD.show r.priv in
      "(" ^ first ^ ", " ^ third  ^ ")"

    let pretty () r =
      text "(" ++
      D2.pretty () r.apr
      ++ text ", " ++
      PrivD.pretty () r.priv
      ++ text ")"

    let printXml f r =
      BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (Goblintutil.escape (D2.name ())) D2.printXml r.apr (Goblintutil.escape (PrivD.name ())) PrivD.printXml r.priv

    let name () = D2.name () ^ " * " ^ PrivD.name ()

    let invariant c {apr; priv} =
      Invariant.(D2.invariant c apr && PrivD.invariant c priv)

    let of_tuple(apr, priv):t = {apr; priv}
    let to_tuple r = (r.apr, r.priv)

    let arbitrary () =
      let tr = QCheck.pair (D2.arbitrary ()) (PrivD.arbitrary ()) in
      QCheck.map ~rev:to_tuple of_tuple tr

    let bot () = { apr = D2.bot (); priv = PrivD.bot ()}
    let is_bot {apr; priv} = D2.is_bot apr && PrivD.is_bot priv
    let top () = {apr = D2.top (); priv = PrivD.bot ()}
    let is_top {apr; priv} = D2.is_top apr && PrivD.is_top priv

    let leq {apr=x1; priv=x3 } {apr=y1; priv=y3} =
      D2.leq x1 y1 && PrivD.leq x3 y3

    let pretty_diff () (({apr=x1; priv=x3}:t),({apr=y1; priv=y3}:t)): Pretty.doc =
      if not (D2.leq x1 y1) then
        D2.pretty_diff () (x1,y1)
      else
        PrivD.pretty_diff () (x3,y3)

    let op_scheme op1 op3 {apr=x1; priv=x3} {apr=y1; priv=y3}: t =
      {apr = op1 x1 y1; priv = op3 x3 y3 }
    let join = op_scheme D2.join PrivD.join
    let meet = op_scheme D2.meet PrivD.meet
    let widen = op_scheme D2.widen PrivD.widen
    let narrow = op_scheme D2.narrow PrivD.narrow
end

module type RD =
sig
  module Var : RelVar
  module V : module type of struct include V(Var) end
  module D2 : (RelD2 with type var = Var.t)
end

