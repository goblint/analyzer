(** domain of the base analysis *)

open Cil
module VD = ValueDomain.Compound
module BI = IntOps.BigIntOps

module CPA =
struct
  module M =
  struct
    include MapDomain.LiftTop (VD) (MapDomain.HashCached (MapDomain.MapBot (Basetype.Variables) (VD)))
    let name () = "value domain"
  end

  include M

  let invariant (c:Invariant.context) (m:t) =
    (* VS is used to detect and break cycles in deref_invariant calls *)
    let module VS = Set.Make (Basetype.Variables) in
    let rec context vs = {c with
        deref_invariant=(fun vi offset lval ->
                          let v = find vi m in
                          key_invariant_lval vi offset lval v vs
        )
      }
    and key_invariant_lval k offset lval v vs =
      if not (VS.mem k vs) then
        let vs' = VS.add k vs in
        let key_context = {(context vs') with offset; lval=Some lval} in
        VD.invariant key_context v
      else
        Invariant.none
    in

    let key_invariant k v = key_invariant_lval k NoOffset (var k) v VS.empty in
    match c.lval with
    | None ->
      fold (fun k v a ->
        let i =
          if not (InvariantCil.var_is_heap k) then
            key_invariant k v
          else
            Invariant.none
        in
        Invariant.(a && i)
      ) m Invariant.none
    | Some (Var k, _) when not (InvariantCil.var_is_heap k) ->
      (try key_invariant k (find k m) with Not_found -> Invariant.none)
    | _ -> Invariant.none

  let invariant c m =
    if GobConfig.get_bool "ana.base.invariant.enabled" then
      invariant c m
    else
      Invariant.none
end


module Glob =
struct
  module Var = Basetype.Variables
  module Val = VD
end

(* Keeps track of which arrays are potentially partitioned according to an expression containing a specific variable *)
(* Map from variables to sets of arrays: var -> {array} *)
module PartDeps =
struct
  module VarSet = SetDomain.Make(Basetype.Variables)
  include MapDomain.MapBot_LiftTop(Basetype.Variables)(VarSet)
  let name () = "array partitioning deps"
end

(** Maintains a set of local variables that need to be weakly updated, because multiple reachbale copies of them may *)
(* exist on the call stack *)
module WeakUpdates =
struct
  include SetDomain.ToppedSet(Basetype.Variables) (struct let topname = "All variables weak" end)
  let name () = "Vars with Weak Update"
end


type 'a basecomponents_t = {
  cpa: CPA.t;
  deps: PartDeps.t;
  weak: WeakUpdates.t;
  priv: 'a;
} [@@deriving eq, ord, hash]


module BaseComponents (PrivD: Lattice.S):
sig
  include Lattice.S with type t = PrivD.t basecomponents_t
  val op_scheme: (CPA.t -> CPA.t -> CPA.t) -> (PartDeps.t -> PartDeps.t -> PartDeps.t) -> (WeakUpdates.t -> WeakUpdates.t -> WeakUpdates.t) -> (PrivD.t -> PrivD.t -> PrivD.t) -> t -> t -> t
end =
struct
  type t = PrivD.t basecomponents_t [@@deriving eq, ord, hash]

  include Printable.Std
  open Pretty

  let show r =
    let first  = CPA.show r.cpa in
    let second  = PartDeps.show r.deps in
    let third  = WeakUpdates.show r.weak in
    let fourth =  PrivD.show r.priv in
    "(" ^ first ^ ", " ^ second ^ ", " ^ third  ^ ", " ^ fourth  ^ ")"

  let pretty () r =
    text "(" ++
    CPA.pretty () r.cpa
    ++ text ", " ++
    PartDeps.pretty () r.deps
    ++ text ", " ++
    WeakUpdates.pretty () r.weak
    ++ text ", " ++
    PrivD.pretty () r.priv
    ++ text ")"

  let printXml f r =
    let e = XmlUtil.escape in
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a\n<key>\n%s\n</key>\n%a</map>\n</value>\n"
      (e @@ CPA.name ()) CPA.printXml r.cpa
      (e @@ PartDeps.name ()) PartDeps.printXml r.deps
      (e @@ WeakUpdates.name ()) WeakUpdates.printXml r.weak
      (e @@ PrivD.name ()) PrivD.printXml r.priv

  let to_yojson r =
    `Assoc [ (CPA.name (), CPA.to_yojson r.cpa); (PartDeps.name (), PartDeps.to_yojson r.deps); (WeakUpdates.name (), WeakUpdates.to_yojson r.weak); (PrivD.name (), PrivD.to_yojson r.priv) ]

  let name () = CPA.name () ^ " * " ^ PartDeps.name () ^ " * " ^ WeakUpdates.name ()  ^ " * " ^ PrivD.name ()

  let invariant c {cpa; deps; weak; priv} =
    Invariant.(CPA.invariant c cpa && PartDeps.invariant c deps && WeakUpdates.invariant c weak && PrivD.invariant c priv)

  let of_tuple(cpa, deps, weak, priv):t = {cpa; deps; weak; priv}
  let to_tuple r = (r.cpa, r.deps, r.weak, r.priv)

  let arbitrary () =
    let tr = QCheck.quad (CPA.arbitrary ()) (PartDeps.arbitrary ()) (WeakUpdates.arbitrary ()) (PrivD.arbitrary ()) in
    QCheck.map ~rev:to_tuple of_tuple tr

  let bot () = { cpa = CPA.bot (); deps = PartDeps.bot (); weak = WeakUpdates.bot (); priv = PrivD.bot ()}
  let is_bot {cpa; deps; weak; priv} = CPA.is_bot cpa && PartDeps.is_bot deps && WeakUpdates.is_bot weak && PrivD.is_bot priv
  let top () = {cpa = CPA.top (); deps = PartDeps.top ();  weak = WeakUpdates.top () ; priv = PrivD.bot ()}
  let is_top {cpa; deps; weak; priv} = CPA.is_top cpa && PartDeps.is_top deps && WeakUpdates.is_top weak && PrivD.is_top priv

  let leq {cpa=x1; deps=x2; weak=x3; priv=x4 } {cpa=y1; deps=y2; weak=y3; priv=y4} =
    CPA.leq x1 y1 && PartDeps.leq x2 y2 && WeakUpdates.leq x3 y3 && PrivD.leq x4 y4

  let pretty_diff () (({cpa=x1; deps=x2; weak=x3; priv=x4}:t),({cpa=y1; deps=y2; weak=y3; priv=y4}:t)): Pretty.doc =
    if not (CPA.leq x1 y1) then
      CPA.pretty_diff () (x1,y1)
    else if not (PartDeps.leq x2 y2) then
      PartDeps.pretty_diff () (x2,y2)
    else if not (WeakUpdates.leq x3 y3) then
      WeakUpdates.pretty_diff () (x3,y3)
    else
      PrivD.pretty_diff () (x4,y4)

  let op_scheme op1 op2 op3 op4 {cpa=x1; deps=x2; weak=x3; priv=x4} {cpa=y1; deps=y2; weak=y3; priv=y4}: t =
    {cpa = op1 x1 y1; deps = op2 x2 y2; weak = op3 x3 y3; priv = op4 x4 y4 }
  let join = op_scheme CPA.join PartDeps.join WeakUpdates.join PrivD.join
  let meet = op_scheme CPA.meet PartDeps.meet WeakUpdates.meet PrivD.meet
  let widen = op_scheme CPA.widen PartDeps.widen WeakUpdates.widen PrivD.widen
  let narrow = op_scheme CPA.narrow PartDeps.narrow WeakUpdates.narrow PrivD.narrow
end

module type ExpEvaluator =
sig
  type t
  val eval_exp: t  ->  Cil.exp -> IntOps.BigIntOps.t option
end

(* Takes a module for privatization component and a module specifying how expressions can be evaluated inside the domain and returns the domain *)
module DomFunctor (PrivD: Lattice.S) (ExpEval: ExpEvaluator with type t = BaseComponents (PrivD).t) =
struct
  include BaseComponents (PrivD)

  let join (one:t) (two:t): t =
    let cpa_join = CPA.join_with_fct (VD.smart_join (ExpEval.eval_exp one) (ExpEval.eval_exp two)) in
    op_scheme cpa_join PartDeps.join WeakUpdates.join PrivD.join one two

  let leq one two =
    let cpa_leq = CPA.leq_with_fct (VD.smart_leq (ExpEval.eval_exp one) (ExpEval.eval_exp two)) in
    cpa_leq one.cpa two.cpa && PartDeps.leq one.deps two.deps && WeakUpdates.leq one.weak two.weak && PrivD.leq one.priv two.priv

  let widen one two: t =
    let cpa_widen = CPA.widen_with_fct (VD.smart_widen (ExpEval.eval_exp one) (ExpEval.eval_exp two)) in
    op_scheme cpa_widen PartDeps.widen WeakUpdates.widen PrivD.widen one two
end


(* The domain with an ExpEval that only returns constant values for top-level vars that are definite ints *)
module DomWithTrivialExpEval (PrivD: Lattice.S) = DomFunctor (PrivD) (struct

  type t = BaseComponents (PrivD).t
  let eval_exp (r: t) e =
    match e with
    | Lval (Var v, NoOffset) ->
      begin
        match CPA.find v r.cpa with
        | `Int i -> ValueDomain.ID.to_int i
        | _ -> None
      end
    | _ -> None
end)
