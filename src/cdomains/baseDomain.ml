(** domain of the base analysis *)

open Cil
module VD = ValueDomain.Compound
module BI = IntOps.BigIntOps

module CPA =
struct
  include MapDomain.MapBot_LiftTop (Basetype.Variables) (VD)

  (* TODO: remove CPA timing *)
  (* let time str f arg = Stats.time "cpa" (Stats.time str f) arg

  let equal x y = time "equal" (equal x) y
  let compare x y = time "compare" (compare x) y
  let hash x = time "hash" hash x

  let leq x y = time "leq" (leq x) y
  let join x y = time "join" (join x) y
  let meet x y = time "meet" (meet x) y
  let widen x y = time "widen" (widen x) y
  let narrow x y = time "narrow" (narrow x) y

  let add k v cpa = time "add" (add k v) cpa
  let remove k cpa = time "remove" (remove k) cpa
  let find k cpa = time "find" (find k) cpa
  let find_opt k cpa = time "find_opt" (find_opt k) cpa
  let mem k cpa = time "mem" (mem k) cpa
  let map f cpa = time "map" (map f) cpa
  let add_list xs cpa = time "add_list" (add_list xs) cpa
  let add_list_set ks v cpa = time "add_list_set" (add_list_set ks v) cpa
  let add_list_fun ks f cpa = time "add_list_fun" (add_list_fun ks f) cpa
  let filter_class i cpa = time "filter_class" (filter_class i) cpa
  let map2 f x y = time "map2" (map2 f x) y
  let long_map2 f x y = time "long_map2" (long_map2 f x) y
  let for_all f cpa = time "for_all" (for_all f) cpa
  let iter f cpa = time "iter" (iter f) cpa
  let fold f x a = time "fold" (fold f x) a
  let filter f x = time "filter" (filter f) x
  let merge f x y = time "merge" (merge f x) y
  let leq_with_fct f x y = time "leq_with_fct" (leq_with_fct f x) y
  let join_with_fct f x y = time "join_with_fct" (join_with_fct f x) y
  let widen_with_fct f x y = time "widen_with_fct" (widen_with_fct f x) y *)

  let name () = "value domain"

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
      if not (InvariantCil.var_is_tmp k) && InvariantCil.var_is_in_scope c.scope k && not (VS.mem k vs) then
        let vs' = VS.add k vs in
        let key_context = {(context vs') with offset; lval=Some lval} in
        VD.invariant key_context v
      else
        Invariant.none
    in

    let key_invariant k v =
      key_invariant_lval k NoOffset (var k) v VS.empty in

    fold (fun k v a ->
        let i =
          if not (InvariantCil.var_is_heap k) then
            key_invariant k v
          else
            Invariant.none
        in
        Invariant.(a && i)
      ) m Invariant.none
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


type 'a basecomponents_t = {
  cpa: CPA.t;
  deps: PartDeps.t;
  priv: 'a;
} [@@deriving to_yojson]

module BaseComponents (PrivD: Lattice.S):
sig
  include Lattice.S with type t = PrivD.t basecomponents_t
  val op_scheme: (CPA.t -> CPA.t -> CPA.t) -> (PartDeps.t -> PartDeps.t -> PartDeps.t) -> (PrivD.t -> PrivD.t -> PrivD.t) -> t -> t -> t
end =
struct
  type t = PrivD.t basecomponents_t [@@deriving to_yojson]

  include Printable.Std
  open Pretty
  let hash r  = CPA.hash r.cpa + PartDeps.hash r.deps * 17 + PrivD.hash r.priv * 33
  let equal r1 r2 =
    CPA.equal r1.cpa r2.cpa && PartDeps.equal r1.deps r2.deps && PrivD.equal r1.priv r2.priv
  let compare r1 r2 =
    let comp1 = CPA.compare r1.cpa r2.cpa in
    if comp1 <> 0
      then comp1
      else let comp2 = PartDeps.compare r1.deps r2.deps in
      if comp2 <> 0
      then comp2
      else PrivD.compare r1.priv r2.priv


  let short w r =
    let first  = CPA.short (w-18) r.cpa in
    let second  = PartDeps.short (w-12- String.length first) r.deps in
    let third  = PrivD.short (w-6- String.length first - String.length second) r.priv in
    "(" ^ first ^ ", " ^ second ^ ", " ^ third  ^ ")"

  let pretty_f _ () r =
    text "(" ++
    CPA.pretty () r.cpa
    ++ text ", " ++
    PartDeps.pretty () r.deps
    ++ text ", " ++
    PrivD.pretty () r.priv
    ++ text ")"

  let isSimple r  = CPA.isSimple r.cpa && PartDeps.isSimple r.deps && PrivD.isSimple r.priv

  let printXml f r =
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (Goblintutil.escape (CPA.name ())) CPA.printXml r.cpa (Goblintutil.escape (PartDeps.name ())) PartDeps.printXml r.deps (Goblintutil.escape (PrivD.name ())) PrivD.printXml r.priv

  let pretty () x = pretty_f short () x
  let name () = CPA.name () ^ " * " ^ PartDeps.name () ^ " * " ^ PrivD.name ()

  let invariant c {cpa; deps; priv} =
    Invariant.(CPA.invariant c cpa && PartDeps.invariant c deps && PrivD.invariant c priv)

  let of_tuple(cpa, deps, priv):t = {cpa; deps; priv}
  let to_tuple r = (r.cpa, r.deps, r.priv)

  let arbitrary () =
    let tr = QCheck.triple (CPA.arbitrary ()) (PartDeps.arbitrary ()) (PrivD.arbitrary ()) in
    QCheck.map ~rev:to_tuple of_tuple tr

  let bot () = { cpa = CPA.bot (); deps = PartDeps.bot (); priv = PrivD.bot ()}
  let is_bot {cpa; deps; priv} = CPA.is_bot cpa && PartDeps.is_bot deps && PrivD.is_bot priv
  let top () = {cpa = CPA.top (); deps = PartDeps.top (); priv = PrivD.bot ()}
  let is_top {cpa; deps; priv} = CPA.is_top cpa && PartDeps.is_top deps && PrivD.is_top priv

  let leq {cpa=x1; deps=x2; priv=x3 } {cpa=y1; deps=y2; priv=y3} =
    CPA.leq x1 y1 && PartDeps.leq x2 y2 && PrivD.leq x3 y3

  let pretty_diff () (({cpa=x1; deps=x2; priv=x3}:t),({cpa=y1; deps=y2; priv=y3}:t)): Pretty.doc =
    if not (CPA.leq x1 y1) then
      CPA.pretty_diff () (x1,y1)
    else if not (PartDeps.leq x2 y2) then
      PartDeps.pretty_diff () (x2,y2)
    else
      PrivD.pretty_diff () (x3,y3)

  let op_scheme op1 op2 op3 {cpa=x1; deps=x2; priv=x3} {cpa=y1; deps=y2; priv=y3}: t =
    {cpa = op1 x1 y1; deps = op2 x2 y2; priv = op3 x3 y3 }
  let join = op_scheme CPA.join PartDeps.join PrivD.join
  let meet = op_scheme CPA.meet PartDeps.meet PrivD.meet
  let widen = op_scheme CPA.widen PartDeps.widen PrivD.widen
  let narrow = op_scheme CPA.narrow PartDeps.narrow PrivD.narrow
end

module type ExpEvaluator =
sig
  type t
  val eval_exp: t  ->  Cil.exp -> IntOps.BigIntOps.t option
end

(* Takes a module specifying how expressions can be evaluated inside the domain and returns the domain *)
module DomFunctor (PrivD: Lattice.S) (ExpEval: ExpEvaluator with type t = BaseComponents (PrivD).t) =
struct
  include BaseComponents (PrivD)

  let (%) = Batteries.(%)
  let eval_exp x = Option.map BI.to_int64 % (ExpEval.eval_exp x)
  let join (one:t) (two:t): t =
    let cpa_join = CPA.join_with_fct (VD.smart_join (eval_exp one) (eval_exp two)) in
    op_scheme cpa_join PartDeps.join PrivD.join one two

  let leq one two =
    let cpa_leq = CPA.leq_with_fct (VD.smart_leq (eval_exp one) (eval_exp two)) in
    cpa_leq one.cpa two.cpa && PartDeps.leq one.deps two.deps && PrivD.leq one.priv two.priv

  let widen one two: t =
    let cpa_widen = CPA.widen_with_fct (VD.smart_widen (eval_exp one) (eval_exp two)) in
    op_scheme cpa_widen PartDeps.widen PrivD.widen one two
end


(* The domain with an ExpEval that only returns constant values for top-level vars that are definite ints *)
module DomWithTrivialExpEval (PrivD: Lattice.S) = DomFunctor (PrivD) (struct
  module M = MapDomain.MapBot_LiftTop (Basetype.Variables) (VD)

  type t = BaseComponents (PrivD).t
  let eval_exp (r: t) e =
    match e with
    | Lval (Var v, NoOffset) ->
      begin
        match M.find v r.cpa with
        | `Int i -> ValueDomain.ID.to_int i
        | _ -> None
      end
    | _ -> None
end)
