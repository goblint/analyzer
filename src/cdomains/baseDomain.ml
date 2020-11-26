(** domain of the base analysis *)

open Cil

module VD = ValueDomain.Compound
module BI = IntOps.BigIntOps

module CPA =
struct
  include MapDomain.MapBot_LiftTop (Basetype.Variables) (VD)
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

module Flag =
struct
  include ConcDomain.SimpleThreadDomain
  let name () = "flag domain"
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

module type ExpEvaluator =
sig
  val eval_exp: CPA.t * Flag.t * PartDeps.t ->  Cil.exp -> IntOps.BigIntOps.t option
end

(* Takes a module specifying how expressions can be evaluated inside the domain and returns the domain *)
module DomFunctor(ExpEval:ExpEvaluator) =
struct
  include Lattice.Prod3(CPA)(Flag)(PartDeps)

  let (%) = Batteries.(%)

  let eval_exp x = Option.map BI.to_int64 % (ExpEval.eval_exp x)
  let join ((a1, b1, c1) as one) ((a2, b2, c2) as two) =
    let cpa_join = CPA.join_with_fct (VD.smart_join (eval_exp one) (eval_exp two)) in
    (cpa_join a1 a2, Flag.join b1 b2, PartDeps.join c1 c2)

  let leq ((a1, b1, c1) as one) ((a2, b2, c2) as two) =
    let cpa_leq = CPA.leq_with_fct (VD.smart_leq (eval_exp one) (eval_exp two)) in
    cpa_leq a1 a2 && Flag.leq b1 b2 && PartDeps.leq c1 c2

  let widen ((a1, b1, c1) as one) ((a2, b2, c2) as two) =
    let cpa_widen = CPA.widen_with_fct (VD.smart_widen (eval_exp one) (eval_exp two)) in
    (cpa_widen a1 a2, Flag.widen b1 b2, PartDeps.widen c1 c2)
end


(* The domain with an ExpEval that only returns constant values for top-level vars that are definite ints *)
module DomWithTrivialExpEval = DomFunctor(struct
  module M = MapDomain.MapBot_LiftTop (Basetype.Variables) (VD)

  let eval_exp (x, _, _) e =
    match e with
    | Lval (Var v, NoOffset) ->
      begin
        match M.find v x with
        | `Int i -> ValueDomain.ID.to_int i
        | _ -> None
      end
    | _ -> None
end)
