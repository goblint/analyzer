(** domain of the base analysis *)

open Cil

module VD = ValueDomain.Compound

module CPA =
struct
  include MapDomain.MapBot_LiftTop (Basetype.Variables) (VD)
  let name () = "value domain"

  let invariant c (m:t) =
    fold (fun k v a ->
        if not (InvariantCil.var_is_tmp k) then
          let i = VD.invariant k.vname v in
          Invariant.(a && i)
        else
          a
      ) m Invariant.none
end

module Flag =
struct
  include ConcDomain.SimpleThreadDomain
  let name () = "flag domain"
end

let heap_hash = Hashtbl.create 113

let get_heap_var loc =
  try Hashtbl.find heap_hash loc
  with Not_found ->
    let name = "(alloc@" ^ loc.file ^ ":" ^ string_of_int loc.line ^ ")" in
    let newvar = Goblintutil.create_var (makeGlobalVar name voidType) in
    Hashtbl.add heap_hash loc newvar;
    newvar

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
  val eval_exp: CPA.t * Flag.t * PartDeps.t ->  Cil.exp -> int64 option
end

(* Takes a module specifying how expressions can be evaluated inside the domain and returns the domain *)
module DomFunctor(ExpEval:ExpEvaluator) =
struct
  include Lattice.Prod3(CPA)(Flag)(PartDeps)

  let join ((a1, b1, c1) as one) ((a2, b2, c2) as two) =
    let cpa_join = CPA.join_with_fct (VD.smart_join (ExpEval.eval_exp one) (ExpEval.eval_exp two)) in
    (cpa_join a1 a2, Flag.join b1 b2, PartDeps.join c1 c2)

  let leq ((a1, b1, c1) as one) ((a2, b2, c2) as two) =
    let cpa_leq = CPA.leq_with_fct (VD.smart_leq (ExpEval.eval_exp one) (ExpEval.eval_exp two)) in
    cpa_leq a1 a2 && Flag.leq b1 b2 && PartDeps.leq c1 c2

  let widen ((a1, b1, c1) as one) ((a2, b2, c2) as two) =
    let cpa_widen = CPA.widen_with_fct (VD.smart_widen (ExpEval.eval_exp one) (ExpEval.eval_exp two)) in
    (cpa_widen a1 a2, Flag.widen b1 b2, PartDeps.widen c1 c2)
end


(* The domain with an ExpEval that only returns constant values for top-level vars that are definite ints *)
module DomWithTrivialExpEval = DomFunctor(struct
  module M = MapDomain.MapBot_LiftTop (Basetype.Variables) (VD)

  let eval_exp x e =
    let (x, _, _) = x in
    match e with
    | Lval (Var v, NoOffset) ->
      begin
        match M.find v x with
        | `Int i -> ValueDomain.ID.to_int i
        | _ -> None
      end
    | _ -> None
end)
