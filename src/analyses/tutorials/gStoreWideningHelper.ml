(** Contains some defintions that are helpful for the tutorial but out of scope *)
open GoblintCil

(* Complicated definition for technical reasons relating to different int types *)
module Intervals = IntDomain.IntDomWithDefaultIkind(IntDomain.IntDomLifter (IntDomain.SOverflowUnlifter (IntDomain.Interval))) (IntDomain.PtrDiffIkind)

let is_tracked_var v =
  Cil.isIntegralType v.vtype && not v.vaddrof

let is_tracked_lval = function
  | Var v, NoOffset when is_tracked_var v -> Some v
  | _ -> None

let ikind_of_typ t =
  Cilfacade.get_ikind t

let ikind_of_exp e =
  Cilfacade.get_ikind_exp e

let top_of_typ t =
  Intervals.top_of (ikind_of_typ t)

let top_of_exp e =
  Intervals.top_of (ikind_of_exp e)

let top_of_var v =
  top_of_exp (Lval (Var v, NoOffset))


let cast_to_typ t x =
  Intervals.cast_to ~kind:Internal (ikind_of_typ t) x

let const_int ik i =
  Intervals.of_int ik i
