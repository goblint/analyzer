open RatOps
open VectorFunctor
open Matrix

(** Some functions inside have the suffix _with, which means that the function has side effects. *)
module type ArrayMatrixFunctor =
  functor (A: RatOps) (V: ArrayVectorFunctor) ->
  sig
    include ArrayMatrix with type vec := V(A).t and type num := A.t
  end

module type SparseMatrixFunctor = 
  functor (A: RatOps) (V: SparseVectorFunctor) ->
  sig
    include SparseMatrix with type vec := V(A).t and type num := A.t
  end

(* let timing_wrap = Timing.wrap *)
(* Disable timing of VectorMatrix and AffineEqualityDomain.
   This is cleaner than a timing functor because the timed functions also call each other. *)
let timing_wrap _ f x = f x
