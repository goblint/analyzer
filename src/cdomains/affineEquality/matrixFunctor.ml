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
