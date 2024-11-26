open RatOps
open AbstractVector
open Matrix

(** Some functions inside have the suffix _with, which means that the function has side effects. *)
module type AbstractMatrix =
  functor (A: RatOps) (V: AbstractVector) ->
  sig
    include Matrix with type vec := V(A).t and type num := A.t
  end

