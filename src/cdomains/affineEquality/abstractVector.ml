open RatOps
open Vector

(** Some functions inside have the suffix _with, which means that the function has side effects. *)
module type AbstractVector =
  functor (A: RatOps) ->
  sig
    include Vector with type num:= A.t
  end

