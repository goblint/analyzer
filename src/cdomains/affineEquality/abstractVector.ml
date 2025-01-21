open RatOps
open Vector

module type AbstractVector =
  functor (A: RatOps) ->
  sig
    include Vector with type num:= A.t
  end

