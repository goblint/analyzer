open RatOps
open Vector

module type ArrayVectorFunctor =
  functor (A: RatOps) ->
  sig
    include ArrayVector with type num:= A.t
  end

module type SparseVectorFunctor =
  functor (A: RatOps) ->
  sig 
    include SparseVector with type num:= A.t
  end

(* let timing_wrap = Timing.wrap *)
(* Disable timing of VectorMatrix and AffineEqualityDomain.
   This is cleaner than a timing functor because the timed functions also call each other. *)
let timing_wrap _ f x = f x
