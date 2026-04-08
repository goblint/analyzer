(** Box operator for warrowing solvers. *)

module type S =
  functor (D: Lattice.S) ->
  sig
    val box: D.t -> D.t -> D.t
  end

module Warrow: S = functor (D: Lattice.S) ->
struct
  let box x y =
    if D.leq y x then
      D.narrow x y
    else
      D.widen x (D.join x y)
end

module Widen: S = functor (D: Lattice.S) ->
struct
  let box x y = D.widen x (D.join x y)
end

module NarrowOption: S = functor (D: Lattice.S) ->
struct
  let box x y =
    if GobConfig.get_bool "exp.no-narrow" then
      x
    else
      D.narrow x y
end
