open Analyses

module type S =
  functor (EqSys: EqConstrSys) ->
  sig
    val box: EqSys.d -> EqSys.d -> EqSys.d
  end

module Warrow: S = functor (S: EqConstrSys) ->
struct
  let box x y =
    if S.Dom.leq y x then
      S.Dom.narrow x y
    else
      S.Dom.widen x (S.Dom.join x y)
end

module Widen: S = functor (S: EqConstrSys) ->
struct
  let box x y = S.Dom.widen x (S.Dom.join x y)
end

module NarrowOption: S = functor (S: EqConstrSys) ->
struct
  let box x y =
    if GobConfig.get_bool "exp.no-narrow" then
      x
    else
      S.Dom.narrow x y
end
