(* GENERATED FILE. DO NOT EDIT.
   Source: extraction/goblint_qsolver_update.ml
   Regenerate with: make goblint-reset-extraction *)
open Batteries
open Goblint_constraint.ConstrSys
open Goblint_constraint.SolverTypes

module type UPDATE =
  functor (S : DemandEqConstrSys) -> sig
    type phase
    val initial_phase : phase
    val update : phase -> S.d -> S.d -> phase * S.d
  end

module WideningUpdate (S : DemandEqConstrSys) = struct
  type phase = unit

  let initial_phase = ()

  let[@inline always] update () old input =
    (), S.Dom.widen old (S.Dom.join old input)
end

module type BOX_UPDATE_CONFIG = sig
  val k : int
  val l : int
end

module BoxUpdate
    (Config : BOX_UPDATE_CONFIG)
    (S : DemandEqConstrSys) =
struct
  type phase =
    | Copy of int
    | BoxW of int
    | BoxN of int
    | Widen

  let initial_phase = Copy Config.k

  let[@inline always] update phase old input =
    match phase with
    | Copy remaining ->
      if remaining = 0 then BoxW Config.l, input
      else Copy (remaining - 1), input
    | BoxW remaining ->
      if S.Dom.leq input old then
        BoxN remaining, S.Dom.narrow old input
      else
        BoxW remaining, S.Dom.widen old input
    | BoxN remaining ->
      if S.Dom.leq input old then
        BoxN remaining, S.Dom.narrow old input
      else if remaining = 0 then
        Widen, S.Dom.widen old input
      else
        BoxW (remaining - 1), S.Dom.widen old input
    | Widen ->
      Widen, S.Dom.widen old input
end

(** Box update for Goblint's current widening convention: every call to
    [widen] receives [join old input] as its second argument. *)
module JoinedBoxUpdate
    (Config : BOX_UPDATE_CONFIG)
    (S : DemandEqConstrSys) =
struct
  type phase =
    | Copy of int
    | BoxW of int
    | BoxN of int
    | Widen

  let initial_phase = Copy Config.k

  let[@inline always] widen old input =
    S.Dom.widen old (S.Dom.join old input)

  let[@inline always] update phase old input =
    match phase with
    | Copy remaining ->
      if remaining = 0 then BoxW Config.l, input
      else Copy (remaining - 1), input
    | BoxW remaining ->
      if S.Dom.leq input old then
        BoxN remaining, S.Dom.narrow old input
      else
        BoxW remaining, widen old input
    | BoxN remaining ->
      if S.Dom.leq input old then
        BoxN remaining, S.Dom.narrow old input
      else if remaining = 0 then
        Widen, widen old input
      else
        BoxW (remaining - 1), widen old input
    | Widen ->
      Widen, widen old input
end

(** Compile-time strategy selection.  [k = 0] performs one copy; [l = 0]
    permits the initial BoxW/BoxN pair and then commits to Widen on reopening. *)
module CopyUpdate = JoinedBoxUpdate (struct
    let k = 0
    let l = 0
  end)

module WNUpdate (S : DemandEqConstrSys) = 
struct
  module M = JoinedBoxUpdate (struct
      let k = 0
      let l = 0
    end) (S)

  type phase = M.phase
  let update = M.update
  let initial_phase = M.BoxW 0
end
