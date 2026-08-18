(* GENERATED FILE. DO NOT EDIT.
   Source: extraction/goblint_qsolver_update.ml
   Regenerate with: make goblint-solvers *)
open Goblint_constraint.ConstrSys

module type UPDATE =
  functor (S : DemandEqConstrSys) -> sig
    type phase

    val initial_phase : bool -> S.v -> phase
    val update : bool -> S.v -> phase -> S.d -> S.d -> phase * S.d
  end

module WideningUpdate (S : DemandEqConstrSys) = struct
  type phase = unit

  let initial_phase _is_side_effect _variable = ()

  let[@inline always] update _is_side_effect _variable () old input =
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

  let initial_phase _is_side_effect _variable = Copy Config.k

  let[@inline always] update _is_side_effect _variable phase old input =
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

(** Box update for Goblint's current widening convention: the second argument
    of [widen] is the join of the old value and the new input. *)
module JoinedBoxUpdate
    (Config : BOX_UPDATE_CONFIG)
    (S : DemandEqConstrSys) =
struct
  type phase =
    | Copy of int
    | BoxW of int
    | BoxN of int
    | Widen

  let initial_phase _is_side_effect _variable = Copy Config.k

  let[@inline always] widen old input =
    S.Dom.widen old (S.Dom.join old input)

  let[@inline always] update _is_side_effect _variable phase old input =
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

module Copy0Update = JoinedBoxUpdate (struct
    let k = 0
    let l = 0
  end)

(** [k = 1] performs two direct copies.  [l = 0] permits the first
    widening/narrowing box before committing to widening when it reopens. *)
module CopyUpdate = JoinedBoxUpdate (struct
    let k = 1
    let l = 0
  end)

module WNUpdate (S : DemandEqConstrSys) =
struct
  module M = JoinedBoxUpdate (struct
      let k = 0
      let l = 0
    end) (S)

  type phase = M.phase

  let initial_phase _is_side_effect _variable = M.BoxW 0
  let update = M.update
end

module DefaultUpdate (S : DemandEqConstrSys) =
struct
  module M = JoinedBoxUpdate (struct
      let k = 0
      let l = 0
    end) (S)

  type phase = M.phase

  let initial_phase is_side_effect _variable =
    if is_side_effect then M.BoxW 0 else M.Copy 0
  let update = M.update
end
