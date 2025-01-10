(** Widening threshold utilities. *)

module Thresholds : Set.S with type elt = Z.t

val thresholds : unit -> Thresholds.t
val thresholds_incl_mul2 : unit -> Thresholds.t
val exps: GoblintCil.exp list ResettableLazy.t

val reset_lazy : unit -> unit
val upper_thresholds : unit -> Thresholds.t
val lower_thresholds : unit -> Thresholds.t
val octagon_thresholds : unit -> Thresholds.t