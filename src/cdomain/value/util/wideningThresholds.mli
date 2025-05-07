(** Widening threshold utilities. *)

module Thresholds : Set.S with type elt = Z.t

val thresholds : Thresholds.t ResettableLazy.t
val thresholds_incl_mul2 : Thresholds.t ResettableLazy.t
val exps: GoblintCil.exp list ResettableLazy.t

val reset_lazy : unit -> unit
val upper_thresholds : Thresholds.t ResettableLazy.t
val lower_thresholds : Thresholds.t ResettableLazy.t
val octagon_thresholds : Thresholds.t ResettableLazy.t