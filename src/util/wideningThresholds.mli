val thresholds : unit -> Z.t list
val thresholds_incl_mul2 : unit -> Z.t list

val exps: Cil.exp list ResettableLazy.t

val reset_lazy : unit -> unit
val upper_thresholds : unit -> Z.t list
val lower_thresholds : unit -> Z.t list
val octagon_thresholds : unit -> Z.t list