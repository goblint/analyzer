val thresholds : unit -> Z.t list
val thresholds_incl_mul2 : unit -> Z.t list
val exps: GoblintCil.exp list ResettableLazy.t

val reset_lazy : unit -> unit
