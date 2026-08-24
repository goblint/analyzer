(** Lifts a [Spec] with the context gas variable. The gas variable limits the number of context-sensitively analyzed function calls in a call stack.
    For every function call the gas is reduced. If the gas is zero, the remaining function calls are analyzed without context-information *)

(** Gets the appropriate lifter (either local or per-function). Should only be called when context gas is active. *)
val get_gas_lifter : unit -> (module Analyses.Spec2Spec)
