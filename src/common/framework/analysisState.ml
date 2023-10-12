(** Global flags for analysis state. *)

(** If this is true we output messages and collect accesses.
    This is set to true in control.ml before we verify the result (or already before solving if warn = 'early') *)
let should_warn = ref false

(** Whether signed overflow or underflow happened *)
let svcomp_may_overflow = ref false

(** Whether the termination analysis detects the program as non-terminating *)
let svcomp_may_not_terminate = ref false
(** Whether an invalid free happened *)
let svcomp_may_invalid_free = ref false

(** Whether an invalid pointer dereference happened *)
let svcomp_may_invalid_deref = ref false

(** Whether an invalid memtrack happened *)
let svcomp_may_invalid_memtrack = ref false

(** A hack to see if we are currently doing global inits *)
let global_initialization = ref false


(** Whether currently in postsolver evaluations (e.g. verify, warn) *)
let postsolving = ref false

(* None if verification is disabled, Some true if verification succeeded, Some false if verification failed *)
let verified : bool option ref = ref None
