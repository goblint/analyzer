(** If this is true we output messages and collect accesses.
    This is set to true in control.ml before we verify the result (or already before solving if warn = 'early') *)
let should_warn = ref false

(** Whether signed overflow or underflow happened *)
let svcomp_may_overflow = ref false

(** A hack to see if we are currently doing global inits *)
let global_initialization = ref false


(** Whether currently in postsolver evaluations (e.g. verify, warn) *)
let postsolving = ref false

(* None if verification is disabled, Some true if verification succeeded, Some false if verification failed *)
let verified : bool option ref = ref None

let widening = ref false