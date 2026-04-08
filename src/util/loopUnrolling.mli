(** Syntactic loop unrolling. *)

val unroll_loops: GoblintCil.fundec -> int -> unit
(** Unroll loops in a function.

    @param totalLoops total number of loops from autotuner *)

val find_original: GoblintCil.stmt -> GoblintCil.stmt
(** Find original un-unrolled instance of the statement. *)
