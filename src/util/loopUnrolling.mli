(** Syntactic loop unrolling. *)

val reset_copyof: unit -> unit
(** Discard provenance from the previous file before unrolling a new file. *)

val rebuild_copyof: (GoblintCil.stmt * GoblintCil.stmt) list -> unit
(** Replace the provenance lookup index with the supplied copy/original pairs,
    after all statements have been numbered.
    Statement IDs must remain stable until the next rebuild. *)

val unroll_loops: GoblintCil.fundec -> int -> (GoblintCil.stmt * GoblintCil.stmt) list
(** Unroll loops in a function and return immediate copy/original pairs.

    @param totalLoops total number of loops from autotuner *)

val find_original: GoblintCil.stmt -> GoblintCil.stmt
(** Find original un-unrolled instance of the statement. *)
