(** IO utilities for working with sequences *)

(** Read lines from an input channel as a sequence.
    The channel is not closed automatically. *)
val lines_of : in_channel -> string Seq.t
