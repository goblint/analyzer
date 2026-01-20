(** File utilities *)

(** Read lines from a file as a sequence.
    The file is opened lazily when the sequence is consumed.
    The file is closed when the sequence ends or is abandoned. *)
val lines_of : string -> string Seq.t
