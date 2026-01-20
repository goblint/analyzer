(** IO utilities for working with sequences *)

(** Read lines from an input channel as a sequence.
    The channel is not closed automatically. *)
let lines_of ic =
  let rec next_line () =
    try
      let line = input_line ic in
      Seq.Cons (line, next_line)
    with End_of_file ->
      Seq.Nil
  in
  next_line
