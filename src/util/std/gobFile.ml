(** File utilities *)

(** Read lines from a file as a sequence.
    The file is opened lazily when the sequence is consumed.
    The file is closed when the sequence ends or is abandoned. *)
let lines_of filename =
  let ic_ref = ref None in
  let rec next_line () =
    match !ic_ref with
    | None ->
      let ic = open_in filename in
      ic_ref := Some ic;
      next_line ()
    | Some ic ->
      try
        let line = input_line ic in
        Seq.Cons (line, next_line)
      with End_of_file ->
        close_in ic;
        ic_ref := None;
        Seq.Nil
  in
  next_line
