(** Dummy implementation when the optional memtrace dependency is not installed. *)

let trace_if_requested ?(context: string option) ?(sampling_rate: float option) () =
  ()
