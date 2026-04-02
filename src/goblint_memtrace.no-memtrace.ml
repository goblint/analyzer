(** Dummy implementation when the optional memtrace dependency is not installed. *)

let trace_if_requested ?context ?sampling_rate () =
  ()
