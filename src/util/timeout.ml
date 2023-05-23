(** Timeout utilities. *)

module Unix = struct
  let timeout f arg tsecs timeout_fn =
    let oldsig = Sys.signal Sys.sigprof (Sys.Signal_handle (fun _ -> timeout_fn ())) in
    (* https://ocaml.org/api/Unix.html#TYPEinterval_timer ITIMER_PROF is user (ITIMER_VIRTUAL) + system time; sends sigprof *)
    ignore Unix.(setitimer ITIMER_PROF { it_interval = 0.0; it_value = tsecs });
    let res = f arg in
    Sys.set_signal Sys.sigprof oldsig;
    res
end

module Js = struct
  let timeout f arg _ _ = f arg
  (* TODO: Implement this *)
end

let timeout = match Sys.backend_type with
  | Other "js_of_ocaml" -> Js.timeout
  | _ -> Unix.timeout
