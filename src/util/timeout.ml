module Unix = struct
  let set_timer tsecs =
    ignore (Unix.setitimer Unix.ITIMER_REAL
              { Unix.it_interval = 0.0; Unix.it_value = tsecs })

  let timeout f arg tsecs timeout_fn =
    let oldsig = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> timeout_fn ())) in
    set_timer tsecs;
    let res = f arg in
    set_timer 0.0;
    Sys.set_signal Sys.sigalrm oldsig;
    res
end

module Js = struct
  let timeout f arg _ _ = f arg
  (* TODO: Implement this *)
end

let timeout = match Sys.backend_type with
  | Other "js_of_ocaml" -> Js.timeout
  | _ -> Unix.timeout
