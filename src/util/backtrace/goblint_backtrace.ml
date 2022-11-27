type mark = ..

exception Marked of mark * exn

let mark m e =
  if Printexc.backtrace_status () then
    Marked (m, e)
  else
    e

(* Copied & modified from Fun. *)
let protect ~mark:(m: unit -> mark) ~(finally: unit -> unit) work =
  let finally_no_exn () =
    try
      finally ()
    with e ->
      let bt = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace (mark (m ()) (Fun.Finally_raised e)) bt
  in
  match work () with
  | result ->
    finally_no_exn ();
    result
  | exception work_exn ->
    let work_bt = Printexc.get_raw_backtrace () in
    finally_no_exn ();
    Printexc.raise_with_backtrace (mark (m ()) work_exn) work_bt


let rec unmark = function
  | Marked (_, e) -> unmark e
  | e -> e

let () = Printexc.register_printer (function
    | Marked _ as e ->
      Some (Printexc.to_string (unmark e))
    | _ -> None (* for other exceptions *)
  )

let mark_printers: (mark -> string option) list ref = ref []

let register_mark_printer f =
  mark_printers := f :: !mark_printers

let apply_mark_printers m =
  List.find_map (fun f ->
      match f m with
      | Some s -> Some s
      | None
      | exception _ -> None
    ) !mark_printers

let mark_to_string_default m =
  Obj.Extension_constructor.(name (of_val m))

let mark_to_string m =
  match apply_mark_printers m with
  | Some s -> s
  | None -> mark_to_string_default m

let rec print_marktrace oc e =
  match e with
  | Marked (m, e) ->
    print_marktrace oc e;
    Printf.fprintf oc "Marked with %s\n" (mark_to_string m)
  | e -> ()

let () =
  Printexc.set_uncaught_exception_handler (fun e bt ->
      (* Copied & modified from Printexc.default_uncaught_exception_handler. *)
      Printf.eprintf "Fatal error: exception %s\n" (Printexc.to_string e);
      print_marktrace stderr e;
      Printexc.print_raw_backtrace stderr bt;
      flush stderr
    )
