type mark = ..

module Exn =
struct
  type t = exn
  let equal = (==)
  let hash = Hashtbl.hash
end

module EWH = Ephemeron.K1.Make (Exn)

let marks: mark EWH.t = EWH.create 10

let add_mark e m =
  EWH.add marks e m


(* Copied & modified from Fun. *)
let protect ~(mark: unit -> mark) ~(finally: unit -> unit) work =
  let finally_no_exn () =
    try
      finally ()
    with e ->
      let bt = Printexc.get_raw_backtrace () in
      let finally_exn = Fun.Finally_raised e in
      add_mark finally_exn (mark ());
      Printexc.raise_with_backtrace finally_exn bt
  in
  match work () with
  | result ->
    finally_no_exn ();
    result
  | exception work_exn ->
    let work_bt = Printexc.get_raw_backtrace () in
    finally_no_exn ();
    add_mark work_exn (mark ());
    Printexc.raise_with_backtrace work_exn work_bt


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

let find_marks e =
  List.rev (EWH.find_all marks e)

let print_marktrace oc e =
  let ms = find_marks e in
  List.iter (fun m ->
      Printf.fprintf oc "Marked with %s\n" (mark_to_string m)
    ) ms

let () =
  Printexc.set_uncaught_exception_handler (fun e bt ->
      (* Copied & modified from Printexc.default_uncaught_exception_handler. *)
      Printf.eprintf "Fatal error: exception %s\n" (Printexc.to_string e);
      if Printexc.backtrace_status () then
        print_marktrace stderr e;
      Printexc.print_raw_backtrace stderr bt;
      flush stderr
    )
