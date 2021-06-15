open Cil
open Pretty
open GobConfig
module GU = Goblintutil

type array_oob_event = PastEnd | BeforeStart | Unknown
type undefined_behavior_event = ArrayOutOfBounds of array_oob_event | NullPointerDereference | UseAfterFree
type behavior_event = Undefined of undefined_behavior_event | Implementation | Machine
type integer_event = Overflow | DivByZero
type cast_event = TypeMismatch
type array_event = OutOfBounds of int*int
type event_type = Behavior of behavior_event | Integer of integer_event | Race | Array of array_event | Cast of cast_event | Unknown
type certainty = May | Must
type log_event = { event_type : event_type; certainty: certainty}

let show_array_oob e =
  match e with
  | PastEnd -> "Index is past the end of the array."
  | BeforeStart -> "Index is before start of the array."
  | Unknown -> "Not enough information about index."

let show_undefined_behavior e =
  match e with
  | ArrayOutOfBounds e -> Printf.sprintf "[Array out of bounds] %s" (show_array_oob e)
  | NullPointerDereference -> "[Null pointer dereference]"
  | UseAfterFree -> "[Use After Free]"

let show_behavior_event e =
  match e with
  | Undefined u -> Printf.sprintf "[Undefined] %s" (show_undefined_behavior u)
  | Implementation -> "[Implementation]"
  | Machine -> "[Machine]"

let show_certainty c =
  match c with
  | May -> "MAY"
  | Must -> "MUST"

let show_event_type e =
  match e with
  | Behavior behavior -> Printf.sprintf "[Behavior] %s" (show_behavior_event behavior)
  | Integer _ -> "[Integer]"
  | Race -> "[Race]"
  | Array _ -> "[Array]"
  | Cast _ -> "[Cast]"
  | Unknown -> "[Unknown]"

let show_log_event {event_type; certainty} =
  Printf.sprintf "[%s] %s" (show_certainty certainty) (show_event_type event_type)


exception Bailure of string
let bailwith s = raise (Bailure s)

let warning_table : [`text of string * location | `group of string * ((string * location) list)] list ref = ref []
let warnings = ref false
let soundness = ref true
let warn_out = ref stdout
let tracing = Config.tracing
let xml_file_name = ref ""

let track m =
  let loc = !Tracing.current_loc in
  Printf.fprintf !warn_out "Track (%s:%d); %s\n" loc.file loc.line m

(*Warning files*)
let warn_race = ref stdout
let warn_safe = ref stdout
let warn_higr = ref stdout
let warn_higw = ref stdout
let warn_lowr = ref stdout
let warn_loww = ref stdout

let init_warn_files () =
  warn_race := (open_out "goblint_warnings_race.txt");
  warn_safe := (open_out "goblint_warnings_safe.txt");
  warn_higr := (open_out "goblint_warnings_highreadrace.txt");
  warn_higw := (open_out "goblint_warnings_highwriterace.txt");
  warn_lowr := (open_out "goblint_warnings_lowreadrace.txt");
  warn_loww := (open_out "goblint_warnings_lowwriterace.txt")

let get_out name alternative = match get_string "dbg.dump" with
  | "" -> alternative
  | path -> open_out (Filename.concat path (name ^ ".out"))

let colors_on () = (* use colors? *)
  let c = get_string "colors" in
  c = "always" || c = "auto" && Unix.(isatty stdout)

let colorize ?on:(on=colors_on ()) msg =
  let colors = [("gray", "30"); ("red", "31"); ("green", "32"); ("yellow", "33"); ("blue", "34");
                ("violet", "35"); ("turquoise", "36"); ("white", "37"); ("reset", "0;00")] in
  let replace (color,code) =
    let modes = [(fun x -> x), "0" (* normal *); String.uppercase_ascii, "1" (* bold *)] in
    List.fold_right (fun (f,m) -> Str.global_replace (Str.regexp ("{"^f color^"}")) (if on then "\027["^m^";"^code^"m" else "")) modes
  in
  let msg = List.fold_right replace colors msg in
  msg^(if on then "\027[0;0;00m" else "") (* reset at end *)

let print_msg msg loc =
  let msgc = colorize msg in
  let msg  = colorize ~on:false msg in
  if (get_string "result") = "fast_xml" then warning_table := (`text (msg,loc))::!warning_table;
  if get_bool "gccwarn" then
    Printf.printf "%s:%d:0: warning: %s\n" loc.file loc.line msg
  else
    let color = if colors_on () then "{violet}" else "" in
    let s = Printf.sprintf "%s %s(%s:%d)" msgc color loc.file loc.line in
    Printf.fprintf !warn_out "%s\n%!" (colorize s)

let print_err msg loc =
  if (get_string "result") = "fast_xml" then warning_table := (`text (msg,loc))::!warning_table;
  if get_bool "gccwarn" then
    Printf.printf "%s:%d:0: error: %s\n" loc.file loc.line msg
  else
    Printf.fprintf !warn_out "%s (%s:%d)\n%!" msg loc.file loc.line


let print_group group_name errors =
  (* Add warnings to global warning list *)
  if (get_string "result") = "fast_xml" then warning_table := (`group (group_name,errors))::!warning_table;
  let f (msg,loc): doc = Pretty.dprintf "%s (%s:%d)" msg loc.file loc.line in
  if (get_bool "ana.osek.warnfiles") then begin
    match (String.sub group_name 0 6) with
    | "Safely" -> ignore (Pretty.fprintf !warn_safe "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)
    | "Datara" -> ignore (Pretty.fprintf !warn_race "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)
    | "High r" -> ignore (Pretty.fprintf !warn_higr "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)
    | "High w" -> ignore (Pretty.fprintf !warn_higw "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)
    | "Low re" -> ignore (Pretty.fprintf !warn_lowr "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)
    | "Low wr" -> ignore (Pretty.fprintf !warn_loww "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)
    | _ -> ()
  end;
  ignore (Pretty.fprintf !warn_out "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)

let warn_urgent msg =
  if !GU.should_warn then begin
    soundness := false;
    print_msg msg (!Tracing.current_loc)
  end

let warn_all msg =
  if !GU.should_warn then begin
    if !warnings then
      print_msg msg (!Tracing.current_loc);
    soundness := false
  end

exception StopTheWorld
let waitWhat s =
  print_msg s (!Tracing.current_loc);
  raise StopTheWorld

let report_lin_hashtbl  = Hashtbl.create 10

let report ?loc:(loc= !Tracing.current_loc) msg =
  if !GU.should_warn then begin
    if (Hashtbl.mem report_lin_hashtbl (msg,loc) == false) then
      begin
        print_msg msg loc;
        Hashtbl.add report_lin_hashtbl (msg,loc) true
      end
  end

let report_error msg =
  if !GU.should_warn then begin
    let loc = !Tracing.current_loc in
    print_err msg loc
  end

let with_context msg = function
  | Some ctx when GobConfig.get_bool "dbg.warn_with_context" -> msg ^ " in context " ^ string_of_int (Hashtbl.hash ctx)
  | _ -> msg

let warn_str_hashtbl = Hashtbl.create 10
let warn_lin_hashtbl = Hashtbl.create 10

let warn ?ctx msg =
  if !GU.should_warn then begin
    let msg = with_context msg ctx in
    if (Hashtbl.mem warn_str_hashtbl msg == false) then
      begin
        warn_all msg;
        Hashtbl.add warn_str_hashtbl msg true
      end
  end

let warn_each ?ctx msg =
  if !GU.should_warn then begin
    let loc = !Tracing.current_loc in
    let msg = with_context msg ctx in
    if (Hashtbl.mem warn_lin_hashtbl (msg,loc) == false) then
      begin
        warn_all msg;
        Hashtbl.add warn_lin_hashtbl (msg,loc) true
      end
  end

(*
let warn_each_ctx ctx msg = (* cyclic dependency... *)
  if not @@ GobConfig.get_bool "dbg.warn_with_context" then warn_each msg else
  (* let module S = (val Control.get_spec ()) in *)
  (* warn_each (msg ^ " in context " ^ S.C.short 99999 (Obj.obj ctx.context ())) *)
  (* warn_each (msg ^ " in context " ^ IO.to_string S.C.printXml (Obj.obj ctx.context ())) *)
  warn_each (msg ^ " in context " ^ string_of_int (Hashtbl.hash (Obj.obj ctx.context ())))
*)

let debug msg =
  if (get_bool "dbg.debug") then warn ("{BLUE}"^msg)

let debug_each msg =
  if (get_bool "dbg.debug") then warn_each ("{blue}"^msg)

let mywarn event_type certainty : unit =
  let log_event = {event_type: event_type; certainty: certainty} in
  let msg = show_log_event log_event in
  warn msg

let mywarn_each event_type certainty : unit =
  let log_event = {event_type: event_type; certainty: certainty} in
  let msg = show_log_event log_event in
  warn_each msg

include Tracing
