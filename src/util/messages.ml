open Cil
open Pretty
open GobConfig
module GU = Goblintutil

module ArrayOOB =
struct
  type t =
    | PastEnd
    | BeforeStart
    | Unknown

  let show e =
    match e with
    | PastEnd -> "Index is past the end of the array."
    | BeforeStart -> "Index is before start of the array."
    | Unknown -> "Not enough information about index."
end

module UndefinedBehavior =
struct
  type t =
    | ArrayOutOfBounds of ArrayOOB.t
    | NullPointerDereference
    | UseAfterFree

  let show e =
    match e with
    | ArrayOutOfBounds e -> Printf.sprintf "[Array out of bounds] %s" (ArrayOOB.show e)
    | NullPointerDereference -> "[Null pointer dereference]"
    | UseAfterFree -> "[Use After Free]"
end

module BehaviorEvent =
struct
  type t =
    | Undefined of UndefinedBehavior.t
    | Implementation
    | Machine

  let show e =
    match e with
    | Undefined u -> Printf.sprintf "[Undefined] %s" (UndefinedBehavior.show u)
    | Implementation -> "[Implementation]"
    | Machine -> "[Machine]"
end

module IntegerEvent =
struct
  type t = Overflow | DivByZero
end

module CastEvent =
struct
  type t = TypeMismatch
end

module ArrayEvent =
struct
  type t = OutOfBounds of int*int
end

module EventType =
struct
  type t =
    | Behavior of BehaviorEvent.t
    | Integer of IntegerEvent.t
    | Race
    | Array of ArrayEvent.t
    | Cast of CastEvent.t
    | Unknown of string
    | Debug of string

  let to_string e =
    match e with
    | Behavior _ -> "behavior"
    | Integer _ -> "integer"
    | Race -> "race"
    | Array _ -> "array"
    | Cast _ -> "cast"
    | Unknown msg -> "unknown"
    | Debug msg -> "debug"

  let should_warn e =
    get_bool ("dbg.warn." ^ (to_string e))

  let show e =
    match e with
    | Behavior behavior -> Printf.sprintf "[Behavior] %s" (BehaviorEvent.show behavior)
    | Integer _ -> "[Integer]"
    | Race -> "[Race]"
    | Array _ -> "[Array]"
    | Cast _ -> "[Cast]"
    | Unknown msg -> Printf.sprintf "[Unknown] %s" msg
    | Debug msg -> Printf.sprintf "[Debug] %s" msg
end

module Certainty = struct
  type t = May | Must

  let to_string e =
    match e with
    | May -> "may"
    | Must -> "must"

  let should_warn e =
    get_bool ("dbg.warn." ^ (to_string e))

  let show c =
    match c with
    | May -> "[MAY]"
    | Must -> "[MUST]"
end

module LogEvent =
struct
  type t = {
    event_type : EventType.t;
    certainty: Certainty.t option
  }

  let may e = {event_type = e; certainty = Some Certainty.May}
  let must e = {event_type = e; certainty = Some Certainty.Must}
  let debug msg = {event_type = EventType.Debug msg; certainty = None}

  let should_warn (e:t) = EventType.should_warn e.event_type && (match e.certainty with Some c -> Certainty.should_warn c | _ -> true)

  let create e c = {event_type = e; certainty = c}
  let show {event_type; certainty} =
    let certainty_str = match certainty with
      | Some c -> (Certainty.show c) ^ " "
      | None -> ""
    in
    Printf.sprintf "%s%s" certainty_str (EventType.show event_type)
end

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


let mywarn ?ctx (log_event: LogEvent.t) =
  if !GU.should_warn && (LogEvent.should_warn log_event) then begin
    let msg = LogEvent.show log_event in
    let msg = with_context msg ctx in
    if (Hashtbl.mem warn_str_hashtbl msg == false) then
      begin
        warn_all msg;
        Hashtbl.add warn_str_hashtbl msg true
      end
  end

let mywarn_each ?ctx (log_event: LogEvent.t) =
  if !GU.should_warn && (LogEvent.should_warn log_event) then begin
    let loc = !Tracing.current_loc in
    let msg = LogEvent.show log_event in
    let msg = with_context msg ctx in
    if (Hashtbl.mem warn_lin_hashtbl (msg,loc) == false) then
      begin
        warn_all msg;
        Hashtbl.add warn_lin_hashtbl (msg,loc) true
      end
  end

let debug msg =
  if (get_bool "dbg.debug") then mywarn (LogEvent.debug ("{BLUE}"^msg))

let debug_each msg =
  if (get_bool "dbg.debug") then mywarn_each (LogEvent.debug ("{blue}"^msg))

include Tracing
