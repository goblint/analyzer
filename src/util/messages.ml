open Cil
open Pretty
open GobConfig
module GU = Goblintutil

type array_oob =
  | PastEnd
  | BeforeStart
  | Unknown

type undefined_behavior =
  | ArrayOutOfBounds of array_oob
  | NullPointerDereference
  | UseAfterFree

type behavior =
  | Undefined of undefined_behavior
  | Implementation
  | Machine

type integer = Overflow | DivByZero

type cast = TypeMismatch

type warning =
  | Behavior of behavior
  | Integer of integer
  | Race
  | Cast of cast
  | Unknown of string
  | Debug of string
  | Analyzer

module Warning =
struct
  type t = warning

  module Behavior =
  struct
    type t = behavior

    let create (e: t): warning = Behavior e
    let undefined e: warning = create @@ Undefined e
    let implementation (): warning = create @@ Implementation
    let machine (): warning = create @@ Machine

    module Undefined =
    struct
      type t = undefined_behavior

      let create (e: t): warning = undefined e
      let array_out_of_bounds e: warning = create @@ ArrayOutOfBounds e
      let nullpointer_dereference (): warning = create @@ NullPointerDereference
      let use_after_free (): warning = create @@ UseAfterFree

      module ArrayOutOfBounds =
      struct
        type t = array_oob

        let create (e: t): warning = array_out_of_bounds e
        let past_end (): warning = create PastEnd
        let before_start (): warning = create BeforeStart
        let unknown (): warning = create Unknown

        let show (e: t): string =
          match e with
          | PastEnd -> "[PastEnd]  Index is past the end of the array."
          | BeforeStart -> "[BeforeStart] Index is before start of the array."
          | Unknown -> "[Unknown] Not enough information about index."
      end

      let show (e: t): string =
        match e with
        | ArrayOutOfBounds e -> Printf.sprintf "[Array out of bounds] %s" (ArrayOutOfBounds.show e)
        | NullPointerDereference -> "[Null pointer dereference]"
        | UseAfterFree -> "[Use After Free]"
    end

    let show (e: t): string =
      match e with
      | Undefined u -> Printf.sprintf "[Undefined] %s" (Undefined.show u)
      | Implementation -> "[Implementation]"
      | Machine -> "[Machine]"
  end

  module Integer =
  struct
    type t = integer

    let create (e: t): warning = Integer e
    let overflow (): warning = create Overflow
    let div_by_zero (): warning = create DivByZero

    let show (e: t): string =
      match e with
      | Overflow -> "[Overflow]"
      | DivByZero -> "[DivByZero]"
  end

  module Cast =
  struct
    type t = cast

    let create (e: t): warning = Cast e
    let type_mismatch (): warning = create TypeMismatch

    let show (e: t): string =
      match e with
      | TypeMismatch -> "[TypeMismatch]"
  end

  let to_string e =
    match e with
    | Behavior _ -> "behavior"
    | Integer _ -> "integer"
    | Race -> "race"
    | Array _ -> "array"
    | Cast _ -> "cast"
    | Unknown msg -> "unknown"
    | Debug msg -> "debug"
    | Analyzer -> "analyzer"

  let should_warn e =
    get_bool ("warn." ^ (to_string e))

  let show e =
    match e with
    | Behavior behavior -> Printf.sprintf "[Behavior] %s" (Behavior.show behavior)
    | Integer _ -> "[Integer]"
    | Race -> "[Race]"
    | Array _ -> "[Array]"
    | Cast _ -> "[Cast]"
    | Unknown msg -> Printf.sprintf "[Unknown] %s" msg
    | Debug msg -> Printf.sprintf "[Debug] %s" msg
    | Analyzer -> "[Analyzer]"
end

module Certainty = struct
  type t = May | Must

  let to_string e =
    match e with
    | May -> "may"
    | Must -> "must"

  let should_warn e =
    get_bool ("warn." ^ (to_string e))

  let show c =
    match c with
    | May -> "[MAY]"
    | Must -> "[MUST]"
end

module WarningWithCertainty =
struct
  type t = {
    warn_type : Warning.t;
    certainty: Certainty.t option
  }

  let should_warn (e:t) = Warning.should_warn e.warn_type && (match e.certainty with Some c -> Certainty.should_warn c | _ -> true)

  let debug msg = {warn_type = Debug msg; certainty = None}

  let create ?must:(must=false) w = {warn_type = w; certainty = Some (if must then Certainty.Must else Certainty.May)}
  let show {warn_type; certainty} =
    let certainty_str = match certainty with
      | Some c -> (Certainty.show c) ^ " "
      | None -> ""
    in
    Printf.sprintf "%s%s" certainty_str (Warning.show warn_type)
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

let warn_internal ?ctx (warning: WarningWithCertainty.t) =
  if !GU.should_warn && (WarningWithCertainty.should_warn warning) then begin
    let msg = WarningWithCertainty.show warning in
    let msg = with_context msg ctx in
    if (Hashtbl.mem warn_str_hashtbl msg == false) then
      begin
        warn_all msg;
        Hashtbl.add warn_str_hashtbl msg true
      end
  end

let warn_internal_with_loc ?ctx (warning: WarningWithCertainty.t) =
  if !GU.should_warn && (WarningWithCertainty.should_warn warning) then begin
    let loc = !Tracing.current_loc in
    let msg = WarningWithCertainty.show warning in
    let msg = with_context msg ctx in
    if (Hashtbl.mem warn_lin_hashtbl (msg,loc) == false) then
      begin
        warn_all msg;
        Hashtbl.add warn_lin_hashtbl (msg,loc) true
      end
  end

let warn ?must:(must=false) ?ctx (warning: Warning.t) =
  warn_internal ~ctx:ctx (WarningWithCertainty.create ~must:must warning)

let warn_each ?must:(must=false) ?ctx (warning: Warning.t) =
  warn_internal_with_loc ~ctx:ctx (WarningWithCertainty.create ~must:must warning)

let debug msg =
  if (get_bool "dbg.debug") then warn_internal (WarningWithCertainty.debug ("{BLUE}"^msg))

let debug_each msg =
  if (get_bool "dbg.debug") then warn_internal_with_loc (WarningWithCertainty.debug ("{blue}"^msg))

let mywarn () =
  warn (Warning.Behavior.Undefined.nullpointer_dereference ())

include Tracing
