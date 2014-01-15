open Cil
open Pretty
open Htmlutil
open Htmldump
open GobConfig
module GU = Goblintutil

exception Bailure of string
let bailwith s = raise (Bailure s)

let warning_table : [`text of string * location | `group of string * ((string * location) list)] list ref = ref []
let warnings = ref false
let soundness = ref true
let warn_out = ref stdout
let tracing = Config.tracing

(*Warning files*)
let warn_race = ref stdout
let warn_safe = ref stdout
let warn_higr = ref stdout
let warn_higw = ref stdout
let warn_lowr = ref stdout
let warn_loww = ref stdout

(*let warn_race = if get_bool "ana.osek.warnfiles" then ref (open_out "goblint_warnings_race.txt") else warn_out
let warn_safe = if get_bool "ana.osek.warnfiles" then ref (open_out "goblint_warnings_safe.txt") else warn_out
let warn_higr = if get_bool "ana.osek.warnfiles" then ref (open_out "goblint_warnings_highreadrace.txt") else warn_out
let warn_higw = if get_bool "ana.osek.warnfiles" then ref (open_out "goblint_warnings_highwriterace.txt") else warn_out
let warn_lowr = if get_bool "ana.osek.warnfiles" then ref (open_out "goblint_warnings_lowreadrace.txt") else warn_out
let warn_loww = if get_bool "ana.osek.warnfiles" then ref (open_out "goblint_warnings_lowwriterace.txt") else warn_out*)


let init_warn_files () =
  warn_race := (open_out "goblint_warnings_race.txt");
  warn_safe := (open_out "goblint_warnings_safe.txt");
  warn_higr := (open_out "goblint_warnings_highreadrace.txt");
  warn_higw := (open_out "goblint_warnings_highwriterace.txt");
  warn_lowr := (open_out "goblint_warnings_lowreadrace.txt");
  warn_loww := (open_out "goblint_warnings_lowwriterace.txt");
  ()

let get_out name alternative = match get_string "dbg.dump" with
  | "" -> alternative
  | path -> open_out (Filename.concat path (name ^ ".out"))

let xml_warn : (location, (string*string) list) Hashtbl.t = Hashtbl.create 10  

let colorize ?on:(on=get_bool "colors") msg =
  let colors = [("gray", "30"); ("red", "31"); ("green", "32"); ("yellow", "33"); ("blue", "34");
    ("violet", "35"); ("turquoise", "36"); ("white", "37"); ("reset", "0;00")] in
  let replace msg (color,code) =
    let msg = Str.global_replace (Str.regexp ("{"^color^"}")) (if on then "\027[0;"^code^"m" else "") msg in (* normal *)
    Str.global_replace (Str.regexp ("{"^String.uppercase color^"}")) (if on then "\027[1;"^code^"m" else "") msg (* bold *)
  in
  let msg = List.fold_left replace msg colors in
  msg^(if on then "\027[0;0;00m" else "") (* reset at end *)

let print_msg msg loc = 
  let msgc = colorize msg in
  let msg  = colorize ~on:false msg in
  if (get_string "result") = "html" then htmlGlobalWarningList := (loc.file,loc.line,msg)::!htmlGlobalWarningList;
  if (get_string "result") = "fast_xml" then warning_table := (`text (msg,loc))::!warning_table;
  if get_bool "gccwarn" then    
    Printf.printf "%s:%d:0: warning: %s\n" loc.file loc.line msg
  else if get_bool "exp.eclipse" then 
    Printf.printf "WARNING /-/ %s /-/ %d /-/ %s\n%!" loc.file loc.line msg
  else
    Printf.fprintf !warn_out (if get_bool "colors" then "%s \027[30m(%s:%d)\027[0;0;00m\n%!" else "%s (%s:%d)\n%!") msgc loc.file loc.line

let print_err msg loc = 
  if (get_string "result") = "html" then htmlGlobalWarningList := (loc.file,loc.line,msg)::!htmlGlobalWarningList;
  if (get_string "result") = "fast_xml" then warning_table := (`text (msg,loc))::!warning_table;
  if get_bool "gccwarn" then    
    Printf.printf "%s:%d:0: error: %s\n" loc.file loc.line msg
  else if get_bool "exp.eclipse" then 
    Printf.printf "WARNING /-/ %s /-/ %d /-/ %s\n%!" loc.file loc.line msg
  else
    Printf.fprintf !warn_out "%s (%s:%d)\n%!" msg loc.file loc.line


let print_group group_name errors =
  (* Add warnings to global warning list *)
  if (get_string "result") = "html" then List.iter (fun (msg,loc) -> htmlGlobalWarningList := (loc.file,loc.line,(group_name^" : "^msg))::!htmlGlobalWarningList ) errors;
  if (get_string "result") = "fast_xml" then warning_table := (`group (group_name,errors))::!warning_table;
  if get_bool "exp.eclipse" then
    List.iter (fun (msg,loc) -> print_msg (group_name ^ ", " ^ msg) loc) errors
  else
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
  if not !GU.may_narrow then begin
    soundness := false;
    print_msg msg (!Tracing.current_loc)
  end
  
let write msg =
  print_msg msg !Tracing.current_loc

let warn_all msg = 
  if not !GU.may_narrow then begin
    if !warnings then 
      print_msg msg (!Tracing.current_loc);
    soundness := false
  end
  
let worldStopped = ref false
exception StopTheWorld
let waitWhat s = 
  worldStopped := true;
  warn_urgent s;
  raise StopTheWorld
  
let report_lin_hashtbl  = Hashtbl.create 10

let report ?loc:(loc= !Tracing.current_loc) msg = 
  if not !GU.may_narrow then begin
    if (Hashtbl.mem report_lin_hashtbl (msg,loc) == false) then
      begin
        print_msg msg loc;
        Hashtbl.add report_lin_hashtbl (msg,loc) true
      end
  end

let report_error msg = 
  if not !GU.may_narrow then begin
    let loc = !Tracing.current_loc in
		  print_err msg loc
  end	
		  
let warn_str_hashtbl = Hashtbl.create 10
let warn_lin_hashtbl = Hashtbl.create 10

let warn msg = 
  if not !GU.may_narrow then begin
    if (Hashtbl.mem warn_str_hashtbl msg == false) then
      begin
        warn_all msg;
        Hashtbl.add warn_str_hashtbl msg true
      end
  end
  
let warn_each msg = 
  if not !GU.may_narrow then begin
    let loc = !Tracing.current_loc in
      if (Hashtbl.mem warn_lin_hashtbl (msg,loc) == false) then
        begin
        warn_all msg;
        Hashtbl.add warn_lin_hashtbl (msg,loc) true
        end
  end
  
let debug msg =
  if (get_bool "dbg.debug") then warn (colorize ("{BLUE}"^msg))
  
let debug_each msg =
  if (get_bool "dbg.debug") then warn_each (colorize ("{blue}"^msg))

include Tracing
