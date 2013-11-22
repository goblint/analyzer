open Cil
open Pretty
open Htmlutil
open Htmldump
open GobConfig
module GU = Goblintutil

exception Bailure of string
let bailwith s = raise (Bailure s)

let warnings = ref false
let soundness = ref true
let warn_out = ref stdout
let tracing = Config.tracing

(*Warning files*)
let warn_race = if get_bool "ana.osek.warnfiles" then ref (open_out "goblint_warnings_race.txt") else warn_out
let warn_safe = if get_bool "ana.osek.warnfiles" then ref (open_out "goblint_warnings_safe.txt") else warn_out
let warn_higr = if get_bool "ana.osek.warnfiles" then ref (open_out "goblint_warnings_highreadrace.txt") else warn_out
let warn_higw = if get_bool "ana.osek.warnfiles" then ref (open_out "goblint_warnings_highwriterace.txt") else warn_out
let warn_lowr = if get_bool "ana.osek.warnfiles" then ref (open_out "goblint_warnings_lowreadrace.txt") else warn_out
let warn_loww = if get_bool "ana.osek.warnfiles" then ref (open_out "goblint_warnings_lowwriterace.txt") else warn_out



let get_out name alternative = match get_string "dbg.dump" with
  | "" -> alternative
  | path -> open_out (Filename.concat path (name ^ ".out"))

let xml_warn = Hashtbl.create 10  

let print_msg msg loc = 
  if ((get_string "result") = "html") then htmlGlobalWarningList := (!htmlGlobalWarningList)@[(loc.file,loc.line,msg)];
  if get_bool "gccwarn" then    
    Printf.printf "%s:%d:0: warning: %s\n" loc.file loc.line msg
  else if get_bool "exp.eclipse" then 
    Printf.printf "WARNING /-/ %s /-/ %d /-/ %s\n%!" loc.file loc.line msg
  else
    Printf.fprintf !warn_out "%s (%s:%d)\n%!" msg loc.file loc.line

let print_err msg loc = 
  if ((get_string "result") = "html") then htmlGlobalWarningList := (!htmlGlobalWarningList)@[(loc.file,loc.line,msg)];
  if get_bool "gccwarn" then    
    Printf.printf "%s:%d:0: error: %s\n" loc.file loc.line msg
  else if get_bool "exp.eclipse" then 
    Printf.printf "WARNING /-/ %s /-/ %d /-/ %s\n%!" loc.file loc.line msg
  else
    Printf.fprintf !warn_out "%s (%s:%d)\n%!" msg loc.file loc.line


let print_group group_name errors =
  (* Add warnings to global warning list *)
  if ((get_string "result") = "html") then List.iter (fun (msg,loc) -> htmlGlobalWarningList := (!htmlGlobalWarningList)@[(loc.file,loc.line,(group_name^" : "^msg))];() ) errors;
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

let report msg = 
  if not !GU.may_narrow then begin
    let loc = !Tracing.current_loc in
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
  if (get_bool "dbg.debug") then warn msg

include Tracing
