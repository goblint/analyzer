open Cil
open Pretty
module GU = Goblintutil

exception Bailure of string
let bailwith s = raise (Bailure s)

let tracing = false  (* Hopefully, when set to false optimizations will kick in *)
let warnings = ref false
let soundness = ref true
let warn_out = ref stdout

let get_out name alternative = match !GU.dump_path with
  | Some path -> open_out (Filename.concat path (name ^ ".out"))
  | _ -> alternative

let current_loc = GU.current_loc

let print_msg msg loc = 
  if !GU.gccwarn then    
    Printf.printf "%s:%d:0: warning: %s\n" loc.file loc.line msg
  else if !Goblintutil.eclipse then 
    Printf.printf "WARNING /-/ %s /-/ %d /-/ %s\n%!" loc.file loc.line msg
  else
    Printf.fprintf !warn_out "%s (%s:%d)\n%!" msg loc.file loc.line

let print_err msg loc = 
  if !GU.gccwarn then    
    Printf.printf "%s:%d:0: error: %s\n" loc.file loc.line msg
  else if !Goblintutil.eclipse then 
    Printf.printf "WARNING /-/ %s /-/ %d /-/ %s\n%!" loc.file loc.line msg
  else
    Printf.fprintf !warn_out "%s (%s:%d)\n%!" msg loc.file loc.line


let print_group group_name errors =
  if !Goblintutil.eclipse then
    List.iter (fun (msg,loc) -> print_msg (group_name ^ ", " ^ msg) loc) errors
  else
    let f (msg,loc): doc = Pretty.dprintf "%s (%s:%d)" msg loc.file loc.line in
      ignore (Pretty.fprintf !warn_out "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)

let warn_urgent msg = 
  if not !GU.may_narrow then begin
    soundness := false;
    print_msg msg (!current_loc)
  end
  
let write msg =
  print_msg msg !current_loc

let warn_all msg = 
  if not !GU.may_narrow then begin
    if !warnings then 
      print_msg msg (!current_loc);
    soundness := false
  end
  
let report_lin_hashtbl  = Hashtbl.create 10

let report msg = 
  if not !GU.may_narrow then begin
    let loc = !current_loc in
    if (Hashtbl.mem report_lin_hashtbl (msg,loc) == false) then
      begin
        print_msg msg loc;
        Hashtbl.add report_lin_hashtbl (msg,loc) true
      end
  end

let report_error msg = 
  if not !GU.may_narrow then begin
    let loc = !current_loc in
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
    let loc = !current_loc in
      if (Hashtbl.mem warn_lin_hashtbl (msg,loc) == false) then
        begin
        warn_all msg;
        Hashtbl.add warn_lin_hashtbl (msg,loc) true
        end
  end
  
let debug msg =
  if !GU.debug then warn msg

let trace n x = Trace.trace n (align++x++unalign)
let tracei n x = Trace.tracei n (align++x++unalign)
let traceu n x = Trace.traceu n (align++x++unalign)

let tracel sys doc = 
  let loc = !current_loc in
  let docloc = text loc.file ++ text ":" ++ num loc.line ++ line ++ indent 2 doc in
    Trace.trace sys docloc

let traceli sys doc =
  let loc = !current_loc in
  let doc = text loc.file ++ text ":" ++ num loc.line ++ line ++ indent 2 doc in
    Trace.tracei sys doc

