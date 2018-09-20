open Cil
open Pretty
open Htmlutil
open Htmldump
open GobConfig
module GU = Goblintutil

exception Bailure of string
let bailwith s = raise (Bailure s)

type warning_class = Warn | MayWarn | Recommendation
(* CWE-ID | CERT Rule *)
type reference = CWE of int | CERT of string | NoRef
(* class * id * desc * reference * loc *)
type warning = warning_class * string * string * reference * location
type debug_msg = string * location

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

let colorize ?on:(on=get_bool "colors") msg =
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
  if (get_string "result") = "html" then htmlGlobalWarningList := (loc.file,loc.line,msg)::!htmlGlobalWarningList;
  if (get_string "result") = "fast_xml" then warning_table := (`text (msg,loc))::!warning_table;
  if get_bool "gccwarn" then
    Printf.printf "%s:%d:0: warning: %s\n" loc.file loc.line msg
  else
    let color = if get_bool "colors" then "{violet}" else "" in
    let s = Printf.sprintf "%s %s(%s:%d)" msgc color loc.file loc.line in
    Printf.fprintf !warn_out "%s\n%!" (colorize s)

let print_err msg loc =
  if (get_string "result") = "html" then htmlGlobalWarningList := (loc.file,loc.line,msg)::!htmlGlobalWarningList;
  if (get_string "result") = "fast_xml" then warning_table := (`text (msg,loc))::!warning_table;
  if get_bool "gccwarn" then
    Printf.printf "%s:%d:0: error: %s\n" loc.file loc.line msg
  else
    Printf.fprintf !warn_out "%s (%s:%d)\n%!" msg loc.file loc.line


let print_group group_name errors =
  (* Add warnings to global warning list *)
  if (get_string "result") = "html" then List.iter (fun (msg,loc) -> htmlGlobalWarningList := (loc.file,loc.line,(group_name^" : "^msg))::!htmlGlobalWarningList ) errors;
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
  if not !GU.may_narrow then begin
    soundness := false;
    print_msg msg (!Tracing.current_loc)
  end

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

let with_context msg = function
  | Some ctx when GobConfig.get_bool "dbg.warn_with_context" -> msg ^ " in context " ^ string_of_int (Hashtbl.hash ctx)
  | _ -> msg

let warn_str_hashtbl = Hashtbl.create 10
let warn_lin_hashtbl = Hashtbl.create 10

let warn ?ctx msg =
  if not !GU.may_narrow then begin
    let msg = with_context msg ctx in
    if (Hashtbl.mem warn_str_hashtbl msg == false) then
      begin
        warn_all msg;
        Hashtbl.add warn_str_hashtbl msg true
      end
  end

let warn_each ?ctx msg =
  if not !GU.may_narrow then begin
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


module type Pool =
sig
  val name : string 
  val report : ?loc:location -> ?wclass:warning_class -> ?reference:reference -> string -> string -> unit
  val debug : ?loc:location -> string -> unit
  val get_warnings : unit -> warning list
  val get_debug_msgs : unit -> debug_msg list
end

module MkPool (N : sig val name : string end) : Pool = struct

  let warnings   : warning list ref   = ref []
  let debug_msgs : debug_msg list ref = ref []

  let name = N.name

  let report ?loc:(loc= !Tracing.current_loc) ?wclass:(wclass=Warn) ?reference:(reference=NoRef) id desc =
    let w = (wclass, id, desc, reference, loc) in
    if not (List.mem w !warnings) then 
      warnings := w::!warnings

  let debug ?loc:(loc= !Tracing.current_loc) msg =
    let d = (msg, loc) in
    if not (List.mem d !debug_msgs) then
      debug_msgs := d::!debug_msgs

  let get_warnings () =
    warnings := List.sort (
      fun (_,_,_,_,x) (_,_,_,_,y) -> Cil.compareLoc x y) !warnings;
    !warnings

  let get_debug_msgs () = 
    debug_msgs := List.sort (fun (_,x) (_,y) -> Cil.compareLoc x y) !debug_msgs;
    !debug_msgs

end

let color_print s = Printf.printf "%s\n" (colorize s)

let cpad ?sym:(sym=' ') i s =
  let d = i - (String.length s) in
  let d1,d2 = (if d mod 2 == 0 then (d/2,d/2) else (d/2)+1,(d/2)) in
  if d > 0 then
    (String.make (d1) sym)^s^(String.make (d2) sym)
  else
    s

let rpad ?sym:(sym=' ') i s =
  let d = i - (String.length s) in
  if d > 0 then
    (String.make d sym)^s
  else
    s
 
let print_title ?col:(col="{red}") ?sym:(sym='-') title =
  color_print (col^(cpad ~sym:sym 80 (" "^title^" ")))

let string_of_loc loc =
  Printf.sprintf "({violet}%s:%d{white})" loc.file loc.line

let shorten_string i s =
  let d = i - (String.length s) - 3 in "..."^(String.sub s 0 d)

let print_warnings analysis_name warnings =
  let print_counter = ref 0 in 
  let only_warn_l = GobConfig.get_list "dbg.only-warn" in
  let ignore_warn_l = GobConfig.get_list "dbg.ignore-warn" in
  let id_pad_len = max 10 (2+(List.fold_left (
    fun acc (_,id,_,_,_) -> max acc (String.length id)
  ) 0 warnings)) in 
  let string_of_wc = function
    | Warn -> ""
    | MayWarn -> "{yellow}may {white}"
    | Recommendation -> "{yellow}recommend {white}"
  in
  let string_of_id id = ("{blue}"^(rpad id_pad_len id)^"{white}: ")
  in
  let string_of_ref = function
    | CWE id -> "(CWE-ID "^(string_of_int id)^") "
    | CERT id -> "(CERT "^id^") "
    | NoRef -> ""
  in
  let get_linenumber s = 
    try
      match (String.split_on_char '@' s) with
        | [x;y] -> x, int_of_string y
        | _ -> s, -1
    with _ -> s, -1
  in
  let in_conf_var (_,id,_,_,loc) conf_l =
    List.exists (
      fun x -> match x with Json.String s -> 
        let iid, lin = get_linenumber s in 
        (* ID fits list ID *)
        (id=iid) &&
        (* Linenumber is -1 or fits warning linenumber *)
        (lin = -1 || loc.line=lin)
      | _ -> false
    ) conf_l
  in
  let is_shown warn = 
    (* dbg.only-warn is empty => warning is shown *)
    List.length only_warn_l = 0 ||
    in_conf_var warn only_warn_l
  in
  let is_ignored warn = in_conf_var warn ignore_warn_l 
  in
  let rec print_description ?sym:(sym=' ') pad_len desc =
    let desc = (String.make pad_len sym)^" "^desc in
    if String.length desc <= 80 then
      desc
    else
      (String.sub desc 0 80)^"\n"^(print_description pad_len (String.sub desc 80 ((String.length desc)-80)))
  in
  let print_warning (wc,id,de,re,loc as warn) =
    let str = (string_of_id id)^(string_of_wc wc)^(string_of_ref re)^(string_of_loc loc) in
    let str = str^"\n"^print_description (id_pad_len+1) de in
    if (is_shown warn) && not (is_ignored warn) then 
      begin 
        color_print (str^"\n"); 
        incr print_counter
      end
  in
  print_title analysis_name;
  List.iter print_warning warnings;
  if (List.length only_warn_l) + (List.length ignore_warn_l) != 0 then
    color_print ("{white}  "^(string_of_int !print_counter)^" of "
    ^(string_of_int (List.length warnings))^" warnings shown\n")

let print_dbg_msgs analysis_name msgs =
  let print_dbg_msg (msg, loc) =
    let str = msg^(string_of_loc loc) in
    color_print str
  in
  print_title analysis_name;
  List.iter print_dbg_msg msgs

let pools : (module Pool) list ref = ref []

let register_pool (module P : Pool) =
  pools := (module P : Pool)::!pools

let print_pools () = 
  let debug_title_printed = ref false in 
  List.iter (
    fun (module P : Pool) ->
      let warnings = P.get_warnings () in
      if List.length warnings > 0 then print_warnings P.name warnings
  ) !pools;
  if GobConfig.get_bool "dbg.debug" then (
      List.iter (
        fun (module P : Pool) ->
          let msgs = P.get_debug_msgs () in
          if List.length msgs > 0 then 
            begin 
              (if not !debug_title_printed then 
                print_title ~sym:'#' "DEBUG"; 
                debug_title_printed := true);
              print_dbg_msgs P.name msgs
            end
      ) !pools 
    )

include Tracing
