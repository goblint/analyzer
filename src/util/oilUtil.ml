open Cil
open Messages
open GobConfig
open Pretty

type attribute_v = Name of (string * ( ((string * attribute_v) list) option)) | Bool of (bool * (((string * attribute_v) list) option)) | Int of int | Float of float | String of string | Auto
and param_t = string * attribute_v [@@deriving show]
type object_t = string*string*(param_t list)
(*		id	pry 	"lock"*)
type res_t = 	string*	int*	exp
(*		id	timed*)
type event_t = 	string*	bool
(*		interruptible	pry	res		events 		timetriggered	autostart	activation*)
type task_t = 	bool*		int*	(string list)*	(string list)*	bool*		bool*		int
(*		pry	res		category *)
type isr_t = 	int*	(string list)*	int
(* id accessing_applications lock *)
type spinlock_t = string * string list * exp

let osek_renames = ref ""
let osek_ids = ref ""
let header = "osek_goblint.h"
let osek_names : (string,string) Hashtbl.t = Hashtbl.create 16
let osek_ISR_PRIORITY = ref ["PRIORITY"; "INTERRUPTPRIORITY"] (*add fancy priority names here*)
let osek_API_funs = ["ActivateTask"; "TerminateTask"; "ChainTask"; "Schedule"; "GetTaskID"; "GetTaskState"; "DisableAllInterrupts"; "EnableAllInterrupts"; "SuspendAllInterrupts"; "ResumeAllInterrupts"; "SuspendOSInterrupts"; "ResumeOSInterrupts"; "GetResource"; "ReleaseResource"; "SetEvent"; "GetEvent"; "ClearEvent"; "WaitEvent"; "GetAlarmBase"; "GetAlarm"; "SetRelAlarm"; "SetAbsAlarm"; "CancelAlarm"; "GetActiveApplicationMode"; "StartOS"; "ShutdownOS"; "GetSpinlock"; "ReleaseSpinlock"]
(* let safe_vars = ref false *)

(*let group_pry : (string,int) Hashtbl.t = Hashtbl.create 16
  let open_isr = ref []
  let osek_ISR_GROUP = ref ["GROUP"]
  let osek_ISR_GROUPPRY = ref ["GROUPPRIORITY"]*)


(* boolean flags *)
let startuphook = ref false
let shutdownhook = ref false
let errorhook = ref false
let pretaskhook = ref false
let posttaskhook = ref false
let bcc1 = ref false
let bcc2 = ref false
let ecc1 = ref false
let ecc2 = ref false
let use_res_scheduler = ref false

(* object tables *)
let resources : (string,res_t) Hashtbl.t = Hashtbl.create 16
let events : (string,event_t) Hashtbl.t = Hashtbl.create 16
let tasks  : (string,task_t) Hashtbl.t = Hashtbl.create 16
let isrs   : (string,isr_t) Hashtbl.t = Hashtbl.create 16
let alarms   : (string,bool*(string list)) Hashtbl.t = Hashtbl.create 16

let spinlocks : (string,spinlock_t) Hashtbl.t = Hashtbl.create 16
let spinlockids : (Cil.exp,string) Hashtbl.t = Hashtbl.create 16

let resourceids : (Cil.exp,string) Hashtbl.t = Hashtbl.create 16 (*Const CInt64 ,_,_ *)
let eventids : (Cil.exp,string) Hashtbl.t = Hashtbl.create 16
let taskids  : (Cil.exp,string) Hashtbl.t = Hashtbl.create 16
let isrids   : (Cil.exp,string) Hashtbl.t = Hashtbl.create 16
let alarmids : (Cil.exp,string) Hashtbl.t = Hashtbl.create 16


(* start analysis here *)
let starting_tasks = ref ([] : string list)
let concurrent_tasks = ref ([] : string list)

let warned = ref ([] :string list)

(* adding the task pre and/or suffix *)
let enclose_string pre suf name = if startsWith pre name && endsWith suf name then name else pre ^ name ^ suf
let make_task name = enclose_string (get_string "ana.osek.taskprefix") (get_string "ana.osek.tasksuffix") name
let make_isr name = enclose_string (get_string "ana.osek.isrprefix") (get_string "ana.osek.isrsuffix") name
(* triming the task pre and/or suffix *)
let trim_task name = (Str.string_after (Str.string_before name ((String.length name) - (String.length (get_string "ana.osek.tasksuffix")))) (String.length (get_string "ana.osek.taskprefix")))
let trim_isr name = (Str.string_after (Str.string_before name ((String.length name) - (String.length (get_string "ana.osek.isrsuffix")))) (String.length (get_string "ana.osek.isrprefix")))
let trim name = if (Hashtbl.mem tasks name) then trim_task name else trim_isr name

(*priority function*)
let pry res = try let (_,pry,_) =Hashtbl.find resources res in pry with Not_found -> print_endline("Priority not found. Using default value -1");
  assert false
(*(-1)*)

let get_api_names name =
  if List.mem name osek_API_funs then name else begin
    try
      let res = Hashtbl.find osek_names name in
      if tracing then trace "osek" "Renameing %s to %s\n" name res;
      res
    with
    | Not_found ->
      if tracing then trace "osek" "API name for %s not found\n" name;
      name
    | e -> raise e
  end

let is_task f = ((Hashtbl.mem tasks f) || (Hashtbl.mem isrs f))
let is_task_res r = is_task (make_task r) || is_task (make_isr r)
let is_starting f = (List.mem f !concurrent_tasks) || (List.mem f !starting_tasks)

(*print id header *)
let generate_header () =
  let f = open_out (Fpath.(to_string (GoblintDir.preprocessed () / header))) in
  let print_resources id value = if not(is_task_res id) then output_string f ("int " ^ id ^ ";\n") else () in
  let print_events id value 	 = output_string f ("int " ^ id           ^ ";\n") in
  let print_tasks id value     = output_string f ("int " ^ trim_task id ^ ";\n") in
  let print_isrs id value      = output_string f ("int " ^ trim_isr id  ^ ";\n") in
  let print_alarms id value    = output_string f ("int " ^ id  ^ ";\n") in
  let print_spinlocks id value = output_string f ("int " ^ id  ^ ";\n") in
  let task_macro () =
    if (get_string "ana.osek.taskprefix") = "" then
      if (get_string "ana.osek.tasksuffix") = "" then
        "TaskName"
      else
        "TaskName##" ^ (get_string "ana.osek.tasksuffix")
    else
    if (get_string "ana.osek.tasksuffix") = "" then
      (get_string "ana.osek.taskprefix")^"##TaskName"
    else
      (get_string "ana.osek.taskprefix")^"##TaskName##" ^ (get_string "ana.osek.tasksuffix")
  in
  let isr_macro () =
    if (get_string "ana.osek.isrprefix") = "" then
      if (get_string "ana.osek.isrsuffix") = "" then
        "ISRName"
      else
        "ISRName##" ^ (get_string "ana.osek.isrsuffix")
    else
    if (get_string "ana.osek.isrsuffix") = "" then
      (get_string "ana.osek.isrprefix")^"##ISRName"
    else
      (get_string "ana.osek.isrprefix")^"##ISRName##" ^ (get_string "ana.osek.isrsuffix")
  in
  output_string f "#ifndef goblint\n";
  output_string f "#define goblint\n";
  Hashtbl.iter print_resources resources;
  Hashtbl.iter print_events events;
  if (get_string "ana.osek.taskprefix") <> "" || (get_string "ana.osek.tasksuffix") <> "" then begin
    Hashtbl.iter print_tasks tasks;
  end else begin
    (*       if tracing then output_string f "//No TASK prefix/suffix. Taskids not generated. ActivateTask might fail.\n"; *)
    if tracing then trace "osek" "//No TASK prefix/suffix. Taskids not generated. ActivateTask might fail.\n";
  end;
  if (get_string "ana.osek.isrprefix") <> "" || (get_string "ana.osek.isrsuffix") <> "" then begin
    Hashtbl.iter print_isrs isrs;
  end else begin
    (*       if tracing then output_string f "//No ISR prefix/suffix. Tasksids not generated. ActivateTask will fail.\n"; *)
    if tracing then trace "osek" "//No ISR prefix/suffix. Tasksids not generated. ActivateTask will fail.\n";
  end;
  Hashtbl.iter print_alarms alarms;
  Hashtbl.iter print_spinlocks spinlocks;
  output_string f "#endif\n";
  if (get_bool "ana.osek.def_header") then begin
    output_string f "#ifndef E_OK\n";
    output_string f "#define E_OK 0\n";
    output_string f "#endif\n";
    output_string f "#ifndef TASK\n";
    output_string f ("#define TASK( TaskName ) void " ^ task_macro () ^ "( void )\n");
    output_string f "#endif\n";
    output_string f "#ifndef ISR\n";
    output_string f ("#define ISR( ISRName ) void " ^ isr_macro() ^ "( void )\n");
    output_string f "#endif\n";
  end;
  close_out f

let finish_alarm_handling () =
  let doit id (active,task_list) =
    let doit_helper task =
      let helper (a,b,c,d,_,f,g) = (a,b,c,d,true,f,g) in
      (* print_endline ("Alarm: "^ id ^" and task: "^task);       *)
      Hashtbl.replace tasks task (helper (Hashtbl.find tasks task));
      concurrent_tasks :=  task :: !concurrent_tasks
    in
    if active then
      List.iter doit_helper task_list
    else
      ()
  in
  Hashtbl.iter doit alarms

let compare_objs obj1 obj2 =
  match obj1,obj2 with
  |(x,_,_),(y,_,_) when x=y -> 0
  |_,("OS",_,_) -> 1
  |("OS",_,_),_ -> -1
  |_,("RESOURCE",_,_) -> 1
  |("RESOURCE",_,_),_ -> -1
  |_,("EVENT",_,_) -> 1
  |("EVENT",_,_),_ -> -1
  |_,("ALARM",_,_) -> -1
  |("ALARM",_,_), _ -> 1
  | _,_ -> 0

let check_api_use cat fname task =
  if task = "???" then let _ = printf "Unable to determine task while checking api usage." in () else
    match cat with
    | 1 -> if Hashtbl.mem isrs task then
        let _,_,c = Hashtbl.find isrs task in if c = 1 then let _ = printf "OSEK function %s must not be called in the category 1 interrupt %s!" fname task in ()
    | 2 ->  if Hashtbl.mem isrs task then
        let _ = printf "OSEK function %s must not be called in the interrupt %s!" fname task in ()
    | 0 -> let _ = printf "OSEK function %s must not be called in the task/interupt %s!" fname task in ()
    | _ -> failwith "Wrong category specified for check_api_usage."

let get_lock name =
  if tracing then trace "osek" "Looking for lock %s\n" name;
  let _,_,lock = Hashtbl.find resources name in
  (*let _ = match lock with
    	  | AddrOf (Var varinfo,NoOffset) -> print_endline varinfo.vname
    in*)
  lock

let get_spinlock name =
  if tracing then trace "osek" "Looking for spinlock %s\n" name;
  let _,_,lock = Hashtbl.find spinlocks name in
  lock

let make_lock name =
  if tracing then trace "osek" "Generating lock for resource %s\n" name;
  let varinfo = Goblintutil.create_var (makeGlobalVar name voidType) in
  AddrOf (Var varinfo,NoOffset)

let make_spinlock name =
  if tracing then trace "osek" "Generating spinlock %s\n" name;
  let varinfo = Goblintutil.create_var (makeGlobalVar name voidType) in
  AddrOf (Var varinfo,NoOffset)

let find_name id =
  if tracing then trace "osek" "Looking up resource %s\n" id;
  let dummy = "" in
  let res = Hashtbl.fold (fun name v acc -> let a,b,c = v in (if a = id then name else acc)) resources dummy  in
  if tracing then trace "osek" "Found resource %s\n" res;
  if res = dummy then failwith ("No resource found for id "^ id ^ "!") else res

let check_res_decl res =
  if not(Hashtbl.mem resources res) then
    if tracing then trace "osek" "Resource %s is undeclared!\n" res

let check_event_decl ev =
  if not(Hashtbl.mem events ev) then
    if tracing then trace "osek" "Event %s is undeclared!\n" ev

let check_task task t_value =
  let (sched,pry,res_list,event_list,timetriggered,autostart,activation) = t_value in
  if (pry = -1) then (failwith ("No priority found for task "^ task ^ "!") );
  List.iter check_res_decl res_list;
  List.iter check_event_decl event_list

let check_isr min_cat2_pry min_cat1_pry isr i_value =
  let (pry,res_list,category) = i_value in
  if (pry = -1001) then (failwith ("No priority found for isr "^ isr ^ "!") );
  if (category = 2 && pry <= min_cat2_pry) then (failwith ("Priority of interrupt "^ isr ^ "below task priorities!") );
  if (category = 1 && pry <= min_cat1_pry) then (failwith ("Priority of category 1 interrupt "^ isr ^ "below category 2 priorities!") );
  List.iter check_res_decl res_list

let check_osek () =
  if tracing then trace "osek" "Checking conventions\n";
  let min_cat2_pry = pry "RES_SCHEDULER" in
  let min_cat1_pry = pry "SuspendOSInterrupts" in
  Hashtbl.iter (check_isr min_cat2_pry min_cat1_pry) isrs;
  Hashtbl.iter check_task  tasks

(*let check_tramp () =
  if tracing then trace "osek" "Checking trampoline IDs\n";
  let check_res_id res_name res_value = match res_value with
    | ("-1",_,_) ->let _ = printf "Warning: No ID found for resource %s!" res_name in ()
    | _ -> ()
  in
  Hashtbl.iter check_res_id resources;
  let check_ev_id ev_name ev_value = match ev_value with
    | ("-1",_) -> let _ = printf "Warning: No ID found for event %s!" ev_name in ()
    | _ -> ()
  in
  Hashtbl.iter check_ev_id events *)

let compute_ceiling_priority res r_value =
  let id,pry,lock = r_value in
  let max_pry_t res task t_value acc =
    let (_,p,res_list,_,_,_,_) = t_value in
    if (List.mem res res_list) then max p acc else acc
  in
  let max_pry_i res irpt i_value acc =
    let (p,res_list,_) = i_value in
    if (List.mem res res_list) then max p acc else acc
  in
  let pry' = Hashtbl.fold (max_pry_t res) tasks pry in
  let pry'' = Hashtbl.fold (max_pry_i res) isrs pry' in
  Hashtbl.replace resources res (id,pry'',lock)

let handle_attribute_os attr =
  let tmp, value = attr in
  let name = String.uppercase_ascii tmp in
  let get_bool value =
    match value with
    | Bool (x,_) -> x
    | _ -> false
  in
  match name with
  | "USERESSCHEDULER" -> use_res_scheduler := true
  | "STARTUPHOOK" -> startuphook := get_bool value
  | "SHUTDOWNHOOK" -> shutdownhook:= get_bool value
  | "ERRORHOOK" -> errorhook := get_bool value
  | "PRETASKHOOK" -> pretaskhook := get_bool value
  | "POSTTASKHOOK" -> posttaskhook := get_bool value
  | _ -> if tracing then trace "oil" "Unhandled OS attribute %s\n" name

let handle_attribute_task object_name t_value (attr : (string*attribute_v)) =
  let (sched,pry,res_list,event_list,timetriggered,autostart,activation) = t_value in
  let tmp, value = attr in
  let a_name = String.uppercase_ascii tmp in
  match a_name with
  | "SCHEDULE" -> (match value with
      | Name (sched,_)  -> ( match (String.uppercase_ascii sched) with  (*This should not occur *)
          | "NON"   ->false,pry,res_list,event_list,timetriggered,autostart,activation
          | "FULL"  ->true, pry,res_list,event_list,timetriggered,autostart,activation
          | other  ->
            if tracing then trace "oil" "Wrong value (%s) for attribute SCHEDULE of TASK %s\n" other object_name;
            t_value
        )
      | String sched  -> ( match (String.uppercase_ascii sched) with
          | "NON"   ->false,pry,res_list,event_list,timetriggered,autostart,activation
          | "FULL"  ->true, pry,res_list,event_list,timetriggered,autostart,activation
          | other  ->
            if tracing then trace "oil" "Wrong value (%s) for attribute SCHEDULE of TASK %s\n" other object_name;
            t_value
        )
      | _  ->
        if tracing then trace "oil" "Wrong value (_) for attribute SCHEDULE of TASK %s\n" object_name;
        t_value
    )
  | "PRIORITY" -> (match value with
      | Int p  -> if (p < 0) then begin
          if tracing then trace "oil" "Negative PRIORITY for TASK %s\n" object_name;
          t_value
        end
        else (sched,p,res_list,event_list,timetriggered,autostart,activation)
      | _  ->
        if tracing then trace "oil" "Wrong value (_) for attribute PRIORITY of TASK %s\n" object_name;
        t_value
    )
  | "ACTIVATION" -> (match value with
      | Int a  -> if (a <= 0) then begin
          if tracing then trace "oil" "Negative ACTIVATION for TASK %s\n" object_name;
          t_value
        end
        else (sched,pry,res_list,event_list,timetriggered,autostart,a)
      | _  ->
        if tracing then trace "oil" "Wrong value (_) for attribute ACTIVATION of TASK %s\n" object_name;
        t_value
    )
  | "AUTOSTART" -> (match value with
      | Bool (true,_)  -> starting_tasks := object_name :: !starting_tasks;
        sched,pry,res_list,event_list,timetriggered,true, activation
      | Bool (false,_)  -> sched,pry,res_list,event_list,timetriggered,false,activation
      | _  ->
        if tracing then trace "oil" "Wrong value (_) for attribute AUTOSTART of TASK %s\n" object_name;
        t_value
    )
  | "RESOURCE" -> (match value with
      | Name (res,None) -> sched,pry,res::res_list,event_list,timetriggered,autostart,activation (*This should not occur *)
      | String res      -> sched,pry,res::res_list,event_list,timetriggered,autostart,activation
      | _  ->
        if tracing then trace "oil" "Wrong value (_) for attribute RESOURCE of TASK %s\n" object_name;
        t_value
    )
  | "EVENT" -> (match value with
      | Name (ev,None)  -> sched,pry,res_list,ev::event_list,timetriggered,autostart,activation
      | String ev       -> sched,pry,res_list,ev::event_list,timetriggered,autostart,activation
      | _  ->
        if tracing then trace "oil" "Wrong value (_) for attribute EVENT of TASK %s\n" object_name;
        t_value
    )
  | "MESSAGE" ->
    if tracing then trace "oil" "MESSAGE attribute ignored for TASK %s\n" a_name;
    t_value
  | _ ->
    if tracing then trace "oil" "Unhandled TASK attribute %s\n" a_name;
    t_value

let handle_attribute_isr object_name i_value (attr : (string*attribute_v)) =
  let (pry,res_list,category) = i_value in
  let tmp, value = attr in
  let a_name = String.uppercase_ascii tmp in
  match a_name with
  | "CATEGORY" -> (match value with
      | Int c  -> (match c with
          | 1 -> (pry,res_list,c)
          | 2 -> (pry,"SuspendOSInterrupts"::res_list,c)
          | _ -> if tracing then trace "oil" "Wrong CATEGORY for ISR %s\n" object_name; i_value)
      | _  ->
        if tracing then trace "oil" "Wrong value (_) for attribute CATEGORY of ISR %s\n" object_name;
        i_value
    )
  | "RESOURCE" -> (match value with
      | Name (res,None) -> pry,res::res_list,category (*This should not occur *)
      | String res      -> pry,res::res_list,category
      | _  ->
        if tracing then trace "oil" "Wrong value (_) for attribute RESOURCE of ISR %s\n" object_name;
        i_value
    )
  | "MESSAGE" ->
    if tracing then trace "oil" "MESSAGE attribute ignored for ISR %s\n" a_name;
    i_value
  | x when List.mem x !osek_ISR_PRIORITY -> (match value with
      | Int p  -> if (p < 0) then begin
          if tracing then trace "oil" "Negative PRIORITY for ISR %s\n" object_name;
          i_value
        end
        else let p' = if x = "INTERRUPTPRIORITY" then p+1000 else p in (p',res_list,category)
      | _  ->
        if tracing then trace "oil" "Wrong value (_) for attribute PRIORITY of ISR %s\n" object_name;
        i_value
    )
  (*  | x when List.mem x !osek_ISR_GROUP -> (match value with
        | String g  -> if Hashtbl.mem group_pry g then begin
                        let p = Hashtbl.find group_pry g in
                        (p,res_list,category)
                      end else begin
                        open_isr := object_name::!open_isr;
                        i_value
                      end
        | _  ->
          if tracing then trace "oil" "Wrong value (_) for attribute GROUP of ISR %s\n" object_name;
            i_value
        )      *)
  | _ ->
    if tracing then trace "oil" "Unhandled ISR attribute %s\n" a_name;
    i_value

let handle_action_alarm object_name attr =
  let subaction, target = attr in (match (String.uppercase_ascii subaction) with
      | "TASK" -> (match target with
          | Name (name,None) -> let task = make_task name in
            if tracing then trace "oil" "ActivateTask %s as Name\n" task;
            Hashtbl.replace alarms object_name ((fun (x,l) -> (x,(make_task name)::l)) (Hashtbl.find alarms object_name))
          (*	    Hashtbl.replace tasks task (helper (Hashtbl.find tasks task)); (*TODO comment that out, instead ad to alarms*)
            	    concurrent_tasks :=  task :: !concurrent_tasks*)
          | String name  -> let task = make_task name in
            if tracing then trace "oil" "ActivateTask %s as String\n" task;
            Hashtbl.replace alarms object_name ((fun (x,l) -> (x,(make_task name)::l)) (Hashtbl.find alarms object_name))
          (*             Hashtbl.replace tasks task (helper (Hashtbl.find tasks task)); *)
          (*             concurrent_tasks :=  task :: !concurrent_tasks *)
          | other  ->
            (* TODO	    if tracing then trace "oil" "Unknown parameter (%s) for ACTIVATETASK of ALARM %s\n" other object_name;*)
            if tracing then trace "oil" "Unable to determine task (_) for ACTIVATETASK of ALARM %s\n" object_name
        )
      | other ->
        (* TODO	  if tracing then trace "oil" "Wrong parameter (%s) for ACTIVATETASK of ALARM %s\n" other object_name;*)
        if tracing then trace "oil" "Wrong parameter (_) for ACTIVATETASK of ALARM %s\n" object_name
    )

let handle_event_alarm object_name attr =
  let subaction, target = attr in (match (String.uppercase_ascii subaction) with
      | "EVENT" ->
        if tracing then trace "oil" "Handling parameter EVENT for SETEVENT of ALARM %s\n" object_name;
        let helper (a,_) = (a,true) in (match target with
            | Name (ev,None) ->
              if tracing then trace "oil" "EVENT %s ALARM %s\n" ev object_name;
              Hashtbl.replace events ev (helper (Hashtbl.find events ev))
            | String ev  ->
              if tracing then trace "oil" "EVENT2 %s ALARM %s\n" ev object_name;
              Hashtbl.replace events ev (helper (Hashtbl.find events ev))
            | other  ->
              (* TODO	    if tracing then trace "oil" "Unknown parameter (%s) for SETEVENT of ALARM %s\n" other object_name;*)
              if tracing then trace "oil" "Unknown parameter (_) for SETEVENT of ALARM %s\n" object_name
          )
      | "TASK" ->if tracing then trace "oil" "Skipped parameter TASK for SETEVENT of ALARM %s\n" object_name
      | other ->
        (* TODO	  if tracing then trace "oil" "Wrong parameter (%s) for SETEVENT of ALARM %s\n" other object_name;*)
        if tracing then trace "oil" "Wrong parameter (_) for SETEVENT of ALARM %s\n" object_name
    )

let handle_attribute_alarm object_name attr =
  let tmp, value = attr in
  let name = String.uppercase_ascii tmp in
  match name with
  | "ACTION" -> (match value with
      | Name (action,params)  -> ( match (String.uppercase_ascii action) with
          | "ACTIVATETASK" -> ( match params with
              | None ->
                if tracing then trace "oil" "No argument for ACTIVATETASK of ALARM %s\n" object_name
              | Some a_params -> List.iter (handle_action_alarm object_name) a_params
            )
          | "SETEVENT" ->  ( match params with
              | None ->
                if tracing then trace "oil" "No argument for SETEVENT of ALARM %s\n" object_name
              | Some a_params -> List.iter (handle_event_alarm object_name) a_params
            )
          | "ALARMCALLBACK" -> print_endline("Found ALARMCALLBACK in alarm " ^ object_name)
          (* TODO add as interrupts above tasks below isr?
             add treatment for the macro
             see page 36 of oil spec *)
          | other  ->
            (* TODO	if tracing then trace "oil" "Wrong parameter (%s) for ACTION of ALARM %s\n" other object_name;*)
            if tracing then trace "oil" "Wrong parameter (_) for ACTION of ALARM %s\n" object_name
        )
      | String s  ->
        if tracing then trace "oil" "String as ACTION attribute: %s\n" s
      | other  ->
        (*  TODO      if tracing then trace "oil" "Wrong value (%s) for ACTION of ALARM %s\n" other object_name; *)
        if tracing then trace "oil" "Wrong value (_) for ACTION of ALARM %s\n" object_name
    )
  | "AUTOSTART" ->
    if tracing then trace "oil" "Activating ALARM %s\n" object_name;
    Hashtbl.replace alarms object_name ((fun (_,l) -> (true,l)) (Hashtbl.find alarms object_name))
  | "COUNTER"
  | _ ->
    if tracing then trace "oil" "Skipped unhandled ALARM attribute %s\n" name

let handle_attribute_resource object_name attr =
  let tmp, value = attr in
  let name = String.uppercase_ascii tmp in
  match name with
  | "RESOURCEPROPERTY" -> (match value with
      | Name (action,params)  -> ( match (String.uppercase_ascii action) with
          | "STANDARD" -> ()
          | "LINKED" -> print_endline("Found LINKED RESOURCE " ^ object_name);
            (* TODO? subresources???
               not found in OSEK spec *)
            ()
          | "INTERNAL" -> print_endline("Found INTERNAL RESOURCE " ^ object_name);
            (* TODO add to tasks useing it when they start
               see page 34 in OSEK spec *)
            ()
          | other  ->
            if tracing then trace "oil" "Wrong RESOURCEPROPERTY (%s) for RESOURCE %s\n" other object_name
        )
      | other  ->
        (*  TODO     if tracing then trace "oil" "Wrong value (%s) for ACTION of ALARM %s\n" other object_name;*)
        if tracing then trace "oil" "Wrong value (_) for ACTION of ALARM %s\n" object_name
    )
  | _ ->
    if tracing then trace "oil" "Unhandled RESOURCE attribute %s\n" name

let handle_attribute_event object_name attr =
  let _ = Hashtbl.replace events object_name ("-1",false) in
  let tmp, value = attr in
  let name = String.uppercase_ascii tmp in
  match name with
  | "MASK" ->
    if tracing then trace "oil" "Skipped MASK of EVENT %s\n" object_name
  | _ ->
    if tracing then trace "oil" "Unhandled EVENT attribute %s\n" name

let add_to_table oil_info =
  let object_type, object_name, attribute_list = oil_info in
  if tracing then trace "oil" "Handling OBJECT %s\n" object_type;
  match object_type with
  | "OS" ->    (match attribute_list with
      | [] -> ()
      | _ -> List.iter handle_attribute_os attribute_list
    )
  | "TASK" -> let name = make_task object_name in
    let def_task = (false,-1,[object_name;"RES_SCHEDULER"],[],false,false,16) in
    let _ = Hashtbl.add resources object_name (object_name,-1,make_lock object_name) in
    (match attribute_list with
     | [] ->
       if tracing then trace "oil" "Empty attribute list for task %s. Using defaults.\n" object_name;
       Hashtbl.add tasks name def_task
     | _ ->
       if tracing then trace "oil" "Registering task %s.\n" name;
       let (sched,pry,res_list, ev_list, timed, auto,act) = (List.fold_left (handle_attribute_task name) def_task attribute_list) in
       let new_pry = if pry = -1 then begin
           if tracing then trace "oil" "No priority for task %s. Assuming 1.\n" name;
           1 end else pry
       in
       Hashtbl.add tasks name (sched,new_pry,res_list, ev_list, timed, auto,act)
    )
  | "ISR"  -> let name = make_isr object_name in
    let def_isr = (-1001,["DisableAllInterrupts";"SuspendAllInterrupts";object_name],-1) in
    let _ = Hashtbl.add resources object_name (object_name,-1, make_lock object_name) in
    concurrent_tasks := name :: !concurrent_tasks;
    (match attribute_list with
     | [] ->
       if tracing then trace "oil" "Empty attribute list for task %s. Using defaults.\n" object_name;
       Hashtbl.add isrs name def_isr
     | _ ->
       if tracing then trace "oil" "Registering interrupt %s.\n" name;
       let (pry, res_list,category) = (List.fold_left (handle_attribute_isr name) def_isr attribute_list) in
       let new_cat = if category = -1 then begin
           if tracing then trace "oil" "No category for interrupt %s. Assuming category 1.\n" name;
           1 end else category
       in
       let new_pry = if pry = -1001 then begin
           if tracing then trace "oil" "No priority for interrupt %s. Assuming 9999.\n" name;
           9999 end else pry
       in
       Hashtbl.add isrs name (new_pry,res_list,new_cat)
    )
  | "ALARM" -> Hashtbl.add alarms object_name (false,[]);
    List.iter (handle_attribute_alarm object_name) attribute_list
  | "RESOURCE" -> Hashtbl.add resources object_name ("-1",-1, make_lock object_name);
    List.iter (handle_attribute_resource object_name) attribute_list
  | "EVENT" -> List.iter (handle_attribute_event object_name) attribute_list
  (*    | "GROUPPRIORITY" -> (*add to group pry check for open isr with that group at the very end check for left over open isr*)
                            ()*)
  | "SPINLOCK" ->
    (*print_endline @@ object_type ^ " " ^ object_name ^ " " ^ String.concat ", " (List.map show_param_t attribute_list);*)
    let accessing_applications = BatList.filter_map (function "ACCESSING_APPLICATION", Name (name, _) -> Some name | _ -> None) attribute_list in
    (*print_endline @@ String.concat ", " accessing_applications;*)
    Hashtbl.add spinlocks object_name ("-1", accessing_applications, make_spinlock object_name);
    Hashtbl.add resources object_name (object_name,-1,make_lock ("spinlock_"^object_name))
  | "COUNTER"
  | "MESSAGE"
  | "COM"
  | "NM"
  | "APPMODE"
  | "IPDU"
  | _ ->
    if tracing then trace "oil" "Skipped unhandled object %s \n" object_type
