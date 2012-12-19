open Cil
open Messages
open GobConfig
open Pretty

type attribute_v = Name of (string * ( ((string * attribute_v) list) option)) | Bool of (bool * (((string * attribute_v) list) option)) | Int of int | Float of float | String of string | Auto
type param_t = string * attribute_v
type object_t = string*string*(param_t list)

(*		id	pry 	"lock"*)
type res_t = 	string*	int*	Cil.exp
(*		id	timed*)
type event_t = 	string*	bool
(*		interruptible	pry	res		events 		timetriggered	autostart	activation*)
type task_t = 	bool*		int*	(string list)*	(string list)*	bool*		bool*		int
(*		pry	res		category *)
type isr_t = 	int*	(string list)*	int

(*Oil/tramp/names files*)
(*let oilFile = ref ""*)
(*let resourceheaders = ref ""*)
(* "/defaultAppWorkstation/tpl_os_generated_configuration.h" *)
let osek_renames = ref ""

(*API function names *)
let osek_names : (string,string) Hashtbl.t = Hashtbl.create 16

let osek_ISR_PRIORITY = ref ["PRIORITY"; "INTERRUPTPRIORITY"]

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

(*unsused*)
let starting_tasks = ref ([] : string list)
let concurrent_tasks = ref ([] : string list)
(*/unsused*)
let warned = ref ([] :string list)

(*DeclareTask "external" *)

let osek_API_funs = ["ActivateTask"; "TerminateTask"; "ChainTask"; "Schedule"; "GetTaskID"; "GetTaskState"; "DisableAllInterrupts"; "EnableAllInterrupts"; "SuspendAllInterrupts"; "ResumeAllInterrupts"; "SuspendOSInterrupts"; "ResumeOSInterrupts"; "GetResource"; "ReleaseResource"; "SetEvent"; "GetEvent"; "ClearEvent"; "WaitEvent"; "GetAlarmBase"; "GetAlarm"; "SetRelAlarm"; "SetAbsAlarm"; "CancelAlarm"; "GetActiveApplicationMode"; "StartOS"; "ShutdownOS"]

(*priority function*)
let pry res = try let (_,pry,_) =Hashtbl.find resources res in pry with Not_found -> print_endline("Priority not found. Using default value -1"); (-1)


let get_api_names name =
  try
    let res = Hashtbl.find osek_names name in
    if tracing then trace "osek" "Renameing %s to %s\n" name res;
    res
  with 
    | Not_found -> 
	if tracing then trace "osek" "API name for %s not found\n" name;
name
    | e -> raise e

let is_task f = (Hashtbl.mem tasks f) || (Hashtbl.mem isrs f)

let is_starting f = (List.mem f !concurrent_tasks) || (List.mem f !starting_tasks)

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
  lock

let make_lock name = 
  if tracing then trace "osek" "Generating lock for resource %s\n" name;
  let varinfo = makeGlobalVar name Cil.voidType in
  AddrOf (Var varinfo,NoOffset)

let find_name id =
  if tracing then trace "osek" "Looking up resource %s\n" id;
  let dummy = "" in
  let res = Hashtbl.fold (fun name v acc -> let a,b,c = v in (if a = id then name else acc)) resources dummy  in
  if tracing then trace "osek" "Found resource %s\n" res;  
  if res = dummy then failwith ("No resource found for id "^ id ^ "!") else res

let check_res_decl res =
  if not(Hashtbl.mem resources res) then
  if tracing then trace "osek" "Resource %s is undeclared!\n" res;
  ()

let check_event_decl ev =
  if not(Hashtbl.mem events ev) then
  if tracing then trace "osek" "Event %s is undeclared!\n" ev;
  ()

let check_task task t_value =
  let (sched,pry,res_list,event_list,timetriggered,autostart,activation) = t_value in
    if (pry = -1) then (failwith ("No priority found for task "^ task ^ "!") );
    let _ = List.map check_res_decl res_list in
    let _ = List.map check_event_decl event_list in
      ()

let check_isr min_cat2_pry min_cat1_pry isr i_value =
  let (pry,res_list,category) = i_value in
    if (pry = -1) then (failwith ("No priority found for isr "^ isr ^ "!") );
    if (category = 2 && pry <= min_cat2_pry) then (failwith ("Priority of interrupt "^ isr ^ "below task priorities!") );
    if (category = 1 && pry <= min_cat1_pry) then (failwith ("Priority of category 1 interrupt "^ isr ^ "below category 2 priorities!") );
    let _ = List.map check_res_decl res_list in
      ()

let check_osek () =
  if tracing then trace "osek" "Checking conventions\n";
  let min_cat2_pry = pry "RES_SCHEDULER" in
  let min_cat1_pry = pry "SuspendOSInterrupts" in
  Hashtbl.iter (check_isr min_cat2_pry min_cat1_pry) isrs;  
  Hashtbl.iter check_task  tasks;
  ()

let check_tramp () =
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
  Hashtbl.iter check_ev_id events;
  ()

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
  let name = String.uppercase tmp in
  let get_bool value =
    match value with
      | Bool (x,_) -> x
      | _ -> false
  in
  match name with
  | "USERESSCHEDULER" -> use_res_scheduler := true; ()
  | "STARTUPHOOK" -> startuphook := get_bool value
  | "SHUTDOWNHOOK" -> shutdownhook:= get_bool value
  | "ERRORHOOK" -> errorhook := get_bool value
  | "PRETASKHOOK" -> pretaskhook := get_bool value
  | "POSTTASKHOOK" -> posttaskhook := get_bool value
  | _ -> 
  if tracing then trace "osek" "Unhandled OS attribute %s\n" name;
	  ()

let handle_attribute_task object_name t_value (attr : (string*attribute_v)) = 
  let (sched,pry,res_list,event_list,timetriggered,autostart,activation) = t_value in
  let tmp, value = attr in
  let a_name = String.uppercase tmp in
  match a_name with
  | "SCHEDULE" -> (match value with
      | Name (sched,_)  -> ( match (String.uppercase sched) with  (*This should not occur *)
	 | "NON"  ->false,pry,res_list,event_list,timetriggered,autostart,activation
	 | "FULL"  ->true,pry,res_list,event_list,timetriggered,autostart,activation
	 | other  ->
	    if tracing then trace "osek" "Wrong value (%s) for attribute SCHEDULE of TASK %s\n" other object_name;
	    t_value
	  )
      | String sched  -> ( match (String.uppercase sched) with
	 | "NON"  ->false,pry,res_list,event_list,timetriggered,autostart,activation
	 | "FULL"  ->true,pry,res_list,event_list,timetriggered,autostart,activation
	 | other  ->
	    if tracing then trace "osek" "Wrong value (%s) for attribute SCHEDULE of TASK %s\n" other object_name;
	    t_value
	  )
      | _  ->
	  if tracing then trace "osek" "Wrong value (_) for attribute SCHEDULE of TASK %s\n" object_name;
	  t_value
      )
  | "PRIORITY" -> (match value with
      | Int p  -> if (p < 0) then begin
		    if tracing then trace "osek" "Negative PRIORITY for TASK %s\n" object_name;
		    t_value
		  end
		  else (sched,p,res_list,event_list,timetriggered,autostart,activation)
      | _  ->
	if tracing then trace "osek" "Wrong value (_) for attribute PRIORITY of TASK %s\n" object_name;
	  t_value
      )
  | "ACTIVATION" -> (match value with
      | Int a  -> if (a <= 0) then begin
		    if tracing then trace "osek" "Negative ACTIVATION for TASK %s\n" object_name;
		    t_value
		  end
		  else (sched,pry,res_list,event_list,timetriggered,autostart,a)
      | _  ->
	if tracing then trace "osek" "Wrong value (_) for attribute ACTIVATION of TASK %s\n" object_name;
	  t_value
      )
  | "AUTOSTART" -> (match value with
      | Bool (true,_)  -> starting_tasks := object_name :: !starting_tasks;
	sched,pry,res_list,event_list,timetriggered,true,activation
      | Bool (false,_)  -> sched,pry,res_list,event_list,timetriggered,false,activation
      | _  ->
	if tracing then trace "osek" "Wrong value (_) for attribute AUTOSTART of TASK %s\n" object_name;
	  t_value
      )
  | "RESOURCE" -> (match value with
      | Name (res,None)  -> sched,pry,res::res_list,event_list,timetriggered,autostart,activation (*This should not occur *)
      | String res  -> sched,pry,res::res_list,event_list,timetriggered,autostart,activation
      | _  ->
	if tracing then trace "osek" "Wrong value (_) for attribute RESOURCE of TASK %s\n" object_name;
	  t_value
      )
  | "EVENT" -> (match value with
      | Name (ev,None)  -> sched,pry,res_list,ev::event_list,timetriggered,autostart,activation
      | String ev  ->  sched,pry,res_list,ev::event_list,timetriggered,autostart,activation
      | _  ->
	if tracing then trace "osek" "Wrong value (_) for attribute EVENT of TASK %s\n" object_name;
	  t_value
      )
  | "MESSAGE" ->
	  if tracing then trace "osek" "MESSAGE attribute ignored for TASK %s\n" a_name;
	t_value
  | _ -> 
	  if tracing then trace "osek" "Unhandled TASK attribute %s\n" a_name;
	t_value

let handle_attribute_isr object_name t_value (attr : (string*attribute_v)) = 
  let (pry,res_list,category) = t_value in
  let tmp, value = attr in
  let a_name = String.uppercase tmp in
  match a_name with
  | "CATEGORY" -> (match value with
      | Int c  -> (match c with 
		  | 1 -> (pry,"DisableAllInterrupts"::"SuspendAllInterrupts"::res_list,c)
		  | 2 -> (pry,res_list,c)
		  | _ -> if tracing then trace "osek" "Wrong CATEGORY for ISR %s\n" object_name; t_value)
      | _  ->
	if tracing then trace "osek" "Wrong value (_) for attribute CATEGORY of ISR %s\n" object_name;
	  t_value
      )
  | "RESOURCE" -> (match value with
      | Name (res,None)  -> pry,res::res_list,category (*This should not occur *)
      | String res  -> pry,res::res_list,category
      | _  ->
	if tracing then trace "osek" "Wrong value (_) for attribute RESOURCE of TASK %s\n" object_name;
	  t_value
      )
  | "MESSAGE" ->
    if tracing then trace "osek" "MESSAGE attribute ignored for TASK %s\n" a_name;
	t_value
  | x when List.mem x !osek_ISR_PRIORITY -> (match value with
      | Int p  -> if (p < 0) then begin
		    if tracing then trace "osek" "Negative PRIORITY for TASK %s\n" object_name;
		    t_value
		  end
		  else (p,res_list,category)
      | _  ->
	if tracing then trace "osek" "Wrong value (_) for attribute PRIORITY of TASK %s\n" object_name;
	  t_value
      )
  | _ -> 
    if tracing then trace "osek" "Unhandled ISR attribute %s\n" a_name;
	t_value

let handle_action_alarm object_name attr =
	let subaction, target = attr in (match (String.uppercase subaction) with
	| "TASK" -> let helper (a,b,c,d,_,f,g) = (a,b,c,d,true,f,g) in (match target with
	  | Name (name,None) -> let task = (get_string "ana.osek.taskprefix") ^ name ^ (get_string "ana.osek.tasksuffix") in
	    if tracing then trace "osek" "ActivateTask %s as Name\n" task;
	    Hashtbl.replace tasks task (helper (Hashtbl.find tasks task));
	    concurrent_tasks :=  task :: !concurrent_tasks
	  | String name  -> let task = (get_string "ana.osek.taskprefix") ^ name ^ (get_string "ana.osek.tasksuffix") in
	    if tracing then trace "osek" "ActivateTask %s as String\n" task;
	    Hashtbl.replace tasks task (helper (Hashtbl.find tasks task))
	  | other  ->
(* TODO	    if tracing then trace "osek" "Unknown parameter (%s) for ACTIVATETASK of ALARM %s\n" other object_name;*)
	    if tracing then trace "osek" "Unknown parameter (_) for ACTIVATETASK of ALARM %s\n" object_name;
	    ()
	  )
	| other ->
(* TODO	  if tracing then trace "osek" "Wrong parameter (%s) for ACTIVATETASK of ALARM %s\n" other object_name;*)
	  if tracing then trace "osek" "Wrong parameter (_) for ACTIVATETASK of ALARM %s\n" object_name;

	  ()
	)

let handle_event_alarm object_name attr =
	let subaction, target = attr in (match (String.uppercase subaction) with
	| "EVENT" -> 
	  if tracing then trace "osek" "Handling parameter EVENT for SETEVENT of ALARM %s\n" object_name;
	  let helper (a,_) = (a,true) in (match target with
	  | Name (ev,None) -> 
if tracing then trace "osek" "EVENT %s ALARM %s\n" ev object_name;
Hashtbl.replace events ev (helper (Hashtbl.find events ev))
	  | String ev  -> 
if tracing then trace "osek" "EVENT2 %s ALARM %s\n" ev object_name;
Hashtbl.replace events ev (helper (Hashtbl.find events ev))
	  | other  ->
(* TODO	    if tracing then trace "osek" "Unknown parameter (%s) for SETEVENT of ALARM %s\n" other object_name;*)
	    if tracing then trace "osek" "Unknown parameter (_) for SETEVENT of ALARM %s\n" object_name;
	    ()
	  )
	| "TASK" ->if tracing then trace "osek" "Skipped parameter TASK for SETEVENT of ALARM %s\n" object_name;
	  ()
	| other ->
(* TODO	  if tracing then trace "osek" "Wrong parameter (%s) for SETEVENT of ALARM %s\n" other object_name;*)
	  if tracing then trace "osek" "Wrong parameter (_) for SETEVENT of ALARM %s\n" object_name;
	  ()
	)

let handle_attribute_alarm object_name attr = 
  let tmp, value = attr in
  let name = String.uppercase tmp in
  match name with
  | "ACTION" -> (match value with
    | Name (action,params)  -> ( match (String.uppercase action) with
      | "ACTIVATETASK" -> ( match params with
	| None ->
	  if tracing then trace "osek" "No argument for ACTIVATETASK of ALARM %s\n" object_name;
	  ()
	| Some a_params -> let _ = List.map (handle_action_alarm object_name) a_params in ()
	)
      | "SETEVENT" ->  ( match params with
	| None ->
	  if tracing then trace "osek" "No argument for SETEVENT of ALARM %s\n" object_name;
	  ()
	| Some a_params -> let _ = List.map (handle_event_alarm object_name) a_params in ()
	)
      | "ALARMCALLBACK" -> print_endline("Found ALARMCALLBACK in alarm " ^ object_name);
(* TODO add as interrupts above tasks below isr?
   add treatment for the macro
   see page 36 of OSEK spec *) 
	()
      | other  ->
(* TODO	if tracing then trace "osek" "Wrong parameter (%s) for ACTION of ALARM %s\n" other object_name;*)
	if tracing then trace "osek" "Wrong parameter (_) for ACTION of ALARM %s\n" object_name;
	()
      )
    | String s  -> 
	if tracing then trace "osek" "String as ACTION attribute: %s\n" s;
	()
    | other  ->
(*  TODO      if tracing then trace "osek" "Wrong value (%s) for ACTION of ALARM %s\n" other object_name; *)
	if tracing then trace "osek" "Wrong value (_) for ACTION of ALARM %s\n" object_name;
      ()
    )
  | "AUTOSTART"
  | "COUNTER"
  | _ -> 
    if tracing then trace "osek" "Skipped unhandled ALARM attribute %s\n" name;
    ()

let handle_attribute_resource object_name attr = 
  let tmp, value = attr in
  let name = String.uppercase tmp in
  match name with
  | "RESOURCEPROPERTY" -> (match value with
    | Name (action,params)  -> ( match (String.uppercase action) with
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
	if tracing then trace "osek" "Wrong RESOURCEPROPERTY (%s) for RESOURCE %s\n" other object_name;
	()
      )
    | other  ->
(*  TODO     if tracing then trace "osek" "Wrong value (%s) for ACTION of ALARM %s\n" other object_name;*)
      if tracing then trace "osek" "Wrong value (_) for ACTION of ALARM %s\n" object_name;
      ()
    )
  | _ -> 
    if tracing then trace "osek" "Unhandled RESOURCE attribute %s\n" name;
    ()

let handle_attribute_event object_name attr =
  let _ = Hashtbl.add events object_name ("-1",false) in
  let tmp, value = attr in
  let name = String.uppercase tmp in
  match name with
  | "MASK" -> 
    if tracing then trace "osek" "Skipped MASK of EVENT %s\n" object_name;
	()
  | _ -> 
    if tracing then trace "osek" "Unhandled EVENT attribute %s\n" name;
    ()

let add_to_table oil_info =
  let object_type, object_name, attribute_list = oil_info in
  if tracing then trace "osek" "Handling OBJECT %s\n" object_type;
  match object_type with 
    | "OS" ->    (match attribute_list with
		    | [] -> ()
		    | _ -> let _ = List.map handle_attribute_os attribute_list in ()
		 )
    | "TASK" -> let name = (get_string "ana.osek.taskprefix") ^ object_name ^ (get_string "ana.osek.tasksuffix") in
		let def_task = (false,-1,[name;"RES_SCHEDULER"],[],false,false,16) in	
		let _ = Hashtbl.add resources name (name,-1,make_lock name) in
		(match attribute_list with
		  | [] -> 
		    if tracing then trace "osek" "Empty attribute list for task %s. Using defaults.\n" object_name;
		    Hashtbl.add tasks name def_task
		  | _ -> Hashtbl.add tasks name (List.fold_left (handle_attribute_task name) def_task attribute_list)
		)
    | "ISR"  -> let name = (get_string "ana.osek.taskprefix") ^ object_name ^ (get_string "ana.osek.tasksuffix") in
		let def_isr = (-1,[name; "SuspendOSInterrupts"],-1) in
		let _ = Hashtbl.add resources name (name,-1, make_lock name) in
		concurrent_tasks := name :: !concurrent_tasks;
		(match attribute_list with
		  | [] -> 
		    if tracing then trace "osek" "Empty attribute list for task %s. Using defaults.\n" object_name;
		    Hashtbl.add isrs name def_isr
		  | _ -> Hashtbl.add isrs name (List.fold_left (handle_attribute_isr name) def_isr attribute_list)
		)
    | "ALARM" -> let _ = List.map (handle_attribute_alarm object_name) attribute_list in ()
    | "RESOURCE" -> let _ = Hashtbl.add resources object_name ("-1",-1, make_lock object_name) in
		    let _ = List.map (handle_attribute_resource object_name) attribute_list in ()
    | "EVENT" -> let _ = List.map (handle_attribute_event object_name) attribute_list in ()
    | "COUNTER"
    | "MESSAGE" 
    | "COM" 
    | "NM" 
    | "APPMODE" 
    | "IPDU"
    | _ -> 
	    if tracing then trace "osek" "Skipped unhandled object %s \n" object_type;
	    ();
    ()















