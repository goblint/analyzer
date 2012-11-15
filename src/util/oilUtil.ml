open Messages
open Pretty

type attribute_v = Name of (string * ( ((string * attribute_v) list) option)) | Bool of (bool * (((string * attribute_v) list) option)) | Int of int | Float of float | String of string | Auto
type param_t = string * attribute_v
type object_t = string*string*(param_t list)

(*		id	pry *)
type res_t = 	int*	int
(*		id	timed*)
type event_t = 	int*	bool
(*		interruptible	pry	res		events 		timetriggered	autostart	activation*)
type task_t = 	bool*		int*	(string list)*	(string list)*	bool*		bool*		int
(*		pry	res		category *)
type isr_t = 	int*	(string list)*	int


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
let check = ref false (* add parameter *)

let res_id = ref (-1)
let get_id () = res_id := !res_id +1; !res_id


(* object tables *)
let resources : (string,res_t) Hashtbl.t = Hashtbl.create 16
let events : (string,event_t) Hashtbl.t = Hashtbl.create 16
let tasks  : (string,task_t) Hashtbl.t = Hashtbl.create 16 
let isrs   : (string,isr_t) Hashtbl.t = Hashtbl.create 16

let regular_tasks = ref ([] : string list)
let timed_tasks = ref ([] : string list)
let isr1 = ref ([] : string list)
let isr2 = ref ([] : string list)

(* OSEK hack: *)
(* let tasks : (string,  string * int * string list) Hashtbl.t = Hashtbl.create 16 *)
let is_task f = Hashtbl.mem tasks f


(*             if tracing then trace "osek" "New %a\n" Var.pretty_trace x; *)


let get_bool value =
  match value with
    | Bool (x,_) -> x
    | _ -> false


let check_task task =
(*TODO error checks and output *)
      ()



let handle_attribute_os attr = 
  let tmp, value = attr in
  let name = String.uppercase tmp in
  match name with
  | "USERESSCHEDULER" -> Hashtbl.add resources "RES_SCHEDULER" (-1,-1); use_res_scheduler := true; ()
  | "STARTUPHOOK" -> startuphook := get_bool value
  | "SHUTDOWNHOOK" -> shutdownhook:= get_bool value
  | "ERRORHOOK" -> errorhook := get_bool value
  | "PRETASKHOOK" -> pretaskhook := get_bool value
  | "POSTTASKHOOK" -> posttaskhook := get_bool value
  | _ -> 
(* if tracing then trace "osek" "Unhandled OS attribute %a\n" name; *)
	  ()

let handle_attribute_task name t_value (attr : (string*attribute_v)) = 
  let (sched,pry,res_list,event_list,timetriggered,autostart,activation) = t_value in
  let tmp, value = attr in
  let a_name = String.uppercase tmp in
  match a_name with
  | "SCHEDULE" -> (match value with
      | Name (sched,_)  -> ( match (String.uppercase sched) with  (*This should not occur *)
	 | "NON"  ->false,pry,res_list,event_list,timetriggered,autostart,activation
	 | "FULL"  ->true,pry,res_list,event_list,timetriggered,autostart,activation
	 | other  ->
(* if tracing then trace "osek" "Wrong value (%a) for attribute SCHEDULE of TASK %a\n" other object_name; *)
	    t_value
	  )
      | String sched  -> ( match (String.uppercase sched) with
	 | "NON"  ->false,pry,res_list,event_list,timetriggered,autostart,activation
	 | "FULL"  ->true,pry,res_list,event_list,timetriggered,autostart,activation
	 | other  ->
(* if tracing then trace "osek" "Wrong value (%a) for attribute SCHEDULE of TASK %a\n" other object_name; *)
	    t_value
	  )
      | _  ->
(* if tracing then trace "osek" "Wrong value (_) for attribute SCHEDULE of TASK %a\n" object_name; *)
	  t_value
      )
  | "PRIORITY" -> (match value with
      | Int p  -> if (p < 0) then
(* if tracing then trace "osek" "Negative PRIORITY for TASK %a\n" object_name; *)
		   t_value
		  else (sched,p,res_list,event_list,timetriggered,autostart,activation)
      | _  ->
(* if tracing then trace "osek" "Wrong value (_) for attribute PRIORITY of TASK %a\n" object_name; *)
	  t_value
      )
  | "ACTIVATION" -> (match value with
      | Int a  -> if (a <= 0) then
(* if tracing then trace "osek" "Negative ACTIVATION for TASK %a\n" object_name; *)
		   t_value
		  else (sched,pry,res_list,event_list,timetriggered,autostart,a)
      | _  ->
(* if tracing then trace "osek" "Wrong value (_) for attribute ACTIVATION of TASK %a\n" object_name; *)
	  t_value
      )
  | "AUTOSTART" -> (match value with
      | Bool (start,_)  -> sched,pry,res_list,event_list,timetriggered,start,activation
      | _  ->
(* if tracing then trace "osek" "Wrong value (_) for attribute AUTOSTART of TASK %a\n" object_name; *)
	  t_value
      )
  | "RESOURCE" -> (match value with
      | Name (res,None)  -> sched,pry,res::res_list,event_list,timetriggered,autostart,activation (*This should not occur *)
      | String res  -> sched,pry,res::res_list,event_list,timetriggered,autostart,activation
      | _  ->
(* if tracing then trace "osek" "Wrong value (_) for attribute RESOURCE of TASK %a\n" object_name; *)
	  t_value
      )
  | "EVENT" -> (match value with
      | Name (ev,None)  -> sched,pry,res_list,ev::event_list,timetriggered,autostart,activation
      | String ev  ->  sched,pry,res_list,ev::event_list,timetriggered,autostart,activation
      | _  ->
(* if tracing then trace "osek" "Wrong value (_) for attribute EVENT of TASK %a\n" object_name; *)
	  t_value
      )
  | "MESSAGE" ->
(* 	  if tracing then trace "osek" "MESSAGE attribute ignored for TASK %a\n" a_name; *)
	t_value
  | _ -> 
(* 	  if tracing then trace "osek" "Unhandled TASK attribute %a\n" a_name; *)
	t_value

let handle_attribute_isr name t_value (attr : (string*attribute_v)) = 
  let (pry,res_list,category) = t_value in
  let tmp, value = attr in
  let a_name = String.uppercase tmp in
  match a_name with
  | "CATEGORY" -> (match value with
      | Int c  -> if (c < 1 || c > 2) then
(* if tracing then trace "osek" "Wrong CATEGORY for ISR %a\n" object_name; *)
		   t_value
		  else (pry,res_list,c)
      | _  ->
(* if tracing then trace "osek" "Wrong value (_) for attribute CATEGORY of ISR %a\n" object_name; *)
	  t_value
      )
  | "RESOURCE" -> (match value with
      | Name (res,None)  -> pry,res::res_list,category (*This should not occur *)
      | String res  -> pry,res::res_list,category
      | _  ->
(* if tracing then trace "osek" "Wrong value (_) for attribute RESOURCE of TASK %a\n" object_name; *)
	  t_value
      )
  | "MESSAGE" ->
(* 	  if tracing then trace "osek" "MESSAGE attribute ignored for TASK %a\n" a_name; *)
	t_value
  | _ -> 
(* 	  if tracing then trace "osek" "Unhandled TASK attribute %a\n" a_name; *)
	t_value

let handle_action_alarm attr =
	let subaction, target = attr in (match (String.uppercase subaction) with
	| "TASK" -> let helper (a,b,c,d,_,f,g) = (a,b,c,d,true,f,g) in (match target with
	  | Name (task,None) -> Hashtbl.replace tasks task (helper (Hashtbl.find tasks task))
	  | String task  -> Hashtbl.replace tasks task (helper (Hashtbl.find tasks task))
	  | other  ->
(* if tracing then trace "osek" "Unknown parameter (%a) for ACTIVATETASK of ALARM %a\n" other object_name; *)
	    ()
	  )
	| other ->
(* if tracing then trace "osek" "Wrong parameter (%a) for ACTIVATETASK of ALARM %a\n" other object_name; *)
	  ()
	)

let handle_event_alarm attr =
	let subaction, target = attr in (match (String.uppercase subaction) with
	| "EVENT" -> let helper (a,_) = (a,true) in (match target with
	  | Name (ev,None) -> Hashtbl.replace events ev (helper (Hashtbl.find events ev))
	  | String ev  -> Hashtbl.replace events ev (helper (Hashtbl.find events ev))
	  | other  ->
(* if tracing then trace "osek" "Unknown parameter (%a) for ACTIVATETASK of ALARM %a\n" other object_name; *)
	    ()
	  )
	| other ->
(* if tracing then trace "osek" "Wrong parameter (%a) for ACTIVATETASK of ALARM %a\n" other object_name; *)
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
(* if tracing then trace "osek" "No argument for ACTIVATETASK of ALARM %a\n" object_name; *)
	  ()
	| Some a_params -> let _ = List.map handle_action_alarm a_params in ()
	)
      | "SETEVENT" ->  ( match params with
	| None ->
(* if tracing then trace "osek" "No argument for ACTIVATETASK of ALARM %a\n" object_name; *)
	  ()
	| Some a_params -> let _ = List.map handle_event_alarm a_params in ()
	)
      | "ALARMCALLBACK" -> print_endline("Found ALARMCALLBACK in alarm " ^ object_name);
(* add as interrupts above tasks below isr?
   add treatment for the macro
   see page 36 of OSEK spec *) 
	()
      | other  ->
(* if tracing then trace "osek" "Wrong parameter (%a) for ACTION of ALARM %a\n" other object_name; *)
	()
      )
    | other  ->
(* if tracing then trace "osek" "Wrong value (%a) for ACTION of ALARM %a\n" other object_name; *)
      ()
    )
  | "AUTOSTART"
  | "COUNTER"
  | _ -> 
(* if tracing then trace "osek" "Unhandled ALARM attribute %a\n" name; *)
    ()

let handle_attribute_resource object_name attr = 
  let tmp, value = attr in
  let name = String.uppercase tmp in
  match name with
  | "RESOURCEPROPERTY" -> (match value with
    | Name (action,params)  -> ( match (String.uppercase action) with
      | "STANDARD" -> Hashtbl.add resources object_name (get_id (),-1)
      | "LINKED" -> print_endline("Found LINKED RESOURCE " ^ object_name);
(* subresources???
   not found in OSEK spec *) 
	  ()
      | "INTERNAL" -> print_endline("Found INTERNAL RESOURCE " ^ object_name);
(* add to tasks useing it when tehy start 
   see page 34 in OSEK spec *) 
	  ()
      | other  ->
(* if tracing then trace "osek" "Wrong RESOURCEPROPERTY (%a) for RESOURCE %a\n" other object_name; *)
	()
      )
    | other  ->
(* if tracing then trace "osek" "Wrong value (%a) for ACTION of ALARM %a\n" other object_name; *)
      ()
    )
  | _ -> 
(* if tracing then trace "osek" "Unhandled RESOURCE attribute %a\n" name; *)
    ()

let handle_attribute_event object_name attr = 
  let tmp, value = attr in
  let name = String.uppercase tmp in
  match name with
  | "MASK" -> 
(* if tracing then trace "osek" "Skipped MASK of EVENT %a\n" other object_name; *)
	()
  | _ -> 
(* if tracing then trace "osek" "Unhandled EVENT attribute %a\n" name; *)
    ()

(* TODO sort list! OS > RES > EVENTS > TASKS > ISR > COUNTER > ALARM > other *)
let add_to_table oil_info =
  let object_type, object_name, attribute_list = oil_info in
  match object_type with 
    | "OS" ->    (match attribute_list with
		    | [] -> ()
		    | _ -> let _ = List.map handle_attribute_os attribute_list in ()
		 )
    | "TASK" ->  let def_task = (false,-1,[],[],false,false,16) in
		 (match attribute_list with
		    | [] -> 
(* 			    if tracing then trace "osek" "Empty attribute list for task %a. Using defaults.\n" object_name; *)
			   Hashtbl.add tasks object_name def_task
		    | _ -> Hashtbl.add tasks object_name (List.fold_left (handle_attribute_task object_name) def_task attribute_list)
		  );  (if !check then check_task object_name)
    | "ISR"  -> let def_isr = (-1,[],-1) in
		 (match attribute_list with
		    | [] -> 
(* 			    if tracing then trace "osek" "Empty attribute list for task %a. Using defaults.\n" object_name; *)
			   Hashtbl.add isrs object_name def_isr
		    | _ -> Hashtbl.add isrs object_name (List.fold_left (handle_attribute_isr object_name) def_isr attribute_list)
		  );  (if !check then check_task object_name)
    | "ALARM" -> let _ = List.map (handle_attribute_alarm object_name) attribute_list in ()
    | "RESOURCE" -> let _ = List.map (handle_attribute_resource object_name) attribute_list in ()
    | "EVENT" -> let _ = List.map (handle_attribute_event object_name) attribute_list in ()
    | "COUNTER"
    | "MESSAGE" 
    | "COM" 
    | "NM" 
    | "APPMODE" 
    | "IPDU"
    | _ -> 
(* if tracing then trace "osek" "Skipped unhandled object %a \n" object_type; *)
	    ();
    (* TODO compute ceiling priorities*)
(* TODO check resources/events declared*)
(* TODO fill lists*)
    ()















