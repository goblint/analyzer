open Cil
open Pretty

module Spec =
struct
  module Dom  = Mutex.Spec.Dom
  module Glob = Mutex.Spec.Glob

  let oilFile = ref ""
  let path = Filename.dirname Sys.executable_name
  let tmp_path = ref ""

  let priorities = Hashtbl.create 16
  let constantlocks = Hashtbl.create 16
  let tasks = Hashtbl.create 16
  let resources = Hashtbl.create 16
  let tramp_id = Hashtbl.create 16

  (*priority function*)
  let pry lock = try Hashtbl.find priorities lock with Not_found -> print_endline("Priority not found. Using default value -1"); (-1)

  (*brutal hacks*)
  let is_task f = 
    (String.length f >= 12 && String.sub f 0 12 = "function_of_")

  let is_constantlock lock = Hashtbl.mem lock constantlocks
 
  let dummy_release f = makeLocalVar f ?insert:(Some false) "ReleaseResource" Cil.voidType

  let dummy_get f = makeLocalVar f ?insert:(Some false) "GetResource" Cil.voidType

  let parse_oil oilp tmp_path = 
    let input = open_in !oilFile in
    let output = open_out oilp in
    let _ = output_string output ("default" ^ "\n") in
    let h = "function_of_" in 	
    let task_re = Str.regexp " *\\(TASK\\|ISR\\) +\\([a-zA-Z]+\\)" in
    let pry_re = Str.regexp " *PRIORITY *= *\\([1-9][0-9]*\\)" in
    let res_re = Str.regexp " *RESOURCE *= *\\([a-zA-Z]+\\)" in
    let flag = ref "" in
    let rec read_info () = try
      let line = input_line input
      in
	if Str.string_match task_re line 0 then begin   
	  output_string output (h ^ (String.lowercase (Str.matched_group 1 line)) ^ "_" ^ (Str.matched_group 2 line) ^"\n");
	  Hashtbl.add tasks (Str.matched_group 2 line) ((Str.matched_group 1 line),-1,[]);
	  let _ = !flag = (Str.matched_group 2 line) in (); 
	end;
	if Str.string_match pry_re line 0 then begin
	  output_string output ((Str.matched_group 1 line)  ^"\n");
	  if (not (!flag="")) then 
	    Hashtbl.replace tasks !flag ((fun (x,_,z) y -> (x,y,z)) (Hashtbl.find tasks !flag) (int_of_string(Str.matched_group 1 line)));
	end;
	if Str.string_match res_re line 0 then begin
	  let res_name = Str.matched_group 1 line in
	  output_string output (res_name  ^"\n");
	  if (not (!flag="")) then begin
	    Hashtbl.replace tasks !flag ((fun (x,y,zs) z -> (x,y,z::zs)) (Hashtbl.find tasks !flag) res_name);
	  end;
	  if (not (Hashtbl.mem resources res_name)) then begin (Hashtbl.add resources res_name (-1)); end;
	end;
	read_info ();
      with 
	| End_of_file -> ()
	| e -> raise e
    in
    let helper2 res_name task value acc = (fun (x,y,z) -> 
      if (List.mem res_name z) then (max acc y) else acc) value 
    in
    let helper res_name pry = let x = (Hashtbl.fold (helper2 res_name) tasks (-1)) in 
      if x > pry then Hashtbl.replace resources res_name x 
    in
    let generate_ceiling_priority = Hashtbl.iter helper resources
    in read_info (); close_in input; close_out output

  let parse_tramp resp tramp = 
    let input = open_in tramp in
    let output = open_out resp in
    let re = Str.regexp ".*resource_id_of_\\([a-zA-Z]+\\) +\\([0-9]+\\).*" in
    let rec read_info () = try
      let line = input_line input
      in
	if Str.string_match re line 0 then begin
	  Hashtbl.add tramp_id (Str.matched_group 1 line) (Str.matched_group 2 line);
	  output_string output ( (Str.matched_group 2 line) ^ "\n");
	  output_string output ( (Str.matched_group 1 line) ^ "\n")
	end;
	read_info ();
      with 
	| End_of_file -> ()
	| e -> raise e
    in read_info (); close_in input;  close_out output

  (*end hacks*)

  let query ask _ (x:Dom.t) (q:Queries.t) : Queries.Result.t = 
    Queries.Result.top ()

  (* transfer functions *)
  let assign a (lval:lval) (rval:exp) gl (st:Dom.t) : Dom.t =
    (Mutex.Spec.assign a lval rval gl  st)
   
  let branch a (exp:exp) (tv:bool) gl (st:Dom.t) : Dom.t = 
    (Mutex.Spec.branch a (exp:exp) (tv:bool) gl st) 
  
  let body a (f:fundec) gl (st:Dom.t) : Dom.t = 
    let m_st = Mutex.Spec.body a (f:fundec) gl st in
    if (is_task f.svar.vname) then 
      let task_lock = Hashtbl.find constantlocks f.svar.vname in
      match Mutex.Spec.special_fn a None (dummy_get f) [Cil.mkAddrOf (Var task_lock, NoOffset)] gl m_st with 
        | [(x,_,_)] -> x 
        | _ -> failwith "This never happens!"     
    else 
      m_st

  let return a (exp:exp option) (f:fundec) gl (st:Dom.t) : Dom.t =
    let m_st = Mutex.Spec.return a (exp:exp option) (f:fundec) gl st in
    if (is_task f.svar.vname) then 
      let task_lock = Hashtbl.find constantlocks f.svar.vname in
      match Mutex.Spec.special_fn a None (dummy_release f) [Cil.mkAddrOf (Var task_lock, NoOffset)] gl m_st with 
        | [(x,_,_)] -> x 
        | _ -> failwith "This never happens!"     
    else 
      m_st
  
  let eval_funvar a (fv:exp) gl (st:Dom.t) : varinfo list = 
    Mutex.Spec.eval_funvar a (fv:exp) gl st
    
  let enter_func a (lval: lval option) (f:varinfo) (args:exp list) gl (st:Dom.t) : (Dom.t * Dom.t) list =
    (Mutex.Spec.enter_func a (lval: lval option) (f:varinfo) (args:exp list) gl st)
  
  let leave_func a (lval:lval option) (f:varinfo) (args:exp list) gl (bu:Dom.t) (au:Dom.t) : Dom.t =
   Mutex.Spec.leave_func a (lval:lval option) (f:varinfo) (args:exp list) gl bu au
  
  let special_fn a (lval: lval option) (f:varinfo) (arglist:exp list) gl (st:Dom.t) : (Dom.t * Cil.exp * bool) list =
    let make_lock varinfo = [AddrOf (Var varinfo,NoOffset)] in
    match f.vname with
      | "GetResource" | "ReleaseResource" -> Mutex.Spec.special_fn a lval f (match arglist with 
        | [Lval l] -> [AddrOf l] 
	| [Const (CInt64 (c,_,_) ) ] -> (make_lock (Hashtbl.find constantlocks (Int64.to_string c)))
        | x -> x)  gl st
      | "ActivateTask" -> Mutex.Spec.special_fn a lval f arglist gl st (*call function *)
      | "ChainTask" -> Mutex.Spec.special_fn a lval f arglist gl st (*call function *)
      | "DisableAllInterrupts" -> Mutex.Spec.special_fn a lval (dummy_get (Cil.emptyFunction f.vname)) (make_lock (Hashtbl.find constantlocks ("DEall"))) gl st
      | "EnsableAllInterrupts" -> Mutex.Spec.special_fn a lval (dummy_release (Cil.emptyFunction f.vname)) (make_lock (Hashtbl.find constantlocks ("DEall"))) gl st
      | "SuspendAllInterrupts" -> Mutex.Spec.special_fn a lval (dummy_get (Cil.emptyFunction f.vname)) (make_lock (Hashtbl.find constantlocks ("SRall"))) gl st
      | "ResumeAllInterrupts" -> Mutex.Spec.special_fn a lval (dummy_release (Cil.emptyFunction f.vname)) (make_lock (Hashtbl.find constantlocks ("SRall"))) gl st
      | "SuspendOSInterrupts" -> Mutex.Spec.special_fn a lval (dummy_get (Cil.emptyFunction f.vname)) (make_lock (Hashtbl.find constantlocks ("SRos"))) gl st
      | "ResumeOSInterrupts" -> Mutex.Spec.special_fn a lval (dummy_release (Cil.emptyFunction f.vname)) (make_lock (Hashtbl.find constantlocks ("SRos"))) gl st
      | "TerminateTask" -> (if not(Dom.is_empty st) then () else print_endline "Warning: Taskgetitfromtasklock? terminated while holding resources xyz!") ; 
			    Mutex.Spec.special_fn a lval f arglist gl st (*check empty lockset*)
      | "WaitEvent" -> (if not(Dom.is_empty st) then () else print_endline "Warning: Task ??? terminated while holding resources xyz!") ; 
			  Mutex.Spec.special_fn a lval f arglist gl st (*check empty lockset*)
      | "SetEvent"
      | "ClearEvent"
      | "GetEvent"
      | "Schedule"
      | "GetTaskID"
      | "GetTaskState"
      | "GetAlarmBase" 
      | "GetAlarm" 
      | "SetRelAlarm" 
      | "SetAbsAlarm" 
      | "CancelAlarm" 
      | "GetActiveApplicationMode" 
      | "StartOS" 
      | "ShutdownOS" 
      | _ -> Mutex.Spec.special_fn a lval f arglist gl st
  
  let fork ask lv f args gs ls = 
    Mutex.Spec.fork ask lv f args gs ls

  let startstate () = Dom.top ()
  let otherstate () = Dom.top ()

  let get_diff _ = []
  let reset_diff x = x
  
  let name = "OSEK analysis"
  let es_to_string f _ = f.svar.vname

  let should_join _ _ = true


(** Finalization and other result printing functions: *)

  (** are we still race free *)
  let race_free = ref true

  type access_status = 
    | Race
    | Guarded of  Mutex.Lockset.t
    | Priority of int
    | ReadOnly
    | ThreadLocal

  (** [postprocess_acc gl] groups and report races in [gl] *)
  let postprocess_acc gl =
    let is_no_glob (gl:varinfo) = (match gl.vtype with TFun _ -> true | _ -> false )in
    if is_no_glob gl then () else
    (* create mapping from offset to access list; set of offsets  *)
    let acc = Mutex.Spec.acc in
    let create_map (accesses_map) =
      let f (((_, _, rw), _, offs) as accs) (map,set) =
        if Mutex.Spec.OffsMap.mem offs map
        then (Mutex.Spec.OffsMap.add offs ([accs] @ (Mutex.Spec.OffsMap.find offs map)) map,
              Mutex.Spec.OffsSet.add offs set)
        else (Mutex.Spec.OffsMap.add offs [accs] map,
              Mutex.Spec.OffsSet.add offs set)
      in
      Mutex.Spec.AccValSet.fold f accesses_map (Mutex.Spec.OffsMap.empty, Mutex.Spec.OffsSet.empty)
    in
    (* join map elements, that we cannot be sure are logically separate *)
    let regroup_map (map,set) =
      let f offs (group_offs, access_list, new_map) = 
        let new_offs = Mutex.Offs.definite offs in
        let new_gr_offs = Mutex.Offs.join new_offs group_offs in
        (* we assume f is called in the right order: we get the greatest offset first (leq'wise) *)
        if (Mutex.Offs.leq new_offs group_offs || (Mutex.Offs.is_bot group_offs)) 
        then (new_gr_offs, Mutex.Spec.OffsMap.find offs map @ access_list, new_map) 
        else (   new_offs, Mutex.Spec.OffsMap.find offs map, Mutex.Spec.OffsMap.add group_offs access_list new_map) 
      in
      let (last_offs,last_set, map) = Mutex.Spec.OffsSet.fold f set (Mutex.Offs.bot (), [], Mutex.Spec.OffsMap.empty) in
        if Mutex.Offs.is_bot last_offs
        then map
        else Mutex.Spec.OffsMap.add last_offs last_set map
    in
    let get_common_locks acc_list = 
      let f locks ((_,_,writing), lock, _) = 
        let lock = 
          if writing then
            (* when writing: ignore reader locks *)
            Mutex.Lockset.filter snd lock 
          else 
            (* when reading: bump reader locks to exclusive as they protect reads *)
            Mutex.Lockset.map (fun (x,_) -> (x,true)) lock 
        in
          Mutex.Lockset.join locks lock 
      in
	List.fold_left f (Mutex.Lockset.bot ()) acc_list
    in
    let is_race acc_list' =
      let acc_list = List.map (fun ((loc, fl, write), dom_elem,o) -> ((loc, fl, write), dom_elem,o)) acc_list' in
      let locks = get_common_locks acc_list in
      let rw ((_,_,x),_,_) = x in
      let non_main ((_,x,_),_,_) = Base.Main.Flag.is_bad x in
      let just_locks = List.map (fun (_, dom_elem,_) -> (Mutex.Lockset.ReverseAddrSet.elements dom_elem) ) acc_list in     
      let prys = List.map (List.map (function (LockDomain.Addr.Addr (x,_) ,_) -> x.vname | _ -> failwith "This (hopefully) never happens!"  )) just_locks in
      let accprys = List.map (List.fold_left (fun y x -> if (pry x) > y then pry x else y) (min_int)) prys in
      let maxpry = List.fold_left (fun y x -> if x > y then x else y) (min_int) accprys in
      let minpry = List.fold_left (fun y x-> if x < y then x else y) (max_int) accprys in
        if not (Mutex.Lockset.is_empty locks || Mutex.Lockset.is_top locks) then
          Guarded locks
	else if (maxpry=minpry) then
	  Priority maxpry
        else if not (List.exists rw acc_list) then
          ReadOnly
        else if not (List.exists non_main acc_list) then
          ThreadLocal
        else
          Race
    in
    let report_race offset acc_list =
        let f  ((loc, fl, write), dom_elem,o) = 
          let lockstr = Mutex.Lockset.short 80 dom_elem in
	  let my_locks = List.map (function (LockDomain.Addr.Addr (x,_) ,_) -> x.vname | _ -> failwith "This (hopefully) never happens!" ) (Mutex.Lockset.ReverseAddrSet.elements dom_elem) in
	  let pry = List.fold_left (fun y x -> if pry x > y then pry x else y) (min_int) my_locks  in
          let action = if write then "write" else "read" in
          let thread = if Mutex.BS.Flag.is_bad fl then "some thread" else "main thread" in
          let warn = action ^ " in " ^ thread ^ " with priority " ^ (string_of_int pry) ^ " and lockset: " ^ lockstr in
            (warn,loc) in
        let warnings =  List.map f acc_list in
            let var_str = gl.vname ^ Mutex.Offs.short 80 offset in
        let safe_str reason = "Safely accessed " ^ var_str ^ " (" ^ reason ^ ")" in
          match is_race acc_list with
            | Race -> begin
                race_free := false;
                let warn = "Datarace over " ^ var_str in
                  Mutex.M.print_group warn warnings
              end
            | Guarded locks ->
                let lock_str = Mutex.Lockset.short 80 locks in
                  if !Mutex.GU.allglobs then
                    Mutex.M.print_group (safe_str "common mutex") warnings
                  else 
                    ignore (printf "Found correlation: %s is guarded by lockset %s\n" var_str lock_str)
            | Priority pry ->
                  if !Mutex.GU.allglobs then
                    Mutex.M.print_group (safe_str "same priority") warnings
                  else 
                    ignore (printf "Found correlation: %s is guarded by priority %s\n" var_str (string_of_int pry))

            | ReadOnly ->
                if !Mutex.GU.allglobs then
                  Mutex.M.print_group (safe_str "only read") warnings
            | ThreadLocal ->
                if !Mutex.GU.allglobs then
                  Mutex.M.print_group (safe_str "thread local") warnings
    in 
    let rw ((_,_,x),_,_) = x in
    let acc = Mutex.Spec.Acc.find acc gl in
    let acc = if !Mutex.no_read then Mutex.Spec.AccValSet.filter rw acc else acc in
    let acc_info = create_map acc in
    let acc_map = if !Mutex.unmerged_fields then fst acc_info else regroup_map acc_info in
      Mutex.Spec.OffsMap.iter report_race acc_map
    
  (** postprocess and print races and other output *)
  let finalize () =
    Mutex.Spec.AccKeySet.iter postprocess_acc !Mutex.Spec.accKeys;
    if !Mutex.GU.multi_threaded then begin
      if !race_free then 
        print_endline "Goblint did not find any Data Races in this program!";
    end else if not !Goblintutil.debug then begin
      print_endline "NB! That didn't seem like a multithreaded program.";
      print_endline "Try `goblint --help' to do something other than Data Race Analysis."
    end;
    Goblintutil.rm_rf !tmp_path;
    Base.Main.finalize ()

  let init () =   
    if !oilFile != "" && Sys.file_exists(!oilFile) then begin     
      let () = tmp_path := Goblintutil.create_dir "osek_temp" in
      let oilp = !tmp_path ^ "/priorities.txt" in     
      let resp = !tmp_path ^  "/resources.txt" in
      let _ = parse_oil oilp tmp_path in
      let _ = Hashtbl.add priorities "default" (-1) in
      let tramp = Filename.dirname(!oilFile) ^ "/defaultAppWorkstation/tpl_os_generated_configuration.h" in
	if Sys.file_exists(tramp) then begin
	  parse_tramp resp tramp
	end else begin
	  prerr_endline "Trampoline headers not found." ;
	  exit 2;
	end;
      let get_res_id name = 
	if Sys.file_exists(resp ) then begin
	  let res_ids = open_in (resp) in
	  let rec look_up id line = if line = name then id else look_up line (input_line res_ids) in 
	  look_up "" (input_line res_ids)
	end else begin
	  prerr_endline "Resource identifiers could not be determined." ;
	  exit 2;
	end 
      in
      if (Sys.file_exists oilp) then begin
	let oilf = open_in oilp in
	let rec genp task line =
	  match line with
	  | "" -> ()
	  | "default" -> let line' = input_line oilf in
	     if (line' = "")  then () else begin try
		let p = int_of_string line' in
		Hashtbl.add priorities line p; 
		genp task (input_line oilf)
	      with Failure "int_of_string" -> genp task line'
	    end
	  | _ -> if is_task(line) then begin
	      Hashtbl.add priorities line (int_of_string(input_line oilf));
	      Hashtbl.add constantlocks line (makeGlobalVar line Cil.voidType);
	      try genp line (input_line oilf)
	      with End_of_file -> ()
	    end else begin
	      if not (Hashtbl.mem priorities line) then begin
		Hashtbl.add constantlocks (get_res_id line) (makeVarinfo true line (TVoid []));
		Hashtbl.add priorities line (Hashtbl.find priorities task);
		try genp task (input_line oilf)      
		with End_of_file -> ()
	      end else if ((Hashtbl.find priorities task) > (Hashtbl.find priorities line)) then
		Hashtbl.add priorities line (Hashtbl.find priorities task);
	      try genp task (input_line oilf)      
	      with End_of_file -> ()
	    end
	in
	let _ = genp "default" (input_line oilf) in
	let hashmax _ next old = max next old in
	let _ =	Hashtbl.add constantlocks "DEall" (makeGlobalVar "DEall" Cil.voidType);
		Hashtbl.add constantlocks "SRall" (makeGlobalVar "SRall" Cil.voidType);
		Hashtbl.add constantlocks "SRos" (makeGlobalVar "SRos" Cil.voidType);
		Hashtbl.add priorities "DEall" (Hashtbl.fold hashmax priorities (-1) );
		Hashtbl.add priorities "SRall" (Hashtbl.fold hashmax priorities (-1) );
		Hashtbl.add priorities "SRos" (Hashtbl.fold hashmax priorities (-1) ); (*NOT GOOD *)
	in 
	close_in oilf
      end else begin
	prerr_endline "Priorites could not be determined." ;
	exit 2;
      end;
      end else begin
	prerr_endline "OIL-file not found." ;
	exit 2;
      end
    end

module ThreadMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "OSEK" 
                type lf = Spec.Dom.t
                let inject_l x = `OSEK x
                let extract_l x = match x with `OSEK x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)
         
module Path     : Analyses.Spec = Compose.PathSensitive (Spec)
module Analysis : Analyses.S    = Multithread.Forward(Path)
