open Cil
open Pretty
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec

  let oilFile = ref ""
  let resourceheaders = ref ""
(* "/defaultAppWorkstation/tpl_os_generated_configuration.h" *)

  let constantlocks = Hashtbl.create 16
  let tasks = Hashtbl.create 16
  let resources = Hashtbl.create 16
  let offensivepriorities = Hashtbl.create 16
  let irpts = ref []

  (*priority function*)
  let pry lock = try Hashtbl.find resources lock with Not_found -> print_endline("Priority not found. Using default value -1"); (-1)
  (*lockset -> priority helper*)
  let names = function (LockDomain.Addr.Addr (x,_) ,_) -> x.vname | _ -> failwith "This (hopefully1) never happens!"
  let resourceset_to_priority = List.fold_left (fun y x -> if (pry x) > y then pry x else y) (min_int)
  (*brutal hack*)
  let is_task = Cilfacade.is_task
 

  module MyParam = 
  struct
    module Glob = LockDomain.OsekGlob
    let effect_fun (ls: LockDomain.Lockset.t) = 
      let locks = LockDomain.Lockset.ReverseAddrSet.elements ls in
      let prys = List.map names locks in
      let staticprys = List.filter is_task prys in
      let pry = resourceset_to_priority staticprys in
        if pry = min_int then `Bot else `Lifted (Int64.of_int pry)
  end

  module M = Mutex.MakeSpec (MyParam)

  module Dom  = M.Dom
  module Glob = M.Glob

  let dummy_release f = makeLocalVar f ?insert:(Some false) "ReleaseResource" Cil.voidType

  let dummy_get f = makeLocalVar f ?insert:(Some false) "GetResource" Cil.voidType

  let parse_oil () = (* requires PRIORITY tag to occur before RESOURCE tag in task definitions. does not take "default" into account *)
    let input = open_in !oilFile in
    let task_re = Str.regexp " *\\(TASK\\|ISR\\) *\\([a-zA-Z][a-zA-Z0-9_]*\\) *" in
    let pry_re = Str.regexp " *PRIORITY *= *\\([1-9][0-9]*\\) *" in
    let res_re = Str.regexp " *RESOURCE *= *\\([a-zA-Z][a-zA-Z0-9_]*\\) *" in
    let flag = ref "" in
    let rec read_info () = try
      let line = input_line input in
(*print_string (line ^ "\n");*)
	if Str.string_match task_re line 0 then begin
(*print_string "task \n";*)
          let name = Goblintutil.taskprefix ^ (Str.matched_group 2 line) in 
          let typ = (Str.matched_group 1 line) in
(*  let _ = print_endline ( "Adding " ^ name) in  *)
	  Hashtbl.add tasks name (typ,-1,[name]);
          Hashtbl.add constantlocks name (makeGlobalVar name  Cil.voidType);
          Hashtbl.add resources name (-1);
	  flag := name;
          if typ = "ISR" then irpts := (Cilfacade.getFun name, -1) :: !irpts;
	end;
	if Str.string_match pry_re line 0 then begin
	  if (not (!flag="")) then begin
(*print_string "pry \n";*)
	      Hashtbl.replace tasks !flag ((fun (x,_,z) y -> (x,y,z)) (Hashtbl.find tasks !flag) (int_of_string(Str.matched_group 1 line)));
              let typ = (Str.matched_group 1 line) in 
              if typ = "ISR" then irpts := (function ((a,b)::xs) -> (a,int_of_string(Str.matched_group 1 line))::xs | [] -> failwith "Impossible!") !irpts;
          end;
	end;
	if Str.string_match res_re line 0 then begin
	  let res_name = Str.matched_group 1 line in
(*print_string "res \n";*)
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
    let ceil_pry res_name _ task_info acc = (fun (t,p,r) -> 
      if (List.mem res_name r) then (max acc p) else acc) task_info
    in
    let genp res_name p = let cp = (Hashtbl.fold (ceil_pry res_name) tasks (-1)) in 
      if cp > p then Hashtbl.replace resources res_name cp 
    in
    let generate_ceiling_priority () = Hashtbl.iter genp resources
    in read_info (); close_in input; generate_ceiling_priority ()

  let parse_tramp tramp = 
    let input = open_in tramp in
    let re = Str.regexp ".*resource_id_of_\\([a-zA-Z][a-zA-Z0-9_]*\\) +\\([0-9]+\\) *" in
    let rec read_info () = try
      let line = input_line input in
	if Str.string_match re line 0 then begin
          let name = (Str.matched_group 1 line) in
          let id = Str.matched_group 2 line in
	  Hashtbl.add constantlocks id (makeGlobalVar name  Cil.voidType);
	end;
	read_info ();
      with 
	| End_of_file -> ()
	| e -> raise e
    in read_info (); close_in input

  let query ctx (q:Queries.t) : Queries.Result.t = 
    let pry = resourceset_to_priority (List.map names (Mutex.Lockset.ReverseAddrSet.elements ctx.local)) in
    match q with
    | Queries.Priority "" ->  `Int (Int64.of_int pry)
    | Queries.Priority vname -> `Int (Int64.of_int (Hashtbl.find offensivepriorities vname) )
    | Queries.IsPrivate v ->
        let off = 
          match (ctx.global v: Glob.Val.t) with
            | `Bot -> min_int 
            | `Lifted i -> Int64.to_int i
            | `Top -> max_int
        in
        let res = off <= pry in 
        let _ = if false then Messages.write (Pretty.sprint ~width:80 (dprintf "Variable %s is %B because %d <= %d" v.vname res off pry)) else () in `Bool res 
    | _ -> Queries.Result.top ()


  (* transfer functions *)
  let intrpt ctx : Dom.t =
    (M.intrpt ctx)

  let assign ctx (lval:lval) (rval:exp) : Dom.t =
    (M.assign ctx lval rval)
   
  let branch ctx (exp:exp) (tv:bool) : Dom.t = 
    (M.branch ctx (exp:exp) (tv:bool)) 
  
  let body ctx (f:fundec) : Dom.t = 
    let m_st = M.body ctx (f:fundec) in
    if (is_task f.svar.vname) then
(*let _ = print_endline ( (string_of_int !Goblintutil.current_loc.line)  ^ " in " ^ !Goblintutil.current_loc.file) in
let _ = print_endline ( "Looking for " ^ f.svar.vname) in*)
      let task_lock = Hashtbl.find constantlocks f.svar.vname in
      match M.special_fn (swap_st ctx m_st) None (dummy_get f) [Cil.mkAddrOf (Var task_lock, NoOffset)] with 
        | [(x,_,_)] -> x 
        | _ -> failwith "This never happens!"     
    else 
      m_st

  let return ctx (exp:exp option) (f:fundec) : Dom.t =
    let m_st = M.return ctx (exp:exp option) (f:fundec) in
    if (is_task f.svar.vname) then 
      let task_lock = Hashtbl.find constantlocks f.svar.vname in
      match M.special_fn (swap_st ctx m_st) None (dummy_release f) [Cil.mkAddrOf (Var task_lock, NoOffset)] with 
        | [(x,_,_)] -> x 
        | _ -> failwith "This never happens!"     
    else 
      m_st
  
  let eval_funvar ctx (fv:exp) = 
    M.eval_funvar ctx (fv:exp)
    
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list =
    (M.enter_func ctx (lval: lval option) (f:varinfo) (args:exp list))
  
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t =
   M.leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) au
  
  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    let make_lock varinfo = [AddrOf (Var varinfo,NoOffset)] in
    match f.vname with
      | "GetResource" | "ReleaseResource" -> M.special_fn ctx lval f (match arglist with 
        | [Lval l] -> [AddrOf l] 
	| [Const (CInt64 (c,_,_) ) ] -> (make_lock (Hashtbl.find constantlocks (Int64.to_string c)))
        | x -> x)  
      | "ActivateTask" -> M.special_fn ctx lval f arglist (*call function *)
      | "ChainTask" -> M.special_fn ctx lval f arglist (*call function *)
      | "DisableAllInterrupts" -> M.special_fn ctx lval (dummy_get (Cil.emptyFunction f.vname)) (make_lock (Hashtbl.find constantlocks ("DEall"))) 
      | "EnsableAllInterrupts" -> M.special_fn ctx lval (dummy_release (Cil.emptyFunction f.vname)) (make_lock (Hashtbl.find constantlocks ("DEall"))) 
      | "SuspendAllInterrupts" -> M.special_fn ctx lval (dummy_get (Cil.emptyFunction f.vname)) (make_lock (Hashtbl.find constantlocks ("SRall"))) 
      | "ResumeAllInterrupts" -> M.special_fn ctx lval (dummy_release (Cil.emptyFunction f.vname)) (make_lock (Hashtbl.find constantlocks ("SRall"))) 
      | "SuspendOSInterrupts" -> M.special_fn ctx lval (dummy_get (Cil.emptyFunction f.vname)) (make_lock (Hashtbl.find constantlocks ("SRos"))) 
      | "ResumeOSInterrupts" -> M.special_fn ctx lval (dummy_release (Cil.emptyFunction f.vname)) (make_lock (Hashtbl.find constantlocks ("SRos"))) 
      | "TerminateTask" -> (if not(Dom.is_empty ctx.local) then () else print_endline "Warning: Taskgetitfromtasklock? terminated while holding resources xyz!") ; 
			    M.special_fn ctx lval f arglist  (*check empty lockset*)
      | "WaitEvent" -> (if not(Dom.is_empty ctx.local) then () else print_endline "Warning: Task ??? waited while holding resources xyz!") ; 
			  M.special_fn ctx lval f arglist (*check empty lockset*)
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
      | _ -> M.special_fn ctx lval f arglist

  let startstate () = Dom.top ()
  let otherstate () = Dom.top ()
  let exitstate  () = Dom.top ()
  
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
    | Defence of int*int
    | ReadOnly
    | ThreadLocal

  (** [postprocess_acc gl] groups and report races in [gl] *)
  let postprocess_acc gl =
    let is_no_glob (gl:varinfo) = (match gl.vtype with TFun _ -> true | _ -> false )in
    if is_no_glob gl then () else
    (* create mapping from offset to access list; set of offsets  *)
    let acc = M.acc in
    let create_map (accesses_map) =
      let f (((_, _, rw), _, offs) as accs) (map,set) =
        if M.OffsMap.mem offs map
        then (M.OffsMap.add offs ([accs] @ (M.OffsMap.find offs map)) map,
              M.OffsSet.add offs set)
        else (M.OffsMap.add offs [accs] map,
              M.OffsSet.add offs set)
      in
      M.AccValSet.fold f accesses_map (M.OffsMap.empty, M.OffsSet.empty)
    in
    (* join map elements, that we cannot be sure are logically separate *)
    let regroup_map (map,set) =
      let f offs (group_offs, access_list, new_map) = 
        let new_offs = Mutex.Offs.definite offs in
        let new_gr_offs = Mutex.Offs.join new_offs group_offs in
        (* we assume f is called in the right order: we get the greatest offset first (leq'wise) *)
        if (Mutex.Offs.leq new_offs group_offs || (Mutex.Offs.is_bot group_offs)) 
        then (new_gr_offs, M.OffsMap.find offs map @ access_list, new_map) 
        else (   new_offs, M.OffsMap.find offs map, M.OffsMap.add group_offs access_list new_map) 
      in
      let (last_offs,last_set, map) = M.OffsSet.fold f set (Mutex.Offs.bot (), [], M.OffsMap.empty) in
        if Mutex.Offs.is_bot last_offs
        then map
        else M.OffsMap.add last_offs last_set map
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


      let prys = List.map (List.map names) just_locks in
      let staticprys = List.map (List.filter is_task) prys in
      let offprys = List.map resourceset_to_priority staticprys in
      let accprys = List.map resourceset_to_priority prys in
      let offpry = List.fold_left (fun y x -> if x > y then x else y) (min_int) offprys in
      let var_str = gl.vname in
      let _ = Hashtbl.add offensivepriorities var_str offpry in
      let maxpry = List.fold_left (fun y x -> if x > y then x else y) (min_int) accprys in
      let minpry = List.fold_left (fun y x-> if x < y then x else y) (max_int) accprys in
        if not (Mutex.Lockset.is_empty locks || Mutex.Lockset.is_top locks) then
          Guarded locks
	else if (maxpry=minpry) then
	  Priority maxpry
	else if (minpry >= offpry) then
	  Defence (minpry,offpry)
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
	  let my_locks = List.map (function (LockDomain.Addr.Addr (x,_) ,_) -> x.vname | _ -> failwith "This (hopefully2) never happens!" ) (Mutex.Lockset.ReverseAddrSet.elements dom_elem) in
	  let pry = List.fold_left (fun y x -> if pry x > y then pry x else y) (min_int) my_locks  in
          let action = if write then "write" else "read" in
          let thread = if Mutex.BS.Flag.is_bad fl then "some thread" else "main thread" in
          let warn = action ^ " in " ^ thread ^ " with priority " ^ (string_of_int pry) ^ " and lockset: " ^ lockstr in
            (warn,loc) 
        in
        let warnings =  List.map f acc_list in
            let var_str = gl.vname ^ Mutex.Offs.short 80 offset in
        let safe_str reason = "Safely accessed " ^ var_str ^ " (" ^ reason ^ ")" in
          match is_race acc_list with
            | Race -> begin
                race_free := false;
                let warn = "Datarace at " ^ var_str in
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
            | Defence (defpry,offpry) ->
                  if !Mutex.GU.allglobs then
                    Mutex.M.print_group (safe_str "defensive priority exceeds offensive priority") warnings
                  else 
                    ignore (printf "Found correlation: %s is guarded by defensive priority %s against offensive priority %s\n" var_str (string_of_int defpry) (string_of_int offpry))
            | ReadOnly ->
                if !Mutex.GU.allglobs then
                  Mutex.M.print_group (safe_str "only read") warnings
            | ThreadLocal ->
                if !Mutex.GU.allglobs then
                  Mutex.M.print_group (safe_str "thread local") warnings
    in 
    let rw ((_,_,x),_,_) = x in
    let acc = M.Acc.find acc gl in
    let acc = if !Mutex.no_read then M.AccValSet.filter rw acc else acc in
    let acc_info = create_map acc in
    let acc_map = if !Mutex.unmerged_fields then fst acc_info else regroup_map acc_info in
      M.OffsMap.iter report_race acc_map
    
  (** postprocess and print races and other output *)
  let finalize () =
    M.AccKeySet.iter postprocess_acc !M.accKeys;
    if !Mutex.GU.multi_threaded then begin
      if !race_free then 
        print_endline "Goblint did not find any Data Races in this program!";
    end else if not !Goblintutil.debug then begin
      print_endline "NB! That didn't seem like a multithreaded program.";
      print_endline "Try `goblint --help' to do something other than Data Race Analysis."
    end;
    Base.Main.finalize ()

  let init () = 
    let hashmax _ next old = max next old in  
    let tramp = !resourceheaders in
    if !oilFile != "" && Sys.file_exists(!oilFile) then begin     
      parse_oil ();
      if Sys.file_exists(tramp) then begin
	parse_tramp tramp;
      end else begin
	prerr_endline "Trampoline headers not found." ;
	exit 2;
      end;
      Hashtbl.add constantlocks "DEall" (makeGlobalVar "DEall" Cil.voidType);
      Hashtbl.add constantlocks "SRall" (makeGlobalVar "SRall" Cil.voidType);
      Hashtbl.add constantlocks "SRos" (makeGlobalVar "SRos" Cil.voidType);
      Hashtbl.add resources "DEall" (Hashtbl.fold hashmax resources (-1) );
      Hashtbl.add resources "SRall" (Hashtbl.fold hashmax resources (-1) );
      Hashtbl.add resources "SRos" (Hashtbl.fold hashmax resources (-1) ); (*NOT GOOD *)
    end else begin
      prerr_endline "OIL-file not found." ;
      exit 2;
    end end

module ThreadMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "OSEK" 
                let depends = []
                type lf = Spec.Dom.t
                let inject_l x = `OSEK x
                let extract_l x = match x with `OSEK x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `Osek x 
                let extract_g x = match x with `Osek x -> x | _ -> raise MCP.SpecificationConversionError
         end)
