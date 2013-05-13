(** Data race analysis for OSEK programs. *)

open Cil
open Pretty
open Analyses
open GobConfig
open Messages

open OilParser
open OilLexer
open OilUtil

module CF = Cilfacade
module GU = Goblintutil

module Spec =
struct
  
  include Analyses.DefaultSpec
  
  (*lockset -> priority helper*)
  let names = function (LockDomain.Addr.Addr (x,_) ,_) -> x.vname | _ -> failwith "This (hopefully1) never happens!"
  let resourceset_to_priority = List.fold_left (fun y x -> if (pry x) > y then pry x else y) (min_int)

  let listrem x l = List.filter (fun y -> not( x=y)) l
  let proj3_1 (a,_,_) = a
  let proj3_2 (_,a,_) = a
  let proj3_3 (_,_,a) = a
  let proj2_1 = fst
  let proj2_2 = snd

  
  (* parsing*)
  let parse_oil () = 
    if tracing then trace "osek" "Parsing OIL-file\n";
    let oil_file = get_string "ana.osek.oil" in
    if not (Sys.file_exists oil_file) then failwith "Cannot find oil file!";
    Hashtbl.add resources "RES_SCHEDULER" ("RES_SCHEDULER",-1, make_lock "RES_SCHEDULER");
    Hashtbl.add resources "DisableAllInterrupts" ("DisableAllInterrupts",-1, make_lock "DisableAllInterrupts");
    Hashtbl.add resources "SuspendAllInterrupts" ("SuspendAllInterrupts",-1, make_lock "SuspendAllInterrupts");
    Hashtbl.add resources "SuspendOSInterrupts" ("SuspendOSInterrupts",-1, make_lock "SuspendOSInterrupts");
    match file token (Lexing.from_channel (open_in oil_file)) with
      | [] -> failwith ( "No OIL-Objects found!")
      | objs -> let _ = List.map add_to_table (List.sort compare_objs objs) in
	if tracing then trace "osek" "Done parsing OIL-file\n";
	();
    if tracing then trace "osek" "Computing ceiling priorities...\n";
    Hashtbl.iter compute_ceiling_priority resources;
    if tracing then trace "osek" "Generating goblint.h...\n";
    generate_header ();
    if tracing then trace "osek" "Activating autostart alarms...\n";
    finish_alarm_handling ();
    if (get_bool "ana.osek.check") then begin
      if tracing then trace "osek" "Checking conventions...\n";
      check_osek ()
      end;
    if tracing then trace "osek" "Done processing OIL-file\n";
    ()

(*  let parse_tramp tramp = 
    if tracing then trace "osek" "Parsing trampolineish header...\n";
    let input = open_in tramp in
    let comment = Str.regexp "//.* \\|/\\*.*" in
    let re = Str.regexp ".*resource_id_of_\\([a-zA-Z][a-zA-Z0-9_]*\\) +\\([0-9]+\\) *" in
    let ev = Str.regexp ".*event_id_of_\\([a-zA-Z][a-zA-Z0-9_]*\\) +\\([0-9]+\\) *" in
    let rec read_info () = try
      let line = input_line input in
(* 	if tracing then trace "osek" "Line: %s\n" line; *)
	if (Str.string_match comment line 0) then begin
	  if tracing then trace "osek" "Trampolineish: Skipping (JUST 1!) line: %s\n" line;
	end else begin
	  if Str.string_match re line 0 then begin
	    let name = (Str.matched_group 1 line) in
	    let id = Str.matched_group 2 line in
	    if tracing then trace "osek" "Adding id (%s) for resource %s\n" id name;
	    let _ = try 
	      let (_,p,l) = Hashtbl.find resources name in
	      Hashtbl.replace resources name (id,p,l)
	    with
	      | Not_found -> print_endline ("Error: Resource " ^ name ^ " not found. ID not added.")
	      | e -> raise e
	    in ()
	  end;
	  if Str.string_match ev line 0 then begin
	  let name = (Str.matched_group 1 line) in
	    let id = Str.matched_group 2 line in
	    if tracing then trace "osek" "Adding id (%s) for event %s\n" id name;
	    let _ = try
	      let (_,b) = Hashtbl.find events name in
	      Hashtbl.replace events name (id,b);
	    with
	      | Not_found -> print_endline ("Error: Event " ^ name ^ " not found. ID not added.")
	      | e -> raise e
	    in ()
	  end;
	end;
	read_info ();
      with 
	| End_of_file -> ()
	| e -> raise e
    in read_info (); 
    if (get_bool "ana.osek.check") then check_tramp ();
    if tracing then trace "osek" "Done parsing trampolineish header\n";
    close_in input*)

  let parse_names names = 
    if tracing then trace "osek" "Parsing API (re)names...\n";
    let input = open_in names in
    let comment = Str.regexp "//.* \\|/\\*.*" in
    let newname = Str.regexp " *\\(#define \\| *\\)\\([a-zA-Z][a-zA-Z0-9_]*\\) +\\([a-zA-Z][a-zA-Z0-9_]*\\)" in
    let rec read_info () = try
      let line = input_line input in
	if tracing then trace "osek" "Line: %s\n" line;
	if (Str.string_match comment line 0) then begin
	  if tracing then trace "osek" "API names: Skipping (JUST 1!) line: %s\n" line;
	end else begin
	  if Str.string_match newname line 0 then begin
	    let newname = (Str.matched_group 3 line) in
	    let oldname = (Str.matched_group 2 line) in
	    if tracing then trace "osek" "Adding newname (%s) for function %s\n" newname oldname;
	    Hashtbl.add osek_names newname oldname;
	    LibraryFunctions.add_lib_funs [newname]
	  end;
	end;
	read_info ();
      with 
	| End_of_file -> ()
	| e -> raise e
    in read_info (); 
    LibraryFunctions.osek_renames := true;
    if tracing then trace "osek" "Done parsing API (re)names\n";
    close_in input

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
  module Offs = ValueDomain.Offs
  module Lockset = LockDomain.Lockset
  
  module Flags = FlagModeDomain.Dom
  module AccValSet = Set.Make (Printable.Prod (Printable.Prod3 (Printable.Prod3 (Basetype.ProgLines) (Base.Main.Flag) (IntDomain.Booleans)) (Lockset) (Offs)) (Flags))
  let acc     : AccValSet.t M.Acc.t = M.Acc.create 100
  let accKeys : M.AccKeySet.t ref   = ref M.AccKeySet.empty

  module Dom  = M.Dom
  module Glob = M.Glob

  let offensivepriorities : (string,int) Hashtbl.t= Hashtbl.create 16
  let off_pry_with_flag : (string,(Flags.t*int) list) Hashtbl.t = Hashtbl.create 16

  (* task resource handling *)
  let dummy_release f = makeLocalVar f ?insert:(Some false) "ReleaseResource" Cil.voidType
  let dummy_get f = makeLocalVar f ?insert:(Some false) "GetResource" Cil.voidType
  let is_task_res lock = is_task (names lock)
  let partition = Dom.ReverseAddrSet.partition is_task_res
  let lockset_to_task lockset =
    match Dom.ReverseAddrSet.elements lockset with
    | [x] -> names x
    | _ -> "???"
  let mem name lockset =
    let vinfo = match name with
      |AddrOf(Var v,_) -> v
      | _ ->  failwith "Impossible resource!" 
    in
    let res_addr = LockDomain.Addr.from_var vinfo in
    Dom.ReverseAddrSet.mem (res_addr,true) lockset
  (*/task resource handling *)

  (* flag stuff*)
  let get_val flag acc : Cil.exp (*const*) = 
(* let _ = print_endline (flag.vname ^"A" ^gl.vname) in     *)
    proj3_3 (Flags.find flag (proj2_2 acc))
  let get_eq flag acc : bool = 
(* let _ = print_endline (flag.vname ^"B"^gl.vname) in     *)
    proj3_2 (Flags.find flag (proj2_2 acc))
  let get_method flag acc : bool = 
(* let _ = print_endline (flag.vname ^ "C"^gl.vname) in     *)
    proj3_1 (Flags.find flag (proj2_2 acc))
  let flag_unknown flag accs = not (Flags.mem flag (proj2_2 accs))
(*   let flag_exists flag accs = List.filter (fun x -> (Flags.mem flag (proj2_2 x))) accs *)
  let rec flag_list_to_string l = match l with
    | []  -> ""
    | [x] -> x.vname
    | x::xs -> x.vname ^ " and " ^ (flag_list_to_string xs)
  let split_accs_method flag accs = 
    let unknowns,accs' = List.partition (flag_unknown flag) accs in
    let ts,fs = List.partition (get_method flag) accs' in
    (ts@unknowns, fs@unknowns)
  let split_accs flag accs = 
    let unknowns,accs' = List.partition (flag_unknown flag) accs in
    let ts,fs = List.partition (get_eq flag) accs' in
    (ts@unknowns, fs@unknowns)
  let split_equals flag accs =
    let unknowns,accs' = List.partition (flag_unknown flag) accs in
    let rec doit flag lists elem = match lists with
      | [] -> [[elem]]
      | x::xs -> if (get_val flag elem) = (get_val flag (List.hd x)) then ([elem]@x)::xs else x::(doit flag xs elem)
    in
    let vals = List.fold_left (doit flag) [] accs'
    in List.map (fun x -> x@unknowns) vals

  let split_may_eq flag value accs = 
    let unknowns,accs' = List.partition (flag_unknown flag) accs in
    let doit value acc = 
      let acc_value = get_val flag acc in
      let acc_eq = get_eq flag acc in
(*let f (Const (CInt64 (x,_,_) )) = Int64.to_string x in                
let _ = print_endline ("may equal " ^ (f value)) in        
let _ = print_endline (string_of_bool acc_eq) in
let _ = print_endline (f acc_value) in
let res =*)
      ( ( (acc_value = value) && acc_eq ) || ( (not acc_eq) && not (acc_value = value) ) )
(*in
let _ = print_endline (string_of_bool res) in res*)
    in 
    (List.filter (doit value) accs')@unknowns

  let strip_flags acc_list' = List.map proj2_1 acc_list'
  
  let rec get_flags (state :local_state list) : Flags.t =
    match state with 
    | [] -> failwith "get_flags"
    | (`FlagModeDom x)::rest -> x
    | x::rest -> get_flags rest
  (*/flagstuff*)
  (*prioritystuff*)
  let just_locks acc_list = List.map (fun (_, dom_elem,_) -> (Lockset.ReverseAddrSet.elements dom_elem) ) acc_list
  let prys acc_list = List.map (List.map names) (just_locks acc_list)
  let staticprys acc_list = List.map (List.filter is_task) acc_list
  let offprys acc_list = List.map resourceset_to_priority (staticprys (prys acc_list))
  let accprys acc_list = List.map resourceset_to_priority (prys acc_list)
  let maxpry acc_list = List.fold_left (fun y x -> if x > y then x else y) (min_int) (accprys acc_list)
  let minpry acc_list = List.fold_left (fun y x -> if x < y then x else y) (max_int) (accprys acc_list)
  let offpry acc_list = List.fold_left (fun y x -> if x > y then x else y) (min_int) (offprys acc_list)
  (*/prioritystuff*)

  (*TODO fill table at accesses*) 
  let offpry_flags (flagstate : Flags.t) (varinfo : Cil.varinfo) : int =
    let var = varinfo.vname in
    if tracing then trace "osek" "Computing flag priority for %s\n" var;
    
    let helper flag flag_value current = 
      if tracing then trace "osek" "Using flag %s\n" flag.vname;
      let equal,value = match flag_value with
        | (_,equal,Const (CInt64 (value,_,_))) -> equal,value
        | _ -> failwith "This never hapens! osekml157"
      in
      
      let doit (acc_flagstate,pry) = 
        let acc_equal,acc_value = 
          if Flags.mem flag acc_flagstate then
            match Flags.find flag acc_flagstate with
            | (_,equal,Const (CInt64 (value,_,_))) -> equal,value
            | _ -> failwith "This never hapens! osekml248"
          else
            (false,0L)
        in
        if (((acc_value = value) && (not acc_equal = equal) ) || ( (acc_equal = equal) && not (acc_value = value) ) ) then 
          pry
        else
          min_int
      in
      
      let acc_info : (Flags.t*int) list = if Hashtbl.mem off_pry_with_flag var then 
        Hashtbl.find off_pry_with_flag var
      else begin
        if tracing then trace "osek" "Empty access information when computing flag offensive priority for variable %s\n" var;
        []
      end
      in
      
      let flag_prys = List.map doit acc_info in
      List.fold_left (fun y x -> if x > y then x else y) (current) flag_prys
    in
    Flags.fold helper flagstate min_int
  
(* from thread*)
  let query_lv ask exp = 
    match ask (Queries.MayPointTo exp) with
      | `LvalSet l when not (Queries.LS.is_top l) -> 
          Queries.LS.elements l
      | _ -> []
      
  let eval_fv ask exp = 
    match query_lv ask exp with
      | [(v,_)] -> Some v (* This currently assumes that there is a single possible l-value. *)
      | _ -> None

  let eval_arg ctx (arg:exp) =
    match arg with
    | Lval (Var vinfo,_) -> vinfo
    | _ -> (match eval_fv ctx.ask arg with
      | Some v -> v
      | None   -> failwith "cannot extract arg")      
    
(* from mutex *)    

  (* Just adds accesses. It says concrete, but we use it to add verified 
     non-concrete accesses too.*)
  let add_concrete_access ctx fl loc ust (flagstate : Flags.t) (v, o, rv: Cil.varinfo * Offs.t * bool) =
    if (Base.is_global ctx.ask v) then begin
      if not (is_task v.vname) then begin
        if not !GU.may_narrow then begin
          let new_acc = ((loc,fl,rv),ust,o) in
          let curr : AccValSet.t = try M.Acc.find acc v with _ -> AccValSet.empty in
          let neww : AccValSet.t = AccValSet.add (new_acc,flagstate) curr in
          M.Acc.replace acc v neww;
          accKeys := M.AccKeySet.add v !accKeys;
          let curr = try Hashtbl.find off_pry_with_flag v.vname with _ -> [] in
          let pry = offpry [new_acc] in
          Hashtbl.replace off_pry_with_flag v.vname ((flagstate,pry)::curr)
        end ;
        let ls = if rv then Lockset.filter proj2_2 ust else ust in
        let el = MyParam.effect_fun ls in
  (*       (if LockDomain.Mutexes.is_empty el then Messages.waitWhat ("Race on "^v.vname)); *)
  (*      let _ = printf "Access to %s with offense priority %a\n" v.vname P.Glob.Val.pretty el in*)
        ctx.geffect v el
        end else begin
          if tracing then trace "osek" "Ignoring access to task/isr variable %s\n" v.vname;
        end
    end
      
  let add_per_element_access a b c d = 
(* print_endline "Per element access not supported.";  *)
    false

  (* All else must have failed --- making a last ditch effort to generate type 
      invariant if that fails then give up and become unsound. *)
  let add_type_access ctx fl loc ust flagstate (e,rw:exp * bool) =
    let eqset =
      match ctx.ask (Queries.EqualSet e) with
        | `ExprSet es 
            when not (Queries.ES.is_bot es) 
            -> Queries.ES.elements es
        | _ -> [e]
    in
      match M.best_type_inv eqset with
      	| Some (v,o) -> add_concrete_access ctx fl loc ust flagstate (v,o,rw)
        | _ -> M.unknown_access ()    

  let add_accesses ctx (accessed: M.accesses) (flagstate: Flags.t) (ust:Dom.t) = 
      let fl = Mutex.get_flag ctx.presub in
      if Base.Main.Flag.is_multi fl then
        let loc = !Tracing.current_loc in
        let dispatch ax =
          match ax with
            | M.Concrete (me,v,o,rw) ->
                begin match me, M.struct_type_inv v o with 
                  | _, Some (v,o) when (get_bool "exp.type-inv") ->
                      add_concrete_access ctx fl loc ust flagstate (v,o,rw)
                  | Some e,_ -> 
                      if   not (add_per_element_access ctx loc ust (e,rw)) 
                      then add_concrete_access ctx fl loc ust flagstate (v,o,rw)
                  | None,_ -> 
                      add_concrete_access ctx fl loc ust flagstate (v,o,rw)
                end
            | M.Region (Some e,v,o,rw) -> 
                if   not (add_per_element_access ctx loc ust (e,rw)) 
                then add_concrete_access ctx fl loc ust flagstate (v,o,rw)
            | M.Region (None,v,o,rw) -> 
                add_concrete_access ctx fl loc ust flagstate (v,o,rw)
            | M.Unknown a -> 
                if   not (add_per_element_access ctx loc ust a) 
                then add_type_access ctx fl loc ust flagstate a 
        in
          List.iter dispatch accessed  
    
  let query ctx (q:Queries.t) : Queries.Result.t = 
    match q with
    | Queries.Priority "" ->  
        let pry = resourceset_to_priority (List.map names (Mutex.Lockset.ReverseAddrSet.elements ctx.local)) in
          `Int (Int64.of_int pry)
    | Queries.Priority vname -> begin try `Int (Int64.of_int (Hashtbl.find offensivepriorities vname) ) with _ -> Queries.Result.top() end
    | Queries.IsPrivate v ->
        let pry = resourceset_to_priority (List.map names (Mutex.Lockset.ReverseAddrSet.elements ctx.local)) in
        if pry = min_int then begin `Bool false end else begin
          let off = 
  (*         if !FlagModes.Spec.flag_list = [] then begin *)
              match (ctx.global v: Glob.Val.t) with
                | `Bot -> min_int 
                | `Lifted i -> Int64.to_int i
                | `Top -> max_int
  (*           end else begin *)
  (*             let flagstate = get_flags ctx.presub in *)
  (*               offpry_flags flagstate v *)
  (*           end *)
          in `Bool (off <= pry)
        end
    | _ -> Queries.Result.top ()

    

  let startstate v = Dom.top ()
  let otherstate v = Dom.top ()
  let exitstate  v = Dom.top ()

  let activate_task ctx (task_name : string) : unit =
    let task = Cilfacade.getFun task_name in
    ctx.spawn task.svar (otherstate ())
  
  (* transfer functions *)
  let intrpt ctx : Dom.t =
    ctx.local (*currently ignored*)

  let assign ctx (lval:lval) (rval:exp) : Dom.t =
    if tracing then trace "osek" "ASSIGN\n";
    if !GU.global_initialization then 
      ctx.local 
    else
      let b1 = M.access_one_byval ctx.ask true (Lval lval) in 
      let b2 = M.access_one_byval ctx.ask false rval in
      add_accesses ctx (b1@b2) (get_flags ctx.presub) ctx.local;
      ctx.local   
   
  let branch ctx (exp:exp) (tv:bool) : Dom.t = 
    let accessed = M.access_one_top ctx.ask false exp in
    add_accesses ctx accessed (get_flags ctx.presub) ctx.local;
    ctx.local

  let body ctx (f:fundec) : Dom.t = 
  if tracing then trace "osek" "Analyszing function %s\n" f.svar.vname;
    let m_st = ctx.local in
    if (is_task f.svar.vname) then begin
(* print_endline ( (string_of_int !Goblintutil.current_loc.line)  ^ " in " ^ !Goblintutil.current_loc.file); *)
(* print_endline ( "Looking for " ^ f.svar.vname); *)
      match M.special_fn (swap_st ctx m_st) None (dummy_get f) [get_lock f.svar.vname] with (*TODO*)
        | [(x,_,_)] -> x 
        | _ -> failwith "This never happens!"     
    end else 
      m_st

  let return ctx (exp:exp option) (f:fundec) : Dom.t =
      let m_st = match exp with 
        | Some exp -> begin
            let accessed = M.access_one_top ctx.ask false exp in
            add_accesses ctx accessed (get_flags ctx.presub) ctx.local;
            ctx.local
          end
        | None -> ctx.local
      in
      let fname = f.svar.vname in
      if (is_task fname) then 
(* let _ = print_endline ( "Leaving task " ^ f.svar.vname) in *)
      match M.special_fn (swap_st ctx m_st) None (dummy_release f) [get_lock fname] with (*TODO*)
        | [(x,_,_)] -> if (get_bool "ana.osek.check") && not(List.mem fname !warned) && not(Dom.is_empty x) then begin
	  warned := fname :: !warned;
	  let typ = if (Hashtbl.mem isrs fname) then "Interrupt " else "Task " in
	  let res = List.fold_left (fun rs r -> (names r) ^ ", " ^ rs) "" (Dom.ReverseAddrSet.elements x) in
	  print_endline( typ ^ fname ^ " terminated while holding resource(s) " ^ res)
	  end; 
	  x 
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
    let fvname = get_api_names f.vname in
    if tracing then trace "osek" "SPECIAL_FN '%s'\n" fvname;
    match fvname with
      | "GetResource" | "ReleaseResource" -> if (get_bool "ana.osek.check") then check_api_use 1 fvname (lockset_to_task (proj2_1 (partition ctx.local)));
	M.special_fn ctx lval f (match arglist with 
	  | [Lval (Var info,_)] -> [get_lock info.vname] 
	  | [CastE (_, Const (CInt64 (c,_,_) ) ) ] | [Const (CInt64 (c,_,_) ) ] -> failwith ("parameter for " ^ fvname ^ " is constant. Remove trampoline header?")
(* | [x] -> let _ = printf "Whatever: %a" (printExp plainCilPrinter) x in [x] *)
	  | x -> x)  
      | "DisableAllInterrupts" -> let res = get_lock "DisableAllInterrupts" in
	if (get_bool "ana.osek.check") then if (mem res ctx.local) then print_endline ( "Nested calls of DisableAllInterrupts are not allowed!");
	M.special_fn ctx lval (dummy_get (Cil.emptyFunction fvname)) [res]
      | "EnableAllInterrupts" -> M.special_fn ctx lval (dummy_release (Cil.emptyFunction fvname)) [get_lock "DisableAllInterrupts"]
      | "SuspendAllInterrupts" -> M.special_fn ctx lval (dummy_get (Cil.emptyFunction fvname)) [get_lock "SuspendAllInterrupts"]
      | "ResumeAllInterrupts" -> M.special_fn ctx lval (dummy_release (Cil.emptyFunction fvname)) [get_lock "SuspendAllInterrupts"] 
      | "SuspendOSInterrupts" -> M.special_fn ctx lval (dummy_get (Cil.emptyFunction fvname)) [get_lock "SuspendOSInterrupts"]
      | "ResumeOSInterrupts" -> M.special_fn ctx lval (dummy_release (Cil.emptyFunction fvname)) [get_lock "SuspendOSInterrupts"]
      | "ActivateTask" -> if (get_bool "ana.osek.check") then check_api_use 1 fvname (lockset_to_task (proj2_1 (partition ctx.local)));
        let _ = (match arglist with (*call function *)
	  | [x] -> let vinfo = eval_arg ctx x in
		   let task_name = make_task(vinfo.vname) in
                   if (is_task task_name) then begin
                     let _ = activate_task ctx task_name in
                     [ctx.local, Cil.integer 1, true]
                   end else failwith (vinfo.vname ^ "is not a task!")
	  | _  -> failwith "ActivateTask arguments are strange") in
	M.special_fn ctx lval f arglist
      | "ChainTask" ->  
        print_endline "Found ChainTask!";
        if (get_bool "ana.osek.check") then check_api_use 2 fvname (lockset_to_task (proj2_1 (partition ctx.local)));
	M.special_fn ctx lval f arglist (*call function *)
      | "WaitEvent" -> (if (get_bool "ana.osek.check") then begin
	let _ = check_api_use 2 fvname (lockset_to_task (proj2_1 (partition ctx.local))) in
	let static,regular = partition ctx.local in
	let task = lockset_to_task static in
	  if not(Dom.is_empty regular) then begin
	    let res = List.fold_left (fun rs r -> (names r) ^ ", " ^ rs) "" (Dom.ReverseAddrSet.elements regular) in
	    let ev = (match arglist with 
	      | [CastE (_, Const (CInt64 (c,_,_) ) ) ] | [Const (CInt64 (c,_,_) ) ] -> let e,_=Hashtbl.find events (Int64.to_string c) in e
	      | _ -> print_endline( "No event found for argument fo WaitEvent");"_not_found_" ) in
	    print_endline( task ^ " waited for event "^ ev^ " while holding resource(s) " ^ res)
	  end;
	end;);
	M.special_fn ctx lval f arglist
      | "SetRelAlarm"
      | "SetAbsAlarm" -> let _ = if (get_bool "ana.osek.check") then check_api_use 1 fvname (lockset_to_task (proj2_1 (partition ctx.local))) in
        let _ = (match arglist with (*call function *)
          | [x;_;_] -> (
            let vinfo = eval_arg ctx x in
            let alarm = vinfo.vname in
            if (Hashtbl.mem alarms alarm) then begin
              let (x,task_list) = Hashtbl.find alarms alarm in
              List.map (activate_task ctx) task_list
            end else
              failwith (alarm ^ "is not an alarm!")
            )    
          | _  -> failwith "SetAlarm arguments are strange")
        in
        M.special_fn ctx lval f arglist
      | "SetEvent"
      | "GetTaskID"
      | "GetTaskState"
      | "GetEvent"
      | "GetAlarmBase" 
      | "GetAlarm" 
      | "CancelAlarm" 
      | "GetActiveApplicationMode" 
      | "ShutdownOS" -> let _ = if (get_bool "ana.osek.check") then check_api_use 1 fvname (lockset_to_task (proj2_1 (partition ctx.local))) in 
	M.special_fn ctx lval f arglist
      | "ClearEvent"
      | "TerminateTask"
      | "Schedule" -> let _ = if (get_bool "ana.osek.check") then check_api_use 2 fvname (lockset_to_task (proj2_1 (partition ctx.local))) in
	M.special_fn ctx lval f arglist
      | "StartOS" -> let _ =if (get_bool "ana.osek.check") then check_api_use 0 fvname (lockset_to_task (proj2_1 (partition ctx.local))) in 
	M.special_fn ctx lval f arglist
      | _ -> M.special_fn ctx lval f arglist
 
  let name = "OSEK analysis"
  let es_to_string f _ = f.svar.vname

  let should_join _ _ = true

(** Finalization and other result printing functions: *)

  (** are we still race free *)
  let race_free = ref true
  let bad_flags = ref []
 
  type access_status = 
    | Race
    | Guarded of  Mutex.Lockset.t
    | Priority of int
    | Defence of int*int
    | Flag of string
    | ReadOnly
    | ThreadLocal
    | BadFlag
    | GoodFlag
 
  let get_acc_map gl =
    let create_map (accesses_map) =
      let f ((((_, _, rw), _, offs),_) as accs) (map,set) =
        if M.OffsMap.mem offs map
        then (M.OffsMap.add offs ([accs] @ (M.OffsMap.find offs map)) map,
              M.OffsSet.add offs set)
        else (M.OffsMap.add offs [accs] map,
              M.OffsSet.add offs set)
      in
      AccValSet.fold f accesses_map (M.OffsMap.empty, M.OffsSet.empty)
    in
    (* join map elements, that we cannot be sure are logically separate *)
    let regroup_map (map,set) =
      let f offs (group_offs, access_list, new_map) = 
        let new_offs = ValueDomain.Offs.definite offs in
        let new_gr_offs = ValueDomain.Offs.join new_offs group_offs in
        (* we assume f is called in the right order: we get the greatest offset first (leq'wise) *)
        if (ValueDomain.Offs.leq new_offs group_offs || (ValueDomain.Offs.is_bot group_offs)) 
        then (new_gr_offs, M.OffsMap.find offs map @ access_list, new_map) 
        else (   new_offs, M.OffsMap.find offs map, M.OffsMap.add group_offs access_list new_map) 
      in
      let (last_offs,last_set, map) = M.OffsSet.fold f set (ValueDomain.Offs.bot (), [], M.OffsMap.empty) in
        if ValueDomain.Offs.is_bot last_offs
        then map
        else M.OffsMap.add last_offs last_set map
    in
    let acc = M.Acc.find acc gl in
    let acc_info = create_map acc in
    let acc_map = if !Mutex.unmerged_fields then proj2_1 acc_info else regroup_map acc_info in
    acc_map
 
  (** [postprocess_acc gl] groups and report races in [gl] *)
  let postprocess_acc gl =
    let is_no_glob (gl:varinfo) = (match gl.vtype with TFun _ -> true | _ -> false )in
    if is_no_glob gl then () else
    (* create mapping from offset to access list; set of offsets  *)
    let get_common_locks acc_list = 
      let f locks ((_,_,writing), lock, _) = 
        let lock = 
          if writing then
            (* when writing: ignore reader locks *)
            Lockset.filter proj2_2 lock 
          else 
            (* when reading: bump reader locks to exclusive as they protect reads *)
            Lockset.map (fun (x,_) -> (x,true)) lock 
        in
          Lockset.join locks lock 
      in
	List.fold_left f (Lockset.bot ()) acc_list
    in
    let rw ((_,_,x),_,_) = x in
    let non_main ((_,x,_),_,_) = Base.Main.Flag.is_bad x in
    let is_race_no_flags acc_list =
      let offpry = offpry acc_list in
      let minpry = minpry acc_list in
      let maxpry = maxpry acc_list in
      let var_str = gl.vname in      
(* let _ = print_endline ("Var: " ^var_str) in *)
(* let _ = print_endline ("Offpry " ^ (string_of_int offpry)) in *)
(* let _ = print_endline ("minpry " ^ (string_of_int minpry)) in *)
(* let _ = print_endline ("maxpry " ^ (string_of_int maxpry)) in*)
      let _ = Hashtbl.add offensivepriorities var_str offpry in
      let locks = get_common_locks acc_list in
      if not (Lockset.is_empty locks || Lockset.is_top locks) then
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
    in (*/is_race_no_flags*)
    let is_race acc_list' =
      let check_valset flag guards acc valset = (*do accesses with flag vaule valset race with guards?*)
        let value = get_val flag (List.hd valset) in
        let may_eq_guards = split_may_eq flag value guards in
        let accs = valset@may_eq_guards in
        acc && (not((is_race_no_flags (strip_flags accs)) = Race))
      in (*/check_valset*)
      let check_one_flag acc_list' flag = (*does flag prevent the race?*)
        let guards,assigns = split_accs_method flag acc_list' in
(* let _ = print_endline ((string_of_int (List.length guards))^" guards and "^string_of_int (List.length assigns)^" assigns"  ) in        *)
        let eq_asgn,weird_asgn = split_accs flag assigns in
        if (is_race_no_flags (strip_flags eq_asgn) = Race) then begin
          [] (* setting does not protect. setters race *)
        end else begin
          (* check each set value against all guard access, which are not ineq value *)
          let valsets = split_equals flag eq_asgn in
          if List.fold_left (check_valset flag guards) true valsets then
            [flag]
          else
            []
        end
      in (*/check_one_flag*)
      let get_keys_from_flag_map flag_map = Flags.fold (fun key value acc -> (key::acc)) flag_map [] in
      let get_flags acc_list' : (Cil.varinfo list) = 
        let flag_list = List.map proj2_2 acc_list' in
        let doit acc flag_map = 
          let rec join l1 l2 = match l2 with 
            | [] -> l1
            | l::ls -> let res = (join l1 ls) in 
            if List.mem l l1 then res else (l :: res)
          in
          let flags = get_keys_from_flag_map flag_map in
          join acc flags
        in  
        List.fold_left doit [] flag_list
      in (*/get_flags*)
      let valid_flag (flag :Cil.varinfo) : bool= 
        let add_flag flag res = 
          if res = BadFlag then begin
            bad_flags := flag::!bad_flags;
            if tracing then trace "osek" "Flag %s is invalid\n" flag.vname
          end else if res = GoodFlag then
            ()
          else
            failwith "This never happens! osekml687"
        in (*/add_flag*)
(*        if List.mem flag !bad_flags then false else*)
        let status_list = ref [] in
        let check_one_list x acc_list' =
          let acc_list = List.map proj2_1 acc_list' in
          let writes,reads = List.partition rw acc_list in
          let res : access_status = 
            if writes = [] || [] = reads then
              BadFlag
            else if ((is_race_no_flags writes) = Race) then
              BadFlag
            else  
              let write_off_pry = offpry writes in
              let read_off_pry = offpry reads in
              if write_off_pry > read_off_pry then
              BadFlag
            else
              GoodFlag
          in
          let _ = add_flag flag res in
          status_list := res::!status_list
        in (*/check_one_list*)
        let _ = M.OffsMap.iter check_one_list  (get_acc_map flag) in
        [] = List.filter (fun x -> x = BadFlag) !status_list
      in (*/valid_flag*)
      let check_flags acc_list' = 
        let flag_list = List.filter valid_flag (get_flags acc_list') in
(* let _ = print_endline ("flaglist: " ^ (List.fold_left (fun x y -> (y.vname ^", " ^x) ) "" flag_list)) in *)
        List.flatten (List.map (check_one_flag acc_list') flag_list) 
      in (*/check_flags*)
      let res = is_race_no_flags (strip_flags acc_list') in
        if res = Race then begin
          if not(List.mem gl.vname !FlagModes.Spec.flag_list) then begin
  (* let _ = print_endline "check flag protection" in *)
            let flags = check_flags acc_list' in 
            if not(flags = []) then
(*   let _ = print_endline "flag!!" in *)
              Flag (flag_list_to_string flags)
            else
              res
          end else begin
          (*handle Flag vars*)
(*            if List.mem gl !bad_flags then
              BadFlag
            else *)
            if (valid_flag gl) then
              GoodFlag 
            else
              BadFlag
          end
        end else begin
          if res = GoodFlag || res = BadFlag then
            failwith "This never happens! osekml650"
          else
            res
        end    
      in
      
      
      let report_race offset acc_list =
        let f  (((loc, fl, write), dom_elem,o),flagstate) = 
          let lock_str = Lockset.short 80 dom_elem in
	  let my_locks = List.map (function (LockDomain.Addr.Addr (x,_) ,_) -> x.vname | _ -> failwith "This (hopefully2) never happens!" ) (Lockset.ReverseAddrSet.elements dom_elem) in
	  let pry = List.fold_left (fun y x -> if pry x > y then pry x else y) (min_int) my_locks in  
	  let flag_str = if !Errormsg.verboseFlag then Pretty.sprint 80 (Flags.pretty () flagstate) else Flags.short 80 flagstate in
          let action = if write then "write" else "read" in
          let thread = "\"" ^ Base.Main.Flag.short 80 fl ^ "\"" in
          let warn = action ^ " in " ^ thread ^ " with priority: " ^ (string_of_int pry) ^ ", lockset: " ^ lock_str ^ " and flag state: " ^flag_str in
            (warn,loc) 
        in (*/f*)       
        let warnings =  List.map f acc_list in
        let var_str = gl.vname ^ ValueDomain.Offs.short 80 offset in
        let safe_str reason = "Safely accessed " ^ var_str ^ " (" ^ reason ^ ")" in
          let res = match is_race acc_list with
            | Race -> begin
                race_free := false;
                let warn = "Datarace at " ^ var_str in
                  print_group warn warnings
              end
            | Guarded locks ->
                let lock_str = Mutex.Lockset.short 80 locks in
                  if (get_bool "allglobs") then
                    print_group (safe_str "common mutex") warnings
                  else 
                    ignore (printf "Found correlation: %s is guarded by lockset %s\n" var_str lock_str)
            | Priority pry ->
                  if (get_bool "allglobs") then
                    print_group (safe_str "same priority") warnings
                  else 
                    ignore (printf "Found correlation: %s is guarded by priority %s\n" var_str (string_of_int pry))
            | Defence (defpry,offpry) ->
                  if (get_bool "allglobs") then
                    print_group (safe_str "defensive priority exceeds offensive priority") warnings
                  else 
                    ignore (printf "Found correlation: %s is guarded by defensive priority %s against offensive priority %s\n" var_str (string_of_int defpry) (string_of_int offpry))
            | Flag (flagvar) ->
                if (get_bool "allglobs") then
		  print_group (safe_str ("variable "^flagvar ^" used to prevent concurrent accesses") ) warnings
		else 
		      ignore (printf "Found correlation: %s is guarded by flag %s\n" var_str flagvar)
	    | ReadOnly ->
                if (get_bool "allglobs") then
                  print_group (safe_str "only read") warnings
            | ThreadLocal ->
                if (get_bool "allglobs") then
                  print_group (safe_str "thread local") warnings
            | BadFlag -> begin
                race_free := false;
                let warn = "Writerace at 'flag' " ^ var_str in
                  print_group warn warnings
              end
            | GoodFlag -> begin 
                if (get_bool "allglobs") then begin
                  print_group (safe_str "is a flag") warnings
                end else  begin
                  ignore (printf "Found flag behaviour: %s\n" var_str)
                end
              end
          in
          res
    in (*/report_race*)
      M.OffsMap.iter report_race (get_acc_map gl)
    
  (** postprocess and print races and other output *)
  let finalize () =
    M.AccKeySet.iter postprocess_acc !accKeys;
    if !Goblintutil.multi_threaded then begin
      if !race_free then 
        print_endline "Goblint did not find any Data Races in this program!";
    end else if not (get_bool "dbg.debug") then begin
      print_endline "NB! That didn't seem like a multithreaded program.";
      print_endline "Try `goblint --help' to do something other than Data Race Analysis."
    end;
    Base.Main.finalize ()

  let init () = (*
    let tramp = get_string "ana.osek.tramp" in
    if Sys.file_exists(tramp) then begin
      parse_tramp tramp;
    end else begin
      prerr_endline "Trampoline headers not found." ;
      exit 2;
    end;*)
    LibraryFunctions.add_lib_funs osek_API_funs;
    let names = !osek_renames in
    if Sys.file_exists(names) then begin
      parse_names names;
    end;
  end

module ThreadMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "OSEK" 
                let depends = ["base";"fmode"]
                type lf = Spec.Dom.t
                let inject_l x = `OSEK x
                let extract_l x = match x with `OSEK x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `Osek x 
                let extract_g x = match x with `Osek x -> x | _ -> raise MCP.SpecificationConversionError
         end)

module Spec2 = Constraints.Spec2OfSpec (Spec)
let _ = 
  MCP.register_analysis "OSEK" ~dep:["base";"fmode"] (module Spec2 : Spec2)         
