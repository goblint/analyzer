open Cil
open Pretty

module Spec =
struct
  module Dom  = Mutex.NoBaseSpec.Dom
  module Glob = Mutex.NoBaseSpec.Glob

  let oilFile = ref ""

  (*priority function, currently onlywokrs for tasks*)
  let pry f = 
    if Sys.file_exists(Filename.dirname(Sys.executable_name) ^ "/osek_temp/priorities.txt") then begin
      let oil = open_in (Filename.dirname(Sys.executable_name) ^ "/osek_temp/priorities.txt") in
      let rec look_up line f =
	match line with
	| "" -> if (input_line oil = "default") then int_of_string (input_line oil) else -1
	| _ -> if line = f.svar.vname then int_of_string(input_line oil) else look_up (input_line oil) f
      in
      let ret = look_up (input_line oil) f in
      close_in oil ; (`Lifted (Int64.of_int ret));
      end else begin
      prerr_endline "Priorites could not be determined." ;
      exit 2;
    end 

  (*brutal hack*)
  let is_task f = 
    (String.length f.svar.vname >= 12 && String.sub f.svar.vname 0 12 = "function_of_")
  let dummyquery _ = `LvalSet (Queries.LS.top())

  let query ask _ (x:Dom.t) (q:Queries.t) : Queries.Result.t = 
    Queries.Result.top ()
 
  (* transfer functions *)
  let assign a (lval:lval) (rval:exp) gl (st:Dom.t) : Dom.t =
    (Mutex.NoBaseSpec.assign a lval rval gl  st)
   
  let branch a (exp:exp) (tv:bool) gl (st:Dom.t) : Dom.t = 
    (Mutex.NoBaseSpec.branch a (exp:exp) (tv:bool) gl st) 
  
  let body a (f:fundec) gl (st:Dom.t) : Dom.t = 
    let m_st = Mutex.NoBaseSpec.body a (f:fundec) gl st in
    if (is_task f) then 
      let task_lock = makeGlobalVar f.svar.vname Cil.voidType  in
      let dummy_edge = makeLocalVar f ?insert:(Some false) "GetResource" Cil.voidType  in
      match Mutex.NoBaseSpec.special_fn a None dummy_edge [Cil.mkAddrOf (Var task_lock, NoOffset)] gl m_st with 
        | [(x,_,_)] -> x 
        | _ -> failwith "This never happens!"     
    else 
      m_st

  let return a (exp:exp option) (f:fundec) gl (st:Dom.t) : Dom.t =
    let m_st = Mutex.NoBaseSpec.return a (exp:exp option) (f:fundec) gl st in
    if (is_task f) then 
      let (x,_,_)= List.hd (Mutex.NoBaseSpec.special_fn a None (makeVarinfo true "ReleaseResource" (TVoid [])) [Lval (Var (makeVarinfo true f.svar.vname (TVoid [])), NoOffset)] gl m_st) in
      x
    else 
      m_st
  
  let eval_funvar a (fv:exp) gl (st:Dom.t) : varinfo list = 
    Mutex.NoBaseSpec.eval_funvar a (fv:exp) gl st
    
  let enter_func a (lval: lval option) (f:varinfo) (args:exp list) gl (st:Dom.t) : (Dom.t * Dom.t) list =
    (Mutex.NoBaseSpec.enter_func a (lval: lval option) (f:varinfo) (args:exp list) gl st)
  
  let leave_func a (lval:lval option) (f:varinfo) (args:exp list) gl (bu:Dom.t) (au:Dom.t) : Dom.t =
   Mutex.NoBaseSpec.leave_func a (lval:lval option) (f:varinfo) (args:exp list) gl bu au
  
  let special_fn a (lval: lval option) (f:varinfo) (arglist:exp list) gl (st:Dom.t) : (Dom.t * Cil.exp * bool) list =
    let constant_lock c =
      if Sys.file_exists(Filename.dirname(Sys.executable_name) ^ "/osek_temp/resources.txt") then begin
	let res = open_in (Filename.dirname(Sys.executable_name) ^ "/osek_temp/resources.txt") in
	let rec look_up line id = 
	  if line = id then input_line res else look_up (input_line res) id in 
	let var = makeVarinfo true (look_up (input_line res) c) (TVoid []) in
	  close_in res; [AddrOf (Var var,NoOffset)];
      end else begin
	prerr_endline "Resources could not be determined." ;
	exit 2;
      end 
    in
    let arglist =
      match f.vname with
        | "GetResource" | "ReleaseResource" -> 
           (match arglist with 
             | [Lval l] -> [AddrOf l] 
	     | [Const (CInt64 (c,_,_) ) ] -> constant_lock (Int64.to_string c)
             | x -> x)
        | _ -> arglist
    in Mutex.NoBaseSpec.special_fn a lval f arglist gl st
  
  let fork ask lv f args gs ls = 
    Mutex.NoBaseSpec.fork ask lv f args gs ls

  let startstate () = Dom.top ()
  let otherstate () = Dom.top ()

  let get_diff _ = []
  let reset_diff x = x
  
  let name = "Thread analysis"
  let es_to_string f _ = f.svar.vname

  let should_join _ _ = true


(** Finalization and other result printing functions: *)

  (** are we still race free *)
  let race_free = ref true

(*
  (** modules used for grouping [varinfo]s by [Offset] *)
  module OffsMap = Map.Make (Mutex.Offs)
  (** modules used for grouping [varinfo]s by [Offset] *)
  module OffsSet = Set.Make (ValueDomain.Offs)

  module AccKeySet = Set.Make (Basetype.Variables)
  module AccValSet = Set.Make (Printable.Prod3 (Printable.Prod3 (Basetype.ProgLines) (Base.Main.Flag) (IntDomain.Booleans)) (Dom) (ValueDomain.Offs))
  module Acc = Hashtbl.Make (Basetype.Variables)
  let accKeys : AccKeySet.t ref   = ref AccKeySet.empty
  let acc     : AccValSet.t Acc.t = Acc.create 100
*)

  type access_status = 
    | Race
    | Guarded of  Mutex.Lockset.t
    | ReadOnly
    | ThreadLocal


  (** [postprocess_acc gl] groups and report races in [gl] *)
  let postprocess_acc gl =
    (* create mapping from offset to access list; set of offsets  *)
    let acc = Mutex.NoBaseSpec.acc in
    let create_map (accesses_map) =
      let f (((_, _, rw), _, offs) as accs) (map,set) =
        if Mutex.NoBaseSpec.OffsMap.mem offs map
        then (Mutex.NoBaseSpec.OffsMap.add offs ([accs] @ (Mutex.NoBaseSpec.OffsMap.find offs map)) map,
              Mutex.NoBaseSpec.OffsSet.add offs set)
        else (Mutex.NoBaseSpec.OffsMap.add offs [accs] map,
              Mutex.NoBaseSpec.OffsSet.add offs set)
      in
      Mutex.NoBaseSpec.AccValSet.fold f accesses_map (Mutex.NoBaseSpec.OffsMap.empty, Mutex.NoBaseSpec.OffsSet.empty)
    in
    (* join map elements, that we cannot be sure are logically separate *)
    let regroup_map (map,set) =
      let f offs (group_offs, access_list, new_map) = 
        let new_offs = Mutex.Offs.definite offs in
        let new_gr_offs = Mutex.Offs.join new_offs group_offs in
        (* we assume f is called in the right order: we get the greatest offset first (leq'wise) *)
        if (Mutex.Offs.leq new_offs group_offs || (Mutex.Offs.is_bot group_offs)) 
        then (new_gr_offs, Mutex.NoBaseSpec.OffsMap.find offs map @ access_list, new_map) 
        else (   new_offs, Mutex.NoBaseSpec.OffsMap.find offs map, Mutex.NoBaseSpec.OffsMap.add group_offs access_list new_map) 
      in
      let (last_offs,last_set, map) = Mutex.NoBaseSpec.OffsSet.fold f set (Mutex.Offs.bot (), [], Mutex.NoBaseSpec.OffsMap.empty) in
        if Mutex.Offs.is_bot last_offs
        then map
        else Mutex.NoBaseSpec.OffsMap.add last_offs last_set map
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
      let locks = get_common_locks acc_list in let _ = print_endline ((Mutex.Lockset.short 80 locks)^"bla") in
      let rw ((_,_,x),_,_) = x in
      let non_main ((_,x,_),_,_) = Base.Main.Flag.is_bad x in      
        if not (Mutex.Lockset.is_empty locks || Mutex.Lockset.is_top locks) then
          Guarded locks
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
	  let priority = "-1" in
          let action = if write then "write" else "read" in
          let thread = if Mutex.BS.Flag.is_bad fl then "some thread" else "main thread" in
          let warn = action ^ " in " ^ thread ^ " with priority " ^ priority ^ "and lockset: " ^ lockstr in
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
            | ReadOnly ->
                if !Mutex.GU.allglobs then
                  Mutex.M.print_group (safe_str "only read") warnings
            | ThreadLocal ->
                if !Mutex.GU.allglobs then
                  Mutex.M.print_group (safe_str "thread local") warnings
    in 
    let rw ((_,_,x),_,_) = x in
    let _ =       print_endline ("bla1") in
    let acc = Mutex.NoBaseSpec.Acc.find acc gl in
    let _ =       print_endline ("bla2") in
    let acc = if !Mutex.no_read then Mutex.NoBaseSpec.AccValSet.filter rw acc else acc in
    let _ =       print_endline ("bla3") in
    let acc_info = create_map acc in
    let acc_map = if !Mutex.unmerged_fields then fst acc_info else regroup_map acc_info in
      Mutex.NoBaseSpec.OffsMap.iter report_race acc_map
    
  (** postprocess and print races and other output *)
  let finalize () = ignore (Unix.system("rm -rf osek_temp") );Mutex.NoBaseSpec.finalize () (*
    Mutex.NoBaseSpec.AccKeySet.iter postprocess_acc !Mutex.NoBaseSpec.accKeys;
    if !Mutex.GU.multi_threaded then begin
      match !race_free, !Messages.soundness with
        | true, true -> 
            print_endline "CONGRATULATIONS!\nYour program has just been certified Free of Data Races!";
            if not (!Mutex.failing_locks) then print_endline  "(Assuming locking operations always succeed.)"
        | true, false -> 
            print_endline "Goblint did not find any Data Races in this program!";
            print_endline "However, the code was too complicated for Goblint to understand all of it."
        | false, true -> 
            print_endline "And that's all. Goblint is certain there are no other races.";
            if not (!Mutex.failing_locks) then print_endline  "(Assuming locking operations always succeed.)"
        | _ -> 
            print_endline "And there may be more races ...";
            print_endline "The code was too complicated for Goblint to understand all of it."
    end else if not !Goblintutil.debug then begin
      print_endline "NB! That didn't seem like a multithreaded program.";
      print_endline "Try `goblint --help' to do something other than Data Race Analysis."
    end;
    ignore (Unix.system("rm -rf osek_temp") );
    Base.Main.finalize ()
*)
  let init () =   
    if !oilFile != "" && Sys.file_exists(!oilFile) then begin
      ignore (Unix.system("mkdir osek_temp") );
      ignore (Unix.system( "ruby " ^ Filename.dirname(Sys.executable_name) ^"/scripts/parse_oil.rb " ^ !oilFile ^ " " ^ Filename.dirname(Sys.executable_name)) );
      let tramp = Filename.dirname(!oilFile) ^ "/defaultAppWorkstation/tpl_os_generated_configuration.h" in
      if Sys.file_exists(tramp) then begin
	ignore (Unix.system ("ruby " ^ Filename.dirname(Sys.executable_name) ^ "/scripts/parse_trampoline.rb " ^ tramp ^ " " ^ Filename.dirname(Sys.executable_name)) )
      end else begin
	prerr_endline "Trampoline headers not found." ;
	exit 2;
      end
    end else begin
      prerr_endline "OIL-file does not exist." ;
      exit 2;
    end
end

module Path     : Analyses.Spec = Compose.PathSensitive (Spec)
module Analysis : Analyses.S    = Multithread.Forward(Path)
