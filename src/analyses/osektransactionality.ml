open Cil
open Pretty
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "OSEK trasactionality"
  module Dom  = Osektupel
  module Glob = Global.Make (Lattice.Unit)
  
  type glob_fun = Glob.Var.t -> Glob.Val.t

  let min' x y =  
    match (x,y) with
      | (-1,-1) -> -1
      | (-1,_)  -> y
      | (_,-1)  -> x
      | _       -> min x y

  (* composition operator  (b \fcon a) *)
  let fcon (a1,a2,a3,a4 as a) (b1,b2,b3,b4 as b) =  if (Osektupel.is_top b) then a else
    match (a2,b2) with
      | (-1,-1) -> (a1,         a2,         a3,          min' a4 b4 )
      | (-1,_)  -> (b1,         b2,         min' a4 b3  ,min' a4 b4 )
      | (_,-1)  -> (a1,         min' a2 b4 ,a3,          min' a4 b4 )
      | _       -> (min' a2 b3 ,min' a2 b4 ,min' a4 b3  ,min' a4 b4 )

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t =
    fcon  ctx.local (-1,-1,-1,-1)
   
  let branch ctx (exp:exp) (tv:bool) : Dom.t = 
    fcon  ctx.local (-1,-1,-1,-1)
  
  let body ctx (f:fundec) : Dom.t = 
    fcon  ctx.local (-1,-1,-1,-1)

  let return ctx (exp:exp option) (f:fundec) : Dom.t = 
    fcon  ctx.local (-1,-1,-1,-1)
  
  let eval_funvar ctx (fv:exp) : varinfo list = 
    []
    
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list =
    [ctx.local,ctx.local]
  
  let leave_func ctx (lval:lval option) (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t =
    au
  
  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =

    match f.vname with
      | "ReleaseResource" -> [fcon  ctx.local (-1,-1,-1,-1),Cil.integer 1, true]
      | _ -> [fcon  ctx.local (-1,-1,-1,-1),Cil.integer 1, true]

  let fork ctx lv f args = 
    [] 

  let startstate () = Dom.bot ()
  let otherstate () = Dom.top ()

(** Finalization and other result printing functions: *)

  (** are we still race free *)
  let race_free = ref true

  let pry = Osek.Spec.pry

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
    Base.Main.finalize ()

  let init () = ()

end

module ThreadMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "OSEK2" 
                let depends = ["OSEK"]
                type lf = Spec.Dom.t
                let inject_l x = `OSEK2 x
                let extract_l x = match x with `OSEK2 x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)
         
module Path     : Analyses.Spec = Compose.PathSensitive (Spec)
module Analysis : Analyses.S    = Multithread.Forward(Path)
