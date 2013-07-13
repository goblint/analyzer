(** An analyzer that takes the CFG from [MyCFG], a solver from [Selector], constraints from [Constraints] (using the specification from [MCP]) *) 

open Cil
open MyCFG
open Pretty
open Analyses
open GobConfig
open Constraints
open Batteries

(** Given a [Cfg], computes the solution to [MCP.Path] *)
module AnalyzeCFG (Cfg:CfgBidir) =
struct
  
  (** The main function to preform the selected analyses. *)
  let analyze (file: file) (startfuns, exitfuns, otherfuns: Analyses.fundecs)  (module Spec : Spec) =   
    (** The Equation system *)
    let module EQSys = FromSpec (Spec) (Cfg) in
  
    (** Hashtbl for locals *)
    let module LHT   = BatHashtbl.Make (EQSys.LVar) in
    (** Hashtbl for globals *)
    let module GHT   = BatHashtbl.Make (EQSys.GVar) in
  
    (** The solver *)
    let module Slvr  = Selector.Make (EQSys) (LHT) (GHT) in
    (** The verifyer *)
    let module Vrfyr = Verify2 (EQSys) (LHT) (GHT) in
    (** Another iterator. Set "exp.use_gen_solver" to false. *)
    let module I = IterateLikeAstree (Spec) (Cfg) (GHT) in

    (** Triple of the function, context, and the local value. *)
    let module RT = Analyses.ResultType2 (Spec) in
    (** Set of triples [RT] *)
    let module LT = SetDomain.HeadlessSet (RT) in
    (** Analysis result structure---a hashtable from porgram points to [LT] *)
    let module Result = Analyses.Result (LT) (struct let result_name = "analysis" end) in

    (** print out information about dead code *)
    let print_dead_code (xs:Result.t) =
      let open BatMap in let open BatPrintf in
      let module StringMap = Make (String) in
      let m = ref StringMap.empty in
      let add_one (l,_,f) v =
        if LT.for_all (fun (_,x,f) -> Spec.D.is_bot x) v &&f.svar.vdecl<>l then
          let add_fun  = BatISet.add l.line in
          let add_file = StringMap.modify_def BatISet.empty f.svar.vname add_fun in
          m := StringMap.modify_def StringMap.empty l.file add_file !m
      in
      Result.iter add_one xs;
      let print_func f xs =
        let one_range b e first =
          if not first then printf ", ";
          begin if b=e then
            printf "%d" b
          else
            printf "%d..%d" b e
          end; false
        in
        printf "  function '%s' has dead code on lines: " f;
        ignore (BatISet.fold_range one_range xs true);
        printf "\n"
      in
      let print_file f =
        printf "File '%s':\n" f;
        StringMap.iter print_func
      in
      if StringMap.is_empty !m
      then printf "No dead code found!\n"
      else StringMap.iter print_file !m
    in
  
    (** convert result that can be out-put *)
    let solver2source_result h : Result.t =
      (* processed result *)
      let res = Result.create 113 in
      
      (* Adding the state at each system variable to the final result *)
      let add_local_var (n,es) state =
        let loc = MyCFG.getLoc n in
        if loc <> locUnknown then try 
          let (_,_, fundec) as p = loc, n, MyCFG.getFun n in
          if Result.mem res p then 
            (* If this source location has been added before, we look it up
             * and add another node to it information to it. *)
            let prev = Result.find res p in
            Result.replace res p (LT.add (es,state,fundec) prev)
          else 
            Result.add res p (LT.singleton (es,state,fundec))
          (* If the function is not defined, and yet has been included to the
           * analysis result, we generate a warning. *)
        with Not_found -> Messages.warn ("Undefined function has escaped.")
      in
        LHT.iter add_local_var h;
        res
    in

    (** exctract global xml from result *)
    let make_global_xml g =
      let one_glob k v = 
        let k = Xml.PCData k.vname in
        let varname = Xml.Element ("td",[],[k]) in
        let varvalue = Xml.Element ("td",[],[Spec.G.toXML v]) in
        Xml.Element ("tr",[],[varname; varvalue])
      in
      let head = 
        Xml.Element ("tr",[],[Xml.Element ("th",[],[Xml.PCData "var"])
                             ;Xml.Element ("th",[],[Xml.PCData "value"])])
      in 
      let collect_globals k v b = one_glob k v :: b in
        Xml.Element ("table", [], head :: GHT.fold collect_globals g [])
    in  

    (** add extern variables to local state *)
    let do_extern_inits ctx (file : file) : Spec.D.t =
      let module VS = Set.Make (Basetype.Variables) in    
      let add_glob s = function
          GVar (v,_,_) -> VS.add v s
        | _            -> s
      in
      let vars = foldGlobals file add_glob VS.empty in
      let set_bad v st =
        Spec.assign {ctx with local = st} (var v) MyCFG.unknown_exp 
      in
      let add_externs s = function
        | GVarDecl (v,_) when not (VS.mem v vars || isFunctionType v.vtype) -> set_bad v s
        | _ -> s
      in    
      foldGlobals file add_externs (Spec.startstate MyCFG.dummy_func.svar)
    in
    
    (** analyze cil's global-inits function to get a starting state *)
    let do_global_inits (file: file) : Spec.D.t * fundec list = 
      let ctx = 
        { ask     = (fun _ -> Queries.Result.top ())
        ; local   = Spec.D.top ()
        ; global  = (fun _ -> Spec.G.bot ())
        ; presub  = []
        ; postsub = []
        ; spawn   = (fun _ -> failwith "Global initializers should never spawn threads. What is going on?") 
        ; split   = (fun _ -> failwith "Global initializers trying to split paths.")
        ; sideg   = (fun _ -> failwith "Global initializers trying to side-effect globals.")
        } 
      in  
      let edges = MyCFG.getGlobalInits file in
      let funs = ref [] in
      (*let count = ref 0 in*)
      let transfer_func (st : Spec.D.t) (edge, loc) : Spec.D.t = 
        try
          if M.tracing then M.trace "con" "Initializer %a\n" d_loc loc;
          (*incr count;
          if (get_bool "dbg.verbose")&& (!count mod 1000 = 0)  then Printf.printf "%d %!" !count;    *)
          Tracing.current_loc := loc;
          match edge with
            | MyCFG.Entry func        -> Spec.body {ctx with local = st} func
            | MyCFG.Assign (lval,exp) -> 
                begin match lval, exp with
                  | (Var v,o), (AddrOf (Var f,NoOffset)) 
                    when v.vstorage <> Static && isFunctionType f.vtype -> 
                    begin try funs := Cilfacade.getdec f :: !funs with Not_found -> () end 
                  | _ -> ()
                end;
                Spec.assign {ctx with local = st} lval exp
            | _                       -> raise (Failure "This iz impossible!") 
        with Failure x -> M.warn x; st
      in
      let with_externs = do_extern_inits ctx file in
      (*if (get_bool "dbg.verbose") then Printf.printf "Number of init. edges : %d\nWorking:" (List.length edges);    *)
      let result : Spec.D.t = List.fold_left transfer_func with_externs edges in
        result, !funs
    in
    
    let print_globals glob = 
      let out = M.get_out Spec.name !GU.out in
      let print_one v st =
        ignore (Pretty.fprintf out "%a -> %a\n" EQSys.GVar.pretty_trace v Spec.G.pretty st)
      in
        GHT.iter print_one glob
    in

    (* real beginning of the [analyze] function *)
    let early = (get_bool "exp.earlyglobs") in
    let _ = GU.global_initialization := true in
    let _ = set_bool "exp.earlyglobs" false in
    Spec.init ();
    
    let startstate, more_funs = 
      if (get_bool "dbg.verbose") then print_endline "Initializing globals.";
      do_global_inits file 
    in
    
    let otherfuns = if get_bool "kernel" then otherfuns @ more_funs else otherfuns in
  
    let enter_with st fd =
      let st = st fd.svar in
      let ctx = 
        { ask     = (fun _ -> Queries.Result.top ())
        ; local   = st
        ; global  = (fun _ -> Spec.G.bot ())
        ; presub  = []
        ; postsub = []
        ; spawn   = (fun _ -> failwith "Bug1: Using enter_func for toplevel functions with 'otherstate'.") 
        ; split   = (fun _ -> failwith "Bug2: Using enter_func for toplevel functions with 'otherstate'.")
        ; sideg   = (fun _ -> failwith "Bug3: Using enter_func for toplevel functions with 'otherstate'.")
        } 
      in
      let args = List.map (fun x -> MyCFG.unknown_exp) fd.sformals in
      let ents = Spec.enter ctx None fd.svar args in
        List.map (fun (_,s) -> fd.svar, s) ents  
    in
  
    let _ = try MyCFG.dummy_func.svar.vdecl <- (List.hd otherfuns).svar.vdecl with Failure _ -> () in
  
    let startvars = 
      if startfuns = [] 
      then [[MyCFG.dummy_func.svar, startstate]]
      else 
        let morph f = Spec.morphstate f startstate in
        List.map (enter_with morph) startfuns 
    in
   
    let exitvars = List.map (enter_with Spec.exitstate) exitfuns in
    let othervars = List.map (enter_with Spec.otherstate) otherfuns in
    let startvars = List.concat (startvars @ exitvars @ othervars) in
  
    let _ = 
    if startvars = [] 
      then failwith "BUG: Empty set of start variables; may happen if\
         enter_func of any analysis returns an empty list." 
    in
    let _ = set_bool "exp.earlyglobs" early in
    let _ = GU.global_initialization := false in
    
    let startvars' = 
      List.map (fun (n,e) -> (MyCFG.Function n, Spec.context e)) startvars in
  
    let entrystates = 
      List.map (fun (n,e) -> (MyCFG.FunctionEntry n, Spec.context e), e) startvars in
  
    
    let local_xml = ref (Result.create 0) in
    let global_xml = ref (Xml.PCData "not-ready" ) in
    let do_analyze_using_solver () = 
      let lh, gh = Slvr.solve entrystates [] startvars' in
      
      if not (get_bool "noverify") then begin
        if (get_bool "dbg.verbose") then print_endline "Verifying the result.";
        Goblintutil.may_narrow := false;
        Vrfyr.verify lh gh;
      end;
      
      local_xml := solver2source_result lh;
      global_xml := make_global_xml gh;
      
      if Progress.tracking then 
        begin 
          Progress.track_with_profile () ;
          Progress.track_call_profile ()
        end ;
      let firstvar = List.hd startvars' in
      let mainfile = match firstvar with (MyCFG.Function fn, _) -> fn.vdecl.file | _ -> "Impossible!" in
      let module S = Set.Make (Int) in
      if (get_bool "dbg.uncalled") then
        begin
          let out = M.get_out "uncalled" Legacy.stdout in
          let f =
            let insrt k _ s = match k with
              | (MyCFG.Function fn,_) -> if not (get_bool "exp.forward") then S.add fn.vid s else s
              | (MyCFG.FunctionEntry fn,_) -> if (get_bool "exp.forward") then S.add fn.vid s else s
              | _ -> s
            in
            (* set of ids of called functions *)
            let calledFuns = LHT.fold insrt lh S.empty in
            function
              | GFun (fn, loc) when loc.file = mainfile && not (S.mem fn.svar.vid calledFuns) ->
                  begin
                    let msg = "Function \"" ^ fn.svar.vname ^ "\" will never be called." in
                    ignore (Pretty.fprintf out "%s (%a)\n" msg Basetype.ProgLines.pretty loc)
                  end
              | _ -> ()
          in
            List.iter f file.globals;
        end;
        
      (* check for dead code at the last state: *)
      let main_sol = try LHT.find lh (List.hd startvars') with Not_found -> Spec.D.bot () in
      (if (get_bool "dbg.debug") && Spec.D.is_bot main_sol then
        Printf.printf "NB! Execution does not reach the end of Main.\n");
        
      if get_bool "dump_globs" then 
        print_globals gh  
    in

    let do_analyze_using_iterator () = 
      let _ = I.iterate file startvars' in
      print_endline "done. "
    in
  
    if get_bool "exp.use_gen_solver" then begin
      (* Use "normal" constraint solving *)
      if (get_bool "dbg.verbose") then 
        print_endline ("Solving the constraint system with " ^ get_string "solver" ^ ".");
      Goblintutil.timeout do_analyze_using_solver () (float_of_int (get_int "dbg.timeout"))
        (fun () -> Messages.waitWhat "Timeout reached!");
    end else begin
      (* ... or give in to peer-pressure? *)
      if (get_bool "dbg.verbose") then
        print_endline ("Pretending to be French ...");
      Goblintutil.timeout do_analyze_using_iterator () (float_of_int (get_int "dbg.timeout"))
        (fun () -> Messages.waitWhat "Timeout reached!");
    end;
  
    Spec.finalize ();
        
    if (get_bool "dbg.print_dead_code") then print_dead_code !local_xml;
    if (get_bool "dbg.verbose") then print_endline "Generating output.";
    Result.output (lazy !local_xml) (lazy (!global_xml :: [])) file
  
  let analyze f sf = 
    if get_bool "ana.hashcons" then
      analyze f sf (module (DeadCodeLifter (HashconsLifter (PathSensitive2 (MCP.MCP2)))) : Spec)
    else 
      analyze f sf (module (DeadCodeLifter (PathSensitive2 (MCP.MCP2))) : Spec)
end

(** The main function to preform the selected analyses. *)
let analyze (file: file) fs = 
  if (get_bool "dbg.verbose") then print_endline "Generating the control flow graph."; 
  let cfgF, cfgB = MyCFG.getCFG file in
  let cfgB' = function
      | MyCFG.Statement s as n -> (MyCFG.SelfLoop, n) :: cfgB n
      | n -> cfgB n
  in
  let cfgB = if (get_bool "ana.osek.intrpts") then cfgB' else cfgB in
  let module CFG = struct let prev = cfgB let next = cfgF end in
  let module A = AnalyzeCFG (CFG) in
    A.analyze file fs 
