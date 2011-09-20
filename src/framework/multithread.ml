module A = Analyses
module M = Messages
module P = Progress
module GU = Goblintutil
module Glob = Basetype.Variables
module Stmt = Basetype.CilStmt
module Func = Basetype.CilFun

open Cil
open Pretty

(** Forward analysis using a specification [Spec] *)
module Forward 
  (Spec : Analyses.Spec) 
  (ToStd: Analyses.DomainTranslator 
          with type from_type = Spec.Dom.t
          and  type to_type   = A.local_state list list
  ) 
  (ToStdG: Analyses.DomainTranslator 
          with type from_type = Spec.Glob.Val.t
          and  type to_type   = Analyses.global_state list
  ) 
  : Analyses.S 
  =
struct
  (** Augment domain to lift dead code *)
  module SD  = A.Dom (Spec.Dom)
  (** Solver variables use global part from [Spec.Dep] *)
  module Var = A.VarF (SD)

  module SP_SOL = Hashtbl.Make (Solver.Prod (Analyses.Var) (SD))
  module SP = SharirPnueli.Algorithm (Analyses.Var) (SD)
  
  module PH = Hashtbl.Make (Analyses.Var)        (* local info from previous phases *)
  module PHG = Hashtbl.Make (Basetype.Variables) (* global info from previous phases *)
  module SH = Hashtbl.Make (Analyses.Edge)       (* spawns from previous phases *)

  module Solver = 
  struct
    module Sol = Solver.Types (Var) (SD) (Spec.Glob)
    include Sol
    
    module EWC  = EffectWCon.Make(Var)(SD)(Spec.Glob)
    module EWNC = EffectWNCon.Make(Var)(SD)(Spec.Glob)
    module SCSRR= SolverConSideRR.Make(Var)(SD)(Spec.Glob)
    module WNRR = SolverConSideWNRR.Make(Var)(SD)(Spec.Glob)
    let solve () : system -> variable list -> (variable * var_domain) list -> solution'  = 
      match !GU.solver with 
        | "effectWNCon"     -> EWNC.solve
        | "effectWCon"      -> EWC.solve
        | "solverConSideRR" -> SCSRR.solve
        | "solverConSideWNRR" -> WNRR.solve
        | _ -> EWC.solve
  end
  (** name the analyzer *)
  let name = "analyzer"
  let top_query x = Queries.Result.top () 
  
  let system (cfg: MyCFG.cfg) (old : Analyses.local_state list list PH.t list) (old_g : Analyses.global_state list PHG.t list) (old_s : (varinfo * int) list SH.t) phase (n,es: Var.t) : Solver.rhs list = 
    if M.tracing then M.trace "con" "%a\n" Var.pretty_trace (n,es);
    
    let lift_st x forks: Solver.var_domain * Solver.diff * Solver.variable list =
      let diff = [] in
      let rx = x in
        (SD.lift rx, diff, forks)
    in
    
    (* this function used to take the spawned function and prepare a solver variable to it --
       now it also assures that spawning is monotone between analysis stages *)
    let prepare_forks phase (fn,ed,tn : MyCFG.node * MyCFG.edge * MyCFG.node) (xs: (varinfo * Spec.Dom.t) list) : Solver.diff * Solver.variable list =
      let f (diff, vars) (fork_fun, fork_st) =
        if !GU.full_context then
          ( diff, (MyCFG.Function fork_fun, SD.lift fork_st) :: vars)
        else
          ( (`L ((MyCFG.FunctionEntry fork_fun, SD.lift (Spec.context_top fork_st)), SD.lift fork_st)) :: diff
          , (MyCFG.Function fork_fun, SD.lift (Spec.context_top fork_st)) :: vars)
      in
      let diffs, spawns = List.fold_left f ([],[]) xs in
      let spawnfuns = List.map fst xs in
      let old_spawns =  try SH.find old_s (fn,ed,tn) with Not_found -> [] in
      let mon_spawns = List.filter (fun (x,y) -> phase>y && not (List.exists (Basetype.Variables.equal x) spawnfuns)) old_spawns in
      let new_spawns = List.filter (fun x -> List.for_all (fun (z,_) -> not (Basetype.Variables.equal x z)) old_spawns) spawnfuns in
      SH.replace old_s (fn,ed,tn) (List.map (fun x -> (x,phase)) new_spawns@(try SH.find old_s (fn,ed,tn) with Not_found -> [])); 
      List.fold_left f (diffs, spawns) (List.map (fun (x,_) -> (x, Spec.otherstate ())) mon_spawns)
    in
    (*
      Compiles a transfer function that handles function calls. The reason for this is
      that we do not want the analyzer to decend into function calls, but rather we want 
      to analyze functions separatly and then merge the result into function calls.
      
      First we get a list of possible functions that can occur evaluating [exp], and 
      for each function [f] we do the following:
      
      If there does not exist a declaration of [f] the result will be [special_fn lval f exp args st].
      
      If a declaration is available:
        1) [enter_func lval f args st] gives us minimal entry states to analize [f]
        2) we analyze [f] with each given entry state, giving us a result list [rs]
        3) we postprocess [rs] with maping it with [leave_func lval f args st]
        4) join the postprocessed result
      
      Also we concatenate each [forks lval f args st] for each [f]
      *)
    let proc_call (fn,ed,tn) sigma (theta:Solver.glob_assign) lval exp args st : Solver.var_domain * Solver.diff * Solver.variable list =
      let forks = ref [] in
      let diffs = ref [] in
      let add_var v d = if not (List.mem v.vname !GU.mainfuns) then forks := (v,d) :: !forks in
      let add_diff g d = diffs := (`G (g,d)) :: !diffs in 
      let getctx v= 
        try
          let oldstate = List.concat (List.map (fun m -> match PH.find m tn with [] -> raise A.Deadcode | x -> x) old) in
          let oldglob = List.map PHG.find old_g in
          A.set_preglob (A.set_precomp (A.context top_query v theta [] add_var add_diff) oldstate) oldglob 
        with Not_found  -> Messages.warn "Analyzing a program point that was thought to be unreachable.";
                           raise A.Deadcode
      in
      let funs  = 
        match Spec.query (getctx st) (Queries.EvalFunvar exp) with
          | `LvalSet ls -> Queries.LS.fold (fun ((x,_)) xs -> x::xs) ls [] 
          | _ -> Messages.bailwith ("ProcCall: Failed to evaluate function expression "^(sprint 80 (d_exp () exp)))
      in
      let dress (f,es)  = (MyCFG.Function f, SD.lift es) in
      let start_vals : Solver.diff ref = ref [] in
      let add_function st' f : Spec.Dom.t =
        let add_one_call x =
          if !GU.full_context then
            sigma (dress (f, x)) 
          else begin
            let ctx_st = Spec.context_top x in
            start_vals := (`L ((MyCFG.FunctionEntry f, SD.lift ctx_st), SD.lift x)) :: !start_vals ;
            sigma (dress (f, ctx_st)) 
          end
        in
        let has_dec = try ignore (Cilfacade.getdec f); true with Not_found -> false in        
        if has_dec && not (LibraryFunctions.use_special f.vname) then
			  begin
          let work = Spec.enter_func (getctx st) lval f args in
          let leave st1 st2 = Spec.leave_func (getctx st1) lval exp f args st2 in
          let general_results = List.map (fun (y,x) -> y, add_one_call x) work in
          let non_bottoms     = List.filter (fun (_,x) -> not (SD.is_bot x)) general_results in
          let joined_result   = 
            if List.length non_bottoms = 0 
              then raise A.Deadcode 
              else List.fold_left (fun st (fst,tst) -> Spec.Dom.join st (leave fst (SD.unlift tst))) (Spec.Dom.bot ()) non_bottoms in
          if P.tracking then P.track_call f (SD.hash st) ;
          Spec.Dom.join st' joined_result
			  end        
        else
				begin
          let joiner d1 (d2,_,_) = Spec.Dom.join d1 d2 in 
          List.fold_left joiner (Spec.Dom.bot ()) (Spec.special_fn (getctx st) lval f args)
			  end 
      in
      try 
        let d = List.fold_left add_function (Spec.Dom.bot ()) funs in      
        let fdiff, fvars = prepare_forks phase (fn,ed,tn) !forks in
        (SD.lift d, fdiff @ !start_vals @ !diffs, fvars)
      with 
				Analyses.Deadcode -> (SD.bot (), !start_vals, [])
    in
    let cfg' n = 
      match n with 
        | MyCFG.Statement s -> (MyCFG.SelfLoop, n) :: cfg n
        | _ -> cfg n
    in
    let cfg = if !GU.intrpts then cfg' else cfg in
      
    (* Find the edges entering this edge *)
    let edges : (MyCFG.edge * MyCFG.node) list = cfg n in
      
    (* For each edge we generate a rhs: a function that takes current state
     * sigma and the global state theta; it outputs the new state, delta, and
     * spawned calls. *)      
    let edge2rhs (edge, pred : MyCFG.edge * MyCFG.node) (sigma, theta: Solver.var_assign * Solver.glob_assign) : Solver.var_domain * Solver.diff * Solver.variable list = 
      (* This is the key computation, only we need to set and reset current_loc,
       * see below. We call a function to avoid ;-confusion *)
      let eval predvar : Solver.var_domain * Solver.diff * Solver.variable list = 
        (* if we use full contexts then the value at entry is the entry state -- if we do 
           not use full contexts then we hope that someting meaningful was side-effected there *)
        let predval =
          match edge with
            | MyCFG.Entry _ when !GU.full_context -> es
            | _ -> sigma predvar 
        in
        if P.tracking then P.track_with (fun n -> M.warn_all (sprint ~width:80 (dprintf "Line visited more than %d times. State:\n%a\n" n SD.pretty predval)));
        (* gives add_var callback into context so that every edge can spawn threads *)
        let diffs = ref [] in
        let add_diff g d = diffs := (`G (g,d)) :: !diffs in 
        let getctx v y = 
          try
            let oldstate = List.concat (List.map (fun m -> match PH.find m pred with [] -> raise A.Deadcode | x -> x) old) in
            let oldglob = List.map PHG.find old_g in
            A.set_preglob (A.set_precomp (A.context top_query v theta [] y add_diff) oldstate) oldglob 
          with Not_found  -> Messages.warn "Analyzing a program point that was thought to be unreachable.";
                             raise A.Deadcode
        in
        let lift f pre =        
          let forks = ref [] in
          let add_var v d = forks := (v,d) :: !forks in
          let x, y, z = lift_st (f (getctx pre add_var)) [] in
          let y', z' = prepare_forks phase (n,edge,pred) !forks in
          x, y @ y', z @ z'
        in
        try
          if !Messages.worldStopped then raise M.StopTheWorld else
          begin match pred with
            | MyCFG.Statement s -> Cilfacade.currentStatement := s 
            | _ -> ()
          end;
          (* We synchronize the predecessor value with the global invariant and
           * then feed the updated value to the transfer functions. *)
          let add_novar v d = M.bailwith "Bug: Sync should not be able to spawn threads. Ignored!" in
          let predval', diff = Spec.sync (getctx (SD.unlift predval) add_novar) in
          let diff = List.map (fun x -> `G x) diff in
          (* For single threaded execution all global information is stored in the local state and 
             data is moved to global state if threads are spawned. In the kernel init. functions
             no threads are spawned and so the global information got lost.*)
          let toplevel_kernel_return r fd ctx =
            let st = if fd.svar.vname = MyCFG.dummy_func.svar.vname then ctx.Analyses.local else Spec.return ctx r fd in
            let spawning_return = Spec.return (A.swap_st ctx st) None MyCFG.dummy_func in
            let nval, ndiff = Spec.sync (getctx spawning_return add_novar) in
            List.iter (fun (x,y) -> add_diff x y) ndiff;
            nval
          in
          (* Generating the constraints is quite straightforward, except maybe
           * the call case. There is an ALMOST constant lifting and unlifting to
           * handle the dead code -- maybe it could be avoided  *)
          let l,gd,sp =
          match edge with
            | MyCFG.Ret    (ret,fundec)    when (fundec.svar.vname = MyCFG.dummy_func.svar.vname || List.mem fundec.svar.vname !GU.mainfuns) && !GU.kernel
                                           -> lift (toplevel_kernel_return ret fundec    ) predval'
            | MyCFG.Entry func             -> lift (fun ctx -> Spec.body   ctx func      ) predval'
            | MyCFG.Assign (lval,exp)      -> lift (fun ctx -> Spec.assign ctx lval exp  ) predval'
            | MyCFG.SelfLoop               -> lift (fun ctx -> Spec.intrpt ctx           ) predval'
            | MyCFG.Test   (exp,tv)        -> lift (fun ctx -> Spec.branch ctx exp tv    ) predval'
            | MyCFG.Ret    (ret,fundec)    -> lift (fun ctx -> Spec.return ctx ret fundec) predval'
            | MyCFG.Proc   (lval,exp,args) -> proc_call (n,edge,pred) sigma theta lval exp args predval'
            | MyCFG.ASM _                  -> M.warn "ASM statement ignored."; SD.lift predval', [], []
            | MyCFG.Skip                   -> SD.lift predval', [], []
          in
            l, gd @ diff @ !diffs, sp
        with
          | M.StopTheWorld
          | A.Deadcode  -> SD.bot (), [], []
          | M.Bailure s -> M.warn_each s; (predval, [], [])
          | x -> M.warn_urgent "Oh no! Something terrible just happened"; raise x
      in
      let old_loc = !GU.current_loc in
      let _   = GU.current_loc := MyCFG.getLoc pred in
      let ans = eval (pred, es) in 
      let _   = GU.current_loc := old_loc in 
        ans
    in
      (* and we generate a list of rh-sides *)
      List.map edge2rhs edges
            
  (* Pretty printing stuff *)
  module RT = A.ResultType (Spec) (Spec.Dom) (SD)
  module LT = SetDomain.HeadlessSet (RT)  (* Multiple results for each node *)
  module RC = struct let result_name = "Analysis" end
  module Result = A.Result (LT) (RC)
    
  type solver_result = Solver.solution'
  type source_result = Result.t
  
  (** convert result that can be out-put *)
  let solver2source_result (ress: solver_result list) : source_result =
    (* processed result *)
    let res : source_result = Result.create 113 in
    
    (* Adding the state at each system variable to the final result *)
    let add_local_var (n,es) state =
      let loc = MyCFG.getLoc n in
      if loc <> locUnknown then try 
        let (_, fundec) as p = loc, MyCFG.getFun n in
        if Result.mem res p then 
          (* If this source location has been added before, we look it up
           * and add another node to it information to it. *)
          let prev = Result.find res p in
          Result.replace res p (LT.add (SD.unlift es,state,fundec) prev)
        else 
          Result.add res p (LT.singleton (SD.unlift es,state,fundec))
        (* If the function is not defined, and yet has been included to the
         * analysis result, we generate a warning. *)
      with Not_found -> M.warn ("Undefined function has escaped.")
    in
      (* Iterate over all solved equations... *)
      List.iter (fun (sol,_) -> Solver.VMap.iter add_local_var sol) ress;
      res

  let print_globals glob = 
    let out = M.get_out RC.result_name !GU.out in
    let print_one v st =
      ignore (Pretty.fprintf out "%a -> %a\n" Spec.Glob.Var.pretty_trace v Spec.Glob.Val.pretty st)
    in
      Solver.GMap.iter print_one glob

  let applySP (cfg: MyCFG.cfg) (old : Analyses.local_state list list PH.t list) (old_g : Analyses.global_state list PHG.t list) (old_s : (varinfo * int) list SH.t) phase startvars : SD.t SP_SOL.t =
    let r x = MyCFG.FunctionEntry x in
    let e x = MyCFG.Function x in
    let succ x =
      List.map (fun (x,y) -> y) (cfg x) 
    in
    let theta x = failwith "SP: partial invariant not supported." in
    let add_var _ _ = failwith "SP: spawning not supported" in
    let add_diff _ = failwith "SP: partial invariant not supported" in
    let getctx v x = 
      try
        let oldstate = List.concat (List.map (fun m -> match PH.find m v with [] -> raise A.Deadcode | x -> x) old) in
        let oldglob = List.map PHG.find old_g in
        A.set_preglob (A.set_precomp (A.context top_query x theta [] add_var add_diff) oldstate) oldglob  
      with Not_found  -> Messages.warn "Analyzing a program point that was thought to be unreachable.";
                         raise A.Deadcode
    in
    let is_special p x =
      let x' = SD.unlift x in
      match p with
        | MyCFG.Statement {skind = Instr [Call (_,f,_,_)]} ->
            begin try 
              let fs =  
                match Spec.query (getctx p x') (Queries.EvalFunvar f) with
                  | `LvalSet ls -> Queries.LS.fold (fun ((x,_)) xs -> x::xs) ls [] 
                  | _ -> Messages.bailwith ("Is_special: Failed to evaluate function expression "^(sprint 80 (d_exp () f)))
              in
              let _  = List.map Cilfacade.getdec fs in
                LibraryFunctions.use_special (List.hd fs).vname
            with Not_found 
               | Failure "hd" -> true end
        | _ -> (* the most "special" case *) true
    in 
    let special v lv f args st =
      let ctx = getctx v st in
      let fs = 
        match Spec.query ctx (Queries.EvalFunvar f) with
          | `LvalSet ls -> Queries.LS.fold (fun ((x,_)) xs -> x::xs) ls [] 
          | _ -> Messages.bailwith ("Special: Failed to evaluate function expression "^(sprint 80 (d_exp () f)))
      in
      let f = List.hd fs in
      let joiner d1 (d2,_,_) = Spec.Dom.join d1 d2 in 
      List.fold_left joiner (Spec.Dom.bot ()) (Spec.special_fn ctx lv f args)    
    in
    let enter p (x:SD.t) =
      let x' = SD.unlift x in
      match p with
        | MyCFG.Statement {skind = Instr [Call (lv,f,args,_)]} ->
            let ctx = getctx p x' in
            let fs = 
              match Spec.query ctx (Queries.EvalFunvar f) with
                | `LvalSet ls -> Queries.LS.fold (fun ((x,_)) xs -> x::xs) ls [] 
                | _ -> Messages.bailwith ("Enter: Failed to evaluate function expression "^(sprint 80 (d_exp () f)))
            in
            List.concat (List.map (fun f -> List.map (fun (_,y) -> (f, SD.lift y)) (Spec.enter_func ctx lv f args)) fs)
        | _ -> failwith "SP: cannot enter a non-call node."
    in 
    let comb n p x y = 
      let x' = SD.unlift x in
      let y' = SD.unlift y in
      match n with
        | MyCFG.Statement {skind = Instr [Call (lv,f,args,_)]} ->
            SD.lift (Spec.leave_func (getctx n x') lv f p args y')
        | _ -> failwith "SP: cannot enter a non-call node."      
    in
    let get_edge n m = 
      try 
        fst (List.find (fun (_,y) -> Analyses.Var.equal m y)(cfg n))
      with Not_found -> 
        ignore (Pretty.printf "edge from \n%a\nto\n%a\n\n" MyCFG.pretty_node n MyCFG.pretty_node m);
        flush stdout;
        failwith "."
    in
    let f (n,m) v =
      let edge = get_edge n m in
      GU.current_loc := MyCFG.getLoc n ;
      let v' = SD.unlift v in
      let next = 
        match edge with
          | MyCFG.Proc (l,f,args)        -> special n l f args v'
          | MyCFG.Entry func             -> Spec.body   (getctx n v') func      
          | MyCFG.Assign (lval,exp)      -> Spec.assign (getctx n v') lval exp  
          | MyCFG.SelfLoop               -> Spec.intrpt (getctx n v')           
          | MyCFG.Test   (exp,tv)        -> Spec.branch (getctx n v') exp tv    
          | MyCFG.Ret    (ret,fundec)    -> Spec.return (getctx n v') ret fundec
          | MyCFG.ASM _                  -> M.warn "ASM statement ignored."; v'
          | MyCFG.Skip                   -> v'
          (*| _ -> failwith "SP: unsupported edge"*)
      in
      SD.lift next
    in
    let dolift f d b =
      try f () with
        | M.StopTheWorld
        | A.Deadcode  -> d
        | M.Bailure s -> M.warn_each s; b
        | x -> M.warn_urgent "Oh no! Something terrible just happened"; raise x
    in
    let f' x y = dolift (fun () -> f x y) (SD.bot ()) y in
    let enter' x y = dolift (fun () -> enter x y) [] [] in
    let comb' x y z w = dolift (fun () -> comb x y z w) (SD.bot ()) w in
    GU.may_narrow := false ;
    SP.solve r e succ startvars f' enter' comb' is_special

  let sp_to_solver_result (r:SD.t SP_SOL.t) : solver_result =
    let vm = Solver.VMap.create (SP_SOL.length r * 2) (SD.bot ()) in
    let gm = Solver.GMap.create 1 (Spec.Glob.Val.bot ()) in
    let f (x,y) z = Solver.VMap.add vm (x,y) z  in
    SP_SOL.iter f r;
    vm, gm

  (** add extern variables to local state *)
  let do_extern_inits (file : Cil.file) : Spec.Dom.t =
    let module VS = Set.Make (Basetype.Variables) in    
    let add_glob s = function
        GVar (v,_,_) -> VS.add v s
      | _            -> s
    in
    let vars = Cil.foldGlobals file add_glob VS.empty in
    let set_bad v st =
      let theta x = Spec.Glob.Val.bot () in
      let error _ = failwith "Bug: Using enter_func for toplevel functions." in 
      let ctx = A.context top_query st theta [] error error in
      Spec.assign ctx (var v) MyCFG.unknown_exp 
    in
    let add_externs s = function
      | GVarDecl (v,_) when not (VS.mem v vars || isFunctionType v.vtype) -> set_bad v s
      | _ -> s
    in    
    Cil.foldGlobals file add_externs (Spec.startstate ())
  
  (** analyze cil's global-inits function to get a starting state *)
  let do_global_inits (file: Cil.file) : SD.t * Cil.fundec list = 
    let early = !GU.earlyglobs in
    let edges = MyCFG.getGlobalInits file in
    let theta x = Spec.Glob.Val.bot () in
    let funs = ref [] in
    let diffs = ref [] in
    let add_diff g d = diffs := (`G (g,d)) :: !diffs in 
    let transfer_func (st : Spec.Dom.t) (edge, loc) : Spec.Dom.t = 
      let add_var _ _ = raise (Failure "Global initializers should never spawn threads. What is going on?")  in
      try
        if M.tracing then M.trace "con" "Initializer %a\n" d_loc loc;
        GU.current_loc := loc;
        match edge with
          | MyCFG.Entry func        -> Spec.body (A.context top_query st theta [] add_var add_diff) func
          | MyCFG.Assign (lval,exp) -> 
              begin match lval, exp with
                | (Var v,o), (Cil.AddrOf (Cil.Var f,Cil.NoOffset)) 
                  when v.Cil.vstorage <> Static && isFunctionType f.vtype -> 
                  begin try funs := Cilfacade.getdec f :: !funs with Not_found -> () end 
                | _ -> ()
              end;
              Spec.assign (A.context top_query st theta [] add_var add_diff) lval exp
          | _                       -> raise (Failure "This iz impossible!") 
      with Failure x -> M.warn x; st
    in
    let _ = GU.global_initialization := true in
    let _ = GU.earlyglobs := false in
    let with_externs = do_extern_inits file in
    let result : Spec.Dom.t = List.fold_left transfer_func with_externs edges in
    let _ = GU.earlyglobs := early in
    let _ = GU.global_initialization := false in
      SD.lift result, !funs
     
  module S = Set.Make(struct
                        type t = int
                        let compare = compare
                      end)
                      
  (** do the analysis and do the output according to flags*)
  let analyze_phase 
        file cfg phase 
        (old : Analyses.local_state list list PH.t list) 
        (old_g : Analyses.global_state list PHG.t list)
        (old_s : (varinfo * int) list SH.t) 
        (startfuns, exitfuns, otherfuns: A.fundecs) =
	let startstate, more_funs = 
      if !GU.verbose then print_endline "Initializing globals.";
      Stats.time "initializers" do_global_inits file in
    let _ = if M.tracing then M.trace "postinit" "The initial state is: %a\n" SD.pretty startstate else () in
    let otherfuns = 
      if !GU.kernel
      then otherfuns @ more_funs
      else otherfuns in
    let enter_with st fd =
      let args = List.map (fun x -> MyCFG.unknown_exp) fd.sformals in
      let theta x = Spec.Glob.Val.bot () in
      let ignore2 _ _ = () in 
      let error _ = failwith "Bug: Using enter_func for toplevel functions with 'otherstate'." in 
      let ctx = A.context top_query st theta [] ignore2 error in
      let ents = Spec.enter_func ctx None fd.svar args in
        List.map (fun (_,s) -> fd.svar, SD.lift s) ents  
    in
    let _ = try MyCFG.dummy_func.svar.vdecl <- (List.hd otherfuns).svar.vdecl with Failure _ -> () in
    let startvars = 
      if startfuns = [] then
        [[MyCFG.dummy_func.svar, startstate]]
      else 
        List.map (enter_with (SD.unlift startstate)) startfuns in 
    let exitvars = List.map (enter_with (Spec.exitstate ())) exitfuns in
    let othervars = List.map (enter_with (Spec.otherstate ())) otherfuns in
    let startvars = List.concat (startvars @ exitvars @ othervars) in
    let _ = if startvars = [] then failwith "BUG: Empty set of start variables; may happen if enter_func of any analysis returns an empty list." in
    let context_fn = if !GU.full_context then fun x->x else Spec.context_top in
    let startvars' = List.map (fun (n,e) -> MyCFG.Function n, SD.lift (context_fn (SD.unlift e))) startvars in
    let entrystates = List.map2 (fun (_,e) (n,d) -> (MyCFG.FunctionEntry n,e), d) startvars' startvars in
    let procs = 
      let f = function
        | ((MyCFG.FunctionEntry n, e), d) -> (n,e,d)
        | _ -> failwith "SP: entry states strange."
      in
      List.map f 
    in
    let sol,gs = 
      let solve () =
        if !Goblintutil.sharir_pnueli 
        then sp_to_solver_result (applySP cfg old old_g old_s phase (procs entrystates))
        else Solver.solve () (system cfg old old_g old_s phase) startvars' entrystates
      in
      if !GU.verbose then print_endline ("Analyzing phase "^string_of_int phase^"!");
      Stats.time "solver" solve () in
    if !GU.verify then begin
      if !GU.verbose then print_endline "Verifying!";
      Stats.time "verification" (Solver.verify () (system cfg old old_g old_s phase)) (sol,gs)
    end;
    if P.tracking then 
      begin 
        P.track_with_profile () ;
        P.track_call_profile ()
      end ;
    let firstvar = List.hd startvars' in
    let mainfile = match firstvar with (MyCFG.Function fn, _) -> fn.vdecl.file | _ -> "Impossible!" in
    if !GU.print_uncalled then
      begin
        let out = M.get_out "uncalled" stdout in
        let f =
          let insrt k _ s = match k with
            | (MyCFG.Function fn,_) -> S.add fn.vid s
            | _ -> s
          in
          (* set of ids of called functions *)
          let calledFuns = Solver.VMap.fold insrt sol S.empty in
          function
            | GFun (fn, loc) when loc.file = mainfile && not (S.mem fn.svar.vid calledFuns) ->
                begin
                  let msg = "Function \"" ^ fn.svar.vname ^ "\" will never be called in phase "^string_of_int phase^"." in
                  ignore (Pretty.fprintf out "%s (%a)\n" msg Basetype.ProgLines.pretty loc)
                end
            | _ -> ()
        in
          List.iter f file.globals;
      end;
    let main_sol = Solver.VMap.find sol firstvar in
    (* check for dead code at the last state: *)
    (if !GU.debug && SD.equal main_sol (SD.bot ()) then
      Printf.printf "NB! Execution does not reach the end of Main.\n");
    (sol,gs)
  
  (* multi staged analyses will throw away contexts using this function *)
  let join_contexts (r,_:solver_result) : Analyses.local_state list list PH.t =
    let hm = PH.create (Solver.VMap.length r) in
    let f (k,_) v =
      let old = try PH.find hm k with Not_found -> SD.bot () in
      PH.replace hm k (SD.join v old)
    in
    Solver.VMap.iter f  r;
    let ha = PH.create (PH.length hm) in
    let f k v = PH.add ha k (try ToStd.translate (SD.unlift v) with Analyses.Deadcode -> [])
    in
    PH.iter f hm;
    ha
  
  (* type conversions for globals *)
  let conserve_globs (_,g:solver_result) : Analyses.global_state list PHG.t =	
	let res = PHG.create 5 in
	let f v x = PHG.add res v (ToStdG.translate x) in
	Solver.GMap.iter f g;
	res
  
  let analyze (file: Cil.file) (fds: A.fundecs) = 
    (*ignore (Printf.printf "Effective conf:%s" (Json.jsonString (Json.Object !GU.conf)));*)
    (* number of phases *)
    let phs = List.length !(Json.array !(Json.field !GU.conf "analyses")) in
    (* get the control flow graph *)
    let cfg = 
      if !GU.verbose then print_endline "Generating constraints."; 
      MyCFG.getCFG file (not !Goblintutil.sharir_pnueli) 
    in
    let oldsol = ref [] in (* list of solutions from previous phases *)
    let precmp = ref [] in (* same as oldsol but without contexts  *)
    let oldgsol = ref [] in (* old globals *)
    let oldspawns = SH.create 23 in 
    (* loop over phases *)
    for ph = 0 to phs -1 do
      GU.phase := ph;
      Spec.init ();
      let sv = (analyze_phase file cfg ph !precmp !oldgsol oldspawns fds) in
      oldsol := sv :: !oldsol;
      oldgsol := conserve_globs sv :: !oldgsol;
      (if ph != phs then precmp := join_contexts sv :: !precmp);
      Spec.finalize ()
    done;
    (* output the result if needed *)
    Result.output (solver2source_result !oldsol);
    if !GU.dump_global_inv then 
      List.iter (fun (_,gs) -> print_globals gs) !oldsol

end
