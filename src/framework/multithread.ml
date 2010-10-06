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
module Forward (Spec : Analyses.Spec) : Analyses.S = 
struct
  (** Augment domain to lift dead code *)
  module SD  = A.Dom (Spec.Dom)
  (** Solver variables use global part from [Spec.Dep] *)
  module Var = A.VarF (SD)

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
  
  let system (cfg: MyCFG.cfg) (n,es: Var.t) : (Solver.var_assign * Solver.glob_assign -> Solver.var_domain * Solver.diff * Solver.calls) list = 
    if M.tracing then M.trace "con" (dprintf "%a\n" Var.pretty_trace (n,es));
    
    let lift_st x forks: Solver.var_domain * Solver.diff * Solver.variable list =
      let diff = List.map (fun x -> `G x) (Spec.get_diff x) in
      let rx = Spec.reset_diff x in
        (SD.lift rx, diff, forks)
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
    let proc_call sigma (theta:Solver.glob_assign) lval exp args st : Solver.var_domain * Solver.diff * Solver.variable list =
      let funs  = Spec.eval_funvar (A.context top_query st theta []) exp in
      let dress (f,es)  = (MyCFG.Function f, SD.lift es) in
      let start_vals : Solver.diff ref = ref [] in
      let add_function st' f : Spec.Dom.t =
        let add_one_call x =
          let ctx_st = Spec.context_top x in
          start_vals := (`L ((MyCFG.FunctionEntry f, SD.lift ctx_st), SD.lift x)) :: !start_vals ;
          sigma (dress (f, ctx_st)) 
        in
        let has_dec = try ignore (Cilfacade.getdec f); true with Not_found -> false in        
        if has_dec && not (LibraryFunctions.use_special f.vname) then
          let work = Spec.enter_func (A.context top_query st theta []) lval f args in
          let leave st1 st2 = Spec.leave_func (A.context top_query st1 theta []) lval exp f args st2 in
          let general_results = List.map (fun (y,x) -> y, SD.unlift (add_one_call x)) work in
          let joined_result   = List.fold_left (fun st (fst,tst) -> Spec.Dom.join st (leave fst tst)) (Spec.Dom.bot ()) general_results in
          if P.tracking then P.track_call f (SD.hash st) ;
          Spec.Dom.join st' joined_result        
        else
          let joiner d1 (d2,_,_) = Spec.Dom.join d1 d2 in 
          List.fold_left joiner (Spec.Dom.bot ()) (Spec.special_fn (A.context top_query st theta []) lval f args) 
      in
      let f xs x =
        let forks = Spec.fork (A.context top_query st theta []) lval x args in
        let add_fork_enter (x,es) = 
          start_vals := (`L ((MyCFG.FunctionEntry x, SD.lift (Spec.context_top es)), SD.lift es)) :: !start_vals 
        in
        List.iter add_fork_enter forks;
        List.map (fun (f,es) -> (MyCFG.Function f, SD.lift (Spec.context_top es))) forks @ xs
      in
      try 
        let crap  = List.fold_left add_function (Spec.Dom.bot ()) funs in      
        let forks = List.fold_left f [] funs in
        let (d, diff, forks) = lift_st crap forks in
        (d,diff @ !start_vals,forks)
      with Analyses.Deadcode -> (SD.bot (), !start_vals, [])
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
      let predvar = (pred, es) in
      if P.tracking then P.track_with (fun n -> M.warn_all (sprint ~width:80 (dprintf "Line visited more than %d times. State:\n%a\n" n SD.pretty (sigma predvar))));
      (* This is the key computation, only we need to set and reset current_loc,
       * see below. We call a function to avoid ;-confusion *)
      let eval () : Solver.var_domain * Solver.diff * Solver.variable list = 
        try  
          (* Generating the constraints is quite straightforward, except maybe
           * the call case. There is an ALMOST constant lifting and unlifting to
           * handle the dead code -- maybe it could be avoided  *)
          match edge with
            | MyCFG.Entry func             -> lift_st (Spec.body   (A.context top_query (SD.unlift (sigma (MyCFG.FunctionEntry func.svar, es))) theta []) func ) []
            | MyCFG.Assign (lval,exp)      -> lift_st (Spec.assign (A.context top_query (SD.unlift (sigma predvar)) theta []) lval exp) []
            | MyCFG.SelfLoop               -> lift_st (Spec.intrpt (A.context top_query (SD.unlift (sigma predvar)) theta [])) []
            | MyCFG.Test   (exp,tv)        -> lift_st (Spec.branch (A.context top_query (SD.unlift (sigma predvar)) theta []) exp tv) []
            | MyCFG.Ret    (ret,fundec)    -> lift_st (Spec.return (A.context top_query (SD.unlift (sigma predvar)) theta []) ret fundec) []
            | MyCFG.Proc   (lval,exp,args) -> proc_call sigma theta lval exp args (SD.unlift (sigma predvar)) 
            | MyCFG.ASM _                  -> M.warn "ASM statement ignored."; sigma predvar, [], []
            | MyCFG.Skip                   -> sigma predvar, [], []
        with
          | A.Deadcode  -> SD.bot (), [], []
          | M.Bailure s -> M.warn_each s; (sigma predvar, [], [])
          | x -> M.warn_urgent "Oh no! Something terrible just happened"; raise x
      in
      let old_loc = !GU.current_loc in
      let _   = GU.current_loc := MyCFG.getLoc pred in
      let ans = eval () in 
      let _   = GU.current_loc := old_loc in 
        ans
    in
      (* and we generate a list of rh-sides *)
      List.map edge2rhs edges
      

      
  (* Pretty printing stuff *)
  module RT = A.ResultType (Spec) (Spec.Dom) (SD)
  module LT = SetDomain.HeadlessSet (RT)  (* Multiple results for each node *)
  module Result = A.Result (LT) (struct let result_name = "Analysis" end)
    
  type solver_result = Solver.solution'
  type source_result = Result.t
  
  (** convert result that can be out-put *)
  let solver2source_result (sol,_: solver_result) : source_result =
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
      Solver.VMap.iter add_local_var sol;
      res

  (** analyze cil's global-inits function to get a starting state *)
  let do_global_inits (file: Cil.file) : SD.t * Cil.fundec list = 
    let early = !GU.earlyglobs in
    let edges = MyCFG.getGlobalInits file in
    let theta x = Spec.Glob.Val.bot () in
    let funs = ref [] in
    let transfer_func (st : Spec.Dom.t) (edge, loc) : Spec.Dom.t = 
      try
        if M.tracing then M.trace "con" (dprintf "Initializer %a\n" d_loc loc);
        GU.current_loc := loc;
        match edge with
          | MyCFG.Entry func        -> Spec.body (A.context top_query st theta []) func
          | MyCFG.Assign (lval,exp) -> 
              begin match lval, exp with
                | (Var v,o), (Cil.AddrOf (Cil.Var f,Cil.NoOffset)) 
                  when v.Cil.vstorage <> Static && isFunctionType f.vtype -> 
                  begin try funs := Cilfacade.getdec f :: !funs with Not_found -> () end 
                | _ -> ()
              end;
              Spec.assign (A.context top_query st theta []) lval exp
          | _                       -> raise (Failure "This iz impossible!") 
      with Failure x -> M.warn x; st
    in
    let _ = GU.earlyglobs := false in
    let result : Spec.Dom.t = List.fold_left transfer_func (Spec.startstate ()) edges in
    let _ = GU.earlyglobs := early in
      SD.lift result, !funs
     
  module S = Set.Make(struct
                        type t = int
                        let compare = compare
                      end)

  (** do the analysis and do the output according to flags*)
  let analyze (file: Cil.file) (funs: Cil.fundec list) =
    let constraints = 
      if !GU.verbose then print_endline "Generating constraints."; 
      system (MyCFG.getCFG file) in
    Spec.init ();
    let startstate, more_funs = 
      if !GU.verbose then print_endline "Initializing globals.";
      Stats.time "initializers" do_global_inits file in
    let funs = 
      if !GU.kernel && !GU.nonstatic 
      then funs@more_funs
      else funs in
    let with_ostartstate x = x.svar, SD.lift (Spec.otherstate ()) in

    let startvars = match !GU.has_main, funs with 
      | true, f :: fs -> 
          let nonf_fs = List.filter (fun x -> x.svar.vid <> f.svar.vid) fs in
          (f.svar, startstate) :: List.map with_ostartstate nonf_fs
      | _ ->
          try 
            MyCFG.dummy_func.svar.vdecl <- (List.hd funs).svar.vdecl;
            (MyCFG.dummy_func.svar, startstate) :: List.map with_ostartstate funs
          with Failure _ -> []
    in
    let startvars' = List.map (fun (n,e) -> MyCFG.Function n, SD.lift (Spec.context_top (SD.unlift e))) startvars in
    let entrystates = List.map2 (fun (_,e) (n,d) -> (MyCFG.FunctionEntry n,e), d) startvars' startvars in
    
    let sol,gs = 
      if !GU.verbose then print_endline "Analyzing!";
      Stats.time "solver" (Solver.solve () constraints startvars') entrystates in
    if !GU.verify then begin
      if !GU.verbose then print_endline "Verifying!";
      Stats.time "verification" (Solver.verify () constraints) (sol,gs)
    end;
    if P.tracking then 
      begin 
        P.track_with_profile () ;
        P.track_call_profile ()
      end ;
    Spec.finalize ();
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
                  let msg = "Function \"" ^ fn.svar.vname ^ "\" will never be called." in
                  ignore (Pretty.fprintf out "%s (%a)\n" msg Basetype.ProgLines.pretty loc)
                end
            | _ -> ()
        in
          List.iter f file.globals;
      end;
    let main_sol = Solver.VMap.find sol firstvar in
    (* check for dead code at the last state: *)
    if !GU.debug && SD.equal main_sol (SD.bot ()) then
      Printf.printf "NB! Execution does not reach the end of Main.\n";
    Result.output (solver2source_result (sol,gs))
    
end
