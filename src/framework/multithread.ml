(* 
 * Copyright (c) 2005-2007,
 *     * University of Tartu
 *     * Vesal Vojdani <vesal.vojdani@gmail.com>
 *     * Kalmer Apinis <kalmera@ut.ee>
 *     * Jaak Randmets <jaak.ra@gmail.com>
 *     * Toomas Römer <toomasr@gmail.com>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 
 *     * Redistributions of source code must retain the above copyright notice,
 *       this list of conditions and the following disclaimer.
 * 
 *     * Redistributions in binary form must reproduce the above copyright notice,
 *       this list of conditions and the following disclaimer in the documentation
 *       and/or other materials provided with the distribution.
 * 
 *     * Neither the name of the University of Tartu nor the names of its
 *       contributors may be used to endorse or promote products derived from
 *       this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

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
  module Var = A.VarF (SD) (Spec.Dep)
 
  (** Deps module for WorklistCon solver *)
  module Deps =
  struct
    module Dom = SD 
    module Var = Var
    
    (** Lift globals & global dep. functions from [Spec] to use domain [SD] and variables [Var] *)
    
    let get_changed_globals x y = 
      try  List.map (fun x -> Var.Global x) (Spec.get_changed_globals (SD.unlift x) (SD.unlift y))
      with Analyses.Deadcode -> []
    let get_global_dep d = 
      try  List.map (fun x -> Var.Global x) (Spec.get_global_dep (SD.unlift d))
      with Analyses.Deadcode -> []
    let filter_globals x = 
      try SD.lifted Spec.filter_globals x
      with _ -> SD.bot ()
    let reset_global_dep y = 
      try SD.lifted Spec.reset_global_dep y
      with _ -> SD.bot ()
    let insert_globals x y = 
      try SD.lifted (Spec.insert_globals (SD.unlift x)) y
      with _ -> SD.bot ()
    end

  (** Modified WorklistCon solver *)
  module Solver = WorklistCon.Make (SD) (Deps)
  
  (** name the analyzer *)
  let name = "analyzer"
  
  let system (cfg: MyCFG.cfg) (var: Var.t) : Solver.rhs list = 
    if M.tracing then M.trace "con" (dprintf "%a\n" Var.pretty_trace var);
    
    (* We can only solve using normal variables *)
    let (n,es) =      
      match var with 
        | Var.Local (n,es) -> (n,es)
        | Var.Global _     -> failwith "cannot solve a global by itself"
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
    let proc_call sigma lval exp args st : Solver.domain * Solver.forks =
      let funs  = Spec.eval_funvar exp st in
      let dress (f,es)  = Var.Local (MyCFG.Function f, SD.lift es) in
      let add_function st' f : Spec.Dom.t =
        let has_dec = try ignore (Cilfacade.getdec f); true with Not_found -> false in        
        if (has_dec)  then
          let work    = Spec.enter_func lval f args st in
          let leave   = Spec.leave_func lval f args st in
          let general_results = List.map (fun x -> SD.unlift (sigma (dress (f,x)))) work in
          let joined_result   = List.fold_left (fun st fst -> Spec.Dom.join st (leave fst)) (Spec.Dom.bot ()) general_results in
          Spec.Dom.join st' joined_result        
        else
          Spec.special_fn lval f args st
      in
      let crap  = List.fold_left add_function (Spec.Dom.bot ()) funs in      
      let forks = List.fold_left (fun xs x -> List.map dress (Spec.fork lval x args st) @ xs) [] funs in
      SD.lift crap, forks
    in
      
    (* Find the edges entering this edge *)
    let edges : (MyCFG.edge * MyCFG.node) list = cfg n in
      
    (* For each edge we generate a rhs: a function that takes current state
     * sigma and the global state theta; it outputs the new state, delta, and
     * spawned calls. *)      
    let edge2rhs (edge, pred : MyCFG.edge * MyCFG.node) (sigma:Solver.assignment) : Solver.domain * Solver.forks = 
      let predvar = Var.Local (pred, es) in
      (*if P.tracking then P.track_with (fun n -> M.warn_all (sprint ~width:80 (dprintf "Line visited more than %d times. State:\n%a\n" n SD.pretty (sigma predvar))));*)
      
      (* This is the key computation, only we need to set and reset current_loc,
       * see below. We call a function to avoid ;-confusion *)
      let eval () : Solver.domain * Solver.forks = 
        try  
          (* Generating the constraints is quite straightforward, except maybe
           * the call case. There is an ALMOST constant lifting and unlifting to
           * handle the dead code -- maybe it could be avoided  *)
          match edge with
            | MyCFG.Entry func             -> SD.lift (Spec.body func (SD.unlift es)), []
            | MyCFG.Assign (lval,exp)      -> SD.lift (Spec.assign lval exp (SD.unlift (sigma predvar))), []
            | MyCFG.Test   (exp,tv)        -> SD.lift (Spec.branch exp tv (SD.unlift (sigma predvar))), []
            | MyCFG.Ret    (ret,fundec)    -> SD.lift (Spec.return ret fundec (SD.unlift (sigma predvar))), []
            | MyCFG.Proc   (lval,exp,args) -> proc_call sigma lval exp args (SD.unlift (sigma predvar)) 
            | MyCFG.ASM _                  -> M.warn "ASM statement ignored."; sigma predvar, []
            | MyCFG.Skip                   -> sigma predvar, []
        with
          | A.Deadcode  -> SD.bot (), []
          | M.Bailure s -> M.warn_each s; (sigma predvar, [])
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
    
  type solver_result = Solver.solution
  type source_result = Result.t
  
  (** convert result that can be out-put *)
  let solver2source_result (sol: solver_result) : source_result =
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
    
    let add_var (v: Var.t) =
      match v with
        | Var.Local (n,es)  -> add_local_var (n,es)
        | Var.Global g      -> failwith "lol wut?" 
    in
      (* Iterate over all solved equations... *)
      Solver.HT.iter add_var sol;
      res

  (** analyze cil's global-inits function to get a starting state *)
  let do_global_inits (file: Cil.file) : SD.t = 
    let early = !GU.earlyglobs in
    let edges = MyCFG.getGlobalInits file in
    let transfer_func (st : Spec.Dom.t) (edge, loc) : Spec.Dom.t = 
      try
        if M.tracing then M.trace "con" (dprintf "Initializer %a\n" d_loc loc);
        GU.current_loc := loc;
        match edge with
          | MyCFG.Entry func        -> Spec.body func st
          | MyCFG.Assign (lval,exp) -> Spec.assign lval exp st
          | _                       -> raise (Failure "This iz impossible!") 
      with Failure x -> M.warn x; st
    in
    let _ = GU.earlyglobs := false in
    let result : Spec.Dom.t = List.fold_left transfer_func Spec.startstate edges in
    let _ = GU.earlyglobs := early in
      SD.lift result
     
  (** do the analysis and do the output according to flags*)
  let analyze (file: Cil.file) (funs: Cil.fundec list) =
    let constraints = 
      if !GU.verbose then print_endline "Generating constraints."; 
      system (MyCFG.getCFG file) in
    let startstate = 
      if !GU.verbose then print_endline "Initializing globals.";
      Stats.time "initializers" do_global_inits file in
    let startvars = match funs with 
      | (f::fs) -> (MyCFG.Function f.svar, startstate) ::
                   List.map (fun x -> (MyCFG.Function x.svar, SD.lift Spec.otherstate)) fs 
      | [] -> [] in
    Spec.init ();
    let lifted_startvars = List.map (fun (x,y) -> Var.Local (x, y)) startvars in
    let sol = 
      if !GU.verbose then print_endline "Analyzing!";
      Stats.time "solver" (Solver.solve constraints) lifted_startvars in
    Spec.finalize ();
    let main_sol = Solver.HT.find sol (List.hd lifted_startvars) in
    (* check for dead code at the last state: *)
    if !GU.debug && SD.equal main_sol (SD.bot ()) then
      Printf.printf "NB! Execution does not reach the end of Main.\n";
    Result.output (solver2source_result sol)
    
end
