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

module type Spec = 
sig
  module LD: Lattice.S
  module GD: Global.S
  type glob_fun = GD.Var.t -> GD.Val.t
  type glob_diff = (GD.Var.t * GD.Val.t) list
  val postprocess_glob: GD.Var.t -> GD.Val.t -> unit
  include Analyses.Spec 
    with type domain = LD.t
     and type transfer = LD.t * glob_fun -> LD.t * glob_diff
     and type trans_in = LD.t * glob_fun
  val spawn: varinfo -> exp list -> trans_in -> (varinfo * domain) list
end

module Forward (Spec: Spec) =
struct
  include Spec (* also makes accessible it's modules LD, GD *)

  module Var = A.VarF (LD)
  module SD = A.Dom (LD)
  
  (* Pretty printing stuff *)
  module RT = A.ResultType (Spec) (LD) (SD)
  module LT = SetDomain.HeadlessSet (RT)  (* Multiple results for each node *)
  module ResultLocal = A.Result (LT) (struct let result_name = "Local" end)
  module ResultGlob = A.Result (GD.Val) (struct let result_name = "Global" end)
  module Result = A.ComposeResults (LT) (GD.Val) (struct let result_name = "Analysis" end)

  module Solver = EffectWCon.Make (Var) (SD) (GD)
  type solver_result = Solver.solution'
  type source_result = ResultLocal.t * ResultGlob.t

  let system (cfg: MyCFG.cfg) ((n,es): Solver.variable) : Solver.rhs list = 
    if M.tracing then M.trace "con" (dprintf "%a\n" Var.pretty_trace (n,es));
    (* Find the edges entering this edge *)
    let edges = cfg n in
    (* For each edge we generate a rhs: a function that takes current state
     * sigma and the global state theta; it outputs the new state, delta, and
     * spawned calls. *)
    let edge2rhs (edge, pred) (sigma, theta) = 
      let predvar = (pred, es) in
      (* This is the key computation, only we need to set and reset current_loc,
       * see below. We call a function to avoid ;-confusion *)
      if P.tracking then P.track_with (fun n -> M.warn_all (sprint ~width:80 (dprintf "Line visited more than %d times. State:\n%a\n" n SD.pretty (sigma predvar))));
      let eval () = 
        try  
          (* Generating the constraints is quite straightforward, except maybe
           * the call case. There is an ALMOST constant lifting and unlifting to
           * handle the dead code -- maybe it could be avoided  *)
          match edge with
            | MyCFG.Entry func ->
                let (b,n) = body func (es, theta) in 
                  (SD.lift b,n,[])
            | MyCFG.Assign (lval,exp) -> 
                let (b,n) = assign lval exp (SD.unlift (sigma predvar), theta) in 
                  (SD.lift b,n,[])
            | MyCFG.Test (exp,tv) -> 
                let (b,n) = branch exp tv (SD.unlift (sigma predvar), theta) in 
                  (SD.lift b,n,[])
            (* The procedure call is a bit unorthodox, we create a callback, so
             * the user can "call" functions on demand, when defining the
             * transfer function for calls: *)
            | MyCFG.Proc (lval,exp,args) ->
                let sp = ref [] in
                (* Helper function to be lifted so we can ignore some dead-code
                 * elimination issues, to avoid confusing it with functions that
                 * return dead code since they don't terminate. *)
                let helper (st,gl as stg) =
                  let (norms,specs) = entry exp args stg in
                  let dress (f,es) = (MyCFG.Function f, es) in
                  let baseval = SD.bot () in
                  let effects = ref [] in
                  let fun_res fe = sigma (dress fe) in
                  let spec_res f = 
                    let (b,n) = special f args stg in
                      effects := n @ !effects;
                      sp := List.map dress (spawn f args stg) @ !sp;
                      SD.lift b
                  in
                  let call_results = List.map fun_res norms in
                  let spec_results = List.map spec_res specs in
                  let res = List.fold_left SD.join baseval (call_results @ spec_results) in
                  let (l,g) = combine lval exp args (SD.unlift res) stg in
                    (l, !effects @ g)
                in
                let (b,n) = helper (SD.unlift (sigma predvar), theta) in
                  (SD.lift b, n, !sp)
            | MyCFG.ASM _ -> M.warn "ASM statement ignored."; (sigma predvar, [], [])
            (* The return edge to function type nodes: *)
            | MyCFG.Ret (ret,fundec) ->
                let (b,n) = return ret fundec (SD.unlift (sigma predvar), theta) in 
                  (SD.lift b,n,[])
            | _ -> (sigma predvar, [], [])
        with
          | A.Deadcode -> (SD.bot (), [], [])
          | M.Bailure s -> M.warn_each s; (sigma predvar, [], [])
          | x -> M.warn_urgent "Oh no! Something terrible just happened"; raise x
      in
      let old_loc = !GU.current_loc in
      let _ = GU.current_loc := MyCFG.getLoc pred in
      let ans = eval () in 
      let _ = GU.current_loc := old_loc in 
        ans

    in
      (* and we generate a list of rh-sides *)
      List.map edge2rhs edges


  let solver2source_result ((sol,globs): solver_result) : source_result =
    let localhash = ResultLocal.create 113 in
    let globhash = ResultGlob.create 113 in
    (* Adding the state at each system variable to the final result *)
    let add_var (n,es) state =
      let loc = MyCFG.getLoc n in
        if loc <> locUnknown then
	try
	  let fundec = MyCFG.getFun n in
          let p = (loc, fundec) in
	    if (ResultLocal.mem localhash p) then begin
              (* If this source location has been added before, we look it up
               * and add another node to it information to it. *)
              let prev = ResultLocal.find localhash p in
                    ResultLocal.replace localhash p (LT.add (es,state,fundec) prev)
            end else 
              ResultLocal.add localhash p (LT.singleton (es,state,fundec))
        (* If the function is not defined, and yet has been included to the
         * analysis result, we generate a warning. *)
	with Not_found -> 
          M.warn ("Undefined function has escaped.")
    in
    (* Globals are associated with the declaration location, this is queried
     * from CIL. *)
    let add_glob glob state = 
      ResultGlob.add globhash (Glob.get_location glob, MyCFG.initfun) (state)
    in
      (* Iterate over all solved equations... *)
      Solver.VMap.iter add_var sol;
      (* And all the computed globals... *)
      Solver.GMap.iter add_glob globs;
      (localhash,globhash)

  let doGlobalInits file = 
    let early = !GU.earlyglobs in
    let edges = MyCFG.getGlobalInits file in
    let theta x = GD.Val.top () in
    let f st (edge, loc) = try
      if M.tracing then M.trace "con" (dprintf "Initializer %a\n" d_loc loc);
      GU.current_loc := loc;
      match edge with
        | MyCFG.Entry func ->
            fst (body func (st, theta))
        | MyCFG.Assign (lval,exp) -> 
            fst (assign lval exp (st, theta))
        | _ -> raise (Failure "This iz impossible!") 
    with
      | Failure x -> M.warn x; st
    in
    let _ = GU.earlyglobs := false in
    let result = List.fold_left f Spec.startstate edges in
    let _ = GU.earlyglobs := early in
      result
      
  let postprocess theta = 
    if !GU.eclipse then begin
	P.show_subtask "Postprocessing" 2;
	P.show_worked 1
      end;
    Solver.GMap.iter postprocess_glob theta;
    if !GU.eclipse then begin
	P.show_subtask "Finishing ... " 1;
	P.show_worked 1
      end

  module S = Set.Make(struct
                        type t = int
                        let compare = compare
                      end)

  let analyze (file: Cil.file) (funs: Cil.fundec list) =
    let constraints = 
      if !GU.verbose then print_endline "Generating constraints."; 
      system (MyCFG.getCFG file) in
    let startstate = 
      if !GU.verbose then print_endline "Initializing globals.";
      Stats.time "initializers" doGlobalInits file in
    let startvars = match funs with 
      | (f::fs) -> (MyCFG.Function f.svar, startstate) ::
                   List.map (fun x -> (MyCFG.Function x.svar, otherstate)) fs 
      | [] -> [] in
    Spec.init ();
    let (sigma,theta) as sol = 
      if !GU.verbose then print_endline "Analyzing!";
      Stats.time "solver" (Solver.solve constraints) startvars in
    if !GU.print_uncalled then
      begin
        let out = M.get_out "uncalled" stdout in
        let f =
          let insrt k _ s = match k with
            | (MyCFG.Function fn, _) -> S.add fn.vid s
            | _ -> s
          in
          (* set of ids of called functions *)
          let calledFuns = Solver.VMap.fold insrt sigma S.empty in
          function
            | GFun (fn, loc) -> if not (S.mem fn.svar.vid calledFuns) then
                begin
                  let msg = "Function \"" ^ fn.svar.vname ^ "\" will never be called." in
                  ignore (Pretty.fprintf out "%s (%a)\n" msg Basetype.ProgLines.pretty loc)
                end
            | _ -> ()
        in
          List.iter f file.globals;
      end;
    let (locals, globs) = solver2source_result sol in
    let _ = match !GU.dump_path with
      | Some _ -> begin
          ResultLocal.output locals;
          ResultGlob.output globs;
        end
      | _ -> Result.output (Result.merge locals globs);
    in
    let last_st = 
      if !GU.allfuns then None
      else begin
        let x = Solver.VMap.find sigma (List.hd startvars) in
          (* check for dead code at the last state: *)
          if !GU.debug && SD.equal x (SD.bot ()) then
            Printf.printf "NB! Execution does not reach the end of Main.\n";
          Some x
      end
    in
      postprocess theta;
      finalize ();
      match !GU.result_style with
        (* when debuggin we output the last state and the globals! *)
        | GU.State -> begin
            match last_st with
              | None -> ()
              | Some st -> 
                  let out = M.get_out "state" !GU.out in
                  ignore (Pretty.fprintf out "%a\n" SD.pretty st);
                  let f key valu = 
                    ignore (Pretty.fprintf out "%a -> %a\n" 
                              Glob.pretty key
                              GD.Val.pretty valu)
                  in
                    Solver.GMap.iter f theta;
                    flush out
          end
        | _ -> ()
end
