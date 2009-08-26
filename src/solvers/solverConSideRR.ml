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

open Messages
open Progress
open Pretty

module GU = Goblintutil

module Make 
  (Var: Analyses.VarType)  
  (VDom: Lattice.S) 
  (G: Global.S) = 
struct
  module Glob = G.Var
  module GDom = G.Val

  module SolverTypes = Solver.Types (Var) (VDom) (G)
  include SolverTypes

  module VarSet = Set.Make(Var)
  
  let solve (system: system) (initialvars: variable list): solution' =
(*     let recal = VMap.create 113 true in *)
    let sigma = VMap.create 113 (VDom.bot ()) in
    let theta = GMap.create 113 (GDom.bot ()) in
    let vInfl = VMap.create 113 ([]: constrain list) in
    let gInfl = GMap.create 113 ([]: constrain list) in
    let todo  = VMap.create 113 ([]: rhs list) in
    let next_wls = ref VarSet.empty in
    let worklist = ref initialvars in
    let globals_changed = ref true in

    let rec constrainOneVar (x: variable) =
      let rhsides = 
(*        if VMap.mem recal x then
          let temp = VMap.find todo x in 
          VMap.remove todo x; temp
        else begin
          if not (VMap.mem sigma x) then
            VMap.add sigma x (VDom.bot ()); 
          VMap.add recal x false;
          system x
        end*)
        if VMap.mem sigma x then
          let temp = VMap.find todo x in VMap.remove todo x; temp
        else begin
          VMap.add sigma x (VDom.bot ());  
          system x
        end
      in 
      
      let addOneRHS local_state (f: rhs) =
        let (nls,ngd,tc) = f (vEval (x,f), gEval (x,f)) in
        let doOneGlobalDelta (g, gstate) = 
          if not ( GDom.leq gstate (GDom.bot ()) ) then
            let oldgstate = GMap.find theta g in
            let compgs = GDom.join oldgstate gstate in
              if not (GDom.leq compgs oldgstate) then begin
                let add_to_next (x,f) = next_wls := VarSet.add x !next_wls in
                List.iter add_to_next (GMap.find gInfl g);
                GMap.remove gInfl g;
                globals_changed := true;  
                GMap.replace theta g compgs
              end
        in
          List.iter doOneGlobalDelta ngd;
          if !GU.eclipse then show_add_work_buf (List.length tc);
          List.iter constrainOneVar tc;
          VDom.join local_state nls
      in

      if rhsides = [] then ()
      else begin
        let old_state   = VMap.find sigma x in
        let local_state = List.fold_left addOneRHS old_state rhsides in
          if tracing then tracei "sol" (dprintf "Entered %a.\n" Var.pretty_trace x);
          if tracing then trace "sol" (dprintf "Current state:\n    %a\n" VDom.pretty old_state );
          if not (VDom.leq local_state old_state) then begin
            if tracing then traceu "sol" (dprintf "Set state to:\n    %a\n" VDom.pretty local_state );
            VMap.replace sigma x local_state;
            let influenced_vars = ref [] in
            let collectInfluence (y,f) = 
              VMap.replace todo y (f :: VMap.find todo y);
              influenced_vars := y :: !influenced_vars
            in
              List.iter collectInfluence (VMap.find vInfl x);
              VMap.remove vInfl x;
              if !GU.eclipse then show_add_work_buf (List.length !influenced_vars);
              List.iter constrainOneVar !influenced_vars;
          end else 
            if tracing then traceu "sol" (dprintf "State didn't change!\n")
      end;
      if !GU.eclipse then show_worked_buf 1
          

    and vEval (c: constrain) var =
      if !GU.eclipse then show_add_work_buf 1;
      constrainOneVar var;
      VMap.replace vInfl var (c :: VMap.find vInfl var);
      VMap.find sigma var 
    
    and gEval (c: constrain) glob = 
      GMap.replace gInfl glob (c :: GMap.find gInfl glob);
      GMap.find theta glob 
    in
    
    if !GU.eclipse then show_subtask "Constant Propagation" 0;  
    while !globals_changed do
      globals_changed := false;
(*       VMap.clear recal; *)
      
      if !GU.eclipse then show_add_work_buf (List.length !worklist);
      List.iter constrainOneVar !worklist;
        
      let add_to_work v =
        worklist := v :: !worklist;
        VMap.replace todo v (system v)
      in
      VarSet.iter add_to_work !next_wls;
      next_wls := VarSet.empty;
    done ;
      (sigma, theta)
end 
