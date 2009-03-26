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

open Pretty (* All printf style functions *)

module Make (Var: Analyses.VarType) (Dom: Lattice.S) = 
struct
  module HT = Hash.Make(Var)
  type 'a table    = 'a HT.t
  type variable    = Var.t
  type domain      = Dom.t

  type assignment  = variable -> domain
  type assignment' = domain table (* data structure representation of an assignment *)
  type rhs         = assignment -> domain (* rhs of the constraint in functional form *)
  type lhs         = variable (* system variable *)
  type constrain   = lhs * rhs  (* a single constraint: lhs \sqsupseteq rhs *)
  type system      = lhs -> rhs list (* maps variables to it's set of constraints *)

  type solution    = assignment'

  (** solves a constraint system in a demand-driven fashion starting from the
    * initialvars *)
  let solve (system: system) (initialvars: variable list): solution =
    (* sigma will be the solution, it is initially empty; new variables are 
     * initialized to bottom as they occur. *)
    let sigma: assignment' = HT.create 113 (Dom.bot ()) in
    (* infl captures the dynamic dependencies: infl x is the set of constaints
     * that should be recomputed whenever x changes *)
    let infl: constrain list table = HT.create 113 [] in
    (* the worklist of rh-sides that should be considered for each variable! *)
    let todo: rhs list table = HT.create 113 [] in

    let rec constrain_one_var (x: variable) = 
      (* here we find the set of constraints that should be considered *)
      let rhsides = 
        if HT.mem sigma x then
          (* If we have seen this variable before, return the constraints that
           * still need processing and remove them from the todo-list  *)
          let temp = HT.find todo x in HT.remove todo x; temp
          else begin
            (* initialize the new variable to bottom (basically we mark the
             * variable as visited!) *)
            HT.add sigma x (Dom.bot ());
            (* return all the constraints for that variable *)
            system x
          end
      in
      if rhsides = [] then () 
      else begin
        let old_state = HT.find sigma x in
        let local_state = ref old_state in 
        (* we apply a constraint by first evaluating the rhs and then joining the
         * result with the present value *)
        let apply_one_constraint (f: rhs) =
          (* first we evaluate the rhs, but instead of just giving it sigma, we
           * wrap it with a helper function that does demand-driven solving of
           * referenced variables and keeps track of dynamic dependencies. *)
          let nls = f (eval (x,f)) in 
            local_state := Dom.join !local_state nls
        in
          List.iter apply_one_constraint rhsides;
          if not (Dom.leq !local_state old_state) then begin
            (* if the state has changed we update it *)
            HT.replace sigma x !local_state;
            (* the following adds the rhs of the influenced constraints to our
             * todo list and immediately solves for their corresponding variables *)
            let influenced_vars = ref [] in
            let collect_influence (y,f) = 
              HT.replace todo y (f :: HT.find todo y);
              influenced_vars := y :: !influenced_vars
            in
              List.iter collect_influence (HT.find infl x);
              HT.remove infl x;
              List.iter constrain_one_var !influenced_vars;
          end
      end

    and eval (c: constrain) (v: variable) =
      (* demand driven computation of the variable *)
      constrain_one_var v;
      (* add c to the set of constraints that are influenced by v *)
      HT.replace infl v (c :: HT.find infl v);
      (* finally forward the value of v *)
      HT.find sigma v 

    in
      List.iter constrain_one_var initialvars;
      sigma
end 
