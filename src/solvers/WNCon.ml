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

open Messages (* The tracing functions are imported *)
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

  let solve (system: system) (initialvars: variable list): solution = 
    (* sigma will be the solution, it is initially empty; new variables are 
     * initialized to bottom as they occur. *)
    let sigma: assignment' = HT.create 113 (Dom.bot ()) in
    let get x = HT.find sigma x in
    let set x d = HT.replace sigma x d in
    (* the worklist of rh-sides that should be considered for each variable! *)
    let todo: rhs list table = HT.create 113 [] in
    let extract_todo x = 
      let is_new = HT.mem todo x in
      let result = if is_new then system x else HT.find todo x in
        HT.replace todo x []; result
    in
    let add_todo x f = HT.replace todo x (f :: HT.find todo x) in
    (* infl captures the dynamic dependencies: infl x is the set of constaints
     * that should be recomputed whenever x changes *)
    let infl_w: constrain list table = HT.create 113 [] in
    let add_infl_w y xf = HT.replace infl_w y (xf :: HT.find infl_w y) in
    let extract_infl_w x: variable list = 
      let fs = HT.find infl_w x in
        HT.remove infl_w x;
        let doit (x,f): variable = add_todo x f; x in
          List.map doit fs  (* XXX Helmut removes duplicates! XXX *)
    in
    (* eval_w, the function that instruments the lookup: *)
    let rec eval_w (x,f) y = 
      solve_w y;
      add_infl_w y (x,f);
      get y
    (* The solve function with widenings *)
    and solve_w x =  
      let old_val = get x in
      (* We want to first join all the changed edges with the previous result,
       * and then apply the widening.  This the closest to a real widening you
       * can get without having to recompute all the constraints. *)
      let joiner d f = 
        (* This is tricky, we evaluate a constraint, and while we delay
         * widenings, we still have to update the state because these calls
         * trigger demand-driven computations that might depend on this
         * variable, such as loops. *)
        let d = Dom.join d (f (eval_w (x,f))) in
          set x d; d
      in
      let con_val = List.fold_left joiner old_val (extract_todo x) in
      let new_val = Dom.widen old_val con_val in
        if Dom.leq new_val old_val then ()
        else begin
          set x new_val; 
          List.iter solve_w (extract_infl_w x)
        end
    in
    (* For the narrowing and final solver we need: *)
    let stable = HT.create 113 () in
    let is_stable = HT.mem stable in
    let add_stable x = HT.add stable x () in
    let rem_stable x = HT.remove stable x in

    let infl: variable list table = HT.create 113 [] in
    let add_infl y x = HT.replace infl y (x :: HT.find infl y) in
    let extract_infl x = let res = HT.find infl x in HT.remove infl x; res in 

    let constrain_all x sigma = 
      let joiner a f = Dom.join a (f sigma) in
        List.fold_left joiner (Dom.bot ()) (system x)
    in
    let rec eval x y = 
      solve y;
      add_infl y x;
      get y
    and solve_n x = 
      if is_stable x then ()
      else begin
        add_stable x;
        let old_val = get x in
        let new_val = Dom.narrow old_val (constrain_all x (eval x)) in
          if Dom.leq old_val new_val then ()
          else begin
            set x new_val;
            let work = extract_infl x in
              List.iter rem_stable work;
              List.iter solve_n work
          end
      end
    and solve x = 
      if HT.mem sigma x then begin
        solve_w x;
        solve_n x
      end else
        solve_n x
    in
      List.iter solve initialvars;
      sigma

end
