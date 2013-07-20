(*
NOTE: Karbyshev has the new version!

(** A top down solver with no-global variables nor side-effecting. *)
(* Use [--full-context --no mutex] on single threaded programs.  *)

open Messages
open Progress
open Pretty

module GU = Goblintutil

exception SolverCannotDoGlobals

module Make 
  (Var: Analyses.VarType)  (* the equation variables *)
  (VDom: Lattice.S) (* the domain *)
  (G: Glob.S) =
struct
  module Glob = G.Var
  module GDom = G.Val

  module SolverTypes = Solver.Types (Var) (VDom) (G)
  include SolverTypes
  
  module VSet = Set.Make (Var)
  
  let solve (system: system) (initialvars: variable list) (start:(Var.t * VDom.t) list): solution' =
    let f x st =
      let join_apply prev rhs =
        let undef _ = Printf.printf "bla\n"; raise SolverCannotDoGlobals in
        let (d,cs) = rhs (st, undef) undef in
        if cs <> [] then raise SolverCannotDoGlobals;
        VDom.join prev d
      in
      List.fold_left join_apply (VDom.bot ()) (system x)
    in
    let sigma : VDom.t VMap.t = VMap.create 113000 (VDom.bot ()) in
    let infl = VMap.create 113000 ([]: variable list) in
    let called = ref VSet.empty in
    let stable = ref VSet.empty in
    let rec destabilize x =
      let t = VMap.find infl x in
        VMap.replace infl x [];
        List.iter (fun y -> stable := VSet.remove y !stable; destabilize y) t
    in
    let rec solve (x:variable) =
      if not (VSet.mem x !stable || VSet.mem x !called) then begin
        if not (VMap.mem sigma x) then (VMap.add sigma x (VDom.bot ()); VMap.add infl x []);
        called := VSet.add x !called;
        let rec loop () =
          stable := VSet.add x !stable;
          let newval = VDom.join (VMap.find sigma x) (f x (eval x)) in
            if not (VDom.equal (VMap.find sigma x) newval) then begin
              VMap.replace sigma x newval; destabilize x
            end ; 
          if not (VSet.mem x !stable) then loop ()
        in loop ();
        called := VSet.remove x !called
      end
    and eval x y =
      solve y; 
      VMap.replace infl y (x :: VMap.find infl y);
      VMap.find sigma y 
    in  
      List.iter solve initialvars;
      (sigma,GMap.create 0 (GDom.bot ()))
    
end
*)
