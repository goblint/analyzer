open Analyses
open GobConfig
open Batteries

(* Registered solvers. *)
let solvers = ref ["effectWCon", (module EffectWCon.Make2 : GenericGlobSolver)]

(** Register your solvers here!!! *)
let add_solver x = solvers := x::!solvers

(** The solver that actually uses the implementation based of [GobConfig.get_string "solver"]. *)
module Make : GenericGlobSolver =
  functor (S:GlobConstrSys) ->
  functor (LH:Hash.H with type key=S.LVar.t) ->
  functor (GH:Hash.H with type key=S.GVar.t) ->
struct  

  (** Dynamically choose the solver. Fall back to 'effectWCon' in case the 
      asked solver is not found. *)
  let choose_solver () = 
    let solver = get_string "solver" in
    try List.assoc solver !solvers 
    with Not_found -> 
      Printf.eprintf "Solver '%s' not found, falling back to 'effectWCon'!\n" solver;
      (module EffectWCon.Make2 : GenericGlobSolver)
    
  (** You wont belive this! It really works! *)
  let solve = 
    (* Watch and learn! *)
    let dark_magic (module SOL : GenericGlobSolver) =  
      let module F = SOL (S) (LH) (GH) in F.solve
    in 
    (* Did you see! *)
    dark_magic (choose_solver ())
    
end
