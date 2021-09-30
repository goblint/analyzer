open Prelude
open Analyses
open GobConfig

(* Registered solvers. *)
let solvers = ref []

(** Register your solvers here!!! *)
let add_solver x = solvers := x::!solvers

(** Dynamically choose the solver. *)
let choose_solver solver =
  try List.assoc solver !solvers
  with Not_found ->
    raise @@ ConfigError ("Solver '"^solver^"' not found. Abort!")

(** The solver that actually uses the implementation based of [GobConfig.get_string "solver"]. *)
module Make =
  functor (S:EqConstrSys) ->
  functor (VH:Hash.H with type key = S.v) ->
  struct

    let solve =
      let module Sol = (val choose_solver (get_string "solver") : GenericEqBoxSolver) in
      let module F = Sol (S) (VH) in
      F.solve
  end

let _ =
  let module T1 : GenericEqBoxSolver = Make in
  ()