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
  functor (Arg: IncrSolverArg) ->
  functor (S:EqConstrSys) ->
  functor (VH:Hashtbl.S with type key = S.v) ->
  struct
    type marshal = Obj.t (* cannot use Sol.marshal because cannot unpack first-class module in applicative functor *)

    let copy_marshal (marshal: marshal) =
      let module Sol = (val choose_solver (get_string "solver") : GenericEqIncrSolver) in
      let module F = Sol (Arg) (S) (VH) in
      Obj.repr (F.copy_marshal (Obj.obj marshal))

    let relift_marshal (marshal: marshal) =
      let module Sol = (val choose_solver (get_string "solver") : GenericEqIncrSolver) in
      let module F = Sol (Arg) (S) (VH) in
      Obj.repr (F.relift_marshal (Obj.obj marshal))

    let solve xs vs (old_data: marshal option) =
      let module Sol = (val choose_solver (get_string "solver") : GenericEqIncrSolver) in
      let module F = Sol (Arg) (S) (VH) in
      let (vh, marshal) = F.solve xs vs (Option.map Obj.obj old_data) in
      (vh, Obj.repr marshal)
  end

let _ =
  let module T1 : GenericEqIncrSolver = Make in
  ()