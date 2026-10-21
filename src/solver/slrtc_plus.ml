(** Experimental SLR-TC+ integration. 
    The generated scheduler and update policies are copied from tc-solvers. *)

open Batteries
open Goblint_constraint.ConstrSys
open Goblint_constraint.SolverTypes

module Make : DemandEqIncrSolver =
  functor (Arg : IncrSolverArg)
    (S : DemandEqConstrSys)
    (VH : Hashtbl.S with type key = S.v) ->
  struct
    (* This adapter deliberately solves from scratch on every call. *)
    type marshal = unit
    let copy_marshal () = ()
    let relift_marshal () = ()

    module EqS = Goblint_constraint.Translators.EqConstrSysFromDemandConstrSys (S)
    module Post = PostSolver.MakeList (PostSolver.ListArgFromStdArg (EqS) (VH) (Arg))

    let solve xs roots _old_data =
      (* Capture the effective portfolio-stage configuration once per solve.
         Neither initialization nor individual updates perform config lookups. *)
      let module Mode = struct
        let enabled = GobConfig.get_bool "solvers.td3.narrow-globs.enabled"
      end in
      let module Features = struct
        let defer_spawns = true
        let side_effect_phase_reset = false
        let recompute_controlled = false
        (* Permanent widening cannot retract obsolete side contributions. *)
        let dead_side_elimination = Mode.enabled
      end in
      let module Update = Slrtc_catching_up_update.Td3NarrowGlobalsUpdate (Mode) in
      let module Solver = Slrtc_plus_restarting.MakeLazyWithAggregateCache
          (Slrtc_plus_restarting.Control) (Features)
          (Slrtc_plus_restarting.ReluctantSidePolicy)
          (Slrtc_plus_restarting.AggregateCacheEnabled) (Update) (Arg) (S) (VH) in
      let values, _ = Solver.solve xs roots None in
      Post.post xs roots values;
      values, ()
  end

let () = Selector.add_solver ("slrtc_plus", (module Make))
