open Goblint_constraint.ConstrSys

module FwdSolver (System: FwdGlobConstrSys) = struct

  open FwdCommon.BaseFwdSolver(System)
  open FwdCommon.SolverStats(System)

  module WorkSet = (val
                     match GobConfig.get_string "solvers.fwd.work_iteration" with
                     | "lifo" -> (module FwdCommon.LIFOWorkList(System.LVar) : FwdCommon.WorkListS with type elt = System.LVar.t)
                     | "set"  -> (module FwdCommon.SetWorkList(System.LVar)  : FwdCommon.WorkListS with type elt = System.LVar.t)
                     | s      -> failwith ("Unknown work_iteration strategy: " ^ s)
                   )

  let get_global x g =
    let glob_data = Gbl.get g in
    Gbl.add_infl glob_data g x;
    glob_data.value

  let set_global x g d =
    match Gbl.update_contribution x g d with
    | Updated g_record -> (
        let work = g_record.infl in
        Gbl.replace g {g_record with infl = LS.empty};
        LS.iter WorkSet.add work
      )
    | NotUpdated _ -> ()

  let get_local _ = raise (Failure "Locals should not be queried in rhs") 

  let set_local contributor y d =
    match Lcl.update_contribution contributor y d true with
    | Updated y_record -> WorkSet.add y
    | NotUpdated _ -> ()

  let wrapped rhs x = (wrap get_local get_global set_local set_global) rhs x

  let evaluate x =
    match System.system x with
    | None -> ()
    | Some f -> eval_rhs_event x; wrapped f x

  module Checker = FwdCommon.Checker(System)(Lcl)(Gbl)

  let solve localinit globalinit start_unknowns =
    solver_start_event ();
    List.iter Lcl.init localinit;
    List.iter Gbl.init globalinit;
    List.iter WorkSet.add start_unknowns;
    WorkSet.map_until_empty evaluate;
    solver_end_event ();
    AnalysisState.should_warn := true;
    AnalysisState.postsolving := true;
    Checker.check localinit globalinit start_unknowns
end
