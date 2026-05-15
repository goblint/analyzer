open Goblint_constraint.ConstrSys

(* This was included in some experiments;  *)
(* but it should make no difference. *)
(* TODO: Confirm and remove *)
(* let precision_recovery_budget = ref 100 *)

module FwdWBuSolver (System: FwdGlobConstrSys) = struct

  open FwdCommon.BaseFwdSolver(System)
  open FwdCommon.SolverStats(System)

  module WorkSet = struct
    let set  = ref LS.empty

    let add x = set := (LS.add x !set)
    let remove x = set := LS.remove x !set

    let rec map_until_empty f =
      match LS.choose_opt !set with
      | None -> ()
      | Some x -> (
          set := LS.remove x !set;
          f x;
          map_until_empty f
        )
  end

  let abort = GobConfig.get_bool "solvers.bu.abort"

  let get_global x g =
    let glob_data = Gbl.get g in
    Gbl.add_infl glob_data g x;
    glob_data.value

  let rec set_global x g d =
    match Gbl.update_contribution x g d with
    | Updated g_record -> (
        let work = g_record.infl in
        Gbl.replace g {g_record with infl = LS.empty};
        let reevaluate x =
          let r = Lcl.get x in
          if r.called then r.aborted <- true
          else WorkSet.add x in
        LS.iter reevaluate work
      )
    | NotUpdated _ -> ()

  and get_local _ = raise (Failure "Locals should not be queried in rhs") 

  and set_local contributor y d =
    let contributor_record = Lcl.get contributor in
    if abort && contributor_record.called && contributor_record.aborted then ()
    else
      (* Commented out for the TODO below, should also be removed *)
      (* let old_y_record = Lcl.get y in *)
      match Lcl.update_contribution contributor y d false with
      | Updated y_record -> (
          if y_record.called then y_record.aborted <- true
          else (
            WorkSet.remove y;
            iterate y
          )
        )
      | NotUpdated _ -> () 
  (* TODO: this was included in some experiments, but should make no difference,
     confirm and remove *)
  (* let new_contrib_value = (Lcl.get_contribution contributor old_y_record).value in *)
  (* if D.leq new_contrib_value old_y_record.loc_value && !precision_recovery_budget > 0 then ( *)
  (*   precision_recovery_budget := !precision_recovery_budget - 1; *)
  (*   let to_remove = LM.fold (fun c _ acc -> *)
  (*       if System.LVar.equal c contributor then acc else c :: acc *)
  (*     ) old_y_record.loc_from [] in *)
  (*   List.iter (fun c -> *)
  (*       LM.remove old_y_record.loc_from c; *)
  (*       WorkSet.add c *)
  (*     ) to_remove; *)
  (*   old_y_record.loc_value <- Lcl.construct_value old_y_record; *)
  (*   if tracing then trace "recover" "doing the thing"; *)
  (*   if old_y_record.called then old_y_record.aborted <- true *)
  (*   else ( *)
  (*     WorkSet.remove y; *)
  (*     iterate y *)
  (*   ) *)
  (* ) *)

  and wrapped rhs x = (wrap get_local get_global set_local set_global) rhs x

  and iterate x =
    let rloc = Lcl.get x in
    match System.system x with
    | None -> ()
    | Some rhs -> (
        eval_rhs_event x;
        rloc.called <- true;
        rloc.aborted <- false;
        wrapped rhs x;
        rloc.called <- false;
        if rloc.aborted then (iterate[@tailcall]) x
      )

  module Checker = FwdCommon.Checker(System)(Lcl)(Gbl)

  let solve localinit globalinit start_unknowns =
    solver_start_event ();
    List.iter Lcl.init localinit;
    List.iter Gbl.init globalinit;
    List.iter WorkSet.add start_unknowns;
    WorkSet.map_until_empty iterate;
    solver_end_event ();
    AnalysisState.should_warn := true;
    AnalysisState.postsolving := true;
    Checker.check localinit globalinit start_unknowns
end
