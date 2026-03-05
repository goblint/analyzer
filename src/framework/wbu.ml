open Goblint_constraint.ConstrSys
open Messages

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
    match Lcl.update_contribution contributor y d with
    | Updated y_record -> (
        if y_record.called then y_record.aborted <- true
        else (
          WorkSet.remove y;
          iterate y 
        )
      )
    | NotUpdated _ -> ()

  and wrapped rhs x d = (wrap get_local get_global set_local set_global) rhs x d

  and iterate x = 
    let rloc = Lcl.get x in
    match System.system x with
    | None -> ()
    | Some rhs -> (
        rloc.called <- true;
        rloc.aborted <- false;
        wrapped rhs x rloc.loc_value;
        rloc.called <- false;
        if rloc.aborted then (iterate[@tailcall]) x
      )

  let solve localinit globalinit start_unknowns =
    solver_start_event ();

    List.iter Lcl.init localinit;
    List.iter Gbl.init globalinit;
    List.iter WorkSet.add start_unknowns;
    WorkSet.map_until_empty iterate;
    let solution = (Lcl.to_seq (), Gbl.to_seq ()) in
    solver_end_event ();
    solution

  module Checker = FwdCommon.Checker(System)(Lcl)(Gbl)
  let check = Checker.check
end
