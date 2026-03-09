open Goblint_constraint.ConstrSys
open Messages

module FwdSolver (System: FwdGlobConstrSys) = struct

  open FwdCommon.BaseFwdSolver(System)
  open FwdCommon.SolverStats(System)

  module WorkSet = struct 
    let list = ref ([]: System.LVar.t list)
    let set = ref LS.empty

    let add x = 
      if not (LS.mem x !set) then (
        list := x::!list;
        set := LS.add x !set 
      )

    let rec map_until_empty f =
      match !list with
      | [] -> ()
      | x::xs -> (
          set := LS.remove x !set;
          list := xs;
          f x;
          map_until_empty f
        )
  end

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
    | Some f -> wrapped f x

  let solve localinit globalinit start_unknowns =
    solver_start_event ();
    List.iter Lcl.init localinit;
    List.iter Gbl.init globalinit;
    List.iter WorkSet.add start_unknowns;
    WorkSet.map_until_empty evaluate;
    let solution = (Lcl.to_seq (), Gbl.to_seq ()) in
    solver_end_event ();
    solution

  module Checker = FwdCommon.Checker(System)(Lcl)(Gbl)
  let check = Checker.check
end
