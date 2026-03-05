open Goblint_constraint.ConstrSys
open Messages

module FwdBuSolver (System: FwdGlobConstrSys) = struct

  module D = System.D
  module G = System.G

  module LS = Set.Make (System.LVar)
  module GM = Hashtbl.Make(System.GVar)
  module LM = Hashtbl.Make(System.LVar)

  (* Use this to set origins to unknown (vs. program point as below)
     module OM = LM
     let source x = x
  *)

  module OM = Hashtbl.Make(Node)
  let source = System.LVar.node
  let rhs_eval_count = ref 0

  (* TODO make this proper default read from a common module or config *)
  let gas_default = FwdCommon.gas_default

  module Gbl = FwdCommon.SolverGlobals(System)(LS)(LM)(GM)(OM)
  module Lcl = FwdCommon.SolverLocals(System)(LM)

  let get_global x g =
    let glob_data = Gbl.get g in
    Gbl.add_infl glob_data g x;
    glob_data.value

  let rec set_global x g d =
    let sx = source x in 
(*
        replaces old contribution with the new one;
        reconstructs value of g from contributions;
        propagates infl and updates value - if value has changed
*)
    let g_record = Gbl.get g in
    let old_contribution: Gbl.contribution = Gbl.get_contribution sx g_record in
    LM.add g_record.last x d;
    let set = LS.add x old_contribution.set in
    let d_new = Gbl.get_last_contrib set g_record.last in
    let new_contribution = Gbl.warrow old_contribution d_new set in
    OM.replace g_record.from sx new_contribution;
    if not (G.equal new_contribution.value old_contribution.value) then (
      let new_g = if G.leq old_contribution.value new_contribution.value then 
          G.join new_contribution.value g_record.value 
        else Gbl.construct_value g_record in
      if not (G.equal g_record.value new_g) then (
        let (work : Gbl.LS.t) = g_record.infl in
        Gbl.replace g {g_record with value = new_g; infl = Gbl.LS.empty};
        let doit (x : Gbl.LM.key) = 
          let r = Lcl.get x in
          if r.called then r.aborted <- true
          else (iterate[@tailcall]) x 
        in
        Gbl.LS.iter doit work 
      )
    )

  and get_local _ = raise (Failure "Locals should not be queried in rhs") 

  (**
      Handle contribution from contributor to y with d
      replaces old contribution with the new one;
      reconstructs value of y from contributions;
      propagates infl together with y and updates value - if value has changed
  *)
  and set_local contributor y d =
    let y_record = Lcl.get y in
    let old_contribution = Lcl.get_contribution contributor y_record in
    let new_contribution =
      (* Automatic detection of warrowing points *)
      if y_record.called then Lcl.warrow old_contribution d 
      else {old_contribution with value=d} in

    if not (D.equal new_contribution.value old_contribution.value) then (
      LM.replace y_record.loc_from contributor new_contribution;
      let new_y = if D.leq old_contribution.value new_contribution.value then 
          (* If the contribution is strictly greater than previous,
             the join with the new contribution is equal to the value on the else
             branch, but much cheaper to calculate *)
          D.join y_record.loc_value new_contribution.value
        else Lcl.construct_value y_record in

      if not (D.equal y_record.loc_value new_y) then (
        y_record.loc_value <- new_y;
        (* Avoid endless loops by not re-iterating now, but mark the unknown,
           so that it will be re-evaluated from its own stack frame *)
        if y_record.called then y_record.aborted <- true
        else (iterate[@tailcall]) y 
      )
    )

  (**
        wrapper around propagation function to collect multiple contributions to same unknowns;
        contributions are delayed until the very end
  *)
  and wrap (x, rhs) d =
    let local_updates = LM.create 10 in
    let global_updates = GM.create 10 in

    let collect_local y d =
      let d = LM.find_opt local_updates y |> BatOption.map_default (D.join d) d in 
      LM.replace local_updates y d in

    let collect_global g d =
      let d = GM.find_opt global_updates g |> BatOption.map_default (G.join d) d in
      GM.replace global_updates g d in

    (* Use the collect functions for set, so that we can delay and re-order the
       contributions *)
    rhs d get_local collect_local (get_global x) collect_global;
    GM.iter (set_global x) global_updates;
    LM.iter (set_local x) local_updates;
    (* possibly better with reversed ordering *)


    (* now the actual propagation! *)
  and iterate x = 
    rhs_eval_count := !rhs_eval_count + 1;
    let rloc = Lcl.get x in
    match System.system x with
    | None -> ()
    | Some rhs -> (
        rloc.called <- true;
        rloc.aborted <- false;
        wrap (x,rhs) rloc.loc_value;
        rloc.called <- false;
        if rloc.aborted then (iterate[@tailcall]) x
      )

  (* ... now the main solver loop ... *)

  (*  These should be handled via event hooks *)
  let solver_start_event () =
    let starttime_ms = int_of_float (Unix.gettimeofday () *. 1000.) in
    Logs.info "Solver start: %d" starttime_ms

  let solver_end_event () =
    let endtime_ms = int_of_float (Unix.gettimeofday () *. 1000.) in
    Logs.info "Solver end: %d" endtime_ms;
    Logs.info "RHS: %d" !rhs_eval_count

  let solve localinit globalinit start_unknowns =
    solver_start_event ();
    List.iter Lcl.init localinit;
    List.iter Gbl.init globalinit;
    List.iter iterate start_unknowns;
    let sigma = LM.to_seq Lcl.loc |> Seq.map (fun (k,(l : Lcl.t)) -> (k,l.loc_value)) in
    let tau = GM.to_seq Gbl.glob |> Seq.map (fun (k,(l : Gbl.t)) -> (k,l.value)) in
    solver_end_event ();
    (sigma,tau)

  (* ... now the checker! *)

(*
       work list just for checking ...
*)

  module Checker = FwdCommon.Checker(System)(Lcl)(Gbl)
  let check = Checker.check
end
