(** Top-down solver with side effects. Baseline for comparisons with td_parallel solvers     ([td_simplified_ref]).
    This is the same as ([td_simplified]), but it uses records for solver that instead of multiple hashmaps.
*)

open Batteries
open Goblint_constraint.ConstrSys
open Goblint_constraint.SolverTypes
open Messages

module M = Messages

module Base : GenericEqSolver =
  functor (S:EqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v) ->
  struct
    open SolverBox.Warrow (S.Dom)
    include Generic.SolverStats (S) (HM)
    module VS = Set.Make (S.Var)

    type var_data = {
      infl: VS.t;
      value: S.Dom.t;
      wpoint: bool;
      stable: bool;
      called: bool;
    }

    let solve st vs =
      let (data : var_data ref HM.t) = HM.create 10 in

      print_solver_stats := (fun () ->
          Logs.debug "|data|=%d" (HM.length data);
        );

      let add_infl y x =
        if tracing then trace "infl" "add_infl %a %a" S.Var.pretty_trace y S.Var.pretty_trace x;
        let y_ref = HM.find data y in
        y_ref := { !y_ref with infl = VS.add x !y_ref.infl }
      in

      let init (x : S.v): var_data ref =
        let x_ref = HM.find_option data x in
        match x_ref with
        | Some r -> r
        | None ->
          begin
            new_var_event x;
            if tracing then trace "init" "init %a" S.Var.pretty_trace x;
            let data_x = ref {
                infl = VS.empty;
                value = S.Dom.bot ();
                wpoint = false;
                stable = false;
                called = false
              } in
            HM.replace data x data_x;
            data_x
          end
      in

      let eq x get set =
        if tracing then trace "eq" "eq %a" S.Var.pretty_trace x;
        match S.system x with
        | None -> S.Dom.bot ()
        | Some f -> f get set
      in

      let rec destabilize x =
        if tracing then trace "destab" "destabilize %a" S.Var.pretty_trace x;
        let x_ref = HM.find data x in
        let w = !x_ref.infl in
        x_ref := { !x_ref with infl = VS.empty };
        VS.iter (fun y ->
            if tracing then trace "destab" "stable remove %a" S.Var.pretty_trace y;
            let y_ref = HM.find data y in
            y_ref := { !y_ref with stable = false };
            destabilize y
          ) w
      in

      let rec query x y =
        let y_ref = init y in
        if tracing then trace "sol_query" "entering query for %a; stable %b; called %b" S.Var.pretty_trace y (!y_ref.stable) (!y_ref.called);
        get_var_event y;
        if not (!y_ref.called) then (
          if S.system y = None then (
            y_ref := { !y_ref with stable = true };
          ) else (
            y_ref := { !y_ref with called = true };
            if tracing then trace "iter" "iterate called from query";
            iterate y;
            y_ref := { !y_ref with called = false };)
        ) else (
          if tracing && not (!y_ref.wpoint) then trace "wpoint" "query adding wpoint %a" S.Var.pretty_trace y;
          y_ref := { !y_ref with wpoint = true };
        );
        let tmp = !y_ref.value in
        add_infl y x;
        if tracing then trace "answer" "exiting query for %a\nanswer: %a" S.Var.pretty_trace y S.Dom.pretty tmp;
        tmp

      and side x y d = (* side from x to y; only to variables y w/o rhs; x only used for trace *)
        assert (S.system y = None);
        let y_ref = init y in
        if tracing then trace "side" "side to %a (wpx: %b) from %a ## value: %a" S.Var.pretty_trace y (!y_ref.wpoint) S.Var.pretty_trace x S.Dom.pretty d;
        let widen a b =
          if M.tracing then M.trace "wpoint" "side widen %a" S.Var.pretty_trace y;
          S.Dom.widen a (S.Dom.join a b)
        in
        let op a b = if !y_ref.wpoint then widen a b else S.Dom.join a b
        in
        let old = !y_ref.value in
        let tmp = op old d in
        y_ref := { !y_ref with stable = true };
        if not (S.Dom.leq tmp old) then (
          if tracing && not (S.Dom.is_bot old) then trace "update" "side to %a (wpx: %b) from %a: %a -> %a" S.Var.pretty_trace y (!y_ref.wpoint) S.Var.pretty_trace x S.Dom.pretty old S.Dom.pretty tmp;
          y_ref := { !y_ref with value = tmp };
          destabilize y;
          (* make y a widening point. This will only matter for the next side _ y.  *)
          if tracing && not (!y_ref.wpoint) then trace "wpoint" "side adding wpoint %a" S.Var.pretty_trace y;
          y_ref := { !y_ref with wpoint = true };
        )

      and iterate x = (* ~(inner) solve in td3*)

        (* begining of iterate*)
        let x_ref = init x in
        if tracing then trace "iter" "iterate %a, called: %b, stable: %b, wpoint: %b" S.Var.pretty_trace x (!x_ref.called) (!x_ref.stable) (!x_ref.wpoint);
        assert (S.system x <> None);
        if not (!x_ref.stable) then (
          x_ref := { !x_ref with stable = true };
          let wp = !x_ref.wpoint in (* if x becomes a wpoint during eq, checking this will delay widening until next iterate *)
          let eqd = eq x (query x) (side x) in (* d from equation/rhs *)
          let old = !x_ref.value in (* d from older iterate *)
          let wpd = (* d after widen/narrow (if wp) *)
            if not wp then eqd
            else (
              if M.tracing then M.trace "wpoint" "widen %a" S.Var.pretty_trace x;
              box old eqd)
          in
          if not (Timing.wrap "S.Dom.equal" (fun () -> S.Dom.equal old wpd) ()) then (
            (* old != wpd *)
            if tracing && not (S.Dom.is_bot old) && !x_ref.wpoint then trace "solchange" "%a (wpx: %b): %a" S.Var.pretty_trace x (!x_ref.wpoint) S.Dom.pretty_diff (wpd, old);
            update_var_event x old wpd;
            x_ref := { !x_ref with value = wpd };
            destabilize x;
            if tracing then trace "iter" "iterate changed %a" S.Var.pretty_trace x;
            (iterate[@tailcall]) x
          ) else (
            (* old == wpd *)
            if not (!x_ref.stable) then (
              (* value unchanged, but not stable, i.e. destabilized itself during rhs *)
              if tracing then trace "iter" "iterate still unstable %a" S.Var.pretty_trace x;
              (iterate[@tailcall]) x
            ) else (
              (* this makes e.g. nested loops precise, ex. tests/regression/34-localization/01-nested.c - if we do not remove wpoint, the inner loop head will stay a wpoint and widen the outer loop variable. *)
              if tracing && (!x_ref.wpoint) then trace "wpoint" "iterate removing wpoint %a" S.Var.pretty_trace x;
              x_ref := { !x_ref with wpoint = false };
            )
          )
        )
      in

      let set_start (x,d) =
        let x_ref = init x in
        x_ref := { !x_ref with value = d; stable = true };
      in

      (* beginning of main solve *)
      start_event ();

      List.iter set_start st;

      List.iter (fun x -> ignore @@ init x) vs;
      (* If we have multiple start variables vs, we might solve v1, then while solving v2 we side some global which v1 depends on with a new value. Then v1 is no longer stable and we have to solve it again. *)
      let i = ref 0 in
      let rec solver () = (* as while loop in paper *)
        incr i;
        let unstable_vs = List.filter (neg (fun x -> !(HM.find data x).stable)) vs in
        if unstable_vs <> [] then (
          if Logs.Level.should_log Debug then (
            if !i = 1 then Logs.newline ();
            Logs.debug "Unstable solver start vars in %d. phase:" !i;
            List.iter (fun v -> Logs.debug "\t%a" S.Var.pretty_trace v) unstable_vs;
            Logs.newline ();
            flush_all ();
          );
          List.iter (fun x ->
              let x_ref = HM.find data x in
              x_ref := { !x_ref with called = true };
              if tracing then trace "multivar" "solving for %a" S.Var.pretty_trace x;
              iterate x;
              x_ref := { !x_ref with called = false }
            ) unstable_vs;
          solver ();
        )
      in
      solver ();
      (* After termination, only those variables are stable which are
       * - reachable from any of the queried variables vs, or
       * - effected by side-effects and have no constraints on their own (this should be the case for all of our analyses). *)

      stop_event ();
      if Logs.Level.should_log Debug then (
        Logs.debug "Data after iterate completed";
        Logs.debug "|data|=%d" (HM.length data);
      );

      if GobConfig.get_bool "dbg.print_wpoints" then (
        Logs.newline ();
        Logs.debug "Widening points:";
        HM.filter (fun x_ref -> !x_ref.wpoint) data |> HM.iter (fun k x_ref ->
            Logs.debug "%a" S.Var.pretty_trace k)
      );

      HM.map (fun x x_ref -> !x_ref.value) data
  end

let () =
  Selector.add_solver ("td_simplified_ref", (module PostSolver.DemandEqIncrSolverFromEqSolver (Base)));
