(** Top down solver. *)

open Batteries
open Analyses
open Constraints
open Messages

module WP =
  functor (S:EqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v) ->
  struct

    include Generic.SolverStats (S) (HM)
    module VS = Set.Make (S.Var)

    module P =
    struct
      type t = S.Var.t * S.Var.t [@@deriving eq, hash]
    end

    type phase = Widen | Narrow

    let solve st vs =
      let stable = HM.create  10 in
      let infl   = HM.create  10 in (* y -> xs *)
      let called = HM.create  10 in
      let rho    = HM.create  10 in
      let rho'   = HM.create  10 in
      let wpoint = HM.create  10 in

      let add_infl y x =
        if tracing then trace "sol2" "add_infl %a %a\n" S.Var.pretty_trace y S.Var.pretty_trace x;
        HM.replace infl y (VS.add x (try HM.find infl y with Not_found -> VS.empty))
      in
      let rec destabilize x =
        if tracing then trace "sol2" "destabilize %a\n" S.Var.pretty_trace x;
        let w = HM.find_default infl x VS.empty in
        HM.replace infl x VS.empty;
        VS.iter (fun y ->
          HM.remove stable y;
          (* if tracing then trace "sol2" "destabilize %a\n" S.Var.pretty_trace y; *)
          if not (HM.mem called y) then destabilize y) w
      and solve x phase =
        if tracing then trace "sol2" "solve %a, called: %b, stable: %b\n" S.Var.pretty_trace x (HM.mem called x) (HM.mem stable x);
        if not (HM.mem called x || HM.mem stable x) then (
          HM.replace stable x ();
          HM.replace called x ();
          let wpx = HM.mem wpoint x in
          init x;
          let old = HM.find rho x in
          let tmp = eq x (eval x) side in
          let tmp = S.Dom.join tmp (try HM.find rho' x with Not_found -> S.Dom.bot ()) in
          if tracing then trace "sol" "Var: %a\n" S.Var.pretty_trace x ;
          if tracing then trace "sol" "Contrib:%a\n" S.Dom.pretty tmp;
          HM.remove called x;
          let tmp = if wpx then match phase with Widen -> S.Dom.widen old (S.Dom.join old tmp) | Narrow -> S.Dom.narrow old tmp else tmp in
          if not (S.Dom.equal old tmp) then (
            (* if tracing then if is_side x then trace "sol2" "solve side: old = %a, tmp = %a, widen = %a\n" S.Dom.pretty old S.Dom.pretty tmp S.Dom.pretty (S.Dom.widen old (S.Dom.join old tmp)); *)
            update_var_event x old tmp;
            if tracing then trace "sol" "New Value:%a\n\n" S.Dom.pretty tmp;
            (* if tracing then trace "sol2" "new value for %a (wpx: %b, is_side: %b) is %a. Old value was %a\n" S.Var.pretty_trace x (HM.mem rho x) (is_side x) S.Dom.pretty tmp S.Dom.pretty old; *)
            HM.replace rho x tmp;
            destabilize x;
            (solve[@tailcall]) x phase;
          ) else if not (HM.mem stable x) then (
            (solve[@tailcall]) x Widen;
          ) else if phase = Widen && S.system x <> None then (
            HM.remove stable x;
            (solve[@tailcall]) x Narrow;
          );
        )
      and eq x get set =
        if tracing then trace "sol2" "eq %a\n" S.Var.pretty_trace x;
        eval_rhs_event x;
        match S.system x with
        | None -> S.Dom.bot ()
        | Some f -> f get set
      and eval x y =
        if tracing then trace "sol2" "eval %a ## %a\n" S.Var.pretty_trace x S.Var.pretty_trace y;
        get_var_event y;
        if HM.mem called y then HM.replace wpoint y ();
        solve y Widen;
        add_infl y x;
        HM.find rho y
      and side y d =
        let old = try HM.find rho' y with Not_found -> S.Dom.bot () in
        if not (S.Dom.leq d old) then (
          HM.replace rho' y (S.Dom.widen old d);
          HM.remove stable y;
          init y;
          solve y Widen;
        )
      and init x =
        if tracing then trace "sol2" "init %a\n" S.Var.pretty_trace x;
        if not (HM.mem rho x) then (
          new_var_event x;
          HM.replace rho  x (S.Dom.bot ())
        )
      in

      let set_start (x,d) =
        if tracing then trace "sol2" "set_start %a ## %a\n" S.Var.pretty_trace x S.Dom.pretty d;
        init x;
        HM.replace rho x d;
        solve x Widen
      in

      start_event ();
      List.iter set_start st;
      List.iter init vs;
      List.iter (fun x -> solve x Widen) vs;
      (* iterate until there are no unstable variables
       * after termination, only those variables are stable which are
       * - reachable from any of the queried variables vs, or
       * - effected by side-effects and have no constraints on their own (this should be the case for all of our analyses)
       *)
      let rec solve_sidevs () =
        let non_stable = List.filter (neg (HM.mem stable)) vs in
        if non_stable <> [] then (
          List.iter (fun x -> solve x Widen) non_stable;
          solve_sidevs ()
        )
      in
      solve_sidevs ();
      stop_event ();

      HM.clear stable;
      HM.clear infl  ;
      HM.clear rho'  ;

      rho

  end

let _ =
  Selector.add_solver ("topdown_term", (module EqIncrSolverFromEqSolver (WP)));
