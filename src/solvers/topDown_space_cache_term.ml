(** Terminating top down solver that only keeps values at widening points and restores other values afterwards. *)
(* This is superseded by td3 but kept as a simpler version without the incremental parts. *)

open Prelude
open Analyses
open Constraints
open Messages

module WP =
  functor (S:EqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
  struct

    include Generic.SolverStats (S) (HM)
    module VS = Set.Make (S.Var)

    module P =
    struct
      type t = S.Var.t * S.Var.t
      let equal (x1,x2) (y1,y2) = S.Var.equal x1 y1 && S.Var.equal x2 y2
      let hash  (x1,x2)         = (S.Var.hash x1 * 13) + S.Var.hash x2
    end

    type phase = Widen | Narrow

    let solve box st vs =
      let stable = HM.create  10 in
      let infl   = HM.create  10 in (* y -> xs *)
      let called = HM.create  10 in
      let rho    = HM.create  10 in
      let rho'   = HM.create  10 in
      let cache_sizes = ref [] in

      let add_infl y x =
        if tracing then trace "sol2" "add_infl %a %a\n" S.Var.pretty_trace y S.Var.pretty_trace x;
        HM.replace infl y (VS.add x (try HM.find infl y with Not_found -> VS.empty))
      in
      let rec destabilize x =
        if tracing then trace "sol2" "destabilize %a on %i\n" S.Var.pretty_trace x (S.Var.line_nr x);
        let w = HM.find_default infl x VS.empty in
        HM.replace infl x VS.empty;
        VS.iter (fun y -> HM.remove stable y; if not (HM.mem called y) then destabilize y) w
      and solve x phase =
        if tracing then trace "sol2" "solve %a on %i, called: %b, stable: %b\n" S.Var.pretty_trace x (S.Var.line_nr x) (HM.mem called x) (HM.mem stable x);
        if not (HM.mem called x || HM.mem stable x) then (
          HM.replace stable x ();
          HM.replace called x ();
          let old = HM.find rho x in
          let l = HM.create 10 in
          let tmp = eq x (eval l x) (side l) in
          let tmp = S.Dom.join tmp (try HM.find rho' x with Not_found -> S.Dom.bot ()) in
          if tracing then trace "sol" "Var: %a\n" S.Var.pretty_trace x ;
          if tracing then trace "sol" "Contrib:%a\n" S.Dom.pretty tmp;
          HM.remove called x;
          let tmp = match phase with Widen -> S.Dom.widen old (S.Dom.join old tmp) | Narrow -> S.Dom.narrow old tmp in
          if tracing then trace "cache" "cache size %d for %a on %i\n" (HM.length l) S.Var.pretty_trace x (S.Var.line_nr x);
          cache_sizes := HM.length l :: !cache_sizes;
          if not (S.Dom.equal old tmp) then (
            (* if tracing then if is_side x then trace "sol2" "solve side: old = %a, tmp = %a, widen = %a\n" S.Dom.pretty old S.Dom.pretty tmp S.Dom.pretty (S.Dom.widen old (S.Dom.join old tmp)); *)
            update_var_event x old tmp;
            if tracing then trace "sol" "New Value:%a\n\n" S.Dom.pretty tmp;
            (* if tracing then trace "sol2" "new value for %a (wpx: %b, is_side: %b) on %i is %a. Old value was %a\n" S.Var.pretty_trace x (HM.mem rho x) (is_side x) (S.Var.line_nr x) S.Dom.pretty tmp S.Dom.pretty old; *)
            HM.replace rho x tmp;
            destabilize x;
            (solve[@tailcall]) x phase;
          ) else if not (HM.mem stable x) then (
            (solve[@tailcall]) x Widen;
          ) else if phase = Widen then (
            HM.remove stable x;
            (solve[@tailcall]) x Narrow;
          );
        )
      and eq x get set =
        if tracing then trace "sol2" "eq %a on %i\n" S.Var.pretty_trace x (S.Var.line_nr x);
        eval_rhs_event x;
        match S.system x with
        | None -> S.Dom.bot ()
        | Some f -> f get set
      and simple_solve l x y =
        if tracing then trace "sol2" "simple_solve %a on %i (rhs: %b)\n" S.Var.pretty_trace y (S.Var.line_nr y) (S.system y <> None);
        if S.system y = None then init y;
        if HM.mem rho y then (solve y Widen; HM.find rho y) else
        if HM.mem called y then (init y; HM.remove l y; HM.find rho y) else
        if HM.mem l y then HM.find l y
        else (
          HM.replace called y ();
          let tmp = eq y (eval l x) (side l) in
          HM.remove called y;
          if HM.mem rho y then (HM.remove l y; solve y Widen; HM.find rho y)
          else (HM.replace l y tmp; tmp)
        )
      and eval l x y =
        if tracing then trace "sol2" "eval %a on %i ## %a on %i\n" S.Var.pretty_trace x (S.Var.line_nr x) S.Var.pretty_trace y (S.Var.line_nr y);
        get_var_event y;
        let tmp = simple_solve l x y in
        if HM.mem rho y then add_infl y x;
        tmp
      and side l y d =
        if tracing then trace "sol2" "side to %a on %i (wpx: %b) ## value: %a\n" S.Var.pretty_trace y (S.Var.line_nr y) (HM.mem rho y) S.Dom.pretty d;
        let old = try HM.find rho' y with Not_found -> S.Dom.bot () in
        if not (S.Dom.leq d old) then (
          HM.replace rho' y (S.Dom.join old d);
          HM.remove l y;
          HM.remove stable y;
          init y;
          solve y Widen;
        )
      and init x =
        if tracing then trace "sol2" "init %a on %i\n" S.Var.pretty_trace x (S.Var.line_nr x);
        if not (HM.mem rho x) then (
          new_var_event x;
          HM.replace rho  x (S.Dom.bot ())
        )
      in

      let set_start (x,d) =
        if tracing then trace "sol2" "set_start %a on %i ## %a\n" S.Var.pretty_trace x  (S.Var.line_nr x) S.Dom.pretty d;
        init x;
        HM.replace rho x d;
        HM.replace rho' x d;
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

      (* verifies values at widening points and adds values for variables in-between *)
      let visited = HM.create 10 in
      let rec get x =
        if HM.mem visited x then (
          if not (HM.mem rho x) then (
            ignore @@ Pretty.printf "Found an unknown that should be a widening point: %a\n" S.Var.pretty_trace x;
            S.Dom.top ()
          ) else
            HM.find rho x
        ) else (
          HM.replace visited x ();
          let check_side y d =
            let d' = try HM.find rho y with Not_found -> S.Dom.bot () in
            if not (S.Dom.leq d d') then ignore @@ Pretty.printf "Fixpoint not reached in restore step at side-effected variable %a: %a not leq %a\n" S.Var.pretty_trace y S.Dom.pretty d S.Dom.pretty d'
          in
          let eq x =
            match S.system x with
            | None -> if HM.mem rho x then HM.find rho x else S.Dom.bot ()
            | Some f -> f get check_side
          in
          if HM.mem rho x then (
            let d1 = HM.find rho x in
            let d2 = eq x in
            if not (S.Dom.leq d2 d1) then
              ignore @@ Pretty.printf "Fixpoint not reached in restore step at %a (%s:%d)\n  @[Variable:\n%a\nRight-Hand-Side:\n%a\nCalculating one more step changes: %a\n@]" S.Var.pretty_trace x (S.Var.file_name x) (S.Var.line_nr x) S.Dom.pretty d1 S.Dom.pretty d2 S.Dom.pretty_diff (d1,d2);
            d1
          ) else (
            let d = eq x in
            HM.replace rho x d;
            d
          )
        )
      in
      (* restore values for non-widening-points *)
      if GobConfig.get_bool "exp.solver.wp.restore" then (
        if (GobConfig.get_bool "dbg.verbose") then
          print_endline ("Restoring missing values.");
        let restore () =
          let get x =
            let d = get x in
            if tracing then trace "sol2" "restored var %a on %i ## %a\n" S.Var.pretty_trace x  (S.Var.line_nr x) S.Dom.pretty d
          in
          List.iter get vs
        in
        Stats.time "restore" restore ();
        if (GobConfig.get_bool "dbg.verbose") then ignore @@ Pretty.printf "Solved %d vars. Total of %d vars after restore.\n" !Goblintutil.vars (HM.length rho);
      );
      let avg xs = float_of_int (BatList.sum xs) /. float_of_int (List.length xs) in
      if tracing then trace "cache" "#caches: %d, max: %d, avg: %.2f\n" (List.length !cache_sizes) (List.max !cache_sizes) (avg !cache_sizes);

      let reachability xs =
        let reachable = HM.create (HM.length rho) in
        let rec one_var x =
          if not (HM.mem reachable x) then (
            HM.replace reachable x ();
            match S.system x with
            | None -> ()
            | Some x -> one_constaint x
          )
        and one_constaint f =
          ignore (f (fun x -> one_var x; try HM.find rho x with Not_found -> S.Dom.bot ()) (fun x _ -> one_var x))
        in
        List.iter one_var xs;
        HM.iter (fun x _ -> if not (HM.mem reachable x) then HM.remove rho x) rho
      in
      reachability vs;
      stop_event ();

      HM.clear stable;
      HM.clear infl  ;

      rho

  end

let _ =
  let module WP = GlobSolverFromIneqSolver (SLR.JoinContr (WP)) in
  Selector.add_solver ("topdown_space_cache_term", (module WP : GenericGlobSolver));
