(** Top down solver using box/warrow. This is superseded by td3 but kept as a simple version without term & space (& incremental). *)

open Prelude
open Analyses
open Constraints
open Messages

module WP =
  functor (S:EqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v) ->
  struct
    open SolverBox.Warrow (S)

    include Generic.SolverStats (S) (HM)
    module VS = Set.Make (S.Var)

    module P =
    struct
      type t = S.Var.t * S.Var.t [@@deriving eq, hash]
    end

    module HPM = Hashtbl.Make (P)

    let solve st vs =
      let stable = HM.create  10 in
      let infl   = HM.create  10 in (* y -> xs *)
      let set    = HM.create  10 in (* y -> xs *)
      let sidevs = HM.create  10 in (* side-effected variables *)
      let called = HM.create  10 in
      let rho    = HM.create  10 in
      let rho'   = HPM.create 10 in (* x,y -> d *)
      let wpoint = HM.create  10 in

      let add_infl y x =
        if tracing then trace "sol2" "add_infl %a %a\n" S.Var.pretty_trace y S.Var.pretty_trace x;
        HM.replace infl y (VS.add x (try HM.find infl y with Not_found -> VS.empty))
      in
      let add_set x y d =
        HM.replace set y (VS.add x (try HM.find set y with Not_found -> VS.empty));
        HPM.add rho' (x,y) d;
        HM.replace sidevs y ()
      in
      let is_side x = HM.mem set x in
      let rec destabilize x =
        (* if tracing then trace "sol2" "destabilize %a\n" S.Var.pretty_trace x; *)
        let w = HM.find_default infl x VS.empty in
        HM.replace infl x VS.empty;
        VS.iter (fun y ->
          HM.remove stable y;
          if tracing then trace "sol2" "destabilize %a\n" S.Var.pretty_trace y;
          if not (HM.mem called y) then destabilize y) w
      and solve x =
        if tracing then trace "sol2" "solve %a, called: %b, stable: %b\n" S.Var.pretty_trace x (HM.mem called x) (HM.mem stable x);
        if not (HM.mem called x || HM.mem stable x) then (
          HM.replace stable x ();
          HM.replace called x ();
          let wpx = HM.mem wpoint x in
          init x;
          let old = HM.find rho x in
          let tmp' = eq x (eval x) (side x) in
          let tmp = S.Dom.join tmp' (sides x) in
          if tracing then trace "sol" "Var: %a\n" S.Var.pretty_trace x ;
          if tracing then trace "sol" "Contrib:%a\n" S.Dom.pretty tmp;
          let tmp = if is_side x then S.Dom.widen old (S.Dom.join old tmp) else if wpx then box x old tmp else tmp in
          HM.remove called x;
          if not (S.Dom.equal old tmp) then (
            if tracing then if is_side x then trace "sol2" "solve side: old = %a, tmp = %a, widen = %a\n" S.Dom.pretty old S.Dom.pretty tmp S.Dom.pretty (S.Dom.widen old (S.Dom.join old tmp));
            update_var_event x old tmp;
            if tracing then trace "sol" "New Value:%a\n\n" S.Dom.pretty tmp;
            if tracing then trace "sol2" "new value for %a (wpx: %b, is_side: %b) is %a. Old value was %a\n" S.Var.pretty_trace x (HM.mem rho x) (is_side x) S.Dom.pretty tmp S.Dom.pretty old;
            HM.replace rho x tmp;
            destabilize x;
          );
          (solve[@tailcall]) x
        )
      and eq x get set =
        if tracing then trace "sol2" "eq %a\n" S.Var.pretty_trace x;
        eval_rhs_event x;
        match S.system x with
        | None -> S.Dom.bot ()
        | Some f ->
          let effects = ref Set.empty in
          let sidef y d =
            if not (Set.mem y !effects) then (
              HPM.replace rho' (x,y) (S.Dom.bot ()); (* TODO needed? tests also work without this... *)
              effects := Set.add y !effects
            );
            set y d
          in
          f get sidef
      and eval x y =
        if tracing then trace "sol2" "eval %a ## %a\n" S.Var.pretty_trace x S.Var.pretty_trace y;
        get_var_event y;
        if HM.mem called y || S.system y = None then HM.replace wpoint y ();
        solve y;
        add_infl y x;
        HM.find rho y
      and sides x =
        let w = try HM.find set x with Not_found -> VS.empty in
        let d = Enum.fold (fun d y -> let r = try S.Dom.join d (HPM.find rho' (y,x)) with Not_found -> d in if tracing then trace "sol2" "sides: side %a from %a: %a\n" S.Var.pretty_trace x S.Var.pretty_trace y S.Dom.pretty r; r) (S.Dom.bot ()) (VS.enum w) in
        if tracing then trace "sol2" "sides %a ## %a\n" S.Var.pretty_trace x S.Dom.pretty d;
        d
      and side x y d =
        if tracing then trace "sol2" "side %a ## %a (wpx: %b) ## %a\n" S.Var.pretty_trace x S.Var.pretty_trace y (HM.mem rho y) S.Dom.pretty d;
        let old = try HPM.find rho' (x,y) with Not_found -> S.Dom.bot () in
        if not (S.Dom.equal old d) then (
          add_set x y (S.Dom.join old d);
          HM.remove stable y;
          solve y;
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
        add_set x x d;
        solve x
      in

      start_event ();
      List.iter set_start st;
      List.iter init vs;
      List.iter solve vs;
      let keys h = HM.fold (fun k _ a -> k::a) h [] in
      let n = ref 1 in
      (* iterate until there are no more new side-effects *)
      let rec solve_sidevs () =
        let gs = keys sidevs in
        HM.clear sidevs;
        if gs <> [] then (
          if tracing then trace "sol2" "Round %d: %d side-effected variables to solve\n" !n (List.length gs);
          incr n;
          List.iter solve gs;
          List.iter solve vs;
          solve_sidevs ()
        )
      in
      solve_sidevs ();
      stop_event ();

      HM.clear stable;
      HM.clear infl  ;
      HM.clear set   ;
      HPM.clear rho'  ;

      rho

  end

let _ =
  Selector.add_solver ("topdown", (module EqIncrSolverFromEqSolver (WP)));
