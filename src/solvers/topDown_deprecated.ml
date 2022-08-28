open Prelude
open Analyses
open Constraints
open Messages

module GU = Goblintutil

exception SolverCannotDoGlobals


(** modified SLR3 as top down solver *)
module TD3 =
  functor (S:EqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v) ->
  struct

    include Generic.SolverStats (S) (HM)
    module VS = Set.Make (S.Var)

    module P =
    struct
      type t = S.Var.t * S.Var.t [@@deriving eq, hash]
    end

    module HPM = Hashtbl.Make (P)

    let solve box st vs =
      let wpoint = HM.create  10 in
      let stable = HM.create  10 in
      let infl   = HM.create  10 in (* y -> xs *)
      let set    = HM.create  10 in (* y -> xs *)
      let sidevs = HM.create  10 in
      let called = HM.create  10 in
      let rho    = HM.create  10 in
      let rho'   = HPM.create 10 in (* x,y -> d *)

      let add_infl y x = HM.replace infl y (VS.add x (try HM.find infl y with Not_found -> VS.empty)) in
      let add_set x y d = HM.replace set y (VS.add x (try HM.find set y with Not_found -> VS.empty)); HPM.add rho' (x,y) d; HM.add sidevs y () in
      let is_side x = HM.mem set x in
      let make_wpoint x =
        if tracing then trace "sol2" "make_wpoint %a\n" S.Var.pp_trace x;
        HM.replace wpoint x ()
      in
      let rec destabilize x =
        if tracing then trace "sol2" "destabilize %a\n" S.Var.pp_trace x;
        let t = HM.find_default infl x VS.empty in
        HM.replace infl x VS.empty;
        VS.iter (fun y -> HM.remove stable y; if not (HM.mem called y) then destabilize y) t
      and solve x =
        if tracing then trace "sol2" "solve %a, called: %b, stable: %b\n" S.Var.pp_trace x (HM.mem called x) (HM.mem stable x);
        if not (HM.mem called x || HM.mem stable x) then begin
          HM.replace called x ();
          let wpx = HM.mem wpoint x in
          HM.remove wpoint x;
          HM.replace stable x ();
          let old = HM.find rho x in
          let tmp = eq x (eval x) (side x) in
          let tmp = S.Dom.join tmp (sides x) in
          if tracing then trace "sol" "Var: %a\n" S.Var.pp_trace x ;
          if tracing then trace "sol" "Contrib:%a\n" S.Dom.pp tmp;
          let tmp = if is_side x then S.Dom.widen old (S.Dom.join old tmp) else if wpx then box x old tmp else tmp in
          HM.remove called x;
          if not (S.Dom.equal old tmp) then begin
            update_var_event x old tmp;
            if tracing then trace "sol" "New Value:%a\n\n" S.Dom.pp tmp;
            if tracing then trace "sol2" "new value for %a (wpx: %b, is_side: %b) is %a. Old value was %a\n" S.Var.pp_trace x wpx (is_side x) S.Dom.pp tmp S.Dom.pp old;
            HM.replace rho x tmp;
            destabilize x;
            (solve[@tailcall]) x;
          end;
        end;
      and eq x get set =
        if tracing then trace "sol2" "eq %a\n" S.Var.pp_trace x;
        eval_rhs_event x;
        match S.system x with
        | None -> S.Dom.bot ()
        | Some f ->
          let effects = ref Set.empty in
          let sidef y d =
            if not (Set.mem y !effects) then (
              HPM.replace rho' (x,y) (S.Dom.bot ());
              effects := Set.add y !effects
            );
            set y d
          in
          f get sidef
      and eval x y =
        if tracing then trace "sol2" "eval %a ## %a\n" S.Var.pp_trace x S.Var.pp_trace y;
        get_var_event y;
        if not (HM.mem rho y) then init y;
        if HM.mem called y then make_wpoint y else if neg is_side y then solve y;
        add_infl y x;
        HM.find rho y
      and sides x =
        let w = try HM.find set x with Not_found -> VS.empty in
        let d = Enum.fold (fun d z -> try S.Dom.join d (HPM.find rho' (z,x)) with Not_found -> d) (S.Dom.bot ()) (VS.enum w) in
        if tracing then trace "sol2" "sides %a ## %a\n" S.Var.pp_trace x S.Dom.pp d;
        d
      and side x y d =
        if S.Dom.is_bot d then () else
        if tracing then trace "sol2" "side %a ## %a (wpx: %b) ## %a\n" S.Var.pp_trace x S.Var.pp_trace y (HM.mem wpoint y) S.Dom.pp d;
        if not (HM.mem rho y) then begin
          init y;
          add_set x y d;
          solve y
        end else begin
          let old = HPM.find rho' (x,y) in
          if not (S.Dom.equal old d) then begin
            add_set x y (S.Dom.join old d);
            (*make_wpoint y;*)
            (*destabilize y;*)
            HM.remove stable y;
            HM.replace sidevs y ();
            (*solve y;*)
          end
        end
      and init x =
        if tracing then trace "sol2" "init %a\n" S.Var.pp_trace x;
        if not (HM.mem rho x) then begin
          new_var_event x;
          HM.replace rho  x (S.Dom.bot ());
          HM.replace infl x (VS.add x VS.empty)
        end
      in

      let set_start (x,d) =
        if tracing then trace "sol2" "set_start %a ## %a\n" S.Var.pp_trace x S.Dom.pp d;
        init x;
        add_set x x d;
        solve x
      in

      start_event ();
      List.iter set_start st;
      List.iter init vs;
      List.iter solve vs;
      let get_gs () =
        let vs = ref [] in
        HM.iter (fun k _ -> vs := k :: !vs) sidevs;
        HM.clear sidevs;
        !vs
      in
      (* iterate until there are no more new side-effects *)
      let rec solveg () =
        let gs = get_gs () in
        if gs <> [] then (
          List.iter solve gs;
          List.iter solve vs;
          solveg ()
        )
      in
      solveg ();
      stop_event ();

      HM.clear wpoint;
      HM.clear stable;
      HM.clear infl  ;
      HM.clear set   ;
      HPM.clear rho'  ;

      rho

  end

let _ =
  Selector.add_solver ("topdown_deprecated", (module EqIncrSolverFromEqSolver (TD3)));
