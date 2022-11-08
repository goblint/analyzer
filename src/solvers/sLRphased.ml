open Prelude
open Analyses
open Constraints
open Messages
open SLR

(** the two-phased terminating SLR3 box solver *)
module Make =
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

    let narrow = narrow S.Dom.narrow

    let solve st vs =
      let key    = HM.create 10 in
      let module H = Heap.Make (struct
          type t = S.Var.t
          let compare x y = compare (HM.find key x) (HM.find key y)
        end)
      in
      let extract_min q =
        let x = H.find_min !q in
        q := H.del_min !q; x
      in
      let min_key q =
        let x = H.find_min !q in
        HM.find key x
      in
      let wpoint = HM.create  10 in
      let infl   = HM.create  10 in
      let set    = HM.create  10 in
      let rho0   = HM.create  10 in (* widening *)
      let rho1   = HM.create  10 in (* narrowing *)
      let rho'   = HPM.create 10 in
      let q      = ref H.empty in
      let count  = ref 0 in
      let count_side  = ref (max_int - 1) in

      let rec iterate b prio =
        if H.size !q = 0 || min_key q > prio then ()
        else
          let x = extract_min q in
          if b then solve1 (HM.find key x - 1) x;
          do_var b x;
          iterate b prio
      and do_var b x =
        let rho = if b then rho1 else rho0 in
        let wpx = HM.mem wpoint x in
        HM.remove wpoint x;
        let old = HM.find rho x in
        let eval y =
          get_var_event y;
          if b then solve1 (HM.find key x - 1) y else solve0 y;
          if HM.find key x <= HM.find key y then begin
            HM.replace wpoint y ()
          end;
          HM.replace infl y (VS.add x (HM.find infl y));
          HM.find rho y
        in
        let effects = ref Set.empty in
        let side y d =
          assert (not (S.Dom.is_bot d));
          trace "sol" "SIDE: Var: %a\nVal: %a\n" S.Var.pretty_trace y S.Dom.pretty d;
          let first = not (Set.mem y !effects) in
          effects := Set.add y !effects;
          if first then (
            (* let old = try HPM.find rho' (x,y) with _ -> S.Dom.bot () in *)
            (* let d = S.Dom.join old d in *)
            HPM.replace rho' (x,y) d;
            HM.replace set y (VS.add x (try HM.find set y with Not_found -> VS.empty));
            if not (HM.mem rho y) then (
              if b then solve1 (HM.find key x - 1) ~side:true y else solve0 ~side:true y
            ) else (
              (* trace "sol" "SIDE: Var: %a already exists with Prio: %i and Val: %a\n" S.Var.pretty_trace y (HM.find key y) S.Dom.pretty d; *)
              if HM.find key y < 0 then HM.replace key y (Ref.post_decr count_side)
            );
            q := H.add y !q
          ) else (
            assert (HM.mem rho y);
            let old = HPM.find rho' (x,y) in
            let newd = S.Dom.join old d in
            HPM.replace rho' (x,y) newd;
            if not (S.Dom.equal old newd) then (
              q := H.add y !q
            )
          );
          HM.replace wpoint y ()
        in
        let tmp = eq x eval side in
        let tmp = S.Dom.join tmp (sides x) in
        (* if (b && not (S.Dom.leq old tmp)) then ( *)
        (*   trace "sol" "Var: %a\nOld: %a\nTmp: %a\n" S.Var.pretty_trace x S.Dom.pretty old S.Dom.pretty tmp; *)
        (*   assert false *)
        (* ); *)
        let val_new =
          if wpx then
            if b then
              let nar = narrow old tmp in
              trace "sol" "NARROW: Var: %a\nOld: %a\nNew: %a\nWiden: %a\n" S.Var.pretty_trace x S.Dom.pretty old S.Dom.pretty tmp S.Dom.pretty nar;
              nar
            else
              let wid = S.Dom.widen old (S.Dom.join old tmp) in
              trace "sol" "WIDEN: Var: %a\nOld: %a\nNew: %a\nWiden: %a\n" S.Var.pretty_trace x S.Dom.pretty old S.Dom.pretty tmp S.Dom.pretty wid;
              wid
          else
            tmp
        in
        if tracing then trace "sol" "Var: %a\n" S.Var.pretty_trace x ;
        if tracing then trace "sol" "Contrib:%a\n" S.Dom.pretty val_new;
        if S.Dom.equal old val_new then ()
        else begin
          update_var_event x old val_new;
          if tracing then trace "sol" "New Value:%a\n\n" S.Dom.pretty val_new;
          HM.replace rho x val_new;
          let w = try HM.find infl x with Not_found -> VS.empty in
          (* let w = if wpx then VS.add x w else w in *)
          q := Enum.fold (fun x y -> H.add y x) !q (VS.enum w);
          HM.replace infl x VS.empty
        end
      and solve0 ?(side=false) x =
        if not (HM.mem rho0 x) then (
          new_var_event x;
          let d = S.Dom.bot () in
          HM.replace rho0 x d;
          HM.replace infl x VS.empty;
          if side then (
            print_endline @@ "Variable by side-effect " ^ S.Var.var_id x ^ " to " ^ string_of_int !count_side;
            HM.replace key  x !count_side; decr count_side
          ) else (
            print_endline @@ "Variable " ^ S.Var.var_id x ^ " to " ^ string_of_int !count;
            HM.replace key  x !count; decr count
          );
          do_var false x;
          if side then
            q := H.add x !q
          else
            iterate false (HM.find key x)
        )
      and solve1 ?(side=false) prio x =
        solve0 ~side:side x;
        if not (HM.mem rho1 x) then (
          new_var_event x;
          let d = HM.find rho0 x in
          HM.replace rho1 x d;
          let w = VS.add x @@ try HM.find infl x with Not_found -> VS.empty in
          HM.replace infl x VS.empty;
          q := Enum.fold (fun x y -> H.add y x) !q (VS.enum w);
          iterate true prio
        )
      and sides x =
        let w = try HM.find set x with Not_found -> VS.empty in
        let v = Enum.fold (fun d z -> try S.Dom.join d (HPM.find rho' (z,x)) with Not_found -> d) (S.Dom.bot ()) (VS.enum w)
        in trace "sol" "SIDES: Var: %a\nVal: %a\n" S.Var.pretty_trace x S.Dom.pretty v; v
      and eq x get set =
        eval_rhs_event x;
        match S.system x with
        | None -> S.Dom.bot ()
        | Some f -> f get set
      in

      let set_start (x,d) =
        solve0 ~side:true x;
        HM.replace rho0 x d;
        HM.replace wpoint x ();
        q := H.add x !q;
        HM.replace set x (VS.add x VS.empty);
        HPM.replace rho' (x,x) d
      in

      start_event ();
      List.iter set_start st;

      List.iter (solve0) vs;
      iterate false max_int;
      List.iter (solve1 max_int) vs;
      iterate true max_int; (* TODO remove? *)
      stop_event ();

      if GobConfig.get_bool "dbg.print_wpoints" then (
        Printf.printf "\nWidening points:\n";
        HM.iter (fun k () -> ignore @@ Pretty.printf "%a\n" S.Var.pretty_trace k) wpoint;
        print_newline ();
      );

      HM.clear key   ;
      HM.clear wpoint;
      HM.clear infl  ;
      HM.clear set   ;
      HPM.clear rho' ;

      rho1
  end

let _ =
  Selector.add_solver ("slr3tp", (module EqIncrSolverFromEqSolver (Make))); (* two-phased slr3t *)
