open Batteries
open Analyses
open Constraints
open Messages
open SLR

(** the terminating SLR3 box solver *)
module SLR3term =
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
      let rebuild = H.of_list % H.to_list in
      let wpoint = HM.create  10 in
      let infl   = HM.create  10 in
      let set    = HM.create  10 in
      let rho    = HM.create  10 in
      let rho'   = HPM.create 10 in
      let q      = ref H.empty in
      let count  = ref 0 in
      let count_side  = ref (max_int - 1) in

      let () = print_solver_stats := fun () ->
        Printf.printf "wpoint: %d, rho: %d, rho': %d, q: %d, count: %d, count_side: %d\n" (HM.length wpoint) (HM.length rho) (HPM.length rho') (H.size !q) (Int.neg !count) (max_int - !count_side);
        let histo = Hashtbl.create 13 in (* histogram: node id -> number of contexts *)
        HM.iter (fun k _ -> Hashtbl.modify_def 1 (S.Var.var_id k) ((+)1) histo) rho;
        let vid,n = Hashtbl.fold (fun k v (k',v') -> if v > v' then k,v else k',v') histo (Obj.magic (), 0) in
        ignore @@ Pretty.printf "max #contexts: %d for var_id %s\n" n vid
      in

      let init ?(side=false) x =
        if not (HM.mem rho x) then begin
          new_var_event x;
          HM.replace rho  x (S.Dom.bot ());
          HM.replace infl x (VS.add x VS.empty);
          let c = if side then count_side else count in
          trace "sol" "INIT: Var: %a with prio %d\n" S.Var.pretty_trace x !c;
          HM.replace key x !c; decr c
        end
      in
      let sides x =
        let w = try HM.find set x with Not_found -> VS.empty in
        let v = Enum.fold (fun d z -> try S.Dom.join d (HPM.find rho' (z,x)) with Not_found -> d) (S.Dom.bot ()) (VS.enum w) in
        trace "sol" "SIDES: Var: %a\nVal: %a\n" S.Var.pretty_trace x S.Dom.pretty v; v
      in
      let rec iterate b_old prio =
        if H.size !q = 0 || min_key q > prio then ()
        else
          let n = min_key q in
          let x = extract_min q in
          let b_new = do_var b_old x in
          if b_old <> b_new && n < prio then (
            iterate b_new n;
            iterate b_old prio
          ) else
            iterate b_new prio
      and solve ?(side=false) x =
        if not (HM.mem rho x) then begin
          init ~side:side x;
          let b_new = do_var false x in
          iterate b_new (HM.find key x)
        end
      and do_var b_old x =
        let wpx = HM.mem wpoint x in
        (* let wpx = true in *)
        (* HM.remove wpoint x; *)
        let old = HM.find rho x in
        let eval y =
          get_var_event y;
          solve y;
          if HM.find key x <= HM.find key y then begin
            HM.replace wpoint y ()
          end;
          HM.replace infl y (VS.add x (try HM.find infl y with Not_found -> VS.empty));
          HM.find rho y
        in
        let effects = ref Set.empty in
        let side y d =
          assert (not (S.Dom.is_bot d));
          (*
          init ~side:true y;
          HM.replace set y (VS.add x (try HM.find set y with Not_found -> VS.empty));
          let old = try HPM.find rho' (x,y) with _ -> S.Dom.bot () in
          let newd = S.Dom.join old d in
          HPM.replace rho' (x,y) newd;
          if not (HM.mem rho y) then
            solve ~side:true y
          else
          if not (S.Dom.equal old newd) then (
            HM.replace wpoint y ();
            q := H.add y !q
          )
          *)
          (* if S.Dom.is_bot d then print_endline "BOT" else *)
          trace "sol" "SIDE: Var: %a\nVal: %a\n" S.Var.pretty_trace y S.Dom.pretty d;
          let first = not (Set.mem y !effects) in
          effects := Set.add y !effects;
          if first then (
            HPM.replace rho' (x,y) d;
            HM.replace set y (VS.add x (try HM.find set y with Not_found -> VS.empty));
            if not (HM.mem rho y) then (
              init ~side:true y;
              ignore @@ do_var false y
              (* solve ~side:true y *)
            ) else (
              (* trace "sol" "SIDE: Var: %a already exists with Prio: %i and Val: %a\n" S.Var.pretty_trace y (HM.find key y) S.Dom.pretty d; *)
              if HM.find key y < 0 then (
                HM.replace key y (Ref.post_decr count_side);
                q := rebuild !q
              )
            );
            q := H.add y !q
          ) else (
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
        let val_new, b_new =
          if wpx then
            if S.Dom.leq tmp old then (
              let nar = narrow old tmp in
              trace "sol" "NARROW1: Var: %a\nOld: %a\nNew: %a\nNarrow: %a" S.Var.pretty_trace x S.Dom.pretty old S.Dom.pretty tmp S.Dom.pretty nar;
              nar, true
            ) else
            if b_old then (
              let nar = narrow old tmp in
              trace "sol" "NARROW2: Var: %a\nOld: %a\nNew: %a\nNarrow: %a" S.Var.pretty_trace x S.Dom.pretty old S.Dom.pretty tmp S.Dom.pretty nar;
              nar, true
            )
            else (
              let wid = S.Dom.widen old (S.Dom.join old tmp) in
              trace "sol" "WIDEN: Var: %a\nOld: %a\nNew: %a\nWiden: %a" S.Var.pretty_trace x S.Dom.pretty old S.Dom.pretty tmp S.Dom.pretty wid;
              wid, false
            )
          else
            tmp, b_old
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
        end;
        b_new
      and eq x get set =
        eval_rhs_event x;
        match S.system x with
        | None -> S.Dom.bot ()
        | Some f -> f get set
      in

      let set_start (x,d) =
        init ~side:true x;
        HM.replace rho x d;
        HM.replace wpoint x ();
        q := H.add x !q;
        HM.replace set x (VS.add x VS.empty);
        HPM.replace rho' (x,x) d
      in

      start_event ();
      List.iter set_start st;

      List.iter solve vs;
      iterate false max_int;
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

      rho
  end

let _ =
  Selector.add_solver ("slr3t", (module EqIncrSolverFromEqSolver (SLR3term))); (* same as S2 but number of W-points may also shrink + terminating? *)
