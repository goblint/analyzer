open Prelude
open Analyses
open Constraints
open Messages
open SLR

(** the two-phased terminating SLR3 box solver *)
module Make =
  functor (S:EqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
  struct

    include Generic.SolverStats (S)
    module VS = Set.Make (S.Var)

    module P =
    struct
      type t = S.Var.t * S.Var.t
      let equal (x1,x2) (y1,y2) = S.Var.equal x1 y1 && S.Var.equal x2 y2
      let hash  (x1,x2)         = (S.Var.hash x1 - 800) * S.Var.hash x2
    end

    module HPM = Hashtbl.Make (P)

    let narrow = narrow S.Dom.narrow

    let solve box st vs =
      let key    = HM.create 10 in
      let module MyHeap (K : BatInterfaces.OrderedType) = struct
        module H = Heap.Make (K)
        module S = Set.Make (K)
        include H
        let empty = H.empty, S.empty
        let size (h,s) = H.size h
        let add e (h,s) = if S.mem e s then h, s else H.add e h, S.add e s
        let del_min (h,s) = H.del_min h, S.remove (H.find_min h) s
        let find_min (h,s) = H.find_min h
        let to_list (h,s) = H.to_list h
        let of_list xs = let s = S.of_list xs in H.of_list (S.to_list s), s
        let print (h,s) = H.print h
      end
      in
      let module H = MyHeap (struct
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
      let rho0   = HM.create  10 in (* widening *)
      let rho1   = HM.create  10 in (* narrowing *)
      let q      = ref H.empty in
      let count  = ref 0 in

      let init x =
        if not (HM.mem rho0 x) then begin
          new_var_event x;
          HM.replace rho0 x (S.Dom.bot ());
          HM.replace infl x VS.empty;
          HM.replace key x !count; decr count
        end
      in
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
        (* TODO this doesn't work yet if there are new variables in the narrowing phase which are side-effected!
        *)
        let side y d =
          assert (not (S.Dom.is_bot d));
          trace "sol" "SIDE: Var: %a\nVal: %a\n" S.Var.pretty_trace y S.Dom.pretty d;
          (* let in_rho0 = HM.mem rho0 y in
          let in_rho1 = HM.mem rho1 y in *)
          init y;
          let old0 = HM.find rho0 y in
          if not b then (
            if not (S.Dom.leq d old0) then (
              let newd = S.Dom.widen old0 (S.Dom.join old0 d) in
              HM.replace rho0 y newd;
              let w = try HM.find infl y with Not_found -> VS.empty in
              q := Enum.fold (fun x y -> let _ = Pretty.printf "add to q: %a\n" S.Var.pretty_trace y in H.add y x) !q (VS.enum w);
              HM.replace infl y VS.empty;
              (* iterate false (HM.find key y); *)
            );
          ) else (
            let old1 = try HM.find rho1 y with Not_found -> S.Dom.bot () in
            if not (S.Dom.leq d old1) then (
              let newd = S.Dom.widen old1 (S.Dom.join (S.Dom.join old0 old1) d) in
              HM.replace rho0 y newd;
              HM.replace rho1 y newd;
              (* if b && not in_rho0 then HM.replace rho0 y newd; *)
              trace "sol" "SIDE-upd: Var: %a\nFrom: %a\nOld: %a\nVal: %a\n" S.Var.pretty_trace y S.Var.pretty_trace x S.Dom.pretty old S.Dom.pretty newd;
              let w = try HM.find infl y with Not_found -> VS.empty in
              (* let w = VS.remove y w in *)
              (* trace "sol" "SIDE-q: Var: %a\nVal: %a\n" S.Var.pretty_trace y H.pretty !q; *)
              let add e h = if not (List.mem e (H.to_list h)) then H.add e h else h in
              q := Enum.fold (fun x y -> let _ = Pretty.printf "add to q: %a\n" S.Var.pretty_trace y in add y x) !q (VS.enum w); (* reason 1 for timeouts *)
              HM.replace infl y VS.empty;
              (* iterate true (HM.find key y) *)
            )
          );
          (* HM.replace wpoint y () *)
        in
        (* let side y d = HM.replace rho0 y (S.Dom.top ()) in *)
        let tmp = eq x eval side in
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
          HM.replace infl x VS.empty;
          iterate b (HM.find key x)
        end
      and solve0 x =
        if S.system x = None then init x else
        if not (HM.mem rho0 x) then (
          init x;
          print_endline @@ "Variable " ^ S.Var.var_id x ^ " ("^ string_of_int (S.Var.line_nr x) ^") to " ^ string_of_int !count;
          do_var false x;
          iterate false (HM.find key x)
        )
      and solve1 prio x =
        solve0 x;
        if not (HM.mem rho1 x) then (
          new_var_event x;
          let d = HM.find rho0 x in
          HM.replace rho1 x d;
          let w = VS.add x @@ try HM.find infl x with Not_found -> VS.empty in
          HM.replace infl x VS.empty;
          q := Enum.fold (fun x y -> H.add y x) !q (VS.enum w); (* reason 2 for timeouts *)
          iterate true prio
        )
      and eq x get set =
        eval_rhs_event x;
        match S.system x with
        | None -> S.Dom.bot ()
        | Some f -> f get set
      in

      let set_start (x,d) =
        init x;
        HM.replace rho0 x d
      in

      start_event ();
      List.iter set_start st;

      List.iter (solve0) vs;
      iterate false max_int;
      List.iter (solve1 max_int) vs;
      iterate true max_int; (* TODO remove? *)

      let reachability rho xs =
        let reachable = HM.create (HM.length rho) in
        let rec one_var x =
          if not (HM.mem reachable x) then begin
            HM.replace reachable x ();
            match S.system x with
            | None -> ()
            | Some x -> one_constaint x
          end
        and one_constaint f =
          ignore (f (fun x -> one_var x; HM.find rho x) (fun x _ -> one_var x))
        in
        List.iter one_var xs;
        HM.iter (fun x _ -> if not (HM.mem reachable x) then HM.remove rho x) rho1
      in
      reachability rho1 vs;
      stop_event ();

      HM.clear key   ;
      HM.clear wpoint;
      HM.clear infl  ;

      rho1
  end

let _ =
  let module S3tp = GlobSolverFromIneqSolver (JoinContr (Make)) in
  Selector.add_solver ("slr3tp", (module S3tp : GenericGlobSolver)); (* two-phased slr3t *)
