open Analyses
open Constraints
open Batteries
open Messages

module GU = Goblintutil

exception SolverCannotDoGlobals

(*
module Make3 =
  functor (S:IneqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  open S

  module VS  = Set.Make (Var)


  module VI =
  struct
    type t = Var.t * int
    let compare (x,n) (y,m) =
      match compare n m with
        | 0 -> Var.compare x y
        | n -> n
  end
  module VIS = Set.Make (VI)

  let hm_find_default t x a = try HM.find t x with Not_found -> a

  module P =
  struct
    type t = S.Var.t * int * S.Var.t
    let equal (x1,(x2:int),x3) (y1,y2,y3) = S.Var.equal x1 y1 && x2=y2 && S.Var.equal x3 y3
    let hash (x1,x2,x3) = (S.Var.hash x1 - 800) * (x2+1) * S.Var.hash x3
  end

  module HPM = Hashtbl.Make (P)

  let hpm_find_default h x d = try HPM.find h x with Not_found -> d

  (** Helper module for values of global contributions. *)
  module XY =
  struct
    let xy = HPM.create 1024

    let get_value x = hpm_find_default xy x (S.Dom.bot ())
    let set_value = HPM.replace xy
  end

  let back   = HPM.create 113  (* debug *)
  let infl   = HM.create 113
  let count = ref 0
  let seen  = ref 0

  let solve : (v -> d -> d -> d) -> (v*d) list -> v list -> d HM.t = fun box sv iv ->
    let dep    = HM.create 113 in
    let set    = HM.create 113 in
    let sigma  = HM.create 113 in
    let called = HM.create 113 in
    let stable = HM.create 113 in
    let reachability xs =
      let reachable = HM.create (HM.length sigma) in
      let rec one_var x =
        if not (HM.mem reachable x) then begin
          HM.replace reachable x ();
          List.iter one_constaint (system x)
        end
      and one_constaint f =
        ignore (f (fun x -> one_var x; hm_find_default sigma x (Dom.bot ())) (fun x _ -> one_var x))
      in
      List.iter one_var xs;
      HM.iter (fun x _ -> if not (HM.mem reachable x) then HM.remove sigma x) sigma
    in
    let f x old side get set =
      let join_apply (d_in, d_back, rhsn) rhs =
        let gets = ref VS.empty in
        let vars = ref VS.empty in
        let get' y =
          vars := VS.add y !vars;
          let r = get y in
          if HM.mem called y then begin
            gets := VS.add y (VS.union !gets (hm_find_default dep y VS.empty))
          end else
            gets := VS.union !gets (hm_find_default dep y VS.empty);
          r
        in
        let d = rhs get' (set rhsn) in
        (* VS.iter (fun d -> if tracing then tracel "sol" "Gets %a: %a\n" Var.pretty_trace x Var.pretty_trace d) !gets  ; *)
        HM.replace dep x (VS.remove x (VS.union !gets (hm_find_default dep x VS.empty)));
        if VS.mem x !gets then begin
          VS.iter (fun y ->
              if tracing && not (HPM.mem back (x,rhsn,y)) then tracel "sol" "Back edge found: %a --- %a\n" Var.pretty_trace x Var.pretty_trace y;
              HPM.replace back (x,rhsn,y) ()) !vars;
          (d_in, Dom.join d_back d,rhsn+1)
        end else  begin
          (Dom.join d_in d, d_back,rhsn+1)
        end
      in
      let d_in, d_back,_ = List.fold_left join_apply (Dom.bot (), side, 0) (system x) in
      if Dom.is_bot d_back then d_in else  begin
        HM.replace infl x (x :: hm_find_default infl x []);
        Dom.join d_in (box x old d_back)
      end
    in
    let rec destabilize x =
      let t = hm_find_default infl x [] in
        HM.replace infl x [];
        List.iter (fun y -> HM.remove stable y; destabilize y) t
    in
    let rec solve (x : Var.t) =
      incr count;
      if !count mod 10 = 0 && !seen < !count then begin
        Printf.printf "(%d)%!" !count;
        seen := !count
      end else if !count mod 10 = 0 && !seen > !count then begin
        Printf.printf "(%d)%!" !count;
        seen := !count
      end;
      if not (HM.mem stable x || HM.mem called x) then begin
        if not (HM.mem sigma x) then begin
          (HM.add sigma x (Dom.bot ()); HM.add infl x []);
        end;
        HM.replace called x ();
        let rec loop () =
          HM.replace stable x ();
          let old    = hm_find_default sigma x (Dom.bot ()) in
          let newval = f x old (do_side x) (eval x) (side x) in
          if not (Dom.equal old newval) then begin
            HM.replace sigma x newval;
            destabilize x
          end ;
          if not (HM.mem stable x) then loop ()
        in loop ();
        HM.remove called x
      end;
      decr count

    and eval x y =
      solve y;
      HM.replace infl y (x :: hm_find_default infl y []);
      HM.find sigma y

    and do_side y =
      let p = hm_find_default set y VIS.empty in
      VIS.fold (fun (x,n) a -> Dom.join a (XY.get_value (x,n,y))) p (Dom.bot ())

    and side x n y d =
      let _ =
        HM.replace set y (VIS.add (x,n) (hm_find_default set y VIS.empty))
      in

      let old = XY.get_value (x,n,y) in
      let tmp = box x old d in
      HM.replace infl x (x :: hm_find_default infl x []);

      if not (Dom.equal tmp old) then begin
        if tracing then
          tracel "sol" "Side-effect: %a to %a old:\n%a\ntmp:\n%a\n" Var.pretty_trace x Var.pretty_trace y Dom.pretty old Dom.pretty tmp;

        let _ = XY.set_value (x,n,y) tmp in
        destabilize y;
        HM.remove stable y;
        solve y
      end
    in
    let add_start (v,d) =
        HM.replace set v (VIS.add (v,0) (hm_find_default set v VIS.empty));
        XY.set_value (v,0,v) d
      in
      List.iter add_start sv;
      if tracing then trace "sol" "Start!\n";
      let rec loop () =
        List.iter solve iv;
        if not (List.for_all (HM.mem stable) iv) then loop ()
      in loop ();
      if tracing then trace "sol" "Reachability...\n";
      reachability iv;
      if tracing then trace "sol" "Done.\n";
      sigma
end

(*module PrintInfluence2 =
  functor (S:IneqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  module S1 = Make3 (S) (HM)
  let solve box x y =
    let ch = Legacy.open_out "test.dot" in
    let r = S1.solve box x y in
    let f k _ =
      let s = Pretty.sprint 80 (S.Var.pretty_trace () k) in
      ignore (Pretty.fprintf ch "%d [label=\"%s\"];\n" (S.Var.hash k) (Goblintutil.escape s));
      let f y =
        if S1.HPM.mem S1.back (y,k) then
          ignore (Pretty.fprintf ch "%d -> %d [arrowhead=box style=dashed];\n" (S.Var.hash k) (S.Var.hash y))
        else
          ignore (Pretty.fprintf ch "%d -> %d ;\n" (S.Var.hash k) (S.Var.hash y))
      in
      List.iter f (try HM.find S1.infl k with Not_found -> [])
    in
    ignore (Pretty.fprintf ch "digraph G {\nedge [arrowhead=vee];\n");
    HM.iter f r;
    ignore (Pretty.fprintf ch "}\n");
    Legacy.close_out_noerr ch;
    r
end
*)
*)

module TD2 =
  functor (S:IneqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
struct
  open S

  module VS  = Set.Make (Var)


  module VI =
  struct
    type t = Var.t * int
    let compare (x,n) (y,m) =
      match compare n m with
        | 0 -> Var.compare x y
        | n -> n
  end
  module VIS = Set.Make (VI)

  let hm_find_default t x a = try HM.find t x with Not_found -> a

  module P =
  struct
    type t = S.Var.t * int * S.Var.t
    let equal (x1,(x2:int),x3) (y1,y2,y3) = S.Var.equal x1 y1 && x2=y2 && S.Var.equal x3 y3
    let hash (x1,x2,x3) = (S.Var.hash x1 - 800) * (x2+1) * S.Var.hash x3
  end

  module HPM = Hashtbl.Make (P)

  let hpm_find_default h x d = try HPM.find h x with Not_found -> d

  (** Helper module for values of global contributions. *)
  module XY =
  struct
    let xy = HPM.create 1024

    let get_value x = hpm_find_default xy x (S.Dom.bot ())
    let set_value = HPM.replace xy
  end

  let back   = HPM.create 113  (* debug *)
  let infl   = HM.create 113
  let count = ref 0
  let seen  = ref 0

  exception Backtrack of VS.t

  let solve : (v -> d -> d -> d) -> (v*d) list -> v list -> d HM.t = fun box sv iv ->
    let dep    = HM.create 113 in
    let set    = HM.create 113 in
    let sigma  = HM.create 113 in
    let called = HM.create 113 in
    let stable = HM.create 113 in
    let reachability xs =
      let reachable = HM.create (HM.length sigma) in
      let rec one_var x =
        if not (HM.mem reachable x) then begin
          HM.replace reachable x ();
          List.iter one_constaint (system x)
        end
      and one_constaint f =
        ignore (f (fun x -> one_var x; hm_find_default sigma x (Dom.bot ())) (fun x _ -> one_var x))
      in
      List.iter one_var xs;
      HM.iter (fun x _ -> if not (HM.mem reachable x) then HM.remove sigma x) sigma
    in
    let f x old side get set =
      let join_apply (d_in, d_back, rhsn) rhs =
        let gets = ref VS.empty in
        let vars = ref VS.empty in
        let get' y =
          vars := VS.add y !vars;
          let r = get y in
          if HM.mem called y then begin
            gets := VS.add y (VS.union !gets (hm_find_default dep y VS.empty))
          end else
            gets := VS.union !gets (hm_find_default dep y VS.empty);
          r
        in
        let d = rhs get' (set rhsn) in
        HM.replace dep x (VS.remove x (VS.union !gets (hm_find_default dep x VS.empty)));
        if VS.mem x !gets then begin
          VS.iter (fun y ->
              HPM.replace back (x,rhsn,y) ()) !vars;
          (d_in, Dom.join d_back d,rhsn+1)
        end else  begin
          (Dom.join d_in d, d_back,rhsn+1)
        end
      in
      let d_in, d_back,_ = List.fold_left join_apply (Dom.bot (), side, 0) (system x) in
      if Dom.is_bot d_back then d_in else  begin
        HM.replace infl x (VS.add x (hm_find_default infl x VS.empty));
        Dom.join d_in (box x old d_back)
      end
    in
    let rec destabilize x =
      let t = hm_find_default infl x VS.empty in
        HM.replace infl x VS.empty;
        VS.iter (fun y -> HM.remove stable y; if not (HM.mem called y) then destabilize y) t
    in
    (* let rec destabilize' x xs =
      let t = hm_find_default infl x VS.empty in
        HM.replace infl x VS.empty;
        let f y xs =
          HM.remove stable y;
          if HM.mem called y then begin
            VS.add y xs
          end else
            destabilize' y xs
        in
        VS.fold f t xs
    in *)
    let rec solve (x : Var.t) =
      if not (HM.mem stable x || HM.mem called x) then begin
        if not (HM.mem sigma x) then begin
          (HM.add sigma x (Dom.bot ()); HM.add infl x VS.empty);
        end;
        HM.replace called x ();
        let rec loop () =
            HM.replace stable x ();
            let old    = hm_find_default sigma x (Dom.bot ()) in
            let newval = f x old (do_side x) (eval x) (side x) in
            if not (Dom.equal old newval) then begin
              HM.replace sigma x newval;
              destabilize x
            end ;
            if not (HM.mem stable x) then loop ()
        in
        let rec loop2 () =
          begin try begin
            loop ()
          end with Backtrack xs ->begin
            if VS.mem x xs then begin
              let xs' = VS.remove x xs in
              if VS.is_empty xs' then begin
                loop2 ()
              end else begin
                destabilize x;
                HM.remove stable x;
                HM.remove called x;
                raise (Backtrack xs')
              end
            end else begin
              destabilize x;
              HM.remove stable x;
              HM.remove called x;
              raise (Backtrack xs)
            end
          end
          end
        in loop2 ();
        HM.remove called x
      end


    and eval x y =
      solve y;
      HM.replace infl y (VS.add x (hm_find_default infl y VS.empty));
      HM.find sigma y

    and do_side y =
      let p = hm_find_default set y VIS.empty in
      VIS.fold (fun (x,n) a -> Dom.join a (XY.get_value (x,n,y))) p (Dom.bot ())

    and side x n y d =
      let _ =
        HM.replace set y (VIS.add (x,n) (hm_find_default set y VIS.empty))
      in

      let old = XY.get_value (x,n,y) in
      let tmp = box x old d in
      HM.replace infl x (VS.add x (hm_find_default infl x VS.empty));

      if not (Dom.equal tmp old) then begin
        let _ = XY.set_value (x,n,y) tmp in
        destabilize y;
        HM.remove stable y;
        solve y
        (*let qs = destabilize' y VS.empty in
        HM.remove stable y;
        solve y;
        if not (VS.is_empty qs) then begin
          raise (Backtrack qs)
        end*)
      end
    in
    let add_start (v,d) =
        HM.replace set v (VIS.add (v,0) (hm_find_default set v VIS.empty));
        XY.set_value (v,0,v) d
      in
      List.iter add_start sv;
      if tracing then trace "sol" "Start!\n";
      let rec loop () =
        List.iter solve iv;
        if not (List.for_all (HM.mem stable) iv) then loop ()
      in loop ();
      if tracing then trace "sol" "Reachability...\n";
      reachability iv;
      if tracing then trace "sol" "Done.\n";
      sigma
end



module Make2GGS : Analyses.GenericGlobSolver = GlobSolverFromIneqSolver (TD2)
let _ =
  Selector.add_solver ("TD", (module Make2GGS : Analyses.GenericGlobSolver))