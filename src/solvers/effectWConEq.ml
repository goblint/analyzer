open Prelude
open Analyses
open Constraints

module Make =
  functor (S:EqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v) ->
  struct
    include Generic.SolverStats (S) (HM)
    module VS = BatSet.Make (S.Var)

    let solve st vs =
      let infl    = HM.create  10 in
      let stable  = HM.create  10 in
      let rho     = HM.create  10 in

      (*     let cnt = ref 0 in
             let var_stats () =
             let reachable = HM.create (HM.length rho) in
             let rec one_var x =
             if not (HM.mem reachable x) then begin
               HM.replace reachable x ();
               match S.system x with
                 | None -> ()
                 | Some x -> one_constaint x
             end
             and one_constaint f =
             ignore (f (fun x -> one_var x; try HM.find rho x with Not_found -> S.Dom.bot ()) (fun x _ -> one_var x))
             in
             List.iter one_var vs;
             let dead = ref 0 in
             HM.iter (fun x _ -> if not (HM.mem rho x) then incr dead) reachable;
             Printf.printf "\n\n\tDEAD VARIABLES: %d/%d\n\n\n" !dead (HM.length rho)
             in *)

      let rec solve x =
        (* incr cnt; *)
        (* if !cnt mod 1000 = 0 then var_stats (); *)
        if not (HM.mem rho x) then begin
          HM.add rho x (S.Dom.bot ());
          HM.replace infl x VS.empty
        end;
        HM.replace stable x ();
        set x (eq x (eval x) set)

      and eq x get set =
        eval_rhs_event x;
        match S.system x with
        | None  -> S.Dom.bot ()
        | Some f -> f get set

      and eval x y =
        get_var_event y;
        if not (HM.mem stable y) then solve y;
        HM.replace infl y (VS.add x (try HM.find infl y with Not_found -> VS.empty));
        HM.find rho y

      and set x d =
        if not (HM.mem rho x) then solve x;
        let old = HM.find rho x in
        let tmp = S.Dom.join old d in
        update_var_event x old tmp;
        if not (S.Dom.equal old tmp) then begin
          HM.replace rho x tmp;
          let w = try HM.find infl x with Not_found -> VS.empty in
          HM.replace infl x VS.empty;
          BatEnum.iter (HM.remove stable) (VS.enum w);
          BatEnum.iter solve (VS.enum w)
        end
      in

      let set_start (x,d) =
        HM.replace rho x d;
      in

      start_event ();
      List.iter set_start st;
      List.iter solve vs;
      stop_event ();

      HM.clear stable;
      HM.clear infl  ;

      rho

  end

let _ =
  Selector.add_solver ("effectWConEq", (module EqIncrSolverFromEqSolver (Make)));
