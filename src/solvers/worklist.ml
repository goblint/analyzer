open Prelude
open Analyses
open Constraints
open Messages

module Make =
  functor (S:EqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
  struct

    include Generic.SolverStats (S) (HM)
    module VS = Set.Make (S.Var)

    open S.Dom

    let eq x get set =
      match S.system x with
      | None -> bot ()
      | Some f ->
        eval_rhs_event x;
        f get set

    let solve _ st vs =
      let infl = HM.create 10 in
      let rho  = HM.create 10 in
      let vs   = ref (VS.of_enum (List.enum vs)) in
      let init x =
        new_var_event x;
        HM.replace rho x (bot ());
        HM.replace infl x VS.empty;
      in
      let eval x y =
        get_var_event y;
        HM.replace infl y (VS.add x (try HM.find infl y with Not_found -> VS.empty));
        try HM.find rho y
        with Not_found ->
          new_var_event y;
          HM.replace rho y (bot ());
          vs := VS.add y !vs;
          bot ()
      in
      let set x d =
        let old = try HM.find rho x with Not_found -> init x; bot () in
        if not (leq d old) then begin
          update_var_event x old d;
          HM.replace rho x (join old d);
          let q = try HM.find infl x with Not_found -> VS.empty in
          HM.replace infl x VS.empty;
          vs := (VS.fold VS.add q !vs)
        end
      in
      start_event ();
      let _ = List.iter (fun (x,d) -> HM.add rho x d) st in
      while not (VS.is_empty !vs) do
        let x, vs' = VS.pop !vs in
        let _ = vs := vs' in
        set x (eq x (eval x) set)
      done;
      stop_event ();
      rho
  end


let _ =
  let module S = GlobSolverFromIneqSolver (SLR.JoinContr (Make)) in
  Selector.add_solver ("WL",  (module S : GenericGlobSolver));
