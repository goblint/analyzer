(** Forward-propagating solver *)
open Prelude
open Analyses_arinc
open Constraints_arinc
open Messages

module WP =
  functor (S:EqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
  struct
    include Generic_arinc.SolverStats (S) (HM)
    module VS = Set.Make (S.Var)

    type solver_data = {
      mutable rho: S.Dom.t HM.t;
      mutable wpoint: unit HM.t;
      mutable stable: unit HM.t
    }

    let create_empty_data () = {
      rho = HM.create 10;
      wpoint = HM.create 10;
      stable = HM.create 10
    }

    let clear_data data =
      HM.clear data.stable

    let print_data data str =
      print_endline (str ^
                     "|rho|="^string_of_int (HM.length data.rho) ^ "\n" ^
                     "|stable|="^string_of_int (HM.length data.stable) ^ "\n" ^
                     "|wpoint|="^string_of_int (HM.length data.wpoint)
                    )

    let exists_key f hm = HM.fold (fun k _ a -> a || f k) hm false

    module P =
    struct
      type t = S.Var.t * S.Var.t
      let equal (x1,x2) (y1,y2) = S.Var.equal x1 y1 && S.Var.equal x2 y2
      let hash  (x1,x2)         = (S.Var.hash x1 * 13) + S.Var.hash x2
    end

    module HPM = Hashtbl.Make (P)

    type phase = Widen | Narrow

    let solve box st vs data =
      let called = HM.create 10 in
      let rho = data.rho in
      let wpoint = data.wpoint in
      let stable = data.stable in
      let sigma2 = HPM.create 10 in

      (* sigma2 contains the contributions of x to y *)

      (* should be a list of ys and the contributions that x has to them *)
      let prop x d =
        (* d == HM.find rho x, but we are not using it in this implementation *)
        let contribs = List.map (fun (x,y) -> (x, y (HM.find rho) (fun _ _ -> ()))) (S.outgoing x) in (* TODO: This is a bit odd, set seems wrong *)
        List.filter (fun (y,d) -> not @@ S.Dom.is_bot d) contribs
      in
      (* this a list of ys and the contributions that x has to them after
         widening / narrowing *)
      let propagate x d =
        let w = prop x d in
        let doit (y,d) =
          let tmp =
            if HM.mem wpoint y then
              (* apply warrowing to the old (x,y) and the new (x,y) *)
              box y (HPM.find sigma2 (x,y)) d
            else
              d
          in
          (* store new (x,y) in second hashmap *)
          HPM.replace sigma2 (x,y) tmp;
          (y, tmp)
        in List.map doit w
      in
      let rec h_solve x:unit =
        M.trace "hsolve" "h_solve for %a\n\n" S.Var.pretty_trace x;
        if HM.mem called x then
          (* if it already called make it a wpoint *)
          HM.replace wpoint x ()
        else if HM.mem stable x then
          (* if it is stable, we're done *)
          ()
        else
          (* we declare it to be stable *)
          HM.replace stable x ();
          (* add to called *)
          HM.replace called x ();
          (* Get list of (y,d) that x contributes to *)
          let work = propagate x (HM.find rho x) in
          let f (y, d) =
            let is_done = try S.Dom.leq d (HM.find rho y) with Not_found -> false in
            if not is_done then
              (* How can rho y ever become smaller here? *)
              HM.replace rho y (S.Dom.join (try HM.find rho y with Not_found -> S.Dom.bot ()) d);
              (* y is no longer stable, we just updated it *)
              HM.remove stable y;
              (* we now need to solve y *)
              h_solve y
          in
          (* ierate over all contributions to (y,d) s *)
          List.iter f work;
          (* x is no longer called *)
          HM.remove called x;
          (* solve x again *)
          h_solve x
      in
      let init x =
        if tracing then trace "sol2" "init %a\n" S.Var.pretty_trace x;
        if not (HM.mem rho x) then (
          new_var_event x;
          HM.replace rho x (S.Dom.bot ())
        )
      in

      let set_start (x,d) =
        if tracing then trace "sol2" "set_start %a ## %a\n" S.Var.pretty_trace x S.Dom.pretty d;
        init x;
        HM.replace rho x d;
        HM.replace stable x ();
      in

      start_event ();

      List.iter set_start st;
      List.iter init vs;
      List.iter (fun x -> h_solve x) vs;
      {rho; wpoint; stable}

    let solve box st vs =
      let data = create_empty_data () in
      let result = solve box st vs data in
      clear_data result;
      result.rho
  end

let _ =
  let module WP = GlobSolverFromIneqSolver (SLR_arinc.JoinContr (WP)) in
  Selector_arinc.add_solver ("prop", (module WP : GenericGlobSolver));
