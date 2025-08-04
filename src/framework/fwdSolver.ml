open Goblint_constraint.ConstrSys

module FwdSolver (System: FwdGlobConstrSys) = struct

  module D = System.D
  module G = System.G

  module LS = Set.Make (System.LVar)

  module GM = Hashtbl.Make(System.GVar)
  module LM = Hashtbl.Make(System.LVar)

  let gwarrow a b = if G.leq b a then G.narrow a b else G.widen a b
  let lwarrow a b = if D.leq b a then D.narrow a b else D.widen a b

  let work = ref (([] : System.LVar.t list), LS.empty)

  let add_work x = let (l,s) = !work in
    if LS.mem x s then ()
    else work := (x::l, LS.add x s)

  let rem_work () = let (l,s) = !work in
    match l with
    | [] -> None
    | x::xs ->
      let s = LS.remove x s in
      let _ = work := (xs,s) in
      Some x

  type glob = {value : G.t; init : G.t;  infl : System.LVar.t list; from : G.t LM.t}
       (*
          one might additionally maintain in table from some widening delay?
       *)

  let glob: glob GM.t = GM.create 100

  (* auxiliary functions for globals *)

  let get_global_ref g =
    try GM.find glob g
    with _ ->
      (let rglob = {value = G.bot (); init = G.bot (); infl = []; from = LM.create 10} in
       GM.add glob g rglob;
       rglob
      )

  let init_global (g, d) =
    GM.add glob g {
      value = d;
      init = d;
      infl = [];
      from = LM.create 10
    }

  let get_global_value init from = LM.fold (fun _ -> G.join) from init

  let get_old_global_value x from =
    try LM.find from x
    with _ ->
      LM.add from x (G.bot ());
      (G. bot ())

  (* now the getters and setters for globals, setters with warrowing per origin *)

  let get_global x g =
    let rglob = get_global_ref g in
    GM.replace glob g { rglob with infl = x::rglob.infl }; (* ensure the global is in the hashtable *)
    rglob.value

  let set_global x g d =
    (*
      replaces old contribution with the new one;
      reconstructs value of g from contributions;
      propagates infl and updates value - if value has changed
    *)
    let {value;init;infl;from} = get_global_ref g in
    let old_value = get_old_global_value x from in
    let new_value = gwarrow old_value d in
    let _ = LM.replace from x new_value in
    let new_g = get_global_value init from in
    if G.equal value new_g then
      ()
    else
      let _ = List.iter add_work infl in
      GM.replace glob g {value = new_g; init = init; infl = []; from}

  type loc = {loc_value : D.t; loc_init : D.t; loc_infl: System.LVar.t list; loc_from : D.t LM.t}
  (*
    init may contain some initial value not provided by separate origin;
    perhaps, dynamic tracking of dependencies required for certain locals?

    One might additionally maintain in table loc_from some widening delay?
  *)

  let loc: loc LM.t = LM.create 100

  (* auxiliary functions for locals *)


  let get_local_ref x =
    try LM.find loc x
    with _ ->
      (let rloc = {loc_value = D.bot (); loc_init = D.bot (); loc_infl = []; loc_from = LM.create 10} in
       LM.add loc x rloc;
       rloc)

  let init_local (x, d) =
    LM.add loc x {
      loc_value = d;
      loc_init = d;
      loc_infl = [];
      loc_from = LM.create 10
    }

  let get_local_value init from = LM.fold (fun _ a rb -> D.join a rb) from init

  let get_old_local_value x from =
    try LM.find from x
    with _ ->
      LM.add from x (D.bot ());
      (D.bot ())

  (* now the getters and setters for locals, setters with warrowing per origin *)

  let get_local x y =
    let rloc = get_local_ref y in
    LM.replace loc y {rloc with loc_infl = x :: rloc.loc_infl};
    rloc.loc_value

  let set_local x y d =
    (*
      replaces old contribution with the new one;
      reconstructs value of y from contributions;
      propagates infl together with y and updates value - if value has changed
    *)
    let {loc_value;loc_init;loc_infl;loc_from} = get_local_ref y in
    let rold = get_old_local_value x loc_from in
    let new_value = lwarrow rold d in
    LM.replace loc_from x new_value;
    let new_y = get_local_value loc_init loc_from in
    if D.equal loc_value new_y then
      ()
    else let _ = add_work y in
      let _ = List.iter add_work loc_infl in
      LM.replace loc y {loc_value = new_y; loc_init; loc_infl = []; loc_from}

(*
        wrapper around propagation function to collect multiple contributions to same unknowns;
        contributions are delayed until the very end
*)

   let wrap (x,f) d =
        let sigma = LM.create 10 in
        let tau = GM.create 10 in
        let add_sigma x d =
                let d = try D.join d (LM.find sigma x) with _ -> d in
                LM.add sigma x d in
        let add_tau g d =
                let d = try G.join d (GM.find tau g) with _ -> d in
                GM.add tau g d in
        let _ = f d (get_local x) add_sigma (get_global x) add_tau in
        let _ = GM.iter (set_global x) tau in
        let _ = LM.iter (set_local x) sigma in
        ()


  (* ... now the main solver loop ... *)

  let solve xs =
    let _ = List.iter add_work xs in
    let rec doit () = match rem_work () with
      | None -> ()
      | Some x -> (
          match System.system x with
          | None -> doit ()
          | Some f ->
            (let rloc = get_local_ref x in
             wrap (x,f) rloc.loc_value;
             doit ())
        ) in
    let _ = doit () in
    let sigma = LM.to_seq loc |> Seq.map (fun (k,l) -> (k,l.loc_value)) in
    let tau = GM.to_seq glob |> Seq.map (fun (k,l) -> (k,l.value)) in
    (sigma, tau)


  let solve localinit globalinit startvars =
    let _ = List.iter init_local localinit in
    let _ = List.iter init_global globalinit in
    solve startvars

   (* ... now the checker! *)

   let check x =

        let sigma_out = LM.create 100 in
        let tau_out   = GM.create 100 in

        let get_local x = (get_local_ref x).loc_value in

        let check_local x d =
                let {loc_value:D.t;loc_init;loc_infl;loc_from} = get_local_ref x in
                if D.leq d loc_value then
                        if LM.mem sigma_out x then ()
                        else (
                                LM.add sigma_out x loc_value;
                                add_work x;
                                List.iter add_work loc_infl
                        )
                else (
                        Logs.error "Fixpoint not reached for local %a" System.LVar.pretty_trace x;
                        if LM.mem sigma_out x then ()
                        else (
                                LM.add sigma_out x loc_value;
                                add_work x;
                                List.iter add_work loc_infl
                        )
                ) in

        let get_global g = (get_global_ref g).value in

        let check_global g d =
                let {value;infl;from} = get_global_ref g in
                if G.leq d value then
                        if GM.mem tau_out g then ()
                        else (
                                GM.add tau_out g value;
                                List.iter add_work infl
                        )
                else (
                        Logs.error "Fixpoint not reached for global %a" System.GVar.pretty_trace g;
                        if GM.mem tau_out g then ()
                        else (
                                GM.add tau_out g value;
                                List.iter add_work infl
                        )
                ) in

        let rec doit () =
                match rem_work () with
                | None -> (sigma_out,tau_out)
                | Some x -> (match System.system x with
                        | None -> doit ()
                        | Some f -> (
                                f (get_local x)
                                   get_local check_local
                                   get_global check_global;
                                doit ()
                                )
                        ) in

        add_work x;
        doit ()

   let check localinit globalinit x =
        let check_local (x,d) =
                if D.leq d (get_local_ref x).loc_value then ()
                else Logs.error "initialization not subsumed for local %a" System.LVar.pretty_trace x in
        let check_global (g,d) =
                if G.leq d (get_global_ref g).value then ()
                else Logs.error "initialization not subsumed for global %a" System.GVar.pretty_trace g in

        let _ = List.iter check_local  localinit in
        let _ = List.iter check_global globalinit in
        
        check x
end
