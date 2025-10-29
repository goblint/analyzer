open Goblint_constraint.ConstrSys
(*
        rhs should not query locals!
*)
open Messages

module FwdBuSolver (System: FwdGlobConstrSys) = struct

  module D = System.D
  module G = System.G

  module LS = Set.Make (System.LVar)

  module GM = Hashtbl.Make(System.GVar)
  module LM = Hashtbl.Make(System.LVar)

  let gwarrow a b = if G.leq b a then G.narrow a b else G.widen a (G.join a b)
  let lwarrow a b = if D.leq b a then D.narrow a b else D.widen a (D.join a b)

(*
       work list just for checking ...
*)

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

  let gas = ref (10,3)

  type glob = {value : G.t; init : G.t;  infl : System.LVar.t list; from : (G.t * int * int) LM.t}

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

  let get_global_value init from = LM.fold (fun _ (b,_,_) a -> G.join a b) from init

  let get_old_global_value x from =
    try LM.find from x
    with _ ->
      let (delay,gas) = !gas in
      LM.add from x (G.bot (),delay,gas);
      (G. bot (),delay,gas)

  (* now the getters for globals *)

  let get_global x g =
    let rglob = get_global_ref g in
    GM.replace glob g { rglob with infl = x::rglob.infl }; (* ensure the global is in the hashtable *)
    rglob.value

  type loc = {loc_value : D.t; loc_init : D.t; 
              called: bool ref; aborted: bool ref;
              loc_from : (D.t * int * int) LM.t}
(*
    init may contain some initial value not provided by separate origin;
    perhaps, dynamic tracking of dependencies required for certain locals?
*)

  let loc: loc LM.t = LM.create 100

  (* auxiliary functions for locals *)


  let get_local_ref x =
    try LM.find loc x
    with _ ->
      (let rloc = {loc_value = D.bot (); loc_init = D.bot (); 
                   called = ref false; aborted = ref false;
                   loc_from = LM.create 10} in
       LM.add loc x rloc;
       rloc)

  let init_local (x, d) =
    LM.add loc x {
      loc_value = d;
      loc_init = d;
      called = ref false;
      aborted = ref false;
      loc_from = LM.create 10
    }

  let get_local_value init from = LM.fold (fun _ (b,_,_) a -> D.join a b) from init

  let get_old_local_value x from =
    try LM.find from x
    with _ -> let (delay,gas) = !gas in
      LM.add from x (D.bot (),delay,gas);
      (D.bot (),delay,gas)

  (* 
        Now the main solving consisting of the mutual recursive functions
        set_globals, set_locals, and iterate
*)

  let rec set_global x g d =
(*
        replaces old contribution with the new one;
        reconstructs value of g from contributions;
        propagates infl and updates value - if value has changed
*)
    if tracing then trace "set_global" "set_global %a %a" System.GVar.pretty_trace g G.pretty d;
    let {value;init;infl;from} = get_global_ref g in
    let (old_value,delay,gas) = get_old_global_value x from in
    if G.equal d old_value then () 
    else let (new_value,delay,gas) = if G.leq d old_value then
             if gas > 0 then (G.narrow old_value d,delay,gas-1)
             else (old_value,delay,0)
           else if delay > 0 then (G.join old_value d,delay-1,gas)
           else (G.widen old_value (G.join old_value d), 0, gas) in
      let _ = LM.replace from x (new_value,delay,gas) in
      let new_g = get_global_value init from in
      if G.equal value new_g then
        ()
      else
        let work = infl in
        let _ = GM.replace glob g {value = new_g; init = init; infl = []; from} in
        let doit x = 
          let r = get_local_ref x in
          if !(r.called) then r.aborted := true
          else iterate x in
        List.iter doit work 

  and set_local x y d =
    (*
      replaces old contribution with the new one;
      reconstructs value of y from contributions;
      propagates infl together with y and updates value - if value has changed
    *)
    if tracing then trace "set_local" "set_local %a %a" System.LVar.pretty_trace y D.pretty d;
    let {loc_value;loc_init;called;aborted;loc_from} as y_record = get_local_ref y in
    let (old_value,delay,gas) = get_old_local_value x loc_from in
    if D.equal d old_value then ()
    else let (new_value,delay,gas) = 
           if !called then 
             if D.leq d old_value then
               if gas > 0 then (D.narrow old_value d,delay,gas-1)
               else (old_value,delay,0)
             else if delay > 0 then (D.join old_value d,delay-1,gas)
             else (D.widen old_value (D.join old_value d), 0, gas) 
           else (d,delay,gas) in
      let _ = LM.replace loc_from x (new_value,delay,gas) in
      let new_y = get_local_value loc_init loc_from in
      if D.equal loc_value new_y then ()
      else (
         let y_record = y_record with {loc_value = new_y} in
         LM.replace loc y y_record;
         if !called then aborted := true
      )      
      else iterate y 

(*
        wrapper around propagation function to collect multiple contributions to same unknowns;
        contributions are delayed until the very end
*)

  and wrap (x,f) d =
    let sigma = LM.create 10 in
    let tau = GM.create 10 in
    let add_sigma x d =
      let d = try D.join d (LM.find sigma x) with _ -> d in
      LM.replace sigma x d in
    let add_tau g d =
      let d = try G.join d (GM.find tau g) with _ -> d in
      GM.replace tau g d in
    let _ = f d (fun _ -> raise (Failure "Locals queried in rhs??")) 
        add_sigma (get_global x) add_tau in
    let _ = GM.iter (set_global x) tau in
    let _ = LM.iter (set_local x) sigma in
    ()

(*
        now the actual propagation!
*)

  and iterate x = 
    if tracing then trace "iter" "iterate %a" System.LVar.pretty_trace x;
    let rloc = get_local_ref x in
    let _ = rloc.called := true in
    let _ = rloc.aborted := false in
    match System.system x with
    | None -> ()
    | Some f ->
      let _ = wrap (x,f) rloc.loc_value in
      let _ = rloc.called := false in
      if !(rloc.aborted) then iterate x
      else ()


  (* ... now the main solver loop ... *)



  let solve localinit globalinit xs =
    if tracing then trace "solver" "Starting bottom-up fixpoint iteration";
    List.iter init_local localinit;
    List.iter init_global globalinit;
    List.iter iterate xs;
    let sigma = LM.to_seq loc |> Seq.map (fun (k,l) -> (k,l.loc_value)) in
    let tau = GM.to_seq glob |> Seq.map (fun (k,l) -> (k,l.value)) in
    (sigma,tau)

  (* ... now the checker! *)

  let check localinit globalinit xs =

    let sigma_out = LM.create 100 in
    let tau_out   = GM.create 100 in

    let get_local x = try (LM.find loc x).loc_value with _ -> D.bot () in

    let check_local x d = if D.leq d (D.bot ()) then ()
      else let {loc_value:D.t;loc_init;called;aborted;loc_from} = get_local_ref x in
        if D.leq d loc_value then
          if LM.mem sigma_out x then ()
          else (
            LM.add sigma_out x loc_value;
            add_work x
          )
        else (
          Logs.error "Fixpoint not reached for local %a" System.LVar.pretty_trace x;
          AnalysisState.verified := Some false;
          if LM.mem sigma_out x then ()
          else (
            LM.add sigma_out x loc_value;
            add_work x
          )
        ) in

    let get_global g = try (GM.find glob g).value with _ -> G.bot () in

    let check_global x g d =
      if G.leq d (G.bot ()) then
        ()
      else if System.GVar.is_write_only g then
        GM.replace tau_out g (G.join (GM.find_opt tau_out g |> BatOption.default (G.bot ())) d)
      else
        let {value;infl;_} = get_global_ref g in
        if G.leq d value then
          if GM.mem tau_out g then ()
          else (
            GM.add tau_out g value;
            List.iter add_work infl
          )
        else (
          Logs.error "Fixpoint not reached for global %a\n Side from %a is %a \n Solver Computed %a\n Diff is %a" System.GVar.pretty_trace g System.LVar.pretty_trace x G.pretty d G.pretty value G.pretty_diff (d,value);
          AnalysisState.verified := Some false;
          if GM.mem tau_out g then ()
          else (
            GM.add tau_out g value;
            List.iter add_work infl
          )
        ) in

    let rec doit () =
      match rem_work () with
      | None -> (LM.to_seq sigma_out, GM.to_seq tau_out)
      | Some x -> (match System.system x with
          | None -> doit ()
          | Some f -> (
              f (get_local x)
                get_local check_local
                get_global (check_global x);
              doit ()
            )
        ) in

    List.iter (fun (x,_) -> let value = get_local x in LM.add sigma_out x value) localinit;
    List.iter (fun (g, _) -> let value = get_global g in GM.add tau_out g value) globalinit;
    List.iter add_work xs;
    doit ()

  let check localinit globalinit xs =
    let check_local (x,d) =
      if D.leq d (get_local_ref x).loc_value then ()
      else (
        Logs.error "initialization not subsumed for local %a" System.LVar.pretty_trace x;
        AnalysisState.verified := Some false)
    in
    let check_global (g,d) =
      if G.leq d (get_global_ref g).value then ()
      else (
        Logs.error "initialization not subsumed for global %a" System.GVar.pretty_trace g;
        AnalysisState.verified := Some false;
      ) in

    let _ = List.iter check_local localinit in
    let _ = List.iter check_global globalinit in

    check localinit globalinit xs
end
