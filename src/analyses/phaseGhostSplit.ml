(** Analysis for checking whether ghost globals are only accessed by one unique thread ([phaseGhostSplit]). *)

open Analyses
open GoblintCil

module TID = ThreadIdDomain.Thread
module TIDs = ConcDomain.ThreadSet
module LF = LibraryFunctions

module Const =
struct
  include Queries.PhaseDigestConst
  let name () = "ghost-constant"
end

module Spec =
struct
  include IdentitySpec

  let name () = "phaseGhostSplit"

  module D = Queries.PhaseDigestState
  include ValueContexts (D)
  module P = IdentityP (D)

  module V = VarinfoV

  module MHPs =
  struct
    include SetDomain.ToppedSet (MCPAccess.A) (struct let topname = "All phase accesses" end)

    let can_any_mhp (other:MCPAccess.A.t) = exists (MCPAccess.A.may_race other)

    let name () = "ghost-phase-accesses"
  end

  module LMust = Queries.LMust

  module MHPsPlusLMust =
  struct
    include Lattice.Prod (MHPs) (Queries.LMust)
  end

  module PhaseChanges =
  struct
    include MapDomain.MapBot (Const) (MHPsPlusLMust)
    let name () = "ghost-phase-changes"
  end

  module G =
  struct
    (* fist component: constant value when the thread returns *)
    (* second component: map from target of phase change to MHP information under which this change can happen  *)
    include Lattice.Prod (Const) (PhaseChanges)
    let const_at_thread_end (const, _) = const
    let changes (_, changes) = changes
    let create_const_at_thread_end const = (const, PhaseChanges.bot ())
    let create_change phase mhp lmust = (Const.bot (), (PhaseChanges.singleton phase (MHPs.singleton mhp, lmust)))

    let can_change_to (_, changes) target currmhp =
      match PhaseChanges.find_opt (`Lifted target) changes with
      | Some (accesses, lmust) when MHPs.can_any_mhp currmhp accesses ->
        Some lmust
      | _ ->
        None
  end

  let initial_ghost_values () =
    List.fold_left (fun acc -> function
        | GVar (v, initinfo, _) when YamlWitness.VarSet.mem v !(YamlWitness.ghostVars) ->
          begin match initinfo.init with
            | Some (SingleInit exp) ->
              begin match Cil.getInteger (Cil.constFold true exp) with
                | Some z -> D.add v (`Lifted z) acc
                | None -> acc
              end
            | None when Cil.isIntegralType v.vtype ->
              D.add v (`Lifted Z.zero) acc
            | _ ->
              acc
          end
        | _ ->
          acc
      ) (D.bot ()) !Cilfacade.current_file.globals

  let startstate _ = initial_ghost_values ()
  let exitstate _ = initial_ghost_values ()

  let is_phase_ghost man var = man.ask (Queries.IsPhaseGhost var)

  let phase_ghosts man =
    YamlWitness.VarSet.elements !(YamlWitness.ghostVars)
    |> List.filter (is_phase_ghost man)

  let top_non_phase_ghosts man state =
    YamlWitness.VarSet.fold (fun var state ->
        if is_phase_ghost man var then
          state
        else
          D.add var (Const.top ()) state
      ) !(YamlWitness.ghostVars) state

  let tids_of_current_thread man =
    match man.ask Queries.CurrentThreadId with
    | `Lifted tid when TID.is_unique tid -> TIDs.singleton tid
    | _ -> TIDs.top ()

  let increment_constant lval rval =
    let is_same_lval e =
      match Cil.stripCasts e with
      | Lval lval' -> CilType.Lval.equal lval lval'
      | _ -> false
    in
    let is_one_constant e =
      match Cil.getInteger (Cil.constFold true e) with
      | Some k -> Z.equal k Z.one
      | None -> false
    in
    match Cil.stripCasts rval with
    | BinOp (PlusA, e1, e2, _) ->
      if is_same_lval e1 then
        is_one_constant e2
      else if is_same_lval e2 then
        is_one_constant e1
      else
        false
    | _ ->
      false

  (* This local constant folding intentionally disregards writes from other threads.
     It is only for the phaseGhost checker itself. *)
  (* This information must **not** be used to refine other analyses,
     because it is unsound in the presence of other threads interfering. By the same token, it must
     be not used to raise Deadcode in branch. *)
  let rec eval_const state e =
    match Cil.stripCasts e with
    | Const _ ->
      Cil.getInteger (Cil.constFold true e)
    | Lval (Var var, NoOffset) when YamlWitness.VarSet.mem var !(YamlWitness.ghostVars) ->
      begin match D.find_opt var state with
        | Some (`Lifted z) -> Some z
        | _ -> None
      end
    | UnOp (Neg, e, _) ->
      Option.map Z.neg (eval_const state e)
    | BinOp (PlusA, e1, e2, _)
    | BinOp (IndexPI, e1, e2, _)
    | BinOp (PlusPI, e1, e2, _) ->
      Option.bind (eval_const state e1) (fun z1 ->
          Option.map (Z.add z1) (eval_const state e2)
        )
    | BinOp (MinusA, e1, e2, _) ->
      Option.bind (eval_const state e1) (fun z1 ->
          Option.map (Z.sub z1) (eval_const state e2)
        )
    | BinOp (Mult, e1, e2, _) ->
      Option.bind (eval_const state e1) (fun z1 ->
          Option.map (Z.mul z1) (eval_const state e2)
        )
    | _ ->
      None

  let is_increment_by_one state lval rval =
    increment_constant lval rval ||
    match eval_const state (Lval lval), eval_const state rval with
    | Some old_value, Some new_value ->
      Z.equal new_value (Z.succ old_value)
    | _ ->
      false

  let current_mhp man: MCPAccess.A.t =
    Obj.obj (man.ask (PartAccess Point))

  let sync man reason =
    (* TODO:
       Observation from ZA: Probably can get away with doing this after release-like operations, all possible advancing would already have been done prior for others *)
    if !AnalysisState.global_initialization then
      man.local
    else
      let local = top_non_phase_ghosts man man.local in
      let may_be_advanced_here m var =
        if man.ask Queries.MustBeAtomic then
          (* Shortcut, would also be caught below, but as it is common cheaper to check here *)
          (if M.tracing then M.tracel "phaseGhost" "Is atomic -> not advancing phase"; None)
        else
          match man.ask (Queries.Owner var), man.ask Queries.CurrentThreadId, D.find var m  with
          | `Bot, _, _
          | _, `Bot, _ ->
            None
          | `Lifted owner, _ , `Lifted z ->
            G.can_change_to (man.global var) (Z.succ z) (current_mhp man)
          | _ ->
            failwith "assumption about ghost owner violated"
      in
      let rec handle_vars (m, lmust) = function
        | []  ->
          man.split m [Events.GrowLMust lmust]
        | var :: vars ->
          match may_be_advanced_here m var with
          | Some curr_lmust ->
            (let v' = (match (D.find var m) with
                 | `Lifted x -> Z.succ x
                 | _ -> failwith "assumption")
             in
             let advanced = D.add var (`Lifted v') m in
             let lmust' = LMust.union lmust curr_lmust in
             handle_vars (advanced, lmust') (var::vars);
             handle_vars (m, lmust) vars)
          | None ->
            handle_vars (m, lmust) vars
      in
      let traceEvolution () =
        YamlWitness.VarSet.iter (fun var ->
            let owner = man.ask (Queries.Owner var) in
            let phase_ghost = is_phase_ghost man var in
            let may_advance = phase_ghost && Option.is_some (may_be_advanced_here local var) in
            M.tracel "phaseGhost" "Ghost %a is %sa phase ghost, has owner %a and may %s be advanced here" (* nosemgrep: trace-not-in-tracing *)
              CilType.Varinfo.pretty var
              (if phase_ghost then "" else "not ")
              ThreadIdDomain.ThreadLifted.pretty owner
              (if may_advance then "" else " not ")
          ) !(YamlWitness.ghostVars)
      in
      if M.tracing then traceEvolution ();
      handle_vars (local, LMust.empty ()) (phase_ghosts man);
      raise Deadcode


  let assign man lval rval =
    if !AnalysisState.global_initialization then
      man.local
    else
      let local = top_non_phase_ghosts man man.local in
      match lval with
      | Var var, NoOffset when YamlWitness.VarSet.mem var !(YamlWitness.ghostVars) && not (is_phase_ghost man var) ->
        D.add var (Const.top ()) local
      | Var var, NoOffset when is_phase_ghost man var ->
        (match eval_const local (Lval lval) with
         | Some z ->
           (let v = Z.succ z in
            let local_new = D.add var (`Lifted v) local in
            man.sideg var (G.create_change (`Lifted v) (current_mhp man) (man.ask LMust));
            (* TODO: Prolong until after atomic is over? *)
            if not (D.equal local local_new) then
              man.emit (Events.PhaseChange {old_phase = `Lifted local; new_phase = `Lifted local_new});
            local_new)
         | None -> failwith "Failed to evaluate ghost to constant")
      | _ ->
        local


  let handle_return man =
    let handle_ghost tid varinfo =
      match man.ask (Queries.Owner varinfo) with
      | `Lifted gtid when TID.equal tid gtid ->
        (match D.find_opt varinfo man.local with
         | Some v -> man.sideg varinfo (G.create_const_at_thread_end v)
         | _ -> ())
      | _ -> ()
    in
    match ThreadId.get_current (Analyses.ask_of_man man) with
    | `Lifted tid when TID.is_unique tid ->
      List.iter (handle_ghost tid) (phase_ghosts man);
    | _ -> ()



  let return man exp fundec =
    if ThreadReturn.is_current (Analyses.ask_of_man man) then
      handle_return man
    ;
    top_non_phase_ghosts man man.local



  let special man (lv:lval option) (f: varinfo) (args: exp list) =
    let desc = LF.find f in
    let st = match desc.special args, f.vname with
      | ThreadExit _, _ ->
        handle_return man; man.local
      | ThreadJoin { thread = id; _ }, _ ->
        (** Also handle transitively joined threads *)
        let handle_ghost tid varinfo  =
          match man.ask (Owner varinfo) with
          | `Lifted gtid when TID.equal tid gtid ->
            (match G.const_at_thread_end (man.global varinfo) with
             | `Lifted z ->
               begin match D.find_opt varinfo man.local with
                 | Some (`Lifted z')  when not (Z.equal z z') -> raise Deadcode
                 | _ -> ()
               end
             | _ -> ())
          | _ -> ()
        in
        let tids = man.ask (Queries.EvalThread id) in
        if TIDs.is_top tids || TIDs.is_empty tids || TIDs.is_bot tids then
          man.local
        else
          (match TIDs.elements tids with
           | [tid] when TID.is_unique tid ->
             List.iter (handle_ghost tid) (phase_ghosts man);
             man.local
           | _ -> man.local)
      | _ -> man.local
    in
    top_non_phase_ghosts man st


  let query man (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    match q with
    | PhaseDigest ->
      `Lifted (top_non_phase_ghosts man man.local)
    | EvalInt e ->
      begin match eval_const (top_non_phase_ghosts man man.local) e with
        | Some z ->
          ID.of_int (Cilfacade.get_ikind_exp e) z
        | None ->
          Result.top q
      end
    | _ ->
      Result.top q
end

let _ =
  MCP.register_analysis ~dep:["access"; "threadid"; "phaseGhost"] (module Spec : MCPSpec)
