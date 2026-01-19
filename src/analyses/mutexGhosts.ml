(** Analysis for generating ghost variables corresponding to mutexes ([mutexGhosts]). *)

open Analyses

module NodeSet = Queries.NS


module Spec =
struct
  include UnitAnalysis.Spec
  let name () = "mutexGhosts"

  module V =
  struct
    include Printable.Either3 (Node) (LockDomain.MustLock) (BoolDomain.Bool)
    let node x = `Left x
    let lock x = `Middle x
    let threadcreate = `Right false
    let update = `Right true
    let is_write_only = function
      | `Left _ -> false
      | `Middle _ -> true
      | `Right false -> false
      | `Right true -> true
  end

  module Locked =
  struct
    include LockDomain.MayLocksetNoRW
    let name () = "locked"
  end
  module Unlocked =
  struct
    include LockDomain.MayLocksetNoRW
    let name () = "unlocked"
  end
  module MultiThread =
  struct
    include BoolDomain.MayBool
    let name () = "multithread"
  end
  module G =
  struct
    include Lattice.Lift2 (Lattice.Prod3 (Locked) (Unlocked) (MultiThread)) (Lattice.Lift2 (BoolDomain.MayBool) (NodeSet))
    let node = function
      | `Bot -> (Locked.bot (), Unlocked.bot (), MultiThread.bot ())
      | `Lifted1 x -> x
      | _ -> failwith "MutexGhosts.node"
    let lock = function
      | `Bot -> BoolDomain.MayBool.bot ()
      | `Lifted2 (`Lifted1 x) -> x
      | _ -> failwith "MutexGhosts.lock"
    let threadcreate = function
      | `Bot -> NodeSet.bot ()
      | `Lifted2 (`Lifted2 x) -> x
      | _ -> failwith "MutexGhosts.threadcreate"
    let update = threadcreate
    let create_node node = `Lifted1 node
    let create_lock lock = `Lifted2 (`Lifted1 lock)
    let create_threadcreate threadcreate = `Lifted2 (`Lifted2 threadcreate)
    let create_update = create_threadcreate
  end

  let event man e oman =
    let verifier_atomic_addr = LockDomain.Addr.of_var LibraryFunctions.verifier_atomic_var in
    begin match e with
      | Events.Lock (l, _) when not (LockDomain.Addr.equal l verifier_atomic_addr) ->
        man.sideg (V.node man.prev_node) (G.create_node (Locked.singleton l, Unlocked.bot (), MultiThread.bot ()));
        if !AnalysisState.postsolving then (
          man.sideg V.update (G.create_update (NodeSet.singleton man.prev_node));
          let (locked, _, _) = G.node (man.global (V.node man.prev_node)) in
          if Locked.cardinal locked > 1 then (
            Locked.iter (fun lock ->
                Option.iter (fun lock ->
                    man.sideg (V.lock lock) (G.create_lock true)
                  ) (LockDomain.MustLock.of_addr lock)
              ) locked
          );
        )
      | Events.Unlock l when not (LockDomain.Addr.equal l verifier_atomic_addr) ->
        man.sideg (V.node man.prev_node) (G.create_node (Locked.bot (), Unlocked.singleton l, MultiThread.bot ()));
        if !AnalysisState.postsolving then (
          man.sideg V.update (G.create_update (NodeSet.singleton man.prev_node));
          let (_, unlocked, _) = G.node (man.global (V.node man.prev_node)) in
          if Locked.cardinal unlocked > 1 then (
            Locked.iter (fun lock ->
                Option.iter (fun lock ->
                    man.sideg (V.lock lock) (G.create_lock true)
                  ) (LockDomain.MustLock.of_addr lock)
              ) unlocked
          );
        )
      | Events.EnterMultiThreaded ->
        man.sideg (V.node man.prev_node) (G.create_node (Locked.bot (), Unlocked.bot (), true));
        if !AnalysisState.postsolving then
          man.sideg V.update (G.create_update (NodeSet.singleton man.prev_node));
      | _ -> ()
    end;
    man.local

  let threadspawn man ~multiple lval f args oman =
    man.sideg V.threadcreate (G.create_threadcreate (NodeSet.singleton man.node));
    man.local

  let ghost_var_available man = function
    | WitnessGhost.Var.Locked ((v, o) as lock) -> not (Offset.Z.contains_index o) && not (G.lock (man.global (V.lock lock)))
    | Multithreaded -> true

  let ghost_var_available man v =
    WitnessGhost.enabled () && ghost_var_available man v

  module VariableSet = Set.Make (YamlWitnessType.GhostInstrumentation.Variable)

  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | GhostVarAvailable v -> ghost_var_available man v
    | YamlEntryGlobal (g, task) ->
      let g: V.t = Obj.obj g in
      begin match g with
        | `Right true when YamlWitness.entry_type_enabled YamlWitnessType.GhostInstrumentation.entry_type ->
          let nodes = G.update (man.global g) in
          let (variables, location_updates) = NodeSet.fold (fun node (variables, location_updates) ->
              let (locked, unlocked, multithread) = G.node (man.global (V.node node)) in
              let variables' =
                Locked.fold (fun l acc ->
                    match LockDomain.MustLock.of_addr l with
                    | Some l when ghost_var_available man (Locked l) ->
                      let variable = WitnessGhost.variable' (Locked l) in
                      VariableSet.add variable acc
                    | _ ->
                      acc
                  ) (Locked.union locked unlocked) variables
              in
              let updates =
                Locked.fold (fun l acc ->
                    match LockDomain.MustLock.of_addr l with
                    | Some l when ghost_var_available man (Locked l) ->
                      let update = WitnessGhost.update' (Locked l) GoblintCil.one in
                      update :: acc
                    | _ ->
                      acc
                  ) locked []
              in
              let updates =
                Unlocked.fold (fun l acc ->
                    match LockDomain.MustLock.of_addr l with
                    | Some l when ghost_var_available man (Locked l) ->
                      let update = WitnessGhost.update' (Locked l) GoblintCil.zero in
                      update :: acc
                    | _ ->
                      acc
                  ) unlocked updates
              in
              let (variables', updates) =
                if not (GobConfig.get_bool "exp.earlyglobs") && multithread then (
                  if ghost_var_available man Multithreaded then (
                    let variable = WitnessGhost.variable' Multithreaded in
                    let update = WitnessGhost.update' Multithreaded GoblintCil.one in
                    let variables' = VariableSet.add variable variables' in
                    (variables', update :: updates)
                  )
                  else
                    (variables', updates)
                )
                else
                  (variables', updates)
              in
              match updates with
              | [] -> (variables', location_updates) (* don't add location_update with no updates *)
              | _ ->
                let location_update = WitnessGhost.location_update' ~node ~updates in
                (variables', location_update :: location_updates)
            ) nodes (VariableSet.empty, [])
          in
          let entry = WitnessGhost.instrumentation_entry ~task ~variables:(VariableSet.elements variables) ~location_updates in
          Queries.YS.singleton entry
        | `Left _ -> Queries.Result.top q
        | `Middle _ -> Queries.Result.top q
        | `Right _ -> Queries.Result.top q
      end
    | InvariantGlobalNodes -> (G.threadcreate (man.global V.threadcreate): NodeSet.t)
    | _ -> Queries.Result.top q
end

let _ =
  MCP.register_analysis ~dep:["mutexEvents"] (module Spec : MCPSpec)
