(** Analysis for generating ghost variables corresponding to mutexes ([mutexGhosts]). *)

open Analyses

module NodeSet = Queries.NS


module Spec =
struct
  include UnitAnalysis.Spec
  let name () = "mutexGhosts"

  module ThreadCreate = Printable.UnitConf (struct let name = "threadcreate" end)
  module V =
  struct
    include Printable.Either3 (Node) (LockDomain.Addr) (ThreadCreate)
    let node x = `Left x
    let lock x = `Middle x
    let threadcreate = `Right ()
    let is_write_only = function
      | `Left _ -> false
      | `Middle _ -> true
      | `Right _ -> false
  end

  module Locked =
  struct
    include LockDomain.Mutexes
    let name () = "locked"
  end
  module Unlocked =
  struct
    include LockDomain.Mutexes
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
    let create_node node = `Lifted1 node
    let create_lock lock = `Lifted2 (`Lifted1 lock)
    let create_threadcreate threadcreate = `Lifted2 (`Lifted2 threadcreate)
  end

  let event ctx e octx =
    begin match e with
      | Events.Lock (l, _) ->
        ctx.sideg (V.node ctx.prev_node) (G.create_node (Locked.singleton l, Unlocked.bot (), MultiThread.bot ()));
        if !AnalysisState.postsolving then (
          let (locked, _, _) = G.node (ctx.global (V.node ctx.prev_node)) in
          if Locked.cardinal locked > 1 then (
            Locked.iter (fun lock ->
                ctx.sideg (V.lock lock) (G.create_lock true)
              ) locked
          );
        )
      | Events.Unlock l ->
        ctx.sideg (V.node ctx.prev_node) (G.create_node (Locked.bot (), Unlocked.singleton l, MultiThread.bot ()));
        if !AnalysisState.postsolving then (
          let (_, unlocked, _) = G.node (ctx.global (V.node ctx.prev_node)) in
          if Locked.cardinal unlocked > 1 then (
            Locked.iter (fun lock ->
                ctx.sideg (V.lock lock) (G.create_lock true)
              ) unlocked
          );
        )
      | Events.EnterMultiThreaded ->
        ctx.sideg (V.node ctx.prev_node) (G.create_node (Locked.bot (), Unlocked.bot (), true))
      | _ -> ()
    end;
    ctx.local

  let threadspawn ctx ~multiple lval f args octx =
    ctx.sideg V.threadcreate (G.create_threadcreate (NodeSet.singleton ctx.node));
    ctx.local

  let ghost_var_available ctx = function
    | WitnessGhost.Var.Locked (Addr (v, o) as lock) -> not (LockDomain.Offs.contains_index o) && not (G.lock (ctx.global (V.lock lock)))
    | Locked _ -> false
    | Multithreaded -> true

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | GhostVarAvailable v -> ghost_var_available ctx v
    | YamlEntryGlobal (g, task) ->
      let g: V.t = Obj.obj g in
      begin match g with
        | `Left g' ->
          let (locked, unlocked, multithread) = G.node (ctx.global g) in
          let g = g' in
          let entries =
            (* TODO: do variable_entry-s only once *)
            Locked.fold (fun l acc ->
                if ghost_var_available ctx (Locked l) then (
                  let entry = WitnessGhost.variable_entry ~task (Locked l) in
                  Queries.YS.add entry acc
                )
                else
                  acc
              ) (Locked.union locked unlocked) (Queries.YS.empty ())
          in
          let entries =
            Locked.fold (fun l acc ->
                if ghost_var_available ctx (Locked l) then (
                  let entry = WitnessGhost.update_entry ~task ~node:g (Locked l) GoblintCil.one in
                  Queries.YS.add entry acc
                )
                else
                  acc
              ) locked entries
          in
          let entries =
            Unlocked.fold (fun l acc ->
                if ghost_var_available ctx (Locked l) then (
                  let entry = WitnessGhost.update_entry ~task ~node:g (Locked l) GoblintCil.zero in
                  Queries.YS.add entry acc
                )
                else
                  acc
              ) unlocked entries
          in
          let entries =
            if not (GobConfig.get_bool "exp.earlyglobs") && multithread then (
              if ghost_var_available ctx Multithreaded then (
                let entry = WitnessGhost.variable_entry ~task Multithreaded in
                let entry' = WitnessGhost.update_entry ~task ~node:g Multithreaded GoblintCil.one in
                Queries.YS.add entry (Queries.YS.add entry' entries)
              )
              else
                entries
            )
            else
              entries
          in
          entries
        | `Middle _ -> Queries.Result.top q
        | `Right _ -> Queries.Result.top q
      end
    | InvariantGlobalNodes -> (G.threadcreate (ctx.global V.threadcreate): NodeSet.t)
    | _ -> Queries.Result.top q
end

let _ =
  MCP.register_analysis ~dep:["mutexEvents"] (module Spec : MCPSpec)
