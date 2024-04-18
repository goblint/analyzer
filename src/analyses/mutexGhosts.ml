(** Analysis for generating ghost variables corresponding to mutexes ([mutexGhosts]). *)

open Analyses


module Spec =
struct
  include UnitAnalysis.Spec
  let name () = "mutexGhosts"

  module V =
  struct
    include Printable.Either (Node) (LockDomain.Addr)
    let node x = `Left x
    let lock x = `Right x
    let is_write_only = function
      | `Left _ -> false
      | `Right _ -> true
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
    include Lattice.Lift2 (Lattice.Prod3 (Locked) (Unlocked) (MultiThread)) (Lattice.Unit)
    let node = function
      | `Bot -> (Locked.bot (), Unlocked.bot (), MultiThread.bot ())
      | `Lifted1 x -> x
      | _ -> failwith "MutexGhosts.node"
    let lock = function
      | `Bot -> Lattice.Unit.bot ()
      | `Lifted2 x -> x
      | _ -> failwith "MutexGhosts.lock"
    let create_node node = `Lifted1 node
    let create_lock lock = `Lifted2 lock
  end

  let event ctx e octx =
    begin match e with
      | Events.Lock (l, _) ->
        ctx.sideg (V.node ctx.prev_node) (G.create_node (Locked.singleton l, Unlocked.bot (), MultiThread.bot ()))
      | Events.Unlock l ->
        ctx.sideg (V.node ctx.prev_node) (G.create_node (Locked.bot (), Unlocked.singleton l, MultiThread.bot ()))
      | Events.EnterMultiThreaded ->
        ctx.sideg (V.node ctx.prev_node) (G.create_node (Locked.bot (), Unlocked.bot (), true))
      | _ -> ()
    end;
    ctx.local

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | YamlEntryGlobal (g, task) ->
      let g: V.t = Obj.obj g in
      begin match g with
        | `Left g' ->
          let (locked, unlocked, multithread) = G.node (ctx.global g) in
          let g = g' in
          let entries =
            (* TODO: do variable_entry-s only once *)
            Locked.fold (fun l acc ->
                let entry = WitnessGhost.variable_entry ~task (Locked l) in
                Queries.YS.add entry acc
              ) (Locked.union locked unlocked) (Queries.YS.empty ())
          in
          let entries =
            Locked.fold (fun l acc ->
                let entry = WitnessGhost.update_entry ~task ~node:g (Locked l) GoblintCil.one in
                Queries.YS.add entry acc
              ) locked entries
          in
          let entries =
            Unlocked.fold (fun l acc ->
                let entry = WitnessGhost.update_entry ~task ~node:g (Locked l) GoblintCil.zero in
                Queries.YS.add entry acc
              ) unlocked entries
          in
          let entries =
            if not (GobConfig.get_bool "exp.earlyglobs") && multithread then (
              let entry = WitnessGhost.variable_entry ~task Multithreaded in
              let entry' = WitnessGhost.update_entry ~task ~node:g Multithreaded GoblintCil.one in
              Queries.YS.add entry (Queries.YS.add entry' entries)
            )
            else
              entries
          in
          entries
        | `Right _ -> assert false
      end
    | _ -> Queries.Result.top q
end

let _ =
  MCP.register_analysis ~dep:["mutexEvents"] (module Spec : MCPSpec)
