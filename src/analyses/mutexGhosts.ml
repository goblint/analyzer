(** Analysis for generating ghost variables corresponding to mutexes ([mutexGhosts]). *)

open Analyses


module Spec =
struct
  include UnitAnalysis.Spec
  let name () = "mutexGhosts"

  module V =
  struct
    include Node
    let is_write_only _ = true
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
  module G = Lattice.Prod3 (Locked) (Unlocked) (MultiThread)

  let event ctx e octx =
    begin match e with
      | Events.Lock (l, _) ->
        ctx.sideg ctx.prev_node (Locked.singleton l, Unlocked.bot (), MultiThread.bot ())
      | Events.Unlock l ->
        ctx.sideg ctx.prev_node (Locked.bot (), Unlocked.singleton l, MultiThread.bot ())
      | Events.EnterMultiThreaded ->
        ctx.sideg ctx.prev_node (Locked.bot (), Unlocked.bot (), true)
      | _ -> ()
    end;
    ctx.local

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | YamlEntryGlobal (g, task) ->
      let g: V.t = Obj.obj g in
      let (locked, unlocked, multithread) = ctx.global g in
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
    | _ -> Queries.Result.top q
end

let _ =
  MCP.register_analysis ~dep:["mutexEvents"] (module Spec : MCPSpec)
