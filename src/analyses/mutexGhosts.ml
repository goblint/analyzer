(** Analysis for generating ghost variables corresponding to mutexes ([mutexGhosts]). *)

open Analyses


module Spec =
struct
  (* Copied & modified from MayLocks. *)
  module Arg =
  struct
    module D = LockDomain.MayLocksetNoRW
    module V =
    struct
      include Node
      let is_write_only _ = true
    end

    module Locked =
    struct
      include D
      let name () = "locked"
    end
    module MultiThread =
    struct
      include BoolDomain.MayBool
      let name () = "multithread"
    end
    module G = Lattice.Prod (Locked) (MultiThread)

    let add ctx (l,r) =
      D.add l ctx.local

    let remove ctx l =
      match D.Addr.to_mval l with
      | Some (v,o) ->
        let mtype = ctx.ask (Queries.MutexType (v, Offset.Unit.of_offs o)) in
        begin match mtype with
          | `Lifted MutexAttrDomain.MutexKind.NonRec -> D.remove l ctx.local
          | _ -> ctx.local (* we cannot remove them here *)
        end
      | None -> ctx.local (* we cannot remove them here *)
  end

  include LocksetAnalysis.MakeMay (Arg)
  let name () = "mutexGhosts"

  open Arg

  let sync ctx reason =
    if !AnalysisState.postsolving then
      ctx.sideg ctx.prev_node (ctx.local, MultiThread.bot ());
    ctx.local

  let event ctx e octx =
    begin match e with
      | Events.EnterMultiThreaded ->
        ctx.sideg ctx.prev_node (Locked.bot (), true)
      | _ -> ()
    end;
    event ctx e octx (* delegate to must lockset analysis *)

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | YamlEntryGlobal (g, task) ->
      let g: V.t = Obj.obj g in
      let module Cfg = (val !MyCFG.current_cfg) in
      let next_lockset = List.fold_left (fun acc (_, next_node) ->
          let (locked, _) = ctx.global next_node in
          D.join acc locked
        ) (D.bot ()) (Cfg.next g)
      in
      let (lockset, multithread) = ctx.global g in
      let unlocked = D.diff lockset next_lockset in
      let locked = D.diff next_lockset lockset in
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
        Locked.fold (fun l acc ->
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
