(** Deadlock analysis. *)

open Batteries
open GoblintCil
open Analyses
open DeadlockDomain


module Spec =
struct

  module LockEventPair = Printable.Prod (LockEvent) (LockEvent)
  module MayLockEventPairs = SetDomain.Make (LockEventPair)

  module Arg =
  struct
    module D = MayLockEvents
    module V =
    struct
      include Lock
      let is_write_only _ = true
    end

    module G = MapDomain.MapBot (Lock) (MayLockEventPairs)

    let side_lock_event_pair ctx ((before_node, _, _) as before) ((after_node, _, _) as after) =
      if !AnalysisState.should_warn then
        ctx.sideg before_node (G.singleton after_node (MayLockEventPairs.singleton (before, after)))

    let part_access ctx: MCPAccess.A.t =
      Obj.obj (ctx.ask (PartAccess Point))

    let add ctx ((l, _): LockDomain.Lockset.Lock.t) =
      let after: LockEvent.t = (l, ctx.prev_node, part_access ctx) in (* use octx for access to use locksets before event *)
      D.iter (fun before ->
          side_lock_event_pair ctx before after
        ) ctx.local;
      D.add after ctx.local

    let remove ctx l =
      let inLockAddrs (e, _, _) = Lock.equal l e in
      D.filter (neg inLockAddrs) ctx.local
  end

  include LocksetAnalysis.MakeMay (Arg)
  let name () = "deadlock"

  module G = Arg.G (* help type checker using explicit constraint *)

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | WarnGlobal g ->
      let g: V.t = Obj.obj g in

      let module LH = Hashtbl.Make (Lock) in
      let module LS = Set.Make (Lock) in
      (* TODO: find all cycles/SCCs *)
      let global_visited_locks = LH.create 100 in

      let may_equal l1 l2 = match l1, l2 with
        | ValueDomain.Addr.UnknownPtr, _
        | _, ValueDomain.Addr.UnknownPtr ->
          true
        | _, _ -> Lock.equal l1 l2
      in

      (* DFS *)
      let rec iter_lock (path_visited_locks: LS.t) (path_visited_lock_event_pairs: LockEventPair.t list) (lock: Lock.t) =
        if LS.mem lock path_visited_locks || LS.mem ValueDomain.Addr.UnknownPtr path_visited_locks || (not (LS.is_empty path_visited_locks) && lock = ValueDomain.Addr.UnknownPtr) then (
          (* cycle may not return to first lock, but an intermediate one, cut off the non-cyclic stem *)
          let path_visited_lock_event_pairs =
            (* path_visited_lock_event_pairs cannot be empty *)
            List.hd path_visited_lock_event_pairs ::
            List.take_while (fun (_, (after_lock, _, _)) ->
                not (may_equal after_lock lock)
              ) (List.tl path_visited_lock_event_pairs)
          in
          let mhp =
            Enum.cartesian_product (List.enum path_visited_lock_event_pairs) (List.enum path_visited_lock_event_pairs)
            |> Enum.for_all (fun (((_, (_, _, access1)) as p1), ((_, (_, _, access2)) as p2)) ->
                LockEventPair.equal p1 p2 || MCPAccess.A.may_race access1 access2
              )
          in
          if mhp then (
            (* normalize path_visited_lock_event_pairs such that we don't get the same cycle multiple times, starting from different events *)
            let min = List.min ~cmp:LockEventPair.compare path_visited_lock_event_pairs in
            let (mini, _) = List.findi (fun i x -> LockEventPair.equal min x) path_visited_lock_event_pairs in
            let (init, tail) = List.split_at mini path_visited_lock_event_pairs in
            let normalized = List.rev_append init (List.rev tail) in (* backwards to get correct printout order *)
            let msgs = List.concat_map (fun ((before_lock, before_node, before_access), (after_lock, after_node, after_access)) ->
                [
                  (Pretty.dprintf "lock before: %a with %a" Lock.pretty before_lock MCPAccess.A.pretty before_access, Some (M.Location.Node before_node));
                  (Pretty.dprintf "lock after: %a with %a" Lock.pretty after_lock MCPAccess.A.pretty after_access, Some (M.Location.Node after_node));
                ]
              ) normalized
            in
            M.msg_group Warning ~category:Deadlock "Locking order cycle" msgs
          )
        )
        else if not (LH.mem global_visited_locks lock) then begin
          LH.replace global_visited_locks lock ();
          let new_path_visited_locks = LS.add lock path_visited_locks in
          G.iter (fun to_lock lock_event_pairs ->
              MayLockEventPairs.iter (fun lock_event_pair ->
                  let new_path_visited_lock_event_pairs' = lock_event_pair :: path_visited_lock_event_pairs in
                  iter_lock new_path_visited_locks new_path_visited_lock_event_pairs' to_lock
                ) lock_event_pairs
            ) (ctx.global lock)
        end
      in

      Timing.wrap ~args:[("lock", `String (Lock.show g))] "deadlock" (iter_lock LS.empty []) g
    | _ -> Queries.Result.top q
end

let _ =
  MCP.register_analysis ~dep:["mutexEvents"] (module Spec : MCPSpec)
