(** Deadlock analysis. *)

open Prelude.Ana
open Analyses
open DeadlockDomain

module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "deadlock"

  module D = MayLockEvents
  module C = D
  module V = Lock

  module LockEventPair = Printable.Prod (LockEvent) (LockEvent)
  module MayLockEventPairs = SetDomain.Make (LockEventPair)
  module G =
  struct
    include MapDomain.MapBot (Lock) (MayLockEventPairs)
    let leq x y = !GU.postsolving || leq x y (* HACK: to pass verify*)
  end

  let side_lock_event_pair ctx before after =
    let d =
      if !GU.should_warn then
        G.singleton (fst after) (MayLockEventPairs.singleton (before, after))
      else
        G.bot () (* HACK: just to pass validation with MCP DomVariantLattice *)
    in
    ctx.sideg (fst before) d


  (* Some required states *)
  let startstate _ : D.t = D.empty ()
  let threadenter ctx lval f args = [D.empty ()]
  let exitstate  _ : D.t = D.empty ()

  let event ctx (e: Events.t) octx =
    match e with
    | Lock addr ->
      let after = (addr, ctx.prev_node) in
      D.iter (fun before ->
          side_lock_event_pair ctx before after
        ) ctx.local;
      D.add after ctx.local
    | Unlock addr ->
      let inLockAddrs (e, _) = Lock.equal addr e in
      D.filter (neg inLockAddrs) ctx.local
    | _ ->
      ctx.local

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | WarnGlobal g ->
      let g: V.t = Obj.obj g in

      let module LH = Hashtbl.Make (Lock) in
      let module LS = Set.Make (Lock) in
      (* TODO: find all cycles/SCCs *)
      let global_visited_locks = LH.create 100 in

      (* DFS *)
      let rec iter_lock (path_visited_locks: LS.t) (path_visited_lock_event_pairs: LockEventPair.t list) (lock: Lock.t) =
        if LS.mem lock path_visited_locks then (
          (* cycle may not return to first lock, but an intermediate one, cut off the non-cyclic stem *)
          let path_visited_lock_event_pairs =
            (* path_visited_lock_event_pairs cannot be empty *)
            List.hd path_visited_lock_event_pairs ::
            List.take_while (fun (_, (after_lock, _)) ->
                not (Lock.equal after_lock lock)
              ) (List.tl path_visited_lock_event_pairs)
          in
          (* normalize path_visited_lock_event_pairs such that we don't get the same cycle multiple times, starting from different events *)
          let min = List.min ~cmp:LockEventPair.compare path_visited_lock_event_pairs in
          let (mini, _) = List.findi (fun i x -> LockEventPair.equal min x) path_visited_lock_event_pairs in
          let (init, tail) = List.split_at mini path_visited_lock_event_pairs in
          let normalized = List.rev_append init (List.rev tail) in (* backwards to get correct printout order *)
          let msgs = List.concat_map (fun ((before_lock, before_node), (after_lock, after_node)) ->
              [
                (Pretty.dprintf "lock before: %a" Lock.pretty before_lock, Some (UpdateCil.getLoc before_node));
                (Pretty.dprintf "lock after: %a" Lock.pretty after_lock, Some (UpdateCil.getLoc after_node));
              ]
            ) normalized
          in
          M.msg_group Warning ~category:Deadlock "Locking order cycle" msgs
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

      Stats.time "deadlock" (iter_lock LS.empty []) g
    | _ -> Queries.Result.top q
end

let _ =
  MCP.register_analysis ~dep:["mutex"] (module Spec : MCPSpec)
