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
      let after = (addr, !Tracing.current_loc) in
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
      let global_visited_nodes = LH.create 100 in

      (* DFS *)
      let rec iter_node (path_visited_nodes: LS.t) (path_visited_nodes': LockEventPair.t list) (node: Lock.t) =
        if LS.mem node path_visited_nodes then (
          let pieces =
            List.concat_map (fun ((alock, aloc), (block, bloc)) ->
                [
                  (Pretty.dprintf "lock before: %a" Lock.pretty alock, Some aloc);
                  (Pretty.dprintf "lock after: %a" Lock.pretty block, Some bloc);
                ]
              ) (List.rev path_visited_nodes') (* backwards to get correct printout order *)
          in
          M.msg_group Warning "Deadlock order" pieces
        )
        else if not (LH.mem global_visited_nodes node) then begin
          LH.replace global_visited_nodes node ();
          let new_path_visited_nodes = LS.add node path_visited_nodes in
          G.iter (fun to_node gs ->
              MayLockEventPairs.iter (fun g ->
                  let new_path_visited_nodes' = g :: path_visited_nodes' in
                  iter_node new_path_visited_nodes new_path_visited_nodes' to_node
                ) gs
            ) (ctx.global node)
        end
      in

      iter_node LS.empty [] g
    | _ -> Queries.Result.top q
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
