(** thread descendants analysis [threadDescendants]
    flow-insensitive construction of descendants may-set for every thread
*)

open Analyses
module TID = ThreadIdDomain.Thread
module TIDs = ConcDomain.ThreadSet

(** reflexive-transitive closure of child relation applied to [tid] and
    filtered to only include threads, where [tid] is a must-ancestor
    @param ask any ask
    @param tid
*)
let must_ancestor_descendants_closure (ask: Queries.ask) tid =
  let descendants = ask.f @@ Queries.DescendantThreads tid in
  let must_ancestors_descendants = TIDs.filter (TID.must_be_ancestor tid) descendants in
  TIDs.add tid must_ancestors_descendants

(** compute all descendant threads that may run along with the ego thread at a program point.
    for all of them, tid must be an ancestor
    @param man man of ego thread at the program point
    @param tid ego thread id
*)
let must_ancestor_running_descendants (ask: Queries.ask) tid =
  let may_created_tids = ask.f Queries.CreatedThreads in
  let may_must_ancestor_created_tids =
    TIDs.filter (TID.must_be_ancestor tid) may_created_tids
  in
  let may_transitively_created_tids =
    TIDs.fold
      (fun child_tid acc ->
         TIDs.union acc (must_ancestor_descendants_closure ask child_tid))
      may_must_ancestor_created_tids
      (TIDs.empty ())
  in
  let must_joined_tids = ask.f Queries.MustJoinedThreads in
  TIDs.diff may_transitively_created_tids must_joined_tids

module Spec = struct
  include IdentityUnitContextsSpec
  module D = Lattice.Unit

  module V = struct
    include TID
    include StdV
  end

  module G = ConcDomain.ThreadSet

  let name () = "threadDescendants"
  let startstate _ = D.bot ()
  let exitstate _ = D.bot ()

  let query man (type a) (x : a Queries.t) : a Queries.result =
    match x with
    | Queries.DescendantThreads t ->
      let children = man.global t in
      (G.fold
         (fun e acc -> G.union (man.ask @@ Queries.DescendantThreads e) acc)
         children
         children
       : G.t)
    | _ -> Queries.Result.top x

  let threadspawn man ~multiple lval f args fman =
    let tid_lifted = man.ask Queries.CurrentThreadId in
    let child_tid_lifted = fman.ask Queries.CurrentThreadId in
    match tid_lifted, child_tid_lifted with
    | `Lifted tid, `Lifted child_tid -> man.sideg tid (G.singleton child_tid)
    | _ -> ()
end

let _ = MCP.register_analysis ~dep:[ "threadid" ] (module Spec : MCPSpec)
