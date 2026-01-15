open Analyses
module TID = ThreadIdDomain.Thread
module TIDs = ConcDomain.ThreadSet
module Lockset = LockDomain.MustLockset

module Spec = struct
  include IdentityUnitContextsSpec
  module D = MapDomain.MapBot (TID) (Lockset)

  let name () = "descendantLockset"
  let startstate _ = D.empty ()
  let exitstate _ = D.empty ()

  (** reflexive-transitive closure of child relation applied to [tid] and
      filtered to only include threads, where [tid] is a must-ancestor
      @param man any man
      @param tid *)
  let must_ancestor_descendants_closure man tid =
    let descendants = man.ask @@ Queries.DescendantThreads tid in
    let must_ancestors_descendants = TIDs.filter (TID.must_be_ancestor tid) descendants in
    TIDs.add tid must_ancestors_descendants

  let threadspawn man ~multiple lval f args fman =
    let tid_lifted = man.ask Queries.CurrentThreadId in
    let child_tid_lifted = fman.ask Queries.CurrentThreadId in
    match tid_lifted, child_tid_lifted with
    | `Lifted tid, `Lifted child_tid when TID.must_be_ancestor tid child_tid ->
      let must_ancestor_descendants = must_ancestor_descendants_closure fman child_tid in
      let lockset = man.ask Queries.MustLockset in
      TIDs.fold
        (fun t_d -> D.join (D.singleton t_d lockset))
        must_ancestor_descendants
        man.local
    | _ -> man.local

  let get_must_ancestor_running_descendants man tid =
    let may_created_tids = man.ask Queries.CreatedThreads in
    let may_must_ancestor_created_tids =
      TIDs.filter (TID.must_be_ancestor tid) may_created_tids
    in
    let may_transitively_created_tids =
      TIDs.fold
        (fun child_tid acc ->
           TIDs.union acc (must_ancestor_descendants_closure man child_tid))
        may_must_ancestor_created_tids
        (TIDs.empty ())
    in
    let must_joined_tids = man.ask Queries.MustJoinedThreads in
    TIDs.diff may_transitively_created_tids must_joined_tids

  let unlock man tid possibly_running_tids lock =
    TIDs.fold
      (fun tid ->
         let old_value = D.find tid man.local in
         let new_value = Lockset.remove lock old_value in
         D.add tid new_value)
      possibly_running_tids
      (D.empty ())

  let unknown_unlock man tid possibly_running_tids =
    TIDs.fold (fun tid -> D.add tid (Lockset.empty ())) possibly_running_tids (D.empty ())

  let event man e _ =
    match e with
    | Events.Unlock addr ->
      let tid_lifted = man.ask Queries.CurrentThreadId in
      (match tid_lifted with
       | `Lifted tid ->
         let possibly_running_tids = get_must_ancestor_running_descendants man tid in
         let lock_opt = LockDomain.MustLock.of_addr addr in
         (match lock_opt with
          | Some lock -> unlock man tid possibly_running_tids lock
          | None -> unknown_unlock man tid possibly_running_tids)
       | _ -> man.local)
    | _ -> man.local

  module A = struct
    (** ego tid * lock history * descendant lockset *)
    include Printable.Prod3 (TID) (Queries.LH) (D)

    let happens_before (t1, dl1) (t2, lh2) =
      let locks_held_creating_t2 = D.find t2 dl1 in
      if Lockset.is_bot locks_held_creating_t2
      then false
      else (
        let relevant_lh2_threads =
          Lockset.fold
            (fun lock -> TIDs.union (Queries.LH.find lock lh2))
            locks_held_creating_t2
            (TIDs.empty ())
        in
        TIDs.exists
          (fun t_lh ->
             TID.must_be_ancestor t1 t_lh
             && (TID.equal t_lh t2 || TID.must_be_ancestor t_lh t2))
          relevant_lh2_threads)

    let may_race (t1, lh1, dl1) (t2, lh2, dl2) =
      not (happens_before (t1, dl1) (t2, lh2) || happens_before (t2, dl2) (t1, lh1))

    let should_print _ = false
  end

  let access man _ =
    let lh = man.ask Queries.MustlockHistory in
    let tid_lifted = man.ask Queries.CurrentThreadId in
    match tid_lifted with
    | `Lifted tid -> tid, lh, man.local
    | _ -> ThreadIdDomain.UnknownThread, Queries.LH.empty (), D.empty ()
end

let _ =
  MCP.register_analysis
    ~dep:[ "threadid"; "mutex"; "threadJoins"; "threadDescendants"; "mustlockHistory" ]
    (module Spec : MCPSpec)
