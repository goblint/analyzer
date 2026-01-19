(** creation lockset analysis [creationLockset]
    constructs edges on the graph over all threads, where the edges are labelled with must-locksets:
    (t_1) ---L--> (t_0) is represented by global t_1 t_0 = L and means that t_1 is protected by all members of L from t_0

    @see https://github.com/goblint/analyzer/pull/1865
*)

open Analyses
module TID = ThreadIdDomain.Thread
module TIDs = ConcDomain.ThreadSet
module Lock = LockDomain.MustLock
module Lockset = LockDomain.MustLockset

module Spec = struct
  include IdentityUnitContextsSpec
  module D = Lattice.Unit

  module V = struct
    include TID
    include StdV
  end

  module G = MapDomain.MapBot (TID) (Lockset)

  let name () = "creationLockset"
  let startstate _ = D.bot ()
  let exitstate _ = D.bot ()

  (** register a global contribution: global.[child_tid] \supseteq [to_contribute]
      @param man man at program point
      @param to_contribute new edges from [child_tid] to ego thread to register
      @param child_tid
  *)
  let contribute_locks man to_contribute child_tid = man.sideg child_tid to_contribute

  (** reflexive-transitive closure of child relation applied to [tid]
      and filtered to only include threads, where [tid] is a must-ancestor
      @param man any man
      @param tid
  *)
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
      let to_contribute = G.singleton tid lockset in
      TIDs.iter (contribute_locks man to_contribute) must_ancestor_descendants
    | _ -> ()

  (** compute all descendant threads that may run along with the ego thread at a program point.
      for all of them, tid must be an ancestor
      @param man man of ego thread at the program point
      @param tid ego thread id
  *)
  let get_must_ancestor_running_descendants man tid =
    let may_created_tids = man.ask Queries.CreatedThreads in
    let may_must_ancestor_created_tids =
      TIDs.filter (TID.must_be_ancestor tid) may_created_tids
    in
    let may_transitively_created_tids =
      TIDs.fold
        (fun child_tid acc -> TIDs.union acc (must_ancestor_descendants_closure man child_tid))
        may_must_ancestor_created_tids
        (TIDs.empty ())
    in
    let must_joined_tids = man.ask Queries.MustJoinedThreads in
    TIDs.diff may_transitively_created_tids must_joined_tids

  (** handle unlock of mutex [lock] *)
  let unlock man tid possibly_running_tids lock =
    let shrink_locksets des_tid =
      let old_creation_lockset = G.find tid (man.global des_tid) in
      (* Bot - {something} = Bot. This is exactly what we want in this case! *)
      let updated_creation_lockset = Lockset.remove lock old_creation_lockset in
      let to_contribute = G.singleton tid updated_creation_lockset in
      man.sideg des_tid to_contribute
    in
    TIDs.iter shrink_locksets possibly_running_tids

  (** handle unlock of an unknown mutex. Assumes that any mutex could have been unlocked *)
  let unknown_unlock man tid possibly_running_tids =
    let evaporate_locksets des_tid =
      let to_contribute = G.singleton tid (Lockset.empty ()) in
      man.sideg des_tid to_contribute
    in
    TIDs.iter evaporate_locksets possibly_running_tids

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
       | _ -> ())
    | _ -> ()

  module A = struct
    (** ego tid * must-lockset * creation-lockset *)
    include Printable.Prod3 (TID) (Lockset) (G)

    let name () = "creationLockset"

    (** checks if [cl1] has a mapping ([tp1] |-> [ls1])
        such that [ls1] and [ls2] are not disjoint and [tp1] != [t2]
        @param cl1 creation-lockset of thread [t1] at first program point
        @param t2 thread id at second program point
        @param ls2 lockset at second program point
        @returns whether [t1] must be running mutually exclusive with second program point
    *)
    let one_protected_inter_threaded_other_intra_threaded cl1 t2 ls2 =
      G.exists (fun tp1 ls1 -> not (Lockset.disjoint ls1 ls2 || TID.equal tp1 t2)) cl1

    (** checks if [cl1] has a member ([tp1] |-> [ls1]) and [cl2] has a member ([tp2] |-> [ls2])
        such that [ls1] and [ls2] are not disjoint and [tp1] != [tp2]
        @param cl1 creation-lockset of first thread [t1]
        @param cl2 creation-lockset of second thread [t2]
        @returns whether [t1] and [t2] must be running mutually exclusive
    *)
    let both_protected_inter_threaded cl1 cl2 =
      G.exists (one_protected_inter_threaded_other_intra_threaded cl1) cl2

    let may_race (t1, ls1, cl1) (t2, ls2, cl2) =
      not
        (both_protected_inter_threaded cl1 cl2
         || one_protected_inter_threaded_other_intra_threaded cl1 t2 ls2
         || one_protected_inter_threaded_other_intra_threaded cl2 t1 ls1)

    let should_print (_t, _ls, cl) = not @@ G.is_empty cl
  end

  let access man _ =
    let tid_lifted = man.ask Queries.CurrentThreadId in
    match tid_lifted with
    | `Lifted tid ->
      let lockset = man.ask Queries.MustLockset in
      let creation_lockset = man.global tid in
      tid, lockset, creation_lockset
    | _ -> ThreadIdDomain.UnknownThread, Lockset.empty (), G.empty ()
end

let _ =
  MCP.register_analysis
    ~dep:[ "threadid"; "mutex"; "threadJoins"; "threadDescendants" ]
    (module Spec : MCPSpec)
