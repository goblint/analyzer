open Analyses
module LF = LibraryFunctions

module TID = ThreadIdDomain.Thread
module TIDs = ConcDomain.ThreadSet
module LID = LockDomain.MustLock
module LIDs = LockDomain.MustLockset

module AncestorLocksetSpec = struct
  include IdentityUnitContextsSpec (* no context necessary(?) *)
  module D = Lattice.Unit

  module V = struct
    include TID
    include StdV
  end

  (** 2 ^ { [TID] \times [LID] } *)
  module G = Queries.ALS

  let startstate _ = D.bot ()
  let exitstate _ = D.bot ()

  (** register a contribution to a global: global.[child_tid] \supseteq [to_contribute]
      @param man man at program point
      @param to_contribute new edges from [child_tid] to register
      @param child_tid
  *)
  let contribute_lock man to_contribute child_tid = man.sideg child_tid to_contribute

  (** compute [tids] \times \{[lock]\} *)
  let singleton_cartesian_prod tids lock =
    TIDs.fold (fun tid acc -> G.add (tid, lock) acc) tids (G.empty ())
  ;;

  (** compute the cartesian product [tids] \times [locks] *)
  let cartesian_prod tids locks =
    LIDs.fold
      (fun lock acc ->
         let tids_times_lock = singleton_cartesian_prod tids lock in
         G.union tids_times_lock acc)
      locks
      (G.empty ())
  ;;

  (** reflexive-transitive closure of child relation applied to [tid]
      @param ask any ask
      @param tid
      @returns [{ tid }] \cup DES([tid])
  *)
  let descendants_closure (ask : Queries.ask) tid =
    let transitive_descendants = ask.f @@ Queries.DescendantThreads tid in
    TIDs.add tid transitive_descendants
  ;;
end

(** collects for each thread t_n pairs of must-ancestors and locks (t_0,l):
    when t_n or a must-ancestor t_1 of t_n was created, the parent t_0 must have held l.
*)
module CreationLocksetSpec = struct
  include AncestorLocksetSpec

  let name () = "creationLockset"

  (** create(t_1) in t_0 with lockset L *)
  let threadspawn man ~multiple lval f args fman =
    let ask = Analyses.ask_of_man man in
    let tid_lifted = ask.f Queries.CurrentThreadId in
    let child_ask = Analyses.ask_of_man fman in
    let child_tid_lifted = child_ask.f Queries.CurrentThreadId in
    match tid_lifted, child_tid_lifted with
    | `Lifted tid, `Lifted child_tid when TID.must_be_ancestor tid child_tid ->
      let descendants = descendants_closure child_ask child_tid in
      let lockset = ask.f Queries.MustLockset in
      let to_contribute = cartesian_prod (TIDs.singleton tid) lockset in
      TIDs.iter (contribute_lock man to_contribute) descendants
    | _ -> (* TODO deal with top or bottom? *) ()
  ;;

  let query man (type a) (x : a Queries.t) : a Queries.result =
    match x with
    | Queries.MayCreationLockset tid -> (man.global tid : G.t)
    | _ -> Queries.Result.top x
  ;;
end

(** collects for each thread t_n pairs of ancestors and locks (t_0,l):
    when l is unlocked in t_0, t_n could be running.
*)
module TaintedCreationLocksetSpec = struct
  include AncestorLocksetSpec

  let name () = "taintedCreationLockset"

  (** compute all threads that may run along with the ego thread at a program point
      @param ask ask of ego thread at the program point
  *)
  let get_possibly_running_tids (ask : Queries.ask) =
    let may_created_tids = ask.f Queries.CreatedThreads in
    let may_transitively_created_tids =
      TIDs.fold
        (fun child_tid acc -> TIDs.union acc (descendants_closure ask child_tid))
        may_created_tids
        (TIDs.empty ())
    in
    let must_joined_tids = ask.f Queries.MustJoinedThreads in
    TIDs.diff may_transitively_created_tids must_joined_tids
  ;;

  (** stolen from mutexGhost.ml. TODO Maybe add to utils? *)
  let mustlock_of_addr (addr : LockDomain.Addr.t) : LID.t option =
    match addr with
    | Addr mv when LockDomain.Mval.is_definite mv -> Some (LID.of_mval mv)
    | _ -> None
  ;;

  let event man e _ =
    match e with
    | Events.Unlock addr ->
      let ask = Analyses.ask_of_man man in
      let tid_lifted = ask.f Queries.CurrentThreadId in
      (match tid_lifted with
       | `Top | `Bot -> ()
       | `Lifted tid ->
         let possibly_running_tids = get_possibly_running_tids ask in
         let lock_opt = mustlock_of_addr addr in
         (match lock_opt with
          | Some lock ->
            (* contribute for all possibly_running_tids: (tid, lock) *)
            let to_contribute = G.singleton (tid, lock) in
            TIDs.iter (contribute_lock man to_contribute) possibly_running_tids
          | None ->
            (* any lock could have been unlocked. Contribute for all possibly_running_tids their full CreationLocksets to invalidate them!! *)
            let contribute_creation_lockset des_tid =
              let creation_lockset = ask.f @@ Queries.MayCreationLockset des_tid in
              man.sideg des_tid creation_lockset
            in
            TIDs.iter contribute_creation_lockset possibly_running_tids))
    | _ -> ()
  ;;

  module A = struct
    (** ego tid * lockset * inter-threaded lockset *)
    include Printable.Prod3 (TID) (LIDs) (G)

    let name () = "InterThreadedLockset"

    (** checks if [itls1] has a member ([tp1], [l]) such that [itls2] has a member ([tp2], [l]) with [tp1] != [tp2]
        @param itls1 inter-threaded lockset of first thread [t1]
        @param itls2 inter-threaded lockset of second thread [t2]
        @returns whether [t1] and [t2] must be running mutually exclusive
    *)
    let both_protected_inter_threaded itls1 itls2 =
      let itls2_has_same_lock_other_tid (tp1, l1) =
        G.exists (fun (tp2, l2) -> LID.equal l1 l2 && (not @@ TID.equal tp1 tp2)) itls2
      in
      G.exists itls2_has_same_lock_other_tid itls1
    ;;

    (** checks if [itls1] has a member ([tp1], [l1]) such that [l1] is in [ls2] and [tp1] != [t2]
        @param itls1 inter-threaded lockset of thread [t1] at first program point
        @param t2 thread id at second program point
        @param ls2 lockset at second program point
        @returns whether [t1] must be running mutually exclusive with second program point
    *)
    let one_protected_inter_threaded_other_intra_threaded itls1 t2 ls2 =
      G.exists (fun (tp1, l1) -> LIDs.mem l1 ls2 && (not @@ TID.equal tp1 t2)) itls1
    ;;

    let may_race (t1, ls1, itls1) (t2, ls2, itls2) =
      not
        (both_protected_inter_threaded itls1 itls2
         || one_protected_inter_threaded_other_intra_threaded itls1 t2 ls2
         || one_protected_inter_threaded_other_intra_threaded itls2 t1 ls1)
    ;;

    let should_print _ = true
  end

  let access man _ =
    let ask = Analyses.ask_of_man man in
    let tid_lifted = ask.f Queries.CurrentThreadId in
    match tid_lifted with
    | `Lifted tid ->
      let lockset = ask.f Queries.MustLockset in
      let creation_lockset = ask.f @@ Queries.MayCreationLockset tid in
      let tainted_creation_lockset = man.global tid in
      (* all values in creation lockset, but not in tainted creation lockset *)
      let inter_threaded_lockset = G.diff creation_lockset tainted_creation_lockset in
      tid, lockset, inter_threaded_lockset
    | _ -> ThreadIdDomain.UnknownThread, LIDs.empty (), G.empty ()
  ;;
end

let _ =
  MCP.register_analysis
    ~dep:[ "threadid"; "mutex"; "race"; "transitiveDescendants" ]
    (module CreationLocksetSpec : MCPSpec)
;;

let _ =
  MCP.register_analysis
    ~dep:
      [ "threadid"
      ; "mutex"
      ; "threadJoins"
      ; "race"
      ; "transitiveDescendants"
      ; "creationLockset"
      ]
    (module TaintedCreationLocksetSpec : MCPSpec)
;;
