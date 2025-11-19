open Analyses
module LF = LibraryFunctions

(* TODO use ThreadLifted instead? Are Top or Bot relevant *)
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

  let contribute_lock man tid lock child_tid =
    man.sideg child_tid (G.singleton (tid, lock))
  ;;

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
end

(** 
    collects for each thread t_n pairs of must-ancestors and locks (t_0,l):
    when t_n or a must-ancestor t_1 of t_n was created, the parent t_0 must have held l.
    TODO: check if this requirement can be loosened
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
    | `Lifted tid, `Lifted child_tid ->
      let lockset = ask.f Queries.MustLockset in
      let to_contribute = cartesian_prod (TIDs.singleton tid) lockset in
      man.sideg child_tid to_contribute
      (* TODO also register for transitive descendants of t_1! *)
    | _ -> (* deal with top or bottom? *) ()
  ;;

  (* TODO: consider edge cases (most likely in creation lockset analysis)!
     - `ana.threads.include-node` is false. Two threads created with different locksets may have the same id that way!
     - child thread is not unique and thus could also ancestor of ego thread. In this case, it can also be created with a different lockset!
     - more? *)

  let query man (type a) (x : a Queries.t) : a Queries.result =
    match x with
    | Queries.MayCreationLockset ->
      let ask = Analyses.ask_of_man man in
      let tid_lifted = ask.f Queries.CurrentThreadId in
      (match tid_lifted with
       | `Lifted tid -> (man.global tid : G.t)
       | _ -> G.top ())
    | _ -> Queries.Result.top x
  ;;
end

module TaintedCreationLocksetSpec = struct
  include AncestorLocksetSpec

  let name () = "taintedCreationLockset"

  (** compute all threads that may run along with the ego thread at a program point
      @param ask ask of ego thread at the program point
  *)
  let get_possibly_running_tids (ask : Queries.ask) =
    let may_created_tids = ask.f Queries.CreatedThreads in
    (* TODO also consider transitive descendants of may_created_tids *)
    let must_joined_tids = ask.f Queries.MustJoinedThreads in
    TIDs.diff may_created_tids must_joined_tids
  ;;

  (** stolen from mutexGhost.ml. TODO Maybe add to library? *)
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
            TIDs.iter (contribute_lock man tid lock) possibly_running_tids
          | None ->
            (* TODO any lock could have been unlocked. Contribute for all possibly_running_tids their full CreationLocksets to invalidate them!! *)
            ()))
    | _ -> ()
  ;;

  let query man (type a) (x : a Queries.t) : a Queries.result =
    match x with
    | Queries.MayCreationLockset ->
      let ask = Analyses.ask_of_man man in
      let creation_lockset = ask.f Queries.MayCreationLockset in
      let tid_lifted = ask.f Queries.CurrentThreadId in
      (match tid_lifted with
       | `Lifted tid ->
         let tainted_creation_lockset = man.global tid in
         G.diff creation_lockset tainted_creation_lockset
       | _ -> G.top ())
    | _ -> Queries.Result.top x
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
      let creation_lockset = ask.f Queries.MayCreationLockset in
      let tainted_creation_lockset = man.global tid in
      (* all values in creation lockset, but not in tainted creation lockset *)
      let inter_threaded_lockset = G.diff creation_lockset tainted_creation_lockset in
      tid, lockset, inter_threaded_lockset
    | _ -> ThreadIdDomain.UnknownThread, LIDs.empty (), G.empty ()
  ;;
end

let _ =
  MCP.register_analysis
    ~dep:[ "threadid"; "mutex" ]
    (module CreationLocksetSpec : MCPSpec)
;;

let _ =
  MCP.register_analysis
    ~dep:[ "threadid"; "mutex"; "threadJoins"; "creationLockset" ]
    (module TaintedCreationLocksetSpec : MCPSpec)
;;
