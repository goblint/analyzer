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

  module G = SetDomain.Make (Printable.Prod (TID) (LID))
  (* 2^{T\times L}. TODO: Prod or ProdSimple? *)

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
         let may_created_tids = ask.f Queries.CreatedThreads in
         let must_joined_tids = ask.f Queries.MustJoinedThreads in
         let possibly_running_tids = TIDs.diff may_created_tids must_joined_tids in
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
end

let _ =
  MCP.register_analysis
    ~dep:[ "threadid"; "mutex" ]
    (module CreationLocksetSpec : MCPSpec)
;;

let _ =
  MCP.register_analysis
    ~dep:[ "threadid"; "mutex"; "threadJoins" ]
    (module TaintedCreationLocksetSpec : MCPSpec)
;;
