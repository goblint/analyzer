open Analyses
open GoblintCil
module LF = LibraryFunctions
module TID = ThreadIdDomain.ThreadLifted
module LID = LockDomain.MustLock

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
    let tid = ask.f Queries.CurrentThreadId in
    let child_ask = Analyses.ask_of_man fman in
    let child_tid = child_ask.f Queries.CurrentThreadId in
    let lockset = ask.f Queries.MustLockset in
    (* contribution (t_1, l) to global of t_0 for all l in L: *)
    (* TODO also register for transitive descendants of t_1! *)
    let contribute_lock lock = man.sideg child_tid (G.singleton (tid, lock)) in
    LockDomain.MustLockset.iter contribute_lock lockset
  ;;
  (* TODO: consider edge cases (most likely in creation lockset analysis)!
     - `ana.threads.include-node` is false. Two threads created with different locksets may have the same id that way!
     - child thread is not unique and thus could also ancestor of ego thread. In this case, it can also be created with a different lockset!
     - more? *)
end

module TaintedCreationLocksetSpec = struct
  include AncestorLocksetSpec

  let name () = "taintedCreationLockset"
  let eval_exp_addr (a : Queries.ask) exp = a.f (Queries.MayPointTo exp)

  (** stolen from mutexGhost.ml. TODO Maybe add to library? *)
  let mustlock_of_addr (addr : LockDomain.Addr.t) : LockDomain.MustLock.t option =
    match addr with
    | Addr mv when LockDomain.Mval.is_definite mv -> Some (LockDomain.MustLock.of_mval mv)
    | _ -> None
  ;;

  let event man (e : Events.t) _ =
    match e with
    | Unlock addr ->
      let ask = Analyses.ask_of_man man in
      let tid = ask.f Queries.CurrentThreadId in
      let may_created_tids = ask.f Queries.CreatedThreads in
      let must_joined_tids = ask.f Queries.MustJoinedThreads in
      let possibly_running_tids =
        ConcDomain.ThreadSet.diff may_created_tids must_joined_tids
      in
      let contribute_tainted_lock lock child_tid =
        man.sideg child_tid (G.singleton (tid, lock))
      in
      let lock_opt = mustlock_of_addr addr in
      (match lock_opt with
       | Some lock ->
         (* contribute possibly_running_tids \times \{lock\} *)
         ConcDomain.ThreadSet.iter (contribute_tainted_lock lock) possibly_running_tids
       | None ->
         (* TODO any lock could have been unlocked. Contribute for all possibly_running_tids their full CreationLocksets to invalidate them!! *)
         ())
    | _ -> ()
  ;;
end

let _ =
  MCP.register_analysis
    ~dep:[ "threadid"; "mutex" ]
    (module CreationLocksetSpec : MCPSpec)
;;

let _ =
  MCP.register_analysis
    ~dep:[ "threadid"; "mutex" ]
    (module TaintedCreationLocksetSpec : MCPSpec)
;;
