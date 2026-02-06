(** descendant lockset analysis [descendantLockset]
    analyzes a happened-before relationship related to thread creations with mutexes held.

    Enabling [creationLockset] may improve the precision of this analysis.

    @see https://github.com/goblint/analyzer/pull/1923
*)

open Analyses
module TID = ThreadIdDomain.Thread
module TIDs = ConcDomain.ThreadSet
module Lockset = LockDomain.MustLockset

module Spec = struct
  include IdentityUnitContextsSpec

  (** [{ t_d |-> L }]

      [t_d] was transitively created with all members of [L] held.
      Additionally, no member of [L] could have been unlocked after the creation of [t_d]
  *)
  module D = MapDomain.MapBot (TID) (Lockset)

  module V = struct
    include TID
    include StdV
  end

  let name () = "descendantLockset"
  let startstate _ = D.empty ()
  let exitstate _ = D.empty ()

  let threadspawn man ~multiple lval f args fman =
    let tid_lifted = man.ask Queries.CurrentThreadId in
    let child_tid_lifted = fman.ask Queries.CurrentThreadId in
    match tid_lifted, child_tid_lifted with
    | `Lifted tid, `Lifted child_tid when TID.must_be_ancestor tid child_tid ->
      let must_ancestor_descendants =
        ThreadDescendants.must_ancestor_descendants_closure (ask_of_man fman) child_tid
      in
      let lockset = man.ask Queries.MustLockset in
      TIDs.fold
        (fun t_d -> D.join (D.singleton t_d lockset))
        must_ancestor_descendants
        man.local
    | _ -> man.local

  let unlock man possibly_running_tids lock =
    TIDs.fold
      (fun des_tid ->
         let old_value = D.find des_tid man.local in
         let new_value = Lockset.remove lock old_value in
         D.add des_tid new_value)
      possibly_running_tids
      (D.empty ())

  let unknown_unlock man possibly_running_tids =
    TIDs.fold
      (fun des_tid -> D.add des_tid (Lockset.empty ()))
      possibly_running_tids
      (D.empty ())

  let event man e _ =
    match e with
    | Events.Unlock addr ->
      let tid_lifted = man.ask Queries.CurrentThreadId in
      (match tid_lifted with
       | `Lifted tid ->
         let possibly_running_tids =
           ThreadDescendants.must_ancestor_running_descendants (ask_of_man man) tid
         in
         let lock_opt = LockDomain.MustLock.of_addr addr in
         (match lock_opt with
          | Some lock -> unlock man possibly_running_tids lock
          | None -> unknown_unlock man possibly_running_tids)
       | _ -> man.local)
    | _ -> man.local

  module A = struct
    module DlLhProd = Printable.Prod (D) (Queries.LH)

    (** ego tid * (local descendant lockset * lock history) *)
    include Printable.Prod (TID) (DlLhProd)

    (** checks if program point 1 must happen before program point 2
        @param (t1,dl1) thread id and descendant lockset of program point 1
        @param (t2,lh2) thread id and mustlock history of program point 2
        @param M module of [dl1]
    *)
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

    let may_race (t1, (dl1, lh1)) (t2, (dl2, lh2)) =
      not (happens_before (t1, dl1) (t2, lh2) || happens_before (t2, dl2) (t1, lh1))

    (* ego tid is already printed elsewhere *)
    let pretty () (_, dl_lh) = DlLhProd.pretty () dl_lh
    let show (_, dl_lh) = DlLhProd.show dl_lh
    let to_yojson (_, dl_lh) = DlLhProd.to_yojson dl_lh
    let printXml f (_, dl_lh) = DlLhProd.printXml f dl_lh

    let should_print (_, (dl, lh)) =
      let ls_not_empty _ ls = not @@ Lockset.is_empty ls in
      D.exists ls_not_empty dl
      || Queries.LH.exists (fun l tids -> not @@ TIDs.is_empty tids) lh
  end

  let access man _ =
    let lh = man.ask Queries.MustlockHistory in
    let tid_lifted = man.ask Queries.CurrentThreadId in
    match tid_lifted with
    | `Lifted tid -> tid, (man.local, lh)
    | _ -> ThreadIdDomain.UnknownThread, (D.empty (), Queries.LH.empty ())
end

let _ =
  MCP.register_analysis
    ~dep:[ "threadid"; "mutex"; "threadJoins"; "threadDescendants"; "mustlockHistory" ]
    (module Spec : MCPSpec)
