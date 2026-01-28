(** descendant lockset analysis [descendantLockset]
    analyzes a happened-before relationship related to thread creations with mutexes held.

    Enabling [creationLockset] may improve the precision of this analysis.

    @see https://github.com/goblint/analyzer/pull/1923
*)

open Analyses
module TID = ThreadIdDomain.Thread
module TIDs = ConcDomain.ThreadSet
module Lockset = LockDomain.MustLockset
module TidToLocksetMapTop = MapDomain.MapTop (TID) (Lockset)

module Spec = struct
  include IdentityUnitContextsSpec

  (** [{ t_d |-> L }]

      [t_d] was transitively created with all members of [L] held.
      Additionally, no member of [L] could have been unlocked after the creation of [t_d]
  *)
  module D = MapDomain.MapBot (TID) (Lockset)

  (** [{ t_0 |-> { t_d |-> L } }]

      [{ t_d |-> L }] is the descendant lockset valid for the [V] value,
      because [t_d] was created in [t_0] with the lockset being a superset of L.

      We suspect [MapBot] to suffice for the inner map. To ensure soundness, we use [MapTop] instead
  *)
  module G = MapDomain.MapBot (TID) (TidToLocksetMapTop)

  module V = struct
    include TID
    include StdV
  end

  let name () = "descendantLockset"
  let startstate _ = D.empty ()
  let exitstate _ = D.empty ()

  let threadspawn_contribute_globals man tid must_ancestor_descendants =
    let descendant_lockset = man.local in
    let contribute_for_descendant t_d =
      let creation_lockset = man.ask @@ Queries.CreationLockset t_d in
      let to_contribute =
        D.fold
          (fun t_l l_dl acc ->
             let l_cl = Queries.CL.find tid creation_lockset in
             let l_inter = Lockset.inter l_cl l_dl in
             TidToLocksetMapTop.add t_l l_inter acc)
          descendant_lockset
          (TidToLocksetMapTop.empty ())
      in
      man.sideg t_d (G.singleton tid to_contribute)
    in
    TIDs.iter contribute_for_descendant must_ancestor_descendants

  let threadspawn_compute_local_contribution man tid must_ancestor_descendants =
    let lockset = man.ask Queries.MustLockset in
    TIDs.fold
      (fun t_d -> D.join (D.singleton t_d lockset))
      must_ancestor_descendants
      man.local

  let threadspawn man ~multiple lval f args fman =
    let tid_lifted = man.ask Queries.CurrentThreadId in
    let child_tid_lifted = fman.ask Queries.CurrentThreadId in
    match tid_lifted, child_tid_lifted with
    | `Lifted tid, `Lifted child_tid when TID.must_be_ancestor tid child_tid ->
      let must_ancestor_descendants =
        ThreadDescendants.must_ancestor_descendants_closure (ask_of_man fman) child_tid
      in
      threadspawn_contribute_globals man tid must_ancestor_descendants;
      threadspawn_compute_local_contribution man tid must_ancestor_descendants
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
    module DlLhProd = Printable.Prod3 (D) (G) (Queries.LH)

    (** ego tid * (local descendant lockset * global descendant lockset * lock history) *)
    include Printable.Prod (TID) (DlLhProd)

    module type TidToLs = MapDomain.S with type key = TID.t and type value = Lockset.t

    (** checks if program point 1 must happen before program point 2
        @param (t1,dl1) thread id and descendant lockset of program point 1
        @param (t2,lh2) thread id and mustlock history of program point 2
        @param M module of [dl1]
    *)
    let happens_before (type t) (t1, dl1) (t2, lh2) (module M : TidToLs with type t = t) =
      let locks_held_creating_t2 = M.find t2 dl1 in
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

    (** checks if the entire execution of a thread must happen before a program point
        @param dlg1 glabal descendant lockset of the thread
        @param (t2,lh2) thread id and mustlock history of the program point
    *)
    let happens_before_global dlg1 (t2, lh2) =
      G.exists
        (fun t dl_map -> happens_before (t, dl_map) (t2, lh2) (module TidToLocksetMapTop))
        dlg1

    let may_race (t1, (dl1, dlg1, lh1)) (t2, (dl2, dlg2, lh2)) =
      not
        (happens_before (t1, dl1) (t2, lh2) (module D)
         || happens_before (t2, dl2) (t1, lh1) (module D)
         || happens_before_global dlg1 (t2, lh2)
         || happens_before_global dlg2 (t1, lh1))

    (* ego tid is already printed elsewhere *)
    let pretty () (_, dl_dlg_lh) = DlLhProd.pretty () dl_dlg_lh
    let show (_, dl_dlg_lh) = DlLhProd.show dl_dlg_lh
    let to_yojson (_, dl_dlg_lh) = DlLhProd.to_yojson dl_dlg_lh
    let printXml f (_, dl_dlg_lh) = DlLhProd.printXml f dl_dlg_lh

    let should_print (_, (dl, dlg, lh)) =
      let ls_not_empty _ ls = not @@ Lockset.is_empty ls in
      D.exists ls_not_empty dl
      || G.exists (fun _ -> TidToLocksetMapTop.exists ls_not_empty) dlg
      || Queries.LH.exists (fun l tids -> not @@ TIDs.is_empty tids) lh
  end

  let access man _ =
    let lh = man.ask Queries.MustlockHistory in
    let tid_lifted = man.ask Queries.CurrentThreadId in
    match tid_lifted with
    | `Lifted tid -> tid, (man.local, man.global tid, lh)
    | _ -> ThreadIdDomain.UnknownThread, (D.empty (), G.empty (), Queries.LH.empty ())
end

let _ =
  MCP.register_analysis
    ~dep:[ "threadid"; "mutex"; "threadJoins"; "threadDescendants"; "mustlockHistory" ]
    (module Spec : MCPSpec)
