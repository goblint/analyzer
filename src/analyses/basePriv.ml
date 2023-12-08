open Batteries
open GoblintCil
open Analyses
open GobConfig
open BaseUtil
module Q = Queries

module IdxDom = ValueDomain.IndexDomain

module VD     = BaseDomain.VD
module CPA    = BaseDomain.CPA
module BaseComponents = BaseDomain.BaseComponents

open CommonPriv


module type S =
sig
  module D: Lattice.S
  module G: Lattice.S
  module V: Printable.S

  val startstate: unit -> D.t

  val read_global: Q.ask -> (V.t -> G.t) -> BaseComponents (D).t -> varinfo -> VD.t

  (* [invariant]: Check if we should avoid producing a side-effect, such as updates to
   * the state when following conditional guards. *)
  val write_global: ?invariant:bool -> Q.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> BaseComponents (D).t -> varinfo -> VD.t -> BaseComponents (D).t

  val lock: Q.ask -> (V.t -> G.t) -> BaseComponents (D).t -> LockDomain.Addr.t -> BaseComponents (D).t
  val unlock: Q.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> BaseComponents (D).t -> LockDomain.Addr.t -> BaseComponents (D).t

  val sync: Q.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> BaseComponents (D).t -> [`Normal | `Join | `Return | `Init | `Thread] -> BaseComponents (D).t

  val escape: Q.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> BaseComponents (D).t -> EscapeDomain.EscapedVars.t -> BaseComponents (D).t
  val enter_multithreaded: Q.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> BaseComponents (D).t -> BaseComponents (D).t
  val threadenter: Q.ask -> BaseComponents (D).t -> BaseComponents (D).t
  val threadspawn: Q.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> BaseComponents (D).t -> BaseComponents (D).t
  val iter_sys_vars: (V.t -> G.t) -> VarQuery.t -> V.t VarQuery.f -> unit

  val thread_join: ?force:bool -> Q.ask -> (V.t -> G.t) -> Cil.exp -> BaseComponents (D).t -> BaseComponents (D).t
  val thread_return: Q.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> ThreadIdDomain.Thread.t -> BaseComponents (D).t -> BaseComponents (D).t

  val invariant_global: (V.t -> G.t) -> V.t -> Invariant.t
  val invariant_vars: Q.ask -> (V.t -> G.t) -> BaseComponents (D).t -> varinfo list

  val init: unit -> unit
  val finalize: unit -> unit
end

module NoFinalize =
struct
  let finalize () = ()
end

let old_threadenter (type d) ask (st: d BaseDomain.basecomponents_t) =
  (* Copy-paste from Base make_entry *)
  let globals = CPA.filter (fun k v -> is_global ask k) st.cpa in
  (* let new_cpa = if !earlyglobs || ThreadFlag.is_multi ctx.ask then CPA.filter (fun k v -> is_private ctx.ask ctx.local k) globals else globals in *)
  let new_cpa = globals in
  {st with cpa = new_cpa}

let startstate_threadenter (type d) (startstate: unit -> d) ask (st: d BaseDomain.basecomponents_t) =
  {st with cpa = CPA.bot (); priv = startstate ()}

(* No Privatization *)
module NonePriv: S =
struct
  include NoFinalize

  module G = BaseDomain.VD
  module V = VarinfoV
  module D = Lattice.Unit

  let init () = ()

  let startstate () = ()

  let lock ask getg st m = st
  let unlock ask getg sideg st m = st

  let escape ask getg sideg st escaped = st
  let enter_multithreaded ask getg sideg st = st
  let threadenter = old_threadenter
  let threadspawn ask getg sideg st = st

  let iter_sys_vars getg vq vf =
    match vq with
    | VarQuery.Global g -> vf g
    | _ -> ()


  let read_global ask getg (st: BaseComponents (D).t) x =
    getg x

  let write_global ?(invariant=false) ask getg sideg (st: BaseComponents (D).t) x v =
    if invariant then (
      (* Do not impose invariant, will not hold without privatization *)
      if M.tracing then M.tracel "set" ~var:x.vname "update_one_addr: BAD! effect = '%B', or else is private! \n" (not invariant);
      st
    )
    else (
      (* Here, an effect should be generated, but we add it to the local
      * state, waiting for the sync function to publish it. *)
      (* Copied from MainFunctor.update_variable *)
      if ((get_bool "exp.volatiles_are_top") && (is_always_unknown x)) then
        {st with cpa = CPA.add x (VD.top ()) st.cpa}
      else
        {st with cpa = CPA.add x v st.cpa}
    )

  let sync ask getg sideg (st: BaseComponents (D).t) reason =
    (* For each global variable, we create the side effect *)
    let side_var (v: varinfo) (value) (st: BaseComponents (D).t) =
      if M.tracing then M.traceli "globalize" ~var:v.vname "Tracing for %s\n" v.vname;
      let res =
        if is_global ask v then begin
          if M.tracing then M.tracec "globalize" "Publishing its value: %a\n" VD.pretty value;
          sideg v value;
          {st with cpa = CPA.remove v st.cpa}
        end else
          st
      in
      if M.tracing then M.traceu "globalize" "Done!\n";
      res
    in
    (* We fold over the local state, and side effect the globals *)
    CPA.fold side_var st.cpa st

  let thread_join ?(force=false) ask get e st = st
  let thread_return ask get set tid st = st

  let invariant_global getg g =
    ValueDomain.invariant_global getg g

  let invariant_vars ask getg st = []
end

module PerMutexPrivBase =
struct
  include NoFinalize
  include ConfCheck.RequireMutexActivatedInit
  include MutexGlobals
  include Protection

  module D = Lattice.Unit
  module G = CPA

  let startstate () = ()

  let get_m_with_mutex_inits ask getg m =
    let get_m = getg (V.mutex m) in
    let get_mutex_inits = getg V.mutex_inits in
    let is_in_Gm x _ = is_protected_by ask m x in
    let get_mutex_inits' = CPA.filter is_in_Gm get_mutex_inits in
    if M.tracing then M.tracel "priv" "get_m_with_mutex_inits %a:\n  get_m: %a\n  get_mutex_inits: %a\n  get_mutex_inits': %a\n" LockDomain.Addr.pretty m CPA.pretty get_m CPA.pretty get_mutex_inits CPA.pretty get_mutex_inits';
    CPA.join get_m get_mutex_inits'

  (** [get_m_with_mutex_inits] optimized for implementation-specialized [read_global]. *)
  let get_mutex_global_x_with_mutex_inits getg x =
    let get_mutex_global_x = getg (V.global x) in
    let get_mutex_inits = getg V.mutex_inits in
    match CPA.find_opt x get_mutex_global_x, CPA.find_opt x get_mutex_inits with
      | Some v1, Some v2 -> Some (VD.join v1 v2)
      | Some v, None
      | None, Some v -> Some v
      | None, None -> None

  let read_unprotected_global getg x =
    let get_mutex_global_x = get_mutex_global_x_with_mutex_inits getg x in
    (* None is VD.top () *)
    get_mutex_global_x |? VD.bot ()

  let escape ask getg sideg (st: BaseComponents (D).t) escaped =
    let escaped_cpa = CPA.filter (fun x _ -> EscapeDomain.EscapedVars.mem x escaped) st.cpa in
    sideg V.mutex_inits escaped_cpa;

    let cpa' = CPA.fold (fun x v acc ->
        if EscapeDomain.EscapedVars.mem x escaped (* && is_unprotected ask x *) then (
          if M.tracing then M.tracel "priv" "ESCAPE SIDE %a = %a\n" CilType.Varinfo.pretty x VD.pretty v;
          sideg (V.global x) (CPA.singleton x v);
          CPA.remove x acc
        )
        else
          acc
      ) st.cpa st.cpa
    in
    {st with cpa = cpa'}

  let enter_multithreaded ask getg sideg (st: BaseComponents (D).t) =
    let global_cpa = CPA.filter (fun x _ -> is_global ask x) st.cpa in
    sideg V.mutex_inits global_cpa;

    let cpa' = CPA.fold (fun x v acc ->
        if is_global ask x (* && is_unprotected ask x *) then (
          if M.tracing then M.tracel "priv" "enter_multithreaded remove %a\n" CilType.Varinfo.pretty x;
          if M.tracing then M.tracel "priv" "ENTER MULTITHREADED SIDE %a = %a\n" CilType.Varinfo.pretty x VD.pretty v;
          sideg (V.global x) (CPA.singleton x v);
          CPA.remove x acc
        )
        else
          acc
      ) st.cpa st.cpa
    in
    {st with cpa = cpa'}

  let threadenter = old_threadenter
  let threadspawn ask get set st = st

  let thread_join ?(force=false) ask get e st = st
  let thread_return ask get set tid st = st

  let invariant_global getg = function
    | `Right g' -> (* global *)
      ValueDomain.invariant_global (read_unprotected_global getg) g'
    | _ -> (* mutex *)
      Invariant.none

end

module PerMutexOplusPriv: S =
struct
  include PerMutexPrivBase

  let read_global ask getg (st: BaseComponents (D).t) x =
    if is_unprotected ask x then
      read_unprotected_global getg x
    else
      CPA.find x st.cpa
  (* let read_global ask getg cpa x =
     let (cpa', v) as r = read_global ask getg cpa x in
     ignore (Pretty.printf "READ GLOBAL %a (%a, %B) = %a\n" CilType.Varinfo.pretty x CilType.Location.pretty !Goblint_tracing.current_loc (is_unprotected ask x) VD.pretty v);
     r *)
  let write_global ?(invariant=false) ask getg sideg (st: BaseComponents (D).t) x v =
    let cpa' = CPA.add x v st.cpa in
    if not invariant then
      sideg (V.global x) (CPA.singleton x v);
    (* Unlock after invariant will still side effect refined value from CPA, because cannot distinguish from non-invariant write. *)
    {st with cpa = cpa'}
  (* let write_global ask getg sideg cpa x v =
     let cpa' = write_global ask getg sideg cpa x v in
     ignore (Pretty.printf "WRITE GLOBAL %a %a = %a\n" CilType.Varinfo.pretty x VD.pretty v CPA.pretty cpa');
     cpa' *)

  let lock ask getg (st: BaseComponents (D).t) m =
    if Locksets.(not (Lockset.mem m (current_lockset ask))) then (
      let get_m = get_m_with_mutex_inits ask getg m in
      (* Really we want is_unprotected, but pthread_cond_wait emits unlock-lock events,
         where our (necessary) original context still has the mutex,
         so the query would be on the wrong lockset.
         TODO: Fixing the event contexts is hard: https://github.com/goblint/analyzer/pull/487#discussion_r765905029.
         Therefore, just use _without to exclude the mutex we shouldn't have.
         In non-cond locks we don't have it anyway, so there's no difference.
         No other privatization uses is_unprotected, so this hack is only needed here. *)
      let is_in_V x _ = is_protected_by ask m x && is_unprotected_without ask x m in
      let cpa' = CPA.filter is_in_V get_m in
      if M.tracing then M.tracel "priv" "PerMutexOplusPriv.lock m=%a cpa'=%a\n" LockDomain.Addr.pretty m CPA.pretty cpa';
      {st with cpa = CPA.fold CPA.add cpa' st.cpa}
    )
    else
      st (* sound w.r.t. recursive lock *)

  let unlock ask getg sideg (st: BaseComponents (D).t) m =
    let is_in_Gm x _ = is_protected_by ask m x in
    let side_m_cpa = CPA.filter is_in_Gm st.cpa in
    if M.tracing then M.tracel "priv" "PerMutexOplusPriv.unlock m=%a side_m_cpa=%a\n" LockDomain.Addr.pretty m CPA.pretty side_m_cpa;
    sideg (V.mutex m) side_m_cpa;
    st

  let sync ask getg sideg (st: BaseComponents (D).t) reason =
    match reason with
    | `Join -> (* required for branched thread creation *)
      let global_cpa = CPA.filter (fun x _ -> is_global ask x && is_unprotected ask x) st.cpa in
      sideg V.mutex_inits global_cpa; (* must be like enter_multithreaded *)
      (* TODO: this makes mutex-oplus less precise in 28-race_reach/10-ptrmunge_racefree and 28-race_reach/trylock2_racefree, why? *)

      CPA.iter (fun x v ->
          (* TODO: is_unprotected - why breaks 02/11 init_mainfun? *)
          if is_global ask x && is_unprotected ask x then
            sideg (V.global x) (CPA.singleton x v)
        ) st.cpa;
      st
    | `Return
    | `Normal
    | `Init
    | `Thread ->
      st

  let invariant_vars ask getg st = protected_vars ask
end

module PerMutexMeetPrivBase =
struct
  include PerMutexPrivBase

  let invariant_vars ask getg (st: _ BaseDomain.basecomponents_t) =
    (* Mutex-meet local states contain precisely the protected global variables,
       so we can do fewer queries than {!protected_vars}. *)
    CPA.fold (fun x v acc ->
        if is_global ask x then
          x :: acc
        else
          acc
      ) st.cpa []
end

module PerMutexMeetPriv: S =
struct
  include PerMutexMeetPrivBase

  let read_global ask getg (st: BaseComponents (D).t) x =
    if is_unprotected ask x then (
      (* If the global is unprotected, all appropriate information should come via the appropriate globals, local value may be too small due to stale values surviving widening *)
      read_unprotected_global getg x
    )
    else
      CPA.find x st.cpa
  let read_global ask getg st x =
    let v = read_global ask getg st x in
    if M.tracing then M.tracel "priv" "READ GLOBAL %a %B %a = %a\n" CilType.Varinfo.pretty x (is_unprotected ask x) CPA.pretty st.cpa VD.pretty v;
    v
  let write_global ?(invariant=false) ask getg sideg (st: BaseComponents (D).t) x v =
    let cpa' =
      if is_unprotected ask x then
        st.cpa
      else
        CPA.add x v st.cpa
    in
    if not invariant then (
      if M.tracing then M.tracel "priv" "WRITE GLOBAL SIDE %a = %a\n" CilType.Varinfo.pretty x VD.pretty v;
      sideg (V.global x) (CPA.singleton x v)
      (* Unlock after invariant will still side effect refined value (if protected) from CPA, because cannot distinguish from non-invariant write. *)
    );
    {st with cpa = cpa'}
  (* let write_global ask getg sideg cpa x v =
     let cpa' = write_global ask getg sideg cpa x v in
     ignore (Pretty.printf "WRITE GLOBAL %a %a = %a\n" CilType.Varinfo.pretty x VD.pretty v CPA.pretty cpa');
     cpa' *)

  let lock (ask: Queries.ask) getg (st: BaseComponents (D).t) m =
    if Locksets.(not (Lockset.mem m (current_lockset ask))) then (
      let get_m = get_m_with_mutex_inits ask getg m in
      (* Additionally filter get_m in case it contains variables it no longer protects. *)
      let is_in_Gm x _ = is_protected_by ask m x in
      let get_m = CPA.filter is_in_Gm get_m in
      let long_meet m1 m2 = CPA.long_map2 VD.meet m1 m2 in
      let meet = long_meet st.cpa get_m in
      if M.tracing then M.tracel "priv" "LOCK %a:\n  get_m: %a\n  meet: %a\n" LockDomain.Addr.pretty m CPA.pretty get_m CPA.pretty meet;
      {st with cpa = meet}
    )
    else
      st (* sound w.r.t. recursive lock *)

  let unlock ask getg sideg (st: BaseComponents (D).t) m =
    let is_in_Gm x _ = is_protected_by ask m x in
    sideg (V.mutex m) (CPA.filter is_in_Gm st.cpa);
    let cpa' = CPA.fold (fun x v cpa ->
        if is_protected_by ask m x && is_unprotected_without ask x m then
          CPA.remove x cpa
          (* CPA.add x (VD.top ()) cpa *)
        else
          cpa
      ) st.cpa st.cpa
    in
    {st with cpa = cpa'}

  let sync ask getg sideg (st: BaseComponents (D).t) reason =
    match reason with
    | `Join -> (* required for branched thread creation *)
      let global_cpa = CPA.filter (fun x _ -> is_global ask x && is_unprotected ask x) st.cpa in
      sideg V.mutex_inits global_cpa; (* must be like enter_multithreaded *)

      let cpa' = CPA.fold (fun x v cpa ->
          if is_global ask x && is_unprotected ask x (* && not (VD.is_top v) *) then (
            if M.tracing then M.tracel "priv" "SYNC SIDE %a = %a\n" CilType.Varinfo.pretty x VD.pretty v;
            sideg (V.global x) (CPA.singleton x v);
            CPA.remove x cpa
          )
          else (
            if M.tracing then M.tracel "priv" "SYNC NOSIDE %a = %a\n" CilType.Varinfo.pretty x VD.pretty v;
            cpa
          )
        ) st.cpa st.cpa
      in
      {st with cpa = cpa'}
    | `Return
    | `Normal
    | `Init
    | `Thread ->
      st
end

module PerMutexMeetTIDPriv: S =
struct
  open Queries.Protection
  include PerMutexMeetPrivBase
  include PerMutexTidCommon(struct
      let exclude_not_started () = GobConfig.get_bool "ana.base.priv.not-started"
      let exclude_must_joined () = GobConfig.get_bool "ana.base.priv.must-joined"
    end)(CPA)

  let iter_sys_vars getg vq vf =
    match vq with
    | VarQuery.Global g -> vf (V.global g)
    | _ -> ()

  let long_meet m1 m2 = CPA.long_map2 VD.meet m1 m2

  let update_if_mem var value m =
    if CPA.mem var m then
      CPA.add var value m
    else
      m

  let get_mutex_global_g_with_mutex_inits inits ask getg g =
    let get_mutex_global_g = get_relevant_writes_nofilter ask @@ G.mutex @@ getg (V.global g) in
    let r = if not inits then
        get_mutex_global_g
      else
        let get_mutex_inits = merge_all @@ G.mutex @@ getg V.mutex_inits in
        let get_mutex_inits' = CPA.singleton g (CPA.find g get_mutex_inits) in
        CPA.join get_mutex_global_g get_mutex_inits'
    in
    r

  let get_relevant_writes (ask:Q.ask) m v =
    let current = ThreadId.get_current ask in
    let must_joined = ask.f Queries.MustJoinedThreads in
    let is_in_Gm x _ = is_protected_by ~protection:Weak ask m x in
    GMutex.fold (fun k v acc ->
        if compatible ask current must_joined k then
          CPA.join acc (CPA.filter is_in_Gm v)
        else
          acc
      ) v (CPA.bot ())

  let get_m_with_mutex_inits inits ask getg m =
    let get_m = get_relevant_writes ask m (G.mutex @@ getg (V.mutex m)) in
    let r =
      if not inits then
        get_m
      else
        let get_mutex_inits = merge_all @@ G.mutex @@ getg V.mutex_inits in
        let is_in_Gm x _ = is_protected_by ~protection:Weak ask m x in
        let get_mutex_inits' = CPA.filter is_in_Gm get_mutex_inits in
        CPA.join get_m get_mutex_inits'
    in
    r

  let read_global ask getg (st: BaseComponents (D).t) x =
    let _,lmust,l = st.priv in
    let lm = LLock.global x in
    let tmp = get_mutex_global_g_with_mutex_inits (not (LMust.mem lm lmust)) ask getg x in
    let local_m = BatOption.default (CPA.bot ()) (L.find_opt lm l) in
    if is_unprotected ask ~protection:Weak x then
      (* We can not rely upon the old value here, it may be too small due to reuse at widening points (and or nice bot/top confusion) in Base *)
      CPA.find x (CPA.join tmp local_m)
    else
      CPA.find x (long_meet st.cpa (CPA.join tmp local_m))

  let read_global ask getg st x =
    let v = read_global ask getg st x in
    if M.tracing then M.tracel "priv" "READ GLOBAL %a %B %a = %a\n" CilType.Varinfo.pretty x (is_unprotected ~protection:Weak ask x) CPA.pretty st.cpa VD.pretty v;
    v

  let write_global ?(invariant=false) ask getg sideg (st: BaseComponents (D).t) x v =
    let w,lmust,l = st.priv in
    let lm = LLock.global x in
    let cpa' =
      if is_unprotected ask ~protection:Weak x then
        st.cpa
      else
        CPA.add x v st.cpa
    in
    if M.tracing then M.tracel "priv" "WRITE GLOBAL SIDE %a = %a\n" CilType.Varinfo.pretty x VD.pretty v;
    let tid = ThreadId.get_current ask in
    let sidev = GMutex.singleton tid (CPA.singleton x v) in
    let l' = L.add lm (CPA.singleton x v) l in
    let is_recovered_st = ask.f (Queries.MustBeSingleThreaded {since_start = false}) && not @@ ask.f (Queries.MustBeSingleThreaded {since_start = true}) in
    let l' = if is_recovered_st then
        (* update value of local record for all where it appears *)
        L.map (update_if_mem x v) l'
      else
        l'
    in
    sideg (V.global x) (G.create_global sidev);
    {st with cpa = cpa'; priv = (W.add x w,LMust.add lm lmust,l')}

  let lock (ask: Queries.ask) getg (st: BaseComponents (D).t) m =
    if Locksets.(not (Lockset.mem m (current_lockset ask))) then (
      let _,lmust,l = st.priv in
      let lm = LLock.mutex m in
      let get_m = get_m_with_mutex_inits (not (LMust.mem lm lmust)) ask getg m in
      let local_m = BatOption.default (CPA.bot ()) (L.find_opt lm l) in
      let is_in_Gm x _ = is_protected_by ~protection:Weak ask m x in
      let local_m = CPA.filter is_in_Gm local_m in
      let r = CPA.join get_m local_m in
      let meet = long_meet st.cpa r in
      {st with cpa = meet}
    )
    else
      st (* sound w.r.t. recursive lock *)

  let unlock ask getg sideg (st: BaseComponents (D).t) m =
    let w,lmust,l = st.priv in
    let cpa' = CPA.fold (fun x v cpa ->
        if is_protected_by ~protection:Weak ask m x && is_unprotected_without ~protection:Weak ask x m then
          CPA.remove x cpa
        else
          cpa
      ) st.cpa st.cpa
    in
    let w' = W.filter (fun v -> not (is_unprotected_without ~protection:Weak ask v m)) w in
    let side_needed = W.exists (fun v -> is_protected_by ~protection:Weak ask m v) w in
    if not side_needed then
      {st with cpa = cpa'; priv = (w',lmust,l)}
    else
      let is_in_Gm x _ = is_protected_by ~protection:Weak ask m x in
      let tid = ThreadId.get_current ask in
      let sidev = GMutex.singleton tid (CPA.filter is_in_Gm st.cpa) in
      sideg (V.mutex m) (G.create_mutex sidev);
      let lm = LLock.mutex m in
      let l' = L.add lm (CPA.filter is_in_Gm st.cpa) l in
      {st with cpa = cpa'; priv = (w',LMust.add lm lmust,l')}

  let thread_join ?(force=false) (ask:Q.ask) getg exp (st: BaseComponents (D).t) =
    let w,lmust,l = st.priv in
    let tids = ask.f (Q.EvalThread exp) in
    if force then (
      if ConcDomain.ThreadSet.is_top tids then (
        M.info ~category:Unsound "Unknown thread ID assume-joined, privatization unsound"; (* TODO: something more sound *)
        st (* cannot find all thread IDs to join them all *)
      )
      else (
        (* fold throws if the thread set is top *)
        let tids' = ConcDomain.ThreadSet.diff tids (ask.f Q.MustJoinedThreads) in (* avoid unnecessary imprecision by force joining already must-joined threads, e.g. 46-apron2/04-other-assume-inprec *)
        let (lmust', l') = ConcDomain.ThreadSet.fold (fun tid (lmust, l) ->
            let lmust',l' = G.thread (getg (V.thread tid)) in
            (LMust.union lmust' lmust, L.join l l')
          ) tids' (lmust, l)
        in
        {st with priv = (w, lmust', l')}
      )
    )
    else (
      if ConcDomain.ThreadSet.is_top tids then 
        st
      else
        match ConcDomain.ThreadSet.elements tids with
        | [tid] ->
          let lmust',l' = G.thread (getg (V.thread tid)) in
          {st with priv = (w, LMust.union lmust' lmust, L.join l l')}
        | _ ->
          (* To match the paper more closely, one would have to join in the non-definite case too *)
          (* Given how we handle lmust (for initialization), doing this might actually be beneficial given that it grows lmust *)
          st
    )

  let thread_return ask getg sideg tid (st: BaseComponents (D).t) =
    let _,lmust,l = st.priv in
    sideg (V.thread tid) (G.create_thread (lmust,l));
    st

  let sync (ask:Q.ask) getg sideg (st: BaseComponents (D).t) reason =
    (* TODO: Is more needed here? *)
    st

  let escape ask getg sideg (st: BaseComponents (D).t) escaped =
    let escaped_cpa = CPA.filter (fun x _ -> EscapeDomain.EscapedVars.mem x escaped) st.cpa in
    let tid = ThreadId.get_current ask in
    let sidev = GMutex.singleton tid escaped_cpa in
    sideg V.mutex_inits (G.create_mutex sidev);
    let cpa' = CPA.fold (fun x v acc ->
        if EscapeDomain.EscapedVars.mem x escaped (* && is_unprotected ask x *) then (
          if M.tracing then M.tracel "priv" "ESCAPE SIDE %a = %a\n" CilType.Varinfo.pretty x VD.pretty v;
          let sidev = GMutex.singleton tid (CPA.singleton x v) in
          sideg (V.global x) (G.create_global sidev);
          CPA.remove x acc
        )
        else
          acc
      ) st.cpa st.cpa
    in
    {st with cpa = cpa'}

  let enter_multithreaded ask getg sideg (st: BaseComponents (D).t) =
    let cpa = st.cpa in
    let cpa_side = CPA.filter (fun x _ -> is_global ask x) cpa in
    let tid = ThreadId.get_current ask in
    let sidev = GMutex.singleton tid cpa_side in
    sideg V.mutex_inits (G.create_mutex sidev);
    (* Introduction into local state not needed, will be read via initializer *)
    (* Also no side-effect to mutex globals needed, the value here will either by read via the initializer, *)
    (* or it will be locally overwitten and in LMust in which case these values are irrelevant anyway *)
    let cpa_local = CPA.filter (fun x _ -> not @@ is_global ask x) cpa in
    {st with cpa= cpa_local }

  let threadenter ask (st: BaseComponents (D).t): BaseComponents (D).t =
    let _,lmust,l = st.priv in
    (* Thread starts without any mutexes, so the local state cannot contain any privatized things. The locals of the created thread are added later, *)
    (* so the cpa component of st is bot. *)
    {st with cpa = CPA.bot (); priv = (W.bot (),lmust,l)}

  let threadspawn (ask:Queries.ask) get set (st: BaseComponents (D).t) =
    let is_recovered_st = ask.f (Queries.MustBeSingleThreaded {since_start = false}) && not @@ ask.f (Queries.MustBeSingleThreaded {since_start = true}) in
    let unprotected_after x = ask.f (Q.MayBePublic {global=x; write=true; protection=Weak}) in
    if is_recovered_st then
      (* Remove all things that are now unprotected *)
      let cpa' = CPA.fold (fun x v cpa ->
          (* recoverable is false as after this, we will be multi-threaded *)
          if unprotected_after x then
            CPA.remove x cpa
          else
            cpa
        ) st.cpa st.cpa
      in
      {st with cpa = cpa'}
    else st

  let read_unprotected_global getg x =
    let get_mutex_global_x = merge_all @@ G.mutex @@ getg (V.global x) in
    let get_mutex_global_x' = CPA.find x get_mutex_global_x in
    let get_mutex_inits = merge_all @@ G.mutex @@ getg V.mutex_inits in
    let get_mutex_inits' = CPA.find x get_mutex_inits in
    VD.join get_mutex_global_x' get_mutex_inits'

  let invariant_global getg = function
    | `Middle  g -> (* global *)
      ValueDomain.invariant_global (read_unprotected_global getg) g
    | `Left _
    | `Right _ -> (* mutex or thread *)
      Invariant.none
end


module type PerGlobalPrivParam =
sig
  (** Whether to also check unprotectedness by reads for extra precision. *)
  val check_read_unprotected: bool
end

(** Protection-Based Reading. *)
module ProtectionBasedPriv (Param: PerGlobalPrivParam): S =
struct
  include NoFinalize
  include ConfCheck.RequireMutexActivatedInit
  open Protection

  module P =
  struct
    include MustVars
    let name () = "P"
  end
  (* W is implicitly represented by CPA domain *)
  module D = P

  module G = VD
  module VUnprot =
  struct
    include VarinfoV (* [g]' *)
    let name () = "unprotected"
    let show x = show x ^ ":unprotected" (* distinguishable variant names for html *)
    include Printable.SimpleShow (struct
        type nonrec t = t
        let show = show
      end)
  end
  module VProt =
  struct
    include VarinfoV (* [g] *)
    let name () = "protected"
    let show x = show x ^ ":protected" (* distinguishable variant names for html *)
    include Printable.SimpleShow (struct
        type nonrec t = t
        let show = show
      end)
  end
  module V =
  struct
    include Printable.Either (VUnprot) (VProt)
    let unprotected x = `Left x
    let protected x = `Right x
  end

  let startstate () = P.empty ()

  let read_global ask getg (st: BaseComponents (D).t) x =
    if P.mem x st.priv then
      CPA.find x st.cpa
    else if is_unprotected ask x then
      getg (V.unprotected x) (* CPA unnecessary because all values in GUnprot anyway *)
    else
      VD.join (CPA.find x st.cpa) (getg (V.protected x))

  let write_global ?(invariant=false) ask getg sideg (st: BaseComponents (D).t) x v =
    if not invariant then (
      sideg (V.unprotected x) v;
      if !earlyglobs then (* earlyglobs workaround for 13/60 *)
        sideg (V.protected x) v
        (* Unlock after invariant will still side effect refined value (if protected) from CPA, because cannot distinguish from non-invariant write since W is implicit. *)
    );
    if is_unprotected ask x then
      st
    else
      {st with cpa = CPA.add x v st.cpa; priv = P.add x st.priv}

  let lock ask getg st m = st

  let unlock ask getg sideg (st: BaseComponents (D).t) m =
    (* TODO: what about G_m globals in cpa that weren't actually written? *)
    CPA.fold (fun x v (st: BaseComponents (D).t) ->
        if is_protected_by ask m x then ( (* is_in_Gm *)
          (* Extra precision in implementation to pass tests:
             If global is read-protected by multiple locks,
             then inner unlock shouldn't yet publish. *)
          if not Param.check_read_unprotected || is_unprotected_without ask ~write:false x m then
            sideg (V.protected x) v;

          if is_unprotected_without ask x m then (* is_in_V' *)
            {st with cpa = CPA.remove x st.cpa; priv = P.remove x st.priv}
          else
            st
        )
        else
          st
      ) st.cpa st

  let sync ask getg sideg (st: BaseComponents (D).t) reason =
    match reason with
    | `Join -> (* required for branched thread creation *)
      CPA.fold (fun x v (st: BaseComponents (D).t) ->
          if is_global ask x && is_unprotected ask x then (
            sideg (V.unprotected x) v;
            sideg (V.protected x) v; (* must be like enter_multithreaded *)
            {st with cpa = CPA.remove x st.cpa; priv = P.remove x st.priv}
          )
          else
            st
        ) st.cpa st
    | `Return
    | `Normal
    | `Init
    | `Thread ->
      st

  let escape ask getg sideg (st: BaseComponents (D).t) escaped =
    let cpa' = CPA.fold (fun x v acc ->
        if EscapeDomain.EscapedVars.mem x escaped then (
          sideg (V.unprotected x) v;
          sideg (V.protected x) v;
          CPA.remove x acc
        )
        else
          acc
      ) st.cpa st.cpa
    in
    {st with cpa = cpa'}

  let enter_multithreaded ask getg sideg (st: BaseComponents (D).t) =
    CPA.fold (fun x v (st: BaseComponents (D).t) ->
        if is_global ask x then (
          sideg (V.unprotected x) v;
          sideg (V.protected x) v;
          {st with cpa = CPA.remove x st.cpa; priv = P.remove x st.priv}
        )
        else
          st
      ) st.cpa st

  let threadenter = startstate_threadenter startstate
  let threadspawn ask get set st = st

  let thread_join ?(force=false) ask get e st = st
  let thread_return ask get set tid st = st

  let iter_sys_vars getg vq vf =
    match vq with
    | VarQuery.Global g ->
      vf (V.unprotected g);
      vf (V.protected g);
    | _ -> ()

  let invariant_global getg g =
    match g with
    | `Left g' -> (* unprotected *)
      ValueDomain.invariant_global (fun g -> getg (V.unprotected g)) g'
    | `Right g -> (* protected *)
      Invariant.none

  let invariant_vars ask getg st = protected_vars ask
end

module AbstractLockCenteredGBase (WeakRange: Lattice.S) (SyncRange: Lattice.S) =
struct
  open Locksets

  module GWeak =
  struct
    include MapDomain.MapBot (Lockset) (WeakRange)
    let name () = "weak"
  end
  module GSync =
  struct
    include MapDomain.MapBot (Lockset) (SyncRange)
    let name () = "synchronized"
  end
  module G =
  struct
    (* weak: G -> (2^M -> WeakRange) *)
    (* sync: M -> (2^M -> SyncRange) *)
    include Lattice.Lift2 (GWeak) (GSync) (Printable.DefaultNames)

    let weak = function
      | `Bot -> GWeak.bot ()
      | `Lifted1 x -> x
      | _ -> failwith "AbstractLockCenteredGBase.weak"
    let sync = function
      | `Bot -> GSync.bot ()
      | `Lifted2 x -> x
      | _ -> failwith "AbstractLockCenteredGBase.sync"
    let create_weak weak = `Lifted1 weak
    let create_sync sync = `Lifted2 sync
  end
end

module type WeakRangeS =
sig
  include Lattice.S
  val fold_weak: (VD.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** Fold over all values represented by weak range. *)
end

module type SyncRangeS =
sig
  include Lattice.S
  val fold_sync_vars: (varinfo -> 'a -> 'a) -> t -> 'a -> 'a
  (** Fold over all variables represented by sync range. *)
end

module AbstractLockCenteredBase (WeakRange: WeakRangeS) (SyncRange: SyncRangeS) =
struct
  include AbstractLockCenteredGBase (WeakRange) (SyncRange)
  include MutexGlobals

  open Locksets

  let invariant_global getg = function
    | `Right g' -> (* global *)
      ValueDomain.invariant_global (fun x ->
          GWeak.fold (fun s' tm acc ->
              WeakRange.fold_weak VD.join tm acc
            ) (G.weak (getg (V.global x))) (VD.bot ())
        ) g'
    | _ -> (* mutex *)
      Invariant.none

  let invariant_vars ask getg st =
    let module VS = Set.Make (CilType.Varinfo) in
    let s = current_lockset ask in
    Lockset.fold (fun m acc ->
        GSync.fold (fun s' cpa' acc ->
            SyncRange.fold_sync_vars VS.add cpa' acc
          ) (G.sync (getg (V.mutex m))) acc
      ) s VS.empty
    |> VS.elements
end

module LockCenteredBase =
struct
  module VD =
  struct
    include VD

    let fold_weak f v a = f v a
  end

  module CPA =
  struct
    include CPA

    let fold_sync_vars f m a = fold (fun k _ a -> f k a) m a
  end

  (* weak: G -> (2^M -> D) *)
  (* sync: M -> (2^M -> (G -> D)) *)
  include AbstractLockCenteredBase (VD) (CPA)
end

module MinePrivBase =
struct
  include NoFinalize
  include ConfCheck.RequireMutexPathSensOneMainInit
  include MutexGlobals (* explicit not needed here because G is Prod anyway? *)

  let thread_join ?(force=false) ask get e st = st
  let thread_return ask get set tid st = st
  let threadspawn ask get set st = st
end

module MineNaivePrivBase =
struct
  include MinePrivBase

  module D = Lattice.Unit

  let startstate () = ()
  let escape ask getg sideg st escaped = st
  let enter_multithreaded ask getg sideg (st: BaseComponents (D).t) = st
  let threadenter = old_threadenter
end

module MinePriv: S =
struct
  include MineNaivePrivBase
  open Locksets

  module Thread = ThreadIdDomain.Thread
  module ThreadMap =
  struct
    include MapDomain.MapBot (Thread) (VD)

    let fold_weak f m a = fold (fun _ v a -> f v a) m a
  end

  (* weak: G -> (2^M -> (T -> D)) *)
  (* sync: M -> (2^M -> (G -> D)) *)
  include AbstractLockCenteredBase (ThreadMap) (LockCenteredBase.CPA)

  let global_init_thread = RichVarinfo.single ~name:"global_init"
  let current_thread (ask: Q.ask): Thread.t =
    if !AnalysisState.global_initialization then
      ThreadIdDomain.Thread.threadinit (global_init_thread ()) ~multiple:false
    else
      ThreadId.get_current_unlift ask

  let read_global ask getg (st: BaseComponents (D).t) x =
    let s = current_lockset ask in
    GWeak.fold (fun s' tm acc ->
        if Lockset.disjoint s s' then
          ThreadMap.fold (fun t' v acc ->
              VD.join v acc
            ) tm acc
        else
          acc
      ) (G.weak (getg (V.global x))) (CPA.find x st.cpa)

  let write_global ?(invariant=false) ask getg sideg (st: BaseComponents (D).t) x v =
    let s = current_lockset ask in
    let t = current_thread ask in
    let cpa' = CPA.add x v st.cpa in
    if not invariant && not (!earlyglobs && is_excluded_from_earlyglobs x) then
      sideg (V.global x) (G.create_weak (GWeak.singleton s (ThreadMap.singleton t v)));
    (* Unlock after invariant will not side effect refined value from weak, because it's not side effected there. *)
    {st with cpa = cpa'}

  let lock ask getg (st: BaseComponents (D).t) m =
    let s = current_lockset ask in
    let cpa' = GSync.fold (fun s' cpa' acc ->
        if Lockset.disjoint s s' then
          CPA.join cpa' acc
        else
          acc
      ) (G.sync (getg (V.mutex m))) st.cpa
    in
    {st with cpa = cpa'}

  let unlock ask getg sideg (st: BaseComponents (D).t) m =
    let s = Lockset.remove m (current_lockset ask) in
    let t = current_thread ask in
    let side_cpa = CPA.filter (fun x _ ->
        GWeak.fold (fun s' tm acc ->
            (* TODO: swap 2^M and T partitioning for lookup by t here first? *)
            let v = ThreadMap.find t tm in
            (Lockset.mem m s' && not (VD.is_bot v)) || acc
          ) (G.weak (getg (V.global x))) false
      ) st.cpa
    in
    sideg (V.mutex m) (G.create_sync (GSync.singleton s side_cpa));
    st

  let sync ask getg sideg (st: BaseComponents (D).t) reason =
    match reason with
    | `Return
    | `Normal
    | `Join (* TODO: no problem with branched thread creation here? *)
    | `Init
    | `Thread ->
      st
end

module MineNoThreadPriv: S =
struct
  include MineNaivePrivBase
  include LockCenteredBase
  open Locksets

  let read_global ask getg (st: BaseComponents (D).t) x =
    let s = current_lockset ask in
    GWeak.fold (fun s' v acc ->
        if Lockset.disjoint s s' then
          VD.join v acc
        else
          acc
      ) (G.weak (getg (V.global x))) (CPA.find x st.cpa)

  let write_global ?(invariant=false) ask getg sideg (st: BaseComponents (D).t) x v =
    let s = current_lockset ask in
    let cpa' = CPA.add x v st.cpa in
    if not invariant && not (!earlyglobs && is_excluded_from_earlyglobs x) then
      sideg (V.global x) (G.create_weak (GWeak.singleton s v));
    (* Unlock after invariant will not side effect refined value from weak, because it's not side effected there. *)
    {st with cpa = cpa'}

  let lock ask getg (st: BaseComponents (D).t) m =
    let s = current_lockset ask in
    let cpa' = GSync.fold (fun s' cpa' acc ->
        if Lockset.disjoint s s' then
          CPA.join cpa' acc
        else
          acc
      ) (G.sync (getg (V.mutex m))) st.cpa
    in
    {st with cpa = cpa'}

  let unlock ask getg sideg (st: BaseComponents (D).t) m =
    let s = Lockset.remove m (current_lockset ask) in
    let side_cpa = CPA.filter (fun x _ ->
        GWeak.fold (fun s' v acc ->
            (Lockset.mem m s' && not (VD.is_bot v)) || acc
          ) (G.weak (getg (V.global x))) false
      ) st.cpa
    in
    sideg (V.mutex m) (G.create_sync (GSync.singleton s side_cpa));
    st

  let sync ask getg sideg (st: BaseComponents (D).t) reason =
    match reason with
    | `Return
    | `Normal
    | `Join (* TODO: no problem with branched thread creation here? *)
    | `Init
    | `Thread ->
      st
end

module type MineWPrivParam =
sig
  (** Whether to side effect global inits to match our traces paper scenario. *)
  val side_effect_global_init: bool
end

(** Interference-Based Reading? Side-effecting Mine using W set. *)
module MineWPriv (Param: MineWPrivParam): S =
struct
  include MinePrivBase
  include LockCenteredBase
  open Locksets

  module W =
  struct
    include SetDomain.ToppedSet (Basetype.Variables) (struct let topname = "All variables" end)
    let name () = "W"
  end
  module D = W

  let startstate () = W.empty ()

  let read_global ask getg (st: BaseComponents (D).t) x =
    let s = current_lockset ask in
    GWeak.fold (fun s' v acc ->
        if Lockset.disjoint s s' then
          VD.join v acc
        else
          acc
      ) (G.weak (getg (V.global x))) (CPA.find x st.cpa)

  let write_global ?(invariant=false) ask getg sideg (st: BaseComponents (D).t) x v =
    let s = current_lockset ask in
    let cpa' = CPA.add x v st.cpa in
    if not invariant && not (!earlyglobs && is_excluded_from_earlyglobs x) then
      sideg (V.global x) (G.create_weak (GWeak.singleton s v));
    let w' = if not invariant then
        W.add x st.priv
      else
        st.priv (* No need to add invariant to W because it doesn't matter for reads after invariant, only unlocks. *)
    in
    {st with cpa = cpa'; priv = w'}

  let lock ask getg (st: BaseComponents (D).t) m =
    let s = current_lockset ask in
    let cpa' = GSync.fold (fun s' cpa' acc ->
        if Lockset.disjoint s s' then
          CPA.join cpa' acc
        else
          acc
      ) (G.sync (getg (V.mutex m))) st.cpa
    in
    {st with cpa = cpa'}

  let unlock ask getg sideg (st: BaseComponents (D).t) m =
    let s = Lockset.remove m (current_lockset ask) in
    let is_in_W x _ = W.mem x st.priv in
    let side_cpa = CPA.filter is_in_W st.cpa in
    sideg (V.mutex m) (G.create_sync (GSync.singleton s side_cpa));
    st

  let sync ask getg sideg (st: BaseComponents (D).t) reason =
    match reason with
    | `Return
    | `Normal
    | `Join (* TODO: no problem with branched thread creation here? *)
    | `Init
    | `Thread ->
      st

  let escape ask getg sideg st escaped = st (* TODO: do something here when side_effect_global_init? *)
  let enter_multithreaded ask getg sideg (st: BaseComponents (D).t) =
    if Param.side_effect_global_init then (
      CPA.fold (fun x v (st: BaseComponents (D).t) ->
          if is_global ask x then (
            sideg (V.global x) (G.create_weak (GWeak.singleton (Lockset.empty ()) v));
            {st with priv = W.add x st.priv} (* TODO: is this add necessary? *)
          )
          else
            st
        ) st.cpa st
    )
    else
      st

  let threadenter =
    if Param.side_effect_global_init then
      startstate_threadenter startstate
    else
      old_threadenter
end

module LockCenteredD =
struct
  open Locksets

  module DV =
  struct
    include MapDomain.MapBot_LiftTop (Lock) (MustVars)
    let name () = "V"
  end

  module L =
  struct
    include MapDomain.MapBot_LiftTop (Lock) (MinLocksets)
    let name () = "L"
  end
end

(** Lock-Centered Reading. *)
module LockCenteredPriv: S =
struct
  include MinePrivBase
  include LockCenteredBase
  open Locksets

  open LockCenteredD
  module D = Lattice.Prod (DV) (L)

  let startstate () = (DV.bot (), L.bot ())

  let lockset_init = Lockset.top ()

  let distr_init getg x v =
    if get_bool "exp.priv-distr-init" then
      let v_init = GWeak.find lockset_init (G.weak (getg (V.global x))) in
      VD.join v v_init
    else
      v

  let read_global ask getg (st: BaseComponents (D).t) x =
    let s = current_lockset ask in
    let (vv, l) = st.priv in
    let d_cpa = CPA.find x st.cpa in
    let d_sync = L.fold (fun m bs acc ->
        if not (MustVars.mem x (DV.find m vv)) then
          let syncs = G.sync (getg (V.mutex m)) in
          MinLocksets.fold (fun b acc ->
              GSync.fold (fun s' cpa' acc ->
                  if Lockset.disjoint b s' then
                    let v = CPA.find x cpa' in
                    VD.join v acc
                  else
                    acc
                ) syncs acc
            ) bs acc
        else
          acc
      ) l (VD.bot ())
    in
    let weaks = G.weak (getg (V.global x)) in
    let d_weak = GWeak.fold (fun s' v acc ->
        if Lockset.disjoint s s' then
          VD.join v acc
        else
          acc
      ) weaks (VD.bot ())
    in
    let d_init =
      if DV.exists (fun m cached -> MustVars.mem x cached) vv then
        VD.bot ()
      else
        GWeak.find lockset_init weaks
    in
    if M.tracing then M.trace "priv" "d_cpa: %a\n" VD.pretty d_cpa;
    if M.tracing then M.trace "priv" "d_sync: %a\n" VD.pretty d_sync;
    if M.tracing then M.trace "priv" "d_weak: %a\n" VD.pretty d_weak;
    if M.tracing then M.trace "priv" "d_init: %a\n" VD.pretty d_init;
    let d_weak = VD.join d_weak d_init in
    let d = VD.join d_cpa (VD.join d_sync d_weak) in
    d

  let write_global ?(invariant=false) ask getg sideg (st: BaseComponents (D).t) x v =
    let s = current_lockset ask in
    let (vv, l) = st.priv in
    let v' = L.fold (fun m _ acc ->
        DV.add m (MustVars.add x (DV.find m acc)) acc
      ) l vv
    in
    let cpa' = CPA.add x v st.cpa in
    if not invariant && not (!earlyglobs && is_excluded_from_earlyglobs x) then (
      let v = distr_init getg x v in
      sideg (V.global x) (G.create_weak (GWeak.singleton s v))
      (* Unlock after invariant will still side effect refined value from CPA, because cannot distinguish from non-invariant write. *)
    );
    {st with cpa = cpa'; priv = (v', l)}

  let lock ask getg (st: BaseComponents (D).t) m =
    let s = current_lockset ask in
    let (v, l) = st.priv in
    let v' = DV.add m (MustVars.empty ()) v in
    let l' = L.add m (MinLocksets.singleton s) l in
    {st with priv = (v', l')}

  let unlock ask getg sideg (st: BaseComponents (D).t) m =
    let s = Lockset.remove m (current_lockset ask) in
    let is_in_G x _ = is_global ask x in
    let side_cpa = CPA.filter is_in_G st.cpa in
    let side_cpa = CPA.mapi (fun x v ->
        let v = distr_init getg x v in
        v
      ) side_cpa
    in
    sideg (V.mutex m) (G.create_sync (GSync.singleton s side_cpa));
    (* m stays in v, l *)
    st

  let sync ask getg sideg (st: BaseComponents (D).t) reason =
    match reason with
    | `Return
    | `Normal
    | `Join (* TODO: no problem with branched thread creation here? *)
    | `Init
    | `Thread ->
      st

  let escape ask getg sideg (st: BaseComponents (D).t) escaped =
    let cpa' = CPA.fold (fun x v acc ->
        if EscapeDomain.EscapedVars.mem x escaped then (
          sideg (V.global x) (G.create_weak (GWeak.singleton lockset_init v));
          CPA.remove x acc
        )
        else
          acc
      ) st.cpa st.cpa
    in
    {st with cpa = cpa'}

  let enter_multithreaded ask getg sideg (st: BaseComponents (D).t) =
    CPA.fold (fun x v (st: BaseComponents (D).t) ->
        if is_global ask x then (
          sideg (V.global x) (G.create_weak (GWeak.singleton lockset_init v));
          {st with cpa = CPA.remove x st.cpa}
        )
        else
          st
      ) st.cpa st

  let threadenter = startstate_threadenter startstate
end

module WriteCenteredBase =
struct
  open Locksets

  module GWeakW =
  struct
    include MapDomain.MapBot (Lockset) (VD)

    let fold_weak f m a = fold (fun _ v a -> f v a) m a
  end
  module GSyncW =
  struct
    include MapDomain.MapBot (Lockset) (LockCenteredBase.CPA)

    let fold_sync_vars f m a =
      fold (fun _ cpa a ->
          LockCenteredBase.CPA.fold_sync_vars f cpa a
        ) m a
  end

  (* weak: G -> (S:2^M -> (W:2^M -> D)) *)
  (* sync: M -> (S:2^M -> (W:2^M -> (G -> D))) *)
  include AbstractLockCenteredBase (GWeakW) (GSyncW)
end

(** Write-Centered Reading. *)
module WriteCenteredPriv: S =
struct
  include MinePrivBase
  include WriteCenteredBase
  open Locksets

  open WriteCenteredD
  module D = Lattice.Prod (W) (P)

  let startstate () = (W.bot (), P.top ())

  let lockset_init = Lockset.top ()

  let distr_init getg x v =
    if get_bool "exp.priv-distr-init" then
      let v_init = GWeakW.find lockset_init (GWeak.find (Lockset.empty ()) (G.weak (getg (V.global x)))) in
      VD.join v v_init
    else
      v

  let read_global ask getg (st: BaseComponents (D).t) x =
    let s = current_lockset ask in
    let (w, p) = st.priv in
    let p_x = P.find x p in
    let d_cpa = CPA.find x st.cpa in
    let d_sync = Lockset.fold (fun m acc ->
        if MinLocksets.exists (fun s''' -> not (Lockset.mem m s''')) p_x then
          let syncs = G.sync (getg (V.mutex m)) in
          GSync.fold (fun s' gsyncw' acc ->
              if Lockset.disjoint s s' then
                GSyncW.fold (fun w' cpa' acc ->
                    if MinLocksets.exists (fun s'' -> Lockset.disjoint s'' w') p_x then
                      let v = CPA.find x cpa' in
                      VD.join v acc
                    else
                      acc
                  ) gsyncw' acc
              else
                acc
            ) syncs acc
        else
          acc
      ) s (VD.bot ())
    in
    let weaks = G.weak (getg (V.global x)) in
    let d_weak = GWeak.fold (fun s' gweakw' acc ->
        if Lockset.disjoint s s' then
          GWeakW.fold (fun w' v acc ->
              if MinLocksets.exists (fun s'' -> Lockset.disjoint s'' w') p_x then
                VD.join v acc
              else
                acc
            ) gweakw' acc
        else
          acc
      ) weaks (VD.bot ())
    in
    if M.tracing then M.trace "priv" "d_cpa: %a\n" VD.pretty d_cpa;
    if M.tracing then M.trace "priv" "d_sync: %a\n" VD.pretty d_sync;
    if M.tracing then M.trace "priv" "d_weak: %a\n" VD.pretty d_weak;
    let d = VD.join d_cpa (VD.join d_sync d_weak) in
    d

  let write_global ?(invariant=false) ask getg sideg (st: BaseComponents (D).t) x v =
    let s = current_lockset ask in
    let (w, p) = st.priv in
    let w' = if not invariant then
        W.add x (MinLocksets.singleton s) w
      else
        w (* No need to add invariant to W because it doesn't matter for reads after invariant, only unlocks. *)
    in
    let p' = P.add x (MinLocksets.singleton s) p in
    let p' = P.map (fun s' -> MinLocksets.add s s') p' in
    let cpa' = CPA.add x v st.cpa in
    if not invariant && not (!earlyglobs && is_excluded_from_earlyglobs x) then (
      let v = distr_init getg x v in
      sideg (V.global x) (G.create_weak (GWeak.singleton s (GWeakW.singleton s v)))
    );
    (* TODO: publish all g under M_g? *)
    {st with cpa = cpa'; priv = (w', p')}

  let lock ask getg (st: BaseComponents (D).t) m = st

  let unlock ask getg sideg (st: BaseComponents (D).t) m =
    let s = Lockset.remove m (current_lockset ask) in
    let (w, p) = st.priv in
    let p' = P.map (fun s' -> MinLocksets.add s s') p in
    if M.tracing then M.traceli "priv" "unlock %a %a\n" Lock.pretty m CPA.pretty st.cpa;
    let side_gsyncw = CPA.fold (fun x v acc ->
        if is_global ask x then (
          let w_x = W.find x w in
          if M.tracing then M.trace "priv" "gsyncw %a %a %a\n" CilType.Varinfo.pretty x VD.pretty v MinLocksets.pretty w_x;
          MinLocksets.fold (fun w acc ->
              let v = distr_init getg x v in
              GSyncW.add w (CPA.add x v (GSyncW.find w acc)) acc
            ) w_x acc
        ) else
          acc
      ) st.cpa (GSyncW.bot ())
    in
    if M.tracing then M.traceu "priv" "unlock %a %a\n" Lock.pretty m GSyncW.pretty side_gsyncw;
    sideg (V.mutex m) (G.create_sync (GSync.singleton s side_gsyncw));
    {st with priv = (w, p')}

  let sync ask getg sideg (st: BaseComponents (D).t) reason =
    match reason with
    | `Return
    | `Normal
    | `Join (* TODO: no problem with branched thread creation here? *)
    | `Init
    | `Thread ->
      st

  let escape ask getg sideg (st: BaseComponents (D).t) escaped =
    let s = current_lockset ask in
    CPA.fold (fun x v acc ->
        if EscapeDomain.EscapedVars.mem x escaped then (
          let (w, p) = st.priv in
          let p' = P.add x (MinLocksets.singleton s) p in
          sideg (V.global x) (G.create_weak (GWeak.singleton (Lockset.empty ()) (GWeakW.singleton lockset_init v)));
          {st with cpa = CPA.remove x st.cpa; priv = (w, p')}
        )
        else
          st
      ) st.cpa st

  let enter_multithreaded ask getg sideg (st: BaseComponents (D).t) =
    CPA.fold (fun x v (st: BaseComponents (D).t) ->
        if is_global ask x then (
          sideg (V.global x) (G.create_weak (GWeak.singleton (Lockset.empty ()) (GWeakW.singleton lockset_init v)));
          {st with cpa = CPA.remove x st.cpa}
        )
        else
          st
      ) st.cpa st

  let threadenter = startstate_threadenter startstate
end

(** Write-Centered Reading and Lock-Centered Reading combined. *)
module WriteAndLockCenteredPriv: S =
struct
  include MinePrivBase
  include WriteCenteredBase
  open Locksets

  open LockCenteredD
  open WriteCenteredD
  module D = Lattice.Prod (Lattice.Prod (W) (P)) (Lattice.Prod (DV) (L))

  let startstate () = ((W.bot (), P.top ()), (DV.bot (), L.bot ()))

  let lockset_init = Lockset.top ()

  let distr_init getg x v =
    if get_bool "exp.priv-distr-init" then
      let v_init = GWeakW.find lockset_init (GWeak.find (Lockset.empty ()) (G.weak (getg (V.global x)))) in
      VD.join v v_init
    else
      v

  let read_global ask getg (st: BaseComponents (D).t) x =
    let s = current_lockset ask in
    let ((w, p), (vv, l)) = st.priv in
    let p_x = P.find x p in
    let d_cpa = CPA.find x st.cpa in
    let d_m_sync = L.fold (fun m bs acc ->
        if not (MustVars.mem x (DV.find m vv)) then
          let syncs = G.sync (getg (V.mutex m)) in
          MinLocksets.fold (fun b acc ->
              GSync.fold (fun s' gsyncw' acc ->
                  if Lockset.disjoint b s' then
                    GSyncW.fold (fun w' cpa' acc ->
                        if MinLocksets.exists (fun s'' -> Lockset.disjoint s'' w') p_x then
                          let v = CPA.find x cpa' in
                          VD.join v acc
                        else
                          acc
                      ) gsyncw' acc
                  else
                    acc
                ) syncs acc
            ) bs acc
        else
          acc
      ) l (VD.bot ())
    in
    let weaks = G.weak (getg (V.global x)) in
    let d_m_weak = GWeak.fold (fun s' gweakw' acc ->
        if Lockset.disjoint s s' then
          GWeakW.fold (fun w' v acc ->
              if MinLocksets.exists (fun s'' -> Lockset.disjoint s'' w') p_x then
                VD.join v acc
              else
                acc
            ) gweakw' acc
        else
          acc
      ) weaks (VD.bot ())
    in
    let d_m = VD.join d_m_sync d_m_weak in
    let d_g_sync = Lockset.fold (fun m acc ->
        if MinLocksets.exists (fun s''' -> not (Lockset.mem m s''')) p_x then
          let syncs = G.sync (getg (V.mutex m)) in
          GSync.fold (fun s' gsyncw' acc ->
              if Lockset.disjoint s s' then
                GSyncW.fold (fun w' cpa' acc ->
                    if MinLocksets.exists (fun s'' -> Lockset.disjoint s'' w') p_x then
                      let v = CPA.find x cpa' in
                      VD.join v acc
                    else
                      acc
                  ) gsyncw' acc
              else
                acc
            ) syncs acc
        else
          acc
      ) s (VD.bot ())
    in
    let d_g_weak = d_m_weak in (* happen to coincide *)
    let d_g = VD.join d_g_sync d_g_weak in
    let d = VD.join d_cpa (VD.meet d_m d_g) in
    d

  let write_global ?(invariant=false) ask getg sideg (st: BaseComponents (D).t) x v =
    let s = current_lockset ask in
    let ((w, p), (vv, l)) = st.priv in
    let w' = if not invariant then
        W.add x (MinLocksets.singleton s) w
      else
        w (* No need to add invariant to W because it doesn't matter for reads after invariant, only unlocks. *)
    in
    let p' = P.add x (MinLocksets.singleton s) p in
    let p' = P.map (fun s' -> MinLocksets.add s s') p' in
    let v' = L.fold (fun m _ acc ->
        DV.add m (MustVars.add x (DV.find m acc)) acc
      ) l vv
    in
    let cpa' = CPA.add x v st.cpa in
    if not invariant && not (!earlyglobs && is_excluded_from_earlyglobs x) then (
      let v = distr_init getg x v in
      sideg (V.global x) (G.create_weak (GWeak.singleton s (GWeakW.singleton s v)))
    );
    (* TODO: publish all g under M_g? *)
    {st with cpa = cpa'; priv = ((w', p'), (v', l))}

  let lock ask getg (st: BaseComponents (D).t) m =
    let s = current_lockset ask in
    let (wp, (v, l)) = st.priv in
    let v' = DV.add m (MustVars.empty ()) v in
    let l' = L.add m (MinLocksets.singleton s) l in
    {st with priv = (wp, (v', l'))}

  let unlock ask getg sideg (st: BaseComponents (D).t) m =
    let s = Lockset.remove m (current_lockset ask) in
    let ((w, p), vl) = st.priv in
    let p' = P.map (fun s' -> MinLocksets.add s s') p in
    let side_gsyncw = CPA.fold (fun x v acc ->
        if is_global ask x then
          MinLocksets.fold (fun w acc ->
              let v = distr_init getg x v in
              GSyncW.add w (CPA.add x v (GSyncW.find w acc)) acc
            ) (W.find x w) acc
        else
          acc
      ) st.cpa (GSyncW.bot ())
    in
    sideg (V.mutex m) (G.create_sync (GSync.singleton s side_gsyncw));
    (* m stays in v, l *)
    {st with priv = ((w, p'), vl)}

  let sync ask getg sideg (st: BaseComponents (D).t) reason =
    match reason with
    | `Return
    | `Normal
    | `Join (* TODO: no problem with branched thread creation here? *)
    | `Init
    | `Thread ->
      st

  let escape ask getg sideg (st: BaseComponents (D).t) escaped =
    let s = current_lockset ask in
    CPA.fold (fun x v acc ->
        if EscapeDomain.EscapedVars.mem x escaped then (
          let ((w, p), (vv, l)) = st.priv in
          let p' = P.add x (MinLocksets.singleton s) p in
          sideg (V.global x) (G.create_weak (GWeak.singleton (Lockset.empty ()) (GWeakW.singleton lockset_init v)));
          {st with cpa = CPA.remove x st.cpa; priv = ((w, p'), (vv, l))}
        )
        else
          st
      ) st.cpa st

  let enter_multithreaded ask getg sideg (st: BaseComponents (D).t) =
    CPA.fold (fun x v (st: BaseComponents (D).t) ->
        if is_global ask x then (
          sideg (V.global x) (G.create_weak (GWeak.singleton (Lockset.empty ()) (GWeakW.singleton lockset_init v)));
          {st with cpa = CPA.remove x st.cpa}
        )
        else
          st
      ) st.cpa st

  let threadenter = startstate_threadenter startstate
end

module TimedPriv (Priv: S): S with module D = Priv.D =
struct
  module D = Priv.D
  module G = Priv.G
  module V = Priv.V

  let time str f arg = Timing.wrap "priv" (Timing.wrap str f) arg

  let startstate = Priv.startstate
  let read_global ask getg st x = time "read_global" (Priv.read_global ask getg st) x
  let write_global ?invariant ask getg sideg st x v = time "write_global" (Priv.write_global ?invariant ask getg sideg st x) v
  let lock ask getg cpa m = time "lock" (Priv.lock ask getg cpa) m
  let unlock ask getg sideg st m = time "unlock" (Priv.unlock ask getg sideg st) m
  let sync reason ctx = time "sync" (Priv.sync reason) ctx
  let escape ask getg sideg st escaped = time "escape" (Priv.escape ask getg sideg st) escaped
  let enter_multithreaded ask getg sideg st = time "enter_multithreaded" (Priv.enter_multithreaded ask getg sideg) st
  let threadenter ask st = time "threadenter" (Priv.threadenter ask) st
  let threadspawn ask get set st = time "threadspawn" (Priv.threadspawn ask get set) st
  let iter_sys_vars getg vq vf = time "iter_sys_vars" (Priv.iter_sys_vars getg vq) vf
  let invariant_global getg v = time "invariant_global" (Priv.invariant_global getg) v
  let invariant_vars ask getg st = time "invariant_vars" (Priv.invariant_vars ask getg) st

  let thread_join ?(force=false) ask get e st = time "thread_join" (Priv.thread_join ~force ask get e) st
  let thread_return ask get set tid st = time "thread_return" (Priv.thread_return ask get set tid) st

  let init () = time "init" (Priv.init) ()
  let finalize () = time "finalize" (Priv.finalize) ()
end

module PrecisionDumpPriv (Priv: S): S with module D = Priv.D =
struct
  include Priv

  open PrivPrecCompareUtil
  module LVH = RH

  let is_dumping = ref false
  let lvh = LVH.create 113

  let init () =
    Priv.init ();
    is_dumping := get_string "exp.priv-prec-dump" <> "";
    LVH.clear lvh

  let read_global ask getg st x =
    let v = Priv.read_global ask getg st x in
    if !AnalysisState.postsolving && !is_dumping then
      LVH.modify_def (VD.bot ()) (!Goblint_tracing.current_loc, x) (VD.join v) lvh;
    v

  let dump () =
    let f = open_out_bin (get_string "exp.priv-prec-dump") in
    (* LVH.iter (fun (l, x) v ->
        ignore (Pretty.printf "%a %a = %a\n" CilType.Location.pretty l CilType.Varinfo.pretty x VD.pretty v)
      ) lvh; *)
    Marshal.output f ({name = get_string "ana.base.privatization"; results = lvh}: result);
    close_out_noerr f

  let finalize () =
    if !is_dumping then
      dump ();
    Priv.finalize ()
end

module TracingPriv (Priv: S): S with module D = Priv.D =
struct
  include Priv

  module BaseComponents = BaseComponents (D)

  let read_global ask getg st x =
    if M.tracing then M.traceli "priv" "read_global %a\n" CilType.Varinfo.pretty x;
    if M.tracing then M.trace "priv" "st: %a\n" BaseComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "priv" "getg %a -> %a\n" V.pretty x G.pretty r;
      r
    in
    let v = Priv.read_global ask getg st x in
    if M.tracing then M.traceu "priv" "-> %a\n" VD.pretty v;
    v

  let write_global ?invariant ask getg sideg st x v =
    if M.tracing then M.traceli "priv" "write_global %a %a\n" CilType.Varinfo.pretty x VD.pretty v;
    if M.tracing then M.trace "priv" "st: %a\n" BaseComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "priv" "getg %a -> %a\n" V.pretty x G.pretty r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "priv" "sideg %a %a\n" V.pretty x G.pretty v;
      sideg x v
    in
    let r = write_global ?invariant ask getg sideg st x v in
    if M.tracing then M.traceu "priv" "-> %a\n" BaseComponents.pretty r;
    r

  let lock ask getg st m =
    if M.tracing then M.traceli "priv" "lock %a\n" LockDomain.Addr.pretty m;
    if M.tracing then M.trace "priv" "st: %a\n" BaseComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "priv" "getg %a -> %a\n" V.pretty x G.pretty r;
      r
    in
    let r = lock ask getg st m in
    if M.tracing then M.traceu "priv" "-> %a\n" BaseComponents.pretty r;
    r

  let unlock ask getg sideg st m =
    if M.tracing then M.traceli "priv" "unlock %a\n" LockDomain.Addr.pretty m;
    if M.tracing then M.trace "priv" "st: %a\n" BaseComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "priv" "getg %a -> %a\n" V.pretty x G.pretty r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "priv" "sideg %a %a\n" V.pretty x G.pretty v;
      sideg x v
    in
    let r = unlock ask getg sideg st m in
    if M.tracing then M.traceu "priv" "-> %a\n" BaseComponents.pretty r;
    r

  let enter_multithreaded ask getg sideg st =
    if M.tracing then M.traceli "priv" "enter_multithreaded\n";
    if M.tracing then M.trace "priv" "st: %a\n" BaseComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "priv" "getg %a -> %a\n" V.pretty x G.pretty r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "priv" "sideg %a %a\n" V.pretty x G.pretty v;
      sideg x v
    in
    let r = enter_multithreaded ask getg sideg st in
    if M.tracing then M.traceu "priv" "-> %a\n" BaseComponents.pretty r;
    r

  let threadenter ask st =
    if M.tracing then M.traceli "priv" "threadenter\n";
    if M.tracing then M.trace "priv" "st: %a\n" BaseComponents.pretty st;
    let r = threadenter ask st in
    if M.tracing then M.traceu "priv" "-> %a\n" BaseComponents.pretty r;
    r

  let sync ask getg sideg st reason =
    if M.tracing then M.traceli "priv" "sync\n";
    if M.tracing then M.trace "priv" "st: %a\n" BaseComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "priv" "getg %a -> %a\n" V.pretty x G.pretty r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "priv" "sideg %a %a\n" V.pretty x G.pretty v;
      sideg x v
    in
    let r = sync ask getg sideg st reason in
    if M.tracing then M.traceu "priv" "-> %a\n" BaseComponents.pretty r;
    r

end

let priv_module: (module S) Lazy.t =
  lazy (
    let module Priv: S =
      (val match get_string "ana.base.privatization" with
        | "none" -> (module NonePriv: S)
        | "mutex-oplus" -> (module PerMutexOplusPriv)
        | "mutex-meet" -> (module PerMutexMeetPriv)
        | "mutex-meet-tid" -> (module PerMutexMeetTIDPriv)
        | "protection" -> (module ProtectionBasedPriv (struct let check_read_unprotected = false end))
        | "protection-read" -> (module ProtectionBasedPriv (struct let check_read_unprotected = true end))
        | "mine" -> (module MinePriv)
        | "mine-nothread" -> (module MineNoThreadPriv)
        | "mine-W" -> (module MineWPriv (struct let side_effect_global_init = true end))
        | "mine-W-noinit" -> (module MineWPriv (struct let side_effect_global_init = false end))
        | "lock" -> (module LockCenteredPriv)
        | "write" -> (module WriteCenteredPriv)
        | "write+lock" -> (module WriteAndLockCenteredPriv)
        | _ -> failwith "ana.base.privatization: illegal value"
      )
    in
    let module Priv = PrecisionDumpPriv (Priv) in
    (* let module Priv = TimedPriv (Priv) in *)
    let module Priv = TracingPriv (Priv) in
    (module Priv)
  )

let get_priv (): (module S) =
  Lazy.force priv_module
