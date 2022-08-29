open Prelude.Ana
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
  val iter_sys_vars: (V.t -> G.t) -> VarQuery.t -> V.t VarQuery.f -> unit

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
  (* let new_cpa = if !GU.earlyglobs || ThreadFlag.is_multi ctx.ask then CPA.filter (fun k v -> is_private ctx.ask ctx.local k) globals else globals in *)
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
          if M.tracing then M.tracec "globalize" "Publishing its value: %a\n" VD.pp value;
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
    if M.tracing then M.tracel "priv" "get_m_with_mutex_inits %a:\n  get_m: %a\n  get_mutex_inits: %a\n  get_mutex_inits': %a\n" LockDomain.Addr.pp m CPA.pp get_m CPA.pp get_mutex_inits CPA.pp get_mutex_inits';
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

  let escape ask getg sideg (st: BaseComponents (D).t) escaped =
    let escaped_cpa = CPA.filter (fun x _ -> EscapeDomain.EscapedVars.mem x escaped) st.cpa in
    sideg V.mutex_inits escaped_cpa;

    let cpa' = CPA.fold (fun x v acc ->
        if EscapeDomain.EscapedVars.mem x escaped (* && is_unprotected ask x *) then (
          if M.tracing then M.tracel "priv" "ESCAPE SIDE %a = %a\n" d_varinfo x VD.pp v;
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
          if M.tracing then M.tracel "priv" "enter_multithreaded remove %a\n" d_varinfo x;
          if M.tracing then M.tracel "priv" "ENTER MULTITHREADED SIDE %a = %a\n" d_varinfo x VD.pp v;
          sideg (V.global x) (CPA.singleton x v);
          CPA.remove x acc
        )
        else
          acc
      ) st.cpa st.cpa
    in
    {st with cpa = cpa'}

  let threadenter = old_threadenter
end

module PerMutexOplusPriv: S =
struct
  include PerMutexPrivBase

  let read_global ask getg (st: BaseComponents (D).t) x =
    if is_unprotected ask x then
      let get_mutex_global_x = get_mutex_global_x_with_mutex_inits getg x in
      get_mutex_global_x |? VD.bot ()
    else
      CPA.find x st.cpa
  (* let read_global ask getg cpa x =
    let (cpa', v) as r = read_global ask getg cpa x in
    Fmt.pr "READ GLOBAL %a (%a, %B) = %a\n" d_varinfo x CilType.Location.pp !Tracing.current_loc (is_unprotected ask x) VD.pp v;
    r *)
  let write_global ?(invariant=false) ask getg sideg (st: BaseComponents (D).t) x v =
    let cpa' = CPA.add x v st.cpa in
    sideg (V.global x) (CPA.singleton x v);
    {st with cpa = cpa'}
  (* let write_global ask getg sideg cpa x v =
    let cpa' = write_global ask getg sideg cpa x v in
    Fmt.pr "WRITE GLOBAL %a %a = %a\n" d_varinfo x VD.pp v CPA.pp cpa';
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
      if M.tracing then M.tracel "priv" "PerMutexOplusPriv.lock m=%a cpa'=%a\n" LockDomain.Addr.pp m CPA.pp cpa';
      {st with cpa = CPA.fold CPA.add cpa' st.cpa}
    )
    else
      st (* sound w.r.t. recursive lock *)

  let unlock ask getg sideg (st: BaseComponents (D).t) m =
    let is_in_Gm x _ = is_protected_by ask m x in
    let side_m_cpa = CPA.filter is_in_Gm st.cpa in
    if M.tracing then M.tracel "priv" "PerMutexOplusPriv.unlock m=%a side_m_cpa=%a\n" LockDomain.Addr.pp m CPA.pp side_m_cpa;
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
end

module PerMutexMeetPriv: S =
struct
  include PerMutexPrivBase

  let read_global ask getg (st: BaseComponents (D).t) x =
    if is_unprotected ask x then (
      let get_mutex_global_x = get_mutex_global_x_with_mutex_inits getg x in
      (* None is VD.top () *)
      match CPA.find_opt x st.cpa, get_mutex_global_x with
      | Some v1, Some v2 -> VD.meet v1 v2
      | Some v, None
      | None, Some v -> v
      | None, None -> VD.bot () (* Except if both None, needed for 09/07 kernel_list_rc *)
      (* get_mutex_global_x |? VD.bot () *)
    )
    else
      CPA.find x st.cpa
  let read_global ask getg st x =
    let v = read_global ask getg st x in
    if M.tracing then M.tracel "priv" "READ GLOBAL %a %B %a = %a\n" d_varinfo x (is_unprotected ask x) CPA.pp st.cpa VD.pp v;
    v
  let write_global ?(invariant=false) ask getg sideg (st: BaseComponents (D).t) x v =
    let cpa' =
      if is_unprotected ask x then
        st.cpa
      else
        CPA.add x v st.cpa
    in
    if M.tracing then M.tracel "priv" "WRITE GLOBAL SIDE %a = %a\n" d_varinfo x VD.pp v;
    sideg (V.global x) (CPA.singleton x v);
    {st with cpa = cpa'}
  (* let write_global ask getg sideg cpa x v =
    let cpa' = write_global ask getg sideg cpa x v in
    Fmt.pr "WRITE GLOBAL %a %a = %a\n" d_varinfo x VD.pp v CPA.pp cpa';
    cpa' *)

  let lock (ask: Queries.ask) getg (st: BaseComponents (D).t) m =
    if Locksets.(not (Lockset.mem m (current_lockset ask))) then (
      let get_m = get_m_with_mutex_inits ask getg m in
      (* Additionally filter get_m in case it contains variables it no longer protects. *)
      let is_in_Gm x _ = is_protected_by ask m x in
      let get_m = CPA.filter is_in_Gm get_m in
      let long_meet m1 m2 = CPA.long_map2 VD.meet m1 m2 in
      let meet = long_meet st.cpa get_m in
      if M.tracing then M.tracel "priv" "LOCK %a:\n  get_m: %a\n  meet: %a\n" LockDomain.Addr.pp m CPA.pp get_m CPA.pp meet;
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
            if M.tracing then M.tracel "priv" "SYNC SIDE %a = %a\n" d_varinfo x VD.pp v;
            sideg (V.global x) (CPA.singleton x v);
            CPA.remove x cpa
          )
          else (
            if M.tracing then M.tracel "priv" "SYNC NOSIDE %a = %a\n" d_varinfo x VD.pp v;
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
  end
  module VProt =
  struct
    include VarinfoV (* [g] *)
    let name () = "protected"
    let show x = show x ^ ":protected" (* distinguishable variant names for html *)
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
    sideg (V.unprotected x) v;
    if !GU.earlyglobs then (* earlyglobs workaround for 13/60 *)
      sideg (V.protected x) v;
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

  let iter_sys_vars getg vq vf =
    match vq with
    | VarQuery.Global g ->
      vf (V.unprotected g);
      vf (V.protected g);
    | _ -> ()
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

module LockCenteredGBase =
struct
  (* weak: G -> (2^M -> D) *)
  (* sync: M -> (2^M -> (G -> D)) *)
  include AbstractLockCenteredGBase (VD) (CPA)
end

module MinePrivBase =
struct
  include NoFinalize
  include ConfCheck.RequireMutexPathSensInit
  include MutexGlobals (* explicit not needed here because G is Prod anyway? *)
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
  module ThreadMap = MapDomain.MapBot (Thread) (VD)

  (* weak: G -> (2^M -> (T -> D)) *)
  (* sync: M -> (2^M -> (G -> D)) *)
  include AbstractLockCenteredGBase (ThreadMap) (CPA)

  let global_init_thread = RichVarinfo.single ~name:"global_init"
  let current_thread (ask: Q.ask): Thread.t =
    if !GU.global_initialization then
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
    if not (!GU.earlyglobs && is_excluded_from_earlyglobs x) then
      sideg (V.global x) (G.create_weak (GWeak.singleton s (ThreadMap.singleton t v)));
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
  include LockCenteredGBase
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
    if not (!GU.earlyglobs && is_excluded_from_earlyglobs x) then
      sideg (V.global x) (G.create_weak (GWeak.singleton s v));
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
  include LockCenteredGBase
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
    if not (!GU.earlyglobs && is_excluded_from_earlyglobs x) then
      sideg (V.global x) (G.create_weak (GWeak.singleton s v));
    {st with cpa = cpa'; priv = W.add x st.priv}

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
  include LockCenteredGBase
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
    if M.tracing then M.trace "priv" "d_cpa: %a\n" VD.pp d_cpa;
    if M.tracing then M.trace "priv" "d_sync: %a\n" VD.pp d_sync;
    if M.tracing then M.trace "priv" "d_weak: %a\n" VD.pp d_weak;
    if M.tracing then M.trace "priv" "d_init: %a\n" VD.pp d_init;
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
    if not (!GU.earlyglobs && is_excluded_from_earlyglobs x) then (
      let v = distr_init getg x v in
      sideg (V.global x) (G.create_weak (GWeak.singleton s v))
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

module WriteCenteredGBase =
struct
  open Locksets

  module GWeakW = MapDomain.MapBot (Lockset) (VD)
  module GSyncW = MapDomain.MapBot (Lockset) (CPA)

  (* weak: G -> (S:2^M -> (W:2^M -> D)) *)
  (* sync: M -> (S:2^M -> (W:2^M -> (G -> D))) *)
  include AbstractLockCenteredGBase (GWeakW) (GSyncW)
end

(** Write-Centered Reading. *)
module WriteCenteredPriv: S =
struct
  include MinePrivBase
  include WriteCenteredGBase
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
    if M.tracing then M.trace "priv" "d_cpa: %a\n" VD.pp d_cpa;
    if M.tracing then M.trace "priv" "d_sync: %a\n" VD.pp d_sync;
    if M.tracing then M.trace "priv" "d_weak: %a\n" VD.pp d_weak;
    let d = VD.join d_cpa (VD.join d_sync d_weak) in
    d

  let write_global ?(invariant=false) ask getg sideg (st: BaseComponents (D).t) x v =
    let s = current_lockset ask in
    let (w, p) = st.priv in
    let w' = W.add x (MinLocksets.singleton s) w in
    let p' = P.add x (MinLocksets.singleton s) p in
    let p' = P.map (fun s' -> MinLocksets.add s s') p' in
    let cpa' = CPA.add x v st.cpa in
    if not (!GU.earlyglobs && is_excluded_from_earlyglobs x) then (
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
    if M.tracing then M.traceli "priv" "unlock %a %a\n" Lock.pp m CPA.pp st.cpa;
    let side_gsyncw = CPA.fold (fun x v acc ->
        if is_global ask x then (
          let w_x = W.find x w in
          if M.tracing then M.trace "priv" "gsyncw %a %a %a\n" d_varinfo x VD.pp v MinLocksets.pp w_x;
          MinLocksets.fold (fun w acc ->
              let v = distr_init getg x v in
              GSyncW.add w (CPA.add x v (GSyncW.find w acc)) acc
            ) w_x acc
        ) else
          acc
      ) st.cpa (GSyncW.bot ())
    in
    if M.tracing then M.traceu "priv" "unlock %a %a\n" Lock.pp m GSyncW.pp side_gsyncw;
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
  include WriteCenteredGBase
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
    let w' = W.add x (MinLocksets.singleton s) w in
    let p' = P.add x (MinLocksets.singleton s) p in
    let p' = P.map (fun s' -> MinLocksets.add s s') p' in
    let v' = L.fold (fun m _ acc ->
        DV.add m (MustVars.add x (DV.find m acc)) acc
      ) l vv
    in
    let cpa' = CPA.add x v st.cpa in
    if not (!GU.earlyglobs && is_excluded_from_earlyglobs x) then (
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

  let time str f arg = Stats.time "priv" (Stats.time str f) arg

  let startstate = Priv.startstate
  let read_global ask getg st x = time "read_global" (Priv.read_global ask getg st) x
  let write_global ?invariant ask getg sideg st x v = time "write_global" (Priv.write_global ?invariant ask getg sideg st x) v
  let lock ask getg cpa m = time "lock" (Priv.lock ask getg cpa) m
  let unlock ask getg sideg st m = time "unlock" (Priv.unlock ask getg sideg st) m
  let sync reason ctx = time "sync" (Priv.sync reason) ctx
  let escape ask getg sideg st escaped = time "escape" (Priv.escape ask getg sideg st) escaped
  let enter_multithreaded ask getg sideg st = time "enter_multithreaded" (Priv.enter_multithreaded ask getg sideg) st
  let threadenter ask st = time "threadenter" (Priv.threadenter ask) st
  let iter_sys_vars getg vq vf = time "iter_sys_vars" (Priv.iter_sys_vars getg vq) vf

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
    if !GU.postsolving && !is_dumping then
      LVH.modify_def (VD.bot ()) (!Tracing.current_loc, x) (VD.join v) lvh;
    v

  let dump () =
    let f = open_out_bin (get_string "exp.priv-prec-dump") in
    (* LVH.iter (fun (l, x) v ->
        Fmt.pr "%a %a = %a\n" CilType.Location.pp l d_varinfo x VD.pp v
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
    if M.tracing then M.traceli "priv" "read_global %a\n" d_varinfo x;
    if M.tracing then M.trace "priv" "st: %a\n" BaseComponents.pp st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "priv" "getg %a -> %a\n" V.pp x G.pp r;
      r
    in
    let v = Priv.read_global ask getg st x in
    if M.tracing then M.traceu "priv" "-> %a\n" VD.pp v;
    v

  let write_global ?invariant ask getg sideg st x v =
    if M.tracing then M.traceli "priv" "write_global %a %a\n" d_varinfo x VD.pp v;
    if M.tracing then M.trace "priv" "st: %a\n" BaseComponents.pp st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "priv" "getg %a -> %a\n" V.pp x G.pp r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "priv" "sideg %a %a\n" V.pp x G.pp v;
      sideg x v
    in
    let r = write_global ?invariant ask getg sideg st x v in
    if M.tracing then M.traceu "priv" "-> %a\n" BaseComponents.pp r;
    r

  let lock ask getg st m =
    if M.tracing then M.traceli "priv" "lock %a\n" LockDomain.Addr.pp m;
    if M.tracing then M.trace "priv" "st: %a\n" BaseComponents.pp st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "priv" "getg %a -> %a\n" V.pp x G.pp r;
      r
    in
    let r = lock ask getg st m in
    if M.tracing then M.traceu "priv" "-> %a\n" BaseComponents.pp r;
    r

  let unlock ask getg sideg st m =
    if M.tracing then M.traceli "priv" "unlock %a\n" LockDomain.Addr.pp m;
    if M.tracing then M.trace "priv" "st: %a\n" BaseComponents.pp st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "priv" "getg %a -> %a\n" V.pp x G.pp r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "priv" "sideg %a %a\n" V.pp x G.pp v;
      sideg x v
    in
    let r = unlock ask getg sideg st m in
    if M.tracing then M.traceu "priv" "-> %a\n" BaseComponents.pp r;
    r

  let enter_multithreaded ask getg sideg st =
    if M.tracing then M.traceli "priv" "enter_multithreaded\n";
    if M.tracing then M.trace "priv" "st: %a\n" BaseComponents.pp st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "priv" "getg %a -> %a\n" V.pp x G.pp r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "priv" "sideg %a %a\n" V.pp x G.pp v;
      sideg x v
    in
    let r = enter_multithreaded ask getg sideg st in
    if M.tracing then M.traceu "priv" "-> %a\n" BaseComponents.pp r;
    r

  let threadenter ask st =
    if M.tracing then M.traceli "priv" "threadenter\n";
    if M.tracing then M.trace "priv" "st: %a\n" BaseComponents.pp st;
    let r = threadenter ask st in
    if M.tracing then M.traceu "priv" "-> %a\n" BaseComponents.pp r;
    r

  let sync ask getg sideg st reason =
    if M.tracing then M.traceli "priv" "sync\n";
    if M.tracing then M.trace "priv" "st: %a\n" BaseComponents.pp st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "priv" "getg %a -> %a\n" V.pp x G.pp r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "priv" "sideg %a %a\n" V.pp x G.pp v;
      sideg x v
    in
    let r = sync ask getg sideg st reason in
    if M.tracing then M.traceu "priv" "-> %a\n" BaseComponents.pp r;
    r

end

let priv_module: (module S) Lazy.t =
  lazy (
    let module Priv: S =
      (val match get_string "ana.base.privatization" with
        | "none" -> (module NonePriv: S)
        | "mutex-oplus" -> (module PerMutexOplusPriv)
        | "mutex-meet" -> (module PerMutexMeetPriv)
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
