open Prelude.Ana
open Analyses
open GobConfig
open BaseUtil

module IdxDom = ValueDomain.IndexDomain

module VD     = BaseDomain.VD
module CPA    = BaseDomain.CPA
module CVars  = BaseDomain.CachedVars
module BaseComponents = BaseDomain.BaseComponents

(* TODO: rename to S *)
module type PrivParam =
sig
  module G: Lattice.S

  val read_global: Q.ask -> (varinfo -> G.t) -> BaseComponents.t -> varinfo -> VD.t
  val write_global: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> BaseComponents.t -> varinfo -> VD.t -> BaseComponents.t

  val lock: Q.ask -> (varinfo -> G.t) -> BaseComponents.t -> LockDomain.Addr.t -> BaseComponents.t
  val unlock: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> BaseComponents.t -> LockDomain.Addr.t -> BaseComponents.t

  val sync: Q.ask -> (varinfo -> G.t) -> BaseComponents.t -> [`Normal | `Join | `Return | `Init | `Thread] -> BaseComponents.t * (varinfo * G.t) list

  val escape: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> BaseComponents.t -> EscapeDomain.EscapedVars.t -> BaseComponents.t
  val enter_multithreaded: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> BaseComponents.t -> BaseComponents.t

  (* TODO: better name *)
  val is_private: Q.ask -> varinfo -> bool
end

module OldPrivBase =
struct
  let lock ask getg cpa m = cpa
  let unlock ask getg sideg st m = st

  let escape ask getg sideg st escaped = st
  let enter_multithreaded ask getg sideg st = st

  let sync_privates reason ask =
    match reason with
    | `Init
    | `Thread ->
      true
    | _ ->
      !GU.earlyglobs && not (ThreadFlag.is_multi ask)
end

(* Copy of OldPriv with is_private constantly false. *)
module NoPriv: PrivParam =
struct
  include OldPrivBase

  module G = BaseDomain.VD

  let read_global ask getg (st: BaseComponents.t) x =
    getg x

  let write_global ask getg sideg (st: BaseComponents.t) x v =
    (* Here, an effect should be generated, but we add it to the local
     * state, waiting for the sync function to publish it. *)
    (* Copied from MainFunctor.update_variable *)
    if ((get_bool "exp.volatiles_are_top") && (is_always_unknown x)) then
      {st with cpa = CPA.add x (VD.top ()) st.cpa}
    else
      {st with cpa = CPA.add x v st.cpa}

  let is_private (a: Q.ask) (v: varinfo): bool = false

  let sync ask getg (st: BaseComponents.t) reason =
    (* For each global variable, we create the diff *)
    let add_var (v: varinfo) (value) ((st: BaseComponents.t),acc) =
      if M.tracing then M.traceli "globalize" ~var:v.vname "Tracing for %s\n" v.vname;
      let res =
        if is_global ask v then begin
          if M.tracing then M.tracec "globalize" "Publishing its value: %a\n" VD.pretty value;
          ({st with cpa = CPA.remove v st.cpa}, (v,value) :: acc)
        end else
          (st,acc)
      in
      if M.tracing then M.traceu "globalize" "Done!\n";
      res
    in
    (* We fold over the local state, and collect the globals *)
    CPA.fold add_var st.cpa (st, [])
end

module OldPriv: PrivParam =
struct
  include OldPrivBase

  module G = BaseDomain.VD

  let read_global ask getg (st: BaseComponents.t) x =
    match CPA.find x st.cpa with
    | `Bot -> (if M.tracing then M.tracec "get" "Using global invariant.\n"; getg x)
    | x -> (if M.tracing then M.tracec "get" "Using privatized version.\n"; x)

  let write_global ask getg sideg (st: BaseComponents.t) x v =
    (* Here, an effect should be generated, but we add it to the local
     * state, waiting for the sync function to publish it. *)
    (* Copied from MainFunctor.update_variable *)
    if ((get_bool "exp.volatiles_are_top") && (is_always_unknown x)) then
      {st with cpa = CPA.add x (VD.top ()) st.cpa}
    else
      {st with cpa = CPA.add x v st.cpa}

  let is_private (a: Q.ask) (v: varinfo): bool =
    (not (ThreadFlag.is_multi a) && is_precious_glob v ||
     match a (Q.MayBePublic {global=v; write=false}) with `MayBool tv -> not tv | _ ->
     if M.tracing then M.tracel "osek" "isPrivate yields top(!!!!)";
     false)

  let sync ask getg (st: BaseComponents.t) reason =
    let privates = sync_privates reason ask in
    if M.tracing then M.tracel "sync" "OldPriv: %a\n" BaseComponents.pretty st;
    (* For each global variable, we create the diff *)
    let add_var (v: varinfo) (value) ((st: BaseComponents.t),acc) =
      if M.tracing then M.traceli "globalize" ~var:v.vname "Tracing for %s\n" v.vname;
      let res =
        if is_global ask v && ((privates && not (is_precious_glob v)) || not (is_private ask v)) then begin
          if M.tracing then M.tracec "globalize" "Publishing its value: %a\n" VD.pretty value;
          ({st with cpa = CPA.remove v st.cpa}, (v,value) :: acc)
        end else
          (st,acc)
      in
      if M.tracing then M.traceu "globalize" "Done!\n";
      res
    in
    (* We fold over the local state, and collect the globals *)
    CPA.fold add_var st.cpa (st, [])
end

module NewPrivBase =
struct
  let is_unprotected ask x: bool =
    let multi = ThreadFlag.is_multi ask in
    (!GU.earlyglobs && not multi && not (is_precious_glob x)) ||
    (
      multi &&
      match ask (Q.MayBePublic {global=x; write=true}) with
      | `MayBool x -> x
      | `Top -> true
      | _ -> failwith "PrivBase.is_unprotected"
    )

  let is_unprotected_without ask ?(write=true) x m: bool =
    ThreadFlag.is_multi ask &&
    match ask (Q.MayBePublicWithout {global=x; write; without_mutex=m}) with
    | `MayBool x -> x
    | `Top -> true
    | _ -> failwith "PrivBase.is_unprotected_without"

  let is_protected_by ask m x: bool =
    is_global ask x &&
    not (VD.is_immediate_type x.vtype) &&
    match ask (Q.MustBeProtectedBy {mutex=m; global=x; write=true}) with
    | `MustBool x -> x
    | `Top -> false
    | _ -> failwith "PrivBase.is_protected_by"

  let is_atomic ask: bool =
    match ask Q.MustBeAtomic with
    | `MustBool x -> x
    | `Top -> false
    | _ -> failwith "PrivBase.is_atomic"
end

module MutexGlobals =
struct
  let mutex_addr_to_varinfo = function
    | LockDomain.Addr.Addr (v, `NoOffset) -> v
    | LockDomain.Addr.Addr (v, offs) ->
      M.warn_each (Pretty.sprint ~width:800 @@ Pretty.dprintf "NewPrivBase: ignoring offset %a%a\n" d_varinfo v LockDomain.Addr.Offs.pretty offs);
      v
    | _ -> failwith "NewPrivBase.mutex_addr_to_varinfo"

  (* let mutex_global x = x *)
  let mutex_global =
    let module VH = Hashtbl.Make (Basetype.Variables) in
    let mutex_globals = VH.create 13 in
    fun x ->
      try
        VH.find mutex_globals x
      with Not_found ->
        let mutex_global_x = Goblintutil.create_var @@ makeGlobalVar ("MUTEX_GLOBAL_" ^ x.vname) voidType in
        VH.replace mutex_globals x mutex_global_x;
        mutex_global_x
  let mutex_global x =
    let r = mutex_global x in
    if M.tracing then M.tracel "priv" "mutex_global %a = %a\n" d_varinfo x d_varinfo r;
    r
end

module PerMutexPrivBase =
struct
  include NewPrivBase
  include MutexGlobals

  module G = CPA

  let mutex_inits =
    lazy (
      Goblintutil.create_var @@ makeGlobalVar "MUTEX_INITS" voidType
    )

  let get_m_with_mutex_inits ask getg m =
    let get_m = getg (mutex_addr_to_varinfo m) in
    let get_mutex_inits = getg (Lazy.force mutex_inits) in
    let is_in_Gm x _ = is_protected_by ask m x in
    let get_mutex_inits' = CPA.filter is_in_Gm get_mutex_inits in
    if M.tracing then M.tracel "priv" "get_m_with_mutex_inits %a:\n  get_m: %a\n  get_mutex_inits: %a\n  get_mutex_inits': %a\n" LockDomain.Addr.pretty m CPA.pretty get_m CPA.pretty get_mutex_inits CPA.pretty get_mutex_inits';
    CPA.join get_m get_mutex_inits'

  (** [get_m_with_mutex_inits] optimized for implementation-specialized [read_global]. *)
  let get_mutex_global_x_with_mutex_inits getg x =
    let get_mutex_global_x = getg (mutex_global x) in
    let get_mutex_inits = getg (Lazy.force mutex_inits) in
    match CPA.find_opt x get_mutex_global_x, CPA.find_opt x get_mutex_inits with
      | Some v1, Some v2 -> Some (VD.join v1 v2)
      | Some v, None
      | None, Some v -> Some v
      | None, None -> None

  let escape ask getg sideg (st: BaseComponents.t) escaped =
    let escaped_cpa = CPA.filter (fun x _ -> EscapeDomain.EscapedVars.mem x escaped) st.cpa in
    sideg (Lazy.force mutex_inits) escaped_cpa;

    let cpa' = CPA.fold (fun x v acc ->
        if EscapeDomain.EscapedVars.mem x escaped (* && is_unprotected ask x *) then (
          if M.tracing then M.tracel "priv" "ESCAPE SIDE %a = %a\n" d_varinfo x VD.pretty v;
          sideg (mutex_global x) (CPA.add x v (CPA.bot ()));
          CPA.remove x acc
        )
        else
          acc
      ) st.cpa st.cpa
    in
    {st with cpa = cpa'}

  let enter_multithreaded ask getg sideg (st: BaseComponents.t) =
    let global_cpa = CPA.filter (fun x _ -> is_global ask x) st.cpa in
    sideg (Lazy.force mutex_inits) global_cpa;

    let cpa' = CPA.fold (fun x v acc ->
        if is_global ask x (* && is_unprotected ask x *) then (
          if M.tracing then M.tracel "priv" "enter_multithreaded remove %a\n" d_varinfo x;
          if M.tracing then M.tracel "priv" "ENTER MULTITHREADED SIDE %a = %a\n" d_varinfo x VD.pretty v;
          sideg (mutex_global x) (CPA.add x v (CPA.bot ()));
          CPA.remove x acc
        )
        else
          acc
      ) st.cpa st.cpa
    in
    {st with cpa = cpa'}

  (* TODO: does this make sense? *)
  let is_private ask x = true
end

module PerMutexOplusPriv: PrivParam =
struct
  include PerMutexPrivBase

  let read_global ask getg (st: BaseComponents.t) x =
    if is_unprotected ask x then
      let get_mutex_global_x = get_mutex_global_x_with_mutex_inits getg x in
      get_mutex_global_x |? VD.bot ()
    else
      CPA.find x st.cpa
  (* let read_global ask getg cpa x =
    let (cpa', v) as r = read_global ask getg cpa x in
    ignore (Pretty.printf "READ GLOBAL %a (%a, %B) = %a\n" d_varinfo x d_loc !Tracing.current_loc (is_unprotected ask x) VD.pretty v);
    r *)
  let write_global ask getg sideg (st: BaseComponents.t) x v =
    let cpa' = CPA.add x v st.cpa in
    if not (is_atomic ask) then
      sideg (mutex_global x) (CPA.add x v (CPA.bot ()));
    {st with cpa = cpa'}
  (* let write_global ask getg sideg cpa x v =
    let cpa' = write_global ask getg sideg cpa x v in
    ignore (Pretty.printf "WRITE GLOBAL %a %a = %a\n" d_varinfo x VD.pretty v CPA.pretty cpa');
    cpa' *)

  let lock ask getg (st: BaseComponents.t) m =
    let get_m = get_m_with_mutex_inits ask getg m in
    let is_in_V x _ = is_protected_by ask m x && is_unprotected ask x in
    let cpa' = CPA.filter is_in_V get_m in
    if M.tracing then M.tracel "priv" "PerMutexOplusPriv.lock m=%a cpa'=%a\n" LockDomain.Addr.pretty m CPA.pretty cpa';
    {st with cpa = CPA.fold CPA.add cpa' st.cpa}
  let unlock ask getg sideg (st: BaseComponents.t) m =
    let is_in_Gm x _ = is_protected_by ask m x in
    let side_m_cpa = CPA.filter is_in_Gm st.cpa in
    if M.tracing then M.tracel "priv" "PerMutexOplusPriv.unlock m=%a side_m_cpa=%a\n" LockDomain.Addr.pretty m CPA.pretty side_m_cpa;
    sideg (mutex_addr_to_varinfo m) side_m_cpa;
    st

  let sync ask getg (st: BaseComponents.t) reason =
    match reason with
    | `Join -> (* required for branched thread creation *)
      let sidegs = CPA.fold (fun x v acc ->
          (* TODO: is_unprotected - why breaks 02/11 init_mainfun? *)
          if is_global ask x && is_unprotected ask x then
            (mutex_global x, CPA.add x v (CPA.bot ())) :: acc
          else
            acc
        ) st.cpa []
      in
      (st, sidegs)
    | `Return -> (* required for thread return *)
      begin match ThreadId.get_current ask with
        | `Lifted x when CPA.mem x st.cpa ->
          let v = CPA.find x st.cpa in
          (st, [(mutex_global x, CPA.add x v (CPA.bot ()))])
        | _ ->
          (st, [])
      end
    | `Normal
    | `Init
    | `Thread ->
      (st, [])
end

module PerMutexMeetPriv: PrivParam =
struct
  include PerMutexPrivBase

  let read_global ask getg (st: BaseComponents.t) x =
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
    if M.tracing then M.tracel "priv" "READ GLOBAL %a %B %a = %a\n" d_varinfo x (is_unprotected ask x) CPA.pretty st.cpa VD.pretty v;
    v
  let write_global ask getg sideg (st: BaseComponents.t) x v =
    let cpa' =
      if is_unprotected ask x then
        st.cpa
      else
        CPA.add x v st.cpa
    in
    if not (is_atomic ask) then (
      if M.tracing then M.tracel "priv" "WRITE GLOBAL SIDE %a = %a\n" d_varinfo x VD.pretty v;
      sideg (mutex_global x) (CPA.add x v (CPA.bot ()))
    );
    {st with cpa = cpa'}
  (* let write_global ask getg sideg cpa x v =
    let cpa' = write_global ask getg sideg cpa x v in
    ignore (Pretty.printf "WRITE GLOBAL %a %a = %a\n" d_varinfo x VD.pretty v CPA.pretty cpa');
    cpa' *)

  let lock ask getg (st: BaseComponents.t) m =
    let get_m = get_m_with_mutex_inits ask getg m in
    (* Additionally filter get_m in case it contains variables it no longer protects. *)
    let is_in_Gm x _ = is_protected_by ask m x in
    let get_m = CPA.filter is_in_Gm get_m in
    let long_meet m1 m2 = CPA.long_map2 VD.meet m1 m2 in
    let meet = long_meet st.cpa get_m in
    if M.tracing then M.tracel "priv" "LOCK %a:\n  get_m: %a\n  meet: %a\n" LockDomain.Addr.pretty m CPA.pretty get_m CPA.pretty meet;
    {st with cpa = meet}
  let unlock ask getg sideg (st: BaseComponents.t) m =
    let is_in_Gm x _ = is_protected_by ask m x in
    sideg (mutex_addr_to_varinfo m) (CPA.filter is_in_Gm st.cpa);
    let cpa' = CPA.fold (fun x v cpa ->
        if is_protected_by ask m x && is_unprotected_without ask x m then
          CPA.remove x cpa
          (* CPA.add x (VD.top ()) cpa *)
        else
          cpa
      ) st.cpa st.cpa
    in
    {st with cpa = cpa'}

  let sync ask getg (st: BaseComponents.t) reason =
    match reason with
    | `Join -> (* required for branched thread creation *)
      let (cpa', sidegs') = CPA.fold (fun x v ((cpa, sidegs) as acc) ->
          if is_global ask x && is_unprotected ask x (* && not (VD.is_top v) *) then (
            if M.tracing then M.tracel "priv" "SYNC SIDE %a = %a\n" d_varinfo x VD.pretty v;
            (CPA.remove x cpa, (mutex_global x, CPA.add x v (CPA.bot ())) :: sidegs)
          )
          else (
            if M.tracing then M.tracel "priv" "SYNC NOSIDE %a = %a\n" d_varinfo x VD.pretty v;
            acc
          )
        ) st.cpa (st.cpa, [])
      in
      ({st with cpa = cpa'}, sidegs')
    | `Return -> (* required for thread return *)
      begin match ThreadId.get_current ask with
        | `Lifted x when CPA.mem x st.cpa ->
          let v = CPA.find x st.cpa in
          ({st with cpa = CPA.remove x st.cpa}, [(mutex_global x, CPA.add x v (CPA.bot ()))])
        | _ ->
          (st, [])
      end
    | `Normal
    | `Init
    | `Thread ->
      (st, [])
end

module PerGlobalVesalPriv: PrivParam =
struct
  include OldPrivBase

  module G = BaseDomain.VD

  let read_global ask getg (st: BaseComponents.t) x =
    match CPA.find x st.cpa with
    | `Bot -> (if M.tracing then M.tracec "get" "Using global invariant.\n"; getg x)
    | x -> (if M.tracing then M.tracec "get" "Using privatized version.\n"; x)

  let write_global ask getg sideg (st: BaseComponents.t) x v =
    (* Here, an effect should be generated, but we add it to the local
     * state, waiting for the sync function to publish it. *)
    (* Copied from MainFunctor.update_variable *)
    if ((get_bool "exp.volatiles_are_top") && (is_always_unknown x)) then
      {st with cpa = CPA.add x (VD.top ()) st.cpa; cached = CVars.add x st.cached}
    else
      {st with cpa = CPA.add x v st.cpa; cached = CVars.add x st.cached}

  let is_invisible (a: Q.ask) (v: varinfo): bool =
    (not (ThreadFlag.is_multi a) && is_precious_glob v ||
    match a (Q.MayBePublic {global=v; write=false}) with `MayBool tv -> not tv | _ ->
    if M.tracing then M.tracel "osek" "isPrivate yields top(!!!!)";
    false)
  let is_private = is_invisible

  let is_protected (a: Q.ask) (v: varinfo): bool =
    (not (ThreadFlag.is_multi a) && is_precious_glob v ||
        match a (Q.MayBePublic {global=v; write=true}) with `MayBool tv -> not tv | _ -> false)

  let sync ask getg (st: BaseComponents.t) reason =
    let privates = sync_privates reason ask in
    (* For each global variable, we create the diff *)
    let add_var (v: varinfo) (value) ((st: BaseComponents.t),acc) =
      if M.tracing then M.traceli "globalize" ~var:v.vname "Tracing for %s\n" v.vname;
      let res =
        if is_global ask v then
          let protected = is_protected ask v in
          if privates && not (is_precious_glob v) || not protected then begin
            if M.tracing then M.tracec "globalize" "Publishing its value: %a\n" VD.pretty value;
            ({ st with cpa = CPA.remove v st.cpa; cached = CVars.remove v st.cached} , (v,value) :: acc)
          end else (* protected == true *)
            let (st, acc) = if not (CVars.mem v st.cached) then
              let joined = VD.join (CPA.find v st.cpa) (getg v) in
              ( {st with cpa = CPA.add v joined st.cpa} ,acc)
             else (st,acc)
            in
            let invisible = is_invisible ask v in
            if not invisible then (st, (v,value) :: acc) else (st,acc)
        else (st,acc)
      in
      if M.tracing then M.traceu "globalize" "Done!\n";
      res
    in
    (* We fold over the local state, and collect the globals *)
    CPA.fold add_var st.cpa (st, [])
end

module type PerGlobalPrivParam =
sig
  (** Whether to also check unprotectedness by reads for extra precision. *)
  val check_read_unprotected: bool
end

module PerGlobalPriv (Param: PerGlobalPrivParam): PrivParam =
struct
  include NewPrivBase

  module GUnprot =
  struct
    include VD
    let name () = "unprotected"
  end
  module GProt =
  struct
    include VD
    let name () = "protected"
  end
  module G = Lattice.Prod (GUnprot) (GProt) (* [g]', [g] *)

  let read_global ask getg (st: BaseComponents.t) x =
    if CVars.mem x st.cached then
      CPA.find x st.cpa
    else if is_unprotected ask x then
      fst (getg x)
    else if CPA.mem x st.cpa then
      VD.join (CPA.find x st.cpa) (snd (getg x))
    else
      snd (getg x)
    (* TODO: sideg? *)

  let write_global ask getg sideg (st: BaseComponents.t) x v =
    let cpa' = CPA.add x v st.cpa in
    if not (is_atomic ask) then
      (* TODO: update to match paper changes: conditional W' and side effect *)
      sideg x (v, VD.bot ());
    let cached' =
      if is_unprotected ask x then
        st.cached
      else
        CVars.add x st.cached
    in
    {st with cpa = cpa'; cached = cached'}

  let lock ask getg cpa m = cpa

  let unlock ask getg sideg (st: BaseComponents.t) m =
    (* TODO: what about G_m globals in cpa that weren't actually written? *)
    CPA.fold (fun x v (st: BaseComponents.t) ->
        if is_protected_by ask m x then ( (* is_in_Gm *)
          (* Extra precision in implementation to pass tests:
             If global is read-protected by multiple locks,
             then inner unlock shouldn't yet publish. *)
          if not Param.check_read_unprotected || is_unprotected_without ask ~write:false x m then
            sideg x (VD.bot (), v);

          if is_unprotected_without ask x m then (* is_in_V' *)
            {st with cpa = CPA.remove x st.cpa; cached = CVars.remove x st.cached}
          else
            st
        )
        else
          st
      ) st.cpa st

  let sync ask getg (st: BaseComponents.t) reason =
    match reason with
    | `Join -> (* required for branched thread creation *)
      let (st', sidegs) =
        CPA.fold (fun x v (((st: BaseComponents.t), sidegs) as acc) ->
            if is_global ask x && is_unprotected ask x then
              ({st with cpa = CPA.remove x st.cpa; cached = CVars.remove x st.cached}, (x, (v, VD.bot ())) :: sidegs)
            else
              acc
          ) st.cpa (st, [])
      in
      (st', sidegs)
    | `Return -> (* required for thread return *)
      begin match ThreadId.get_current ask with
        | `Lifted x when CPA.mem x st.cpa ->
          let v = CPA.find x st.cpa in
          ({st with cpa = CPA.remove x st.cpa; cached = CVars.remove x st.cached}, [(x, (v, VD.bot ()))])
        | _ ->
          (st, [])
      end
    | `Normal
    | `Init
    | `Thread ->
      (st, [])

  let escape ask getg sideg (st: BaseComponents.t) escaped =
    let cpa' = CPA.fold (fun x v acc ->
        if EscapeDomain.EscapedVars.mem x escaped then (
          sideg x (v, v);
          CPA.remove x acc
        )
        else
          acc
      ) st.cpa st.cpa
    in
    {st with cpa = cpa'}

  let enter_multithreaded ask getg sideg (st: BaseComponents.t) =
    CPA.fold (fun x v (st: BaseComponents.t) ->
        if is_global ask x then (
          sideg x (v, v);
          {st with cpa = CPA.remove x st.cpa; cached = CVars.remove x st.cached}
        )
        else
          st
      ) st.cpa st

  (* ??? *)
  let is_private ask x = true
end

module MinePrivBase =
struct
  include MutexGlobals
  let mutex_global x = x (* MutexGlobals.mutex_global not needed here because G is Prod anyway? *)

  module Lock = LockDomain.Addr
  module Lockset =
  struct
    include SetDomain.Make (Lock)
    let disjoint s t = is_empty (inter s t)
  end

  let rec conv_offset = function
    | `NoOffset -> `NoOffset
    | `Field (f, o) -> `Field (f, conv_offset o)
    (* TODO: better indices handling *)
    | `Index (_, o) -> `Index (IdxDom.top (), conv_offset o)

  let current_lockset (ask: Q.ask): Lockset.t =
    (* TODO: remove this global_init workaround *)
    if !GU.global_initialization then
      Lockset.empty ()
    else
      match ask Queries.CurrentLockset with
      | `LvalSet ls ->
        Q.LS.fold (fun (var, offs) acc ->
            Lockset.add (Lock.from_var_offset (var, conv_offset offs)) acc
          ) ls (Lockset.empty ())
      | _ -> failwith "MinePrivBase.current_lockset"


  let escape ask getg sideg st escaped = st
  let enter_multithreaded ask getg sideg (st: BaseComponents.t) = st

  (* ??? *)
  let is_private ask x = true
end

module MinePriv: PrivParam =
struct
  include MinePrivBase

  module Thread = ConcDomain.Thread
  module ThreadMap = MapDomain.MapBot (Thread) (VD)
  module GWeak =
  struct
    include MapDomain.MapBot (Lockset) (ThreadMap)
    let name () = "weak"
  end
  module GSync =
  struct
    include MapDomain.MapBot (Lockset) (CPA)
    let name () = "sync"
  end
  (* weak: G -> (2^M -> (T -> D)) *)
  (* sync: M -> (2^M -> (G -> D)) *)
  module G = Lattice.Prod (GWeak) (GSync)

  let global_init_thread = lazy (
    Goblintutil.create_var @@ makeGlobalVar "global_init" voidType
  )
  let current_thread (ask: Q.ask): Thread.t =
    if !GU.global_initialization then
      Lazy.force global_init_thread
    else
      ThreadId.get_current_unlift ask

  let read_global ask getg (st: BaseComponents.t) x =
    let s = current_lockset ask in
    GWeak.fold (fun s' tm acc ->
        if Lockset.disjoint s s' then
          ThreadMap.fold (fun t' v acc ->
              VD.join v acc
            ) tm acc
        else
          acc
      ) (fst (getg (mutex_global x))) (CPA.find x st.cpa)

  let write_global ask getg sideg (st: BaseComponents.t) x v =
    let s = current_lockset ask in
    let t = current_thread ask in
    let cpa' = CPA.add x v st.cpa in
    if not (!GU.earlyglobs && is_precious_glob x || NewPrivBase.is_atomic ask) then
      sideg (mutex_global x) (GWeak.add s (ThreadMap.add t v (ThreadMap.bot ())) (GWeak.bot ()), GSync.bot ());
    {st with cpa = cpa'}

  let lock ask getg (st: BaseComponents.t) m =
    let s = current_lockset ask in
    let cpa' = GSync.fold (fun s' cpa' acc ->
        if Lockset.disjoint s s' then
          CPA.join cpa' acc
        else
          acc
      ) (snd (getg (mutex_addr_to_varinfo m))) st.cpa
    in
    {st with cpa = cpa'}

  let unlock ask getg sideg (st: BaseComponents.t) m =
    let s = Lockset.remove m (current_lockset ask) in
    let t = current_thread ask in
    let side_cpa = CPA.filter (fun x _ ->
        GWeak.fold (fun s' tm acc ->
            (* TODO: swap 2^M and T partitioning for lookup by t here first? *)
            let v = ThreadMap.find t tm in
            (Lockset.mem m s' && not (VD.is_bot v)) || acc
          ) (fst (getg (mutex_global x))) false
      ) st.cpa
    in
    sideg (mutex_addr_to_varinfo m) (GWeak.bot (), GSync.add s side_cpa (GSync.bot ()));
    st

  let sync ask getg (st: BaseComponents.t) reason =
    match reason with
    | `Return -> (* required for thread return *)
      begin match ThreadId.get_current ask with
      | `Lifted x when CPA.mem x st.cpa ->
          let v = CPA.find x st.cpa in
          (st, [(mutex_global x, (GWeak.add (Lockset.empty ()) (ThreadMap.add x v (ThreadMap.bot ())) (GWeak.bot ()), GSync.bot ()))])
        | _ ->
          (st, [])
      end
    | `Normal
    | `Join (* TODO: no problem with branched thread creation here? *)
    | `Init
    | `Thread ->
      (st, [])
end

module MineNoThreadPriv: PrivParam =
struct
  include MinePrivBase

  module GWeak =
  struct
    include MapDomain.MapBot (Lockset) (VD)
    let name () = "weak"
  end
  module GSync =
  struct
    include MapDomain.MapBot (Lockset) (CPA)
    let name () = "sync"
  end
  (* weak: G -> (2^M -> D) *)
  (* sync: M -> (2^M -> (G -> D)) *)
  module G = Lattice.Prod (GWeak) (GSync)

  let read_global ask getg (st: BaseComponents.t) x =
    let s = current_lockset ask in
    GWeak.fold (fun s' v acc ->
        if Lockset.disjoint s s' then
          VD.join v acc
        else
          acc
      ) (fst (getg (mutex_global x))) (CPA.find x st.cpa)

  (* TODO: update to match paper changes: W set in Mine *)
  let write_global ask getg sideg (st: BaseComponents.t) x v =
    let s = current_lockset ask in
    let cpa' = CPA.add x v st.cpa in
    if not (!GU.earlyglobs && is_precious_glob x || NewPrivBase.is_atomic ask) then
      sideg (mutex_global x) (GWeak.add s v (GWeak.bot ()), GSync.bot ());
    {st with cpa = cpa'}

  let lock ask getg (st: BaseComponents.t) m =
    let s = current_lockset ask in
    let cpa' = GSync.fold (fun s' cpa' acc ->
        if Lockset.disjoint s s' then
          CPA.join cpa' acc
        else
          acc
      ) (snd (getg (mutex_addr_to_varinfo m))) st.cpa
    in
    {st with cpa = cpa'}

  let unlock ask getg sideg (st: BaseComponents.t) m =
    let s = Lockset.remove m (current_lockset ask) in
    let side_cpa = CPA.filter (fun x _ ->
        GWeak.fold (fun s' v acc ->
            (Lockset.mem m s' && not (VD.is_bot v)) || acc
          ) (fst (getg (mutex_global x))) false
      ) st.cpa
    in
    sideg (mutex_addr_to_varinfo m) (GWeak.bot (), GSync.add s side_cpa (GSync.bot ()));
    st

  let sync ask getg (st: BaseComponents.t) reason =
    match reason with
    | `Return -> (* required for thread return *)
      begin match ThreadId.get_current ask with
      | `Lifted x when CPA.mem x st.cpa ->
          let v = CPA.find x st.cpa in
          (st, [(mutex_global x, (GWeak.add (Lockset.empty ()) v (GWeak.bot ()), GSync.bot ()))])
        | _ ->
          (st, [])
      end
    | `Normal
    | `Join (* TODO: no problem with branched thread creation here? *)
    | `Init
    | `Thread ->
      (st, [])
end

module StatsPriv (Priv: PrivParam): PrivParam =
struct
  module G = Priv.G

  let time str f arg = Stats.time "priv" (Stats.time str f) arg

  let read_global ask getg st x = time "read_global" (Priv.read_global ask getg st) x
  let write_global ask getg sideg st x v = time "write_global" (Priv.write_global ask getg sideg st x) v
  let lock ask getg cpa m = time "lock" (Priv.lock ask getg cpa) m
  let unlock ask getg sideg st m = time "unlock" (Priv.unlock ask getg sideg st) m
  let sync reason ctx = time "sync" (Priv.sync reason) ctx
  let escape ask getg sideg st escaped = time "escape" (Priv.escape ask getg sideg st) escaped
  let enter_multithreaded ask getg sideg st = time "enter_multithreaded" (Priv.enter_multithreaded ask getg sideg) st

  let is_private = Priv.is_private
end