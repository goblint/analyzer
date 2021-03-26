open Prelude.Ana
open Analyses
open GobConfig
open BaseUtil
module Q = Queries

module IdxDom = ValueDomain.IndexDomain

module VD     = BaseDomain.VD
module CPA    = BaseDomain.CPA
module BaseComponents = BaseDomain.BaseComponents

module CachedVars =
struct
  module VarSet = SetDomain.ToppedSet(Basetype.Variables) (struct let topname = "All Variables" end)
  include VarSet
  include Lattice.Reverse (VarSet)
  let name () = "definitely cached variables"
end


module type S =
sig
  module D: Lattice.S
  module G: Lattice.S

  val startstate: unit -> D.t

  val read_global: Q.ask -> (varinfo -> G.t) -> BaseComponents (D).t -> varinfo -> VD.t
  val write_global: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> BaseComponents (D).t -> varinfo -> VD.t -> BaseComponents (D).t

  val lock: Q.ask -> (varinfo -> G.t) -> BaseComponents (D).t -> LockDomain.Addr.t -> BaseComponents (D).t
  val unlock: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> BaseComponents (D).t -> LockDomain.Addr.t -> BaseComponents (D).t

  val sync: Q.ask -> (varinfo -> G.t) -> BaseComponents (D).t -> [`Normal | `Join | `Return | `Init | `Thread] -> BaseComponents (D).t * (varinfo * G.t) list

  val escape: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> BaseComponents (D).t -> EscapeDomain.EscapedVars.t -> BaseComponents (D).t
  val enter_multithreaded: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> BaseComponents (D).t -> BaseComponents (D).t

  (* TODO: better name *)
  val is_private: Q.ask -> varinfo -> bool

  val init: unit -> unit
  val finalize: unit -> unit
end

module NoInitFinalize =
struct
  let init () = ()
  let finalize () = ()
end

module OldPrivBase =
struct
  include NoInitFinalize
  module D = Lattice.Unit

  let startstate () = ()

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
module NoPriv: S =
struct
  include OldPrivBase

  module G = BaseDomain.VD

  let read_global ask getg (st: BaseComponents (D).t) x =
    getg x

  let write_global ask getg sideg (st: BaseComponents (D).t) x v =
    (* Here, an effect should be generated, but we add it to the local
     * state, waiting for the sync function to publish it. *)
    (* Copied from MainFunctor.update_variable *)
    if ((get_bool "exp.volatiles_are_top") && (is_always_unknown x)) then
      {st with cpa = CPA.add x (VD.top ()) st.cpa}
    else
      {st with cpa = CPA.add x v st.cpa}

  let is_private (a: Q.ask) (v: varinfo): bool = false

  let sync ask getg (st: BaseComponents (D).t) reason =
    (* For each global variable, we create the diff *)
    let add_var (v: varinfo) (value) ((st: BaseComponents (D).t),acc) =
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

module OldPriv: S =
struct
  include OldPrivBase

  module G = BaseDomain.VD

  let read_global ask getg (st: BaseComponents (D).t) x =
    match CPA.find x st.cpa with
    | `Bot -> (if M.tracing then M.tracec "get" "Using global invariant.\n"; getg x)
    | x -> (if M.tracing then M.tracec "get" "Using privatized version.\n"; x)

  let write_global ask getg sideg (st: BaseComponents (D).t) x v =
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

  let sync ask getg (st: BaseComponents (D).t) reason =
    let privates = sync_privates reason ask in
    let module BaseComponents = BaseComponents (D) in
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
  include NoInitFinalize

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
      M.warn_each (Pretty.sprint ~width:800 @@ Pretty.dprintf "NewPrivBase: ignoring offset %a%a" d_varinfo v LockDomain.Addr.Offs.pretty offs);
      v
    | _ -> failwith "NewPrivBase.mutex_addr_to_varinfo"

  (* let mutex_global x = x *)
  let mutex_global = RichVarinfo.Variables.map ~name:(fun x -> "MUTEX_GLOBAL_" ^ x.vname)
  let mutex_global x =
    let r = mutex_global x in
    if M.tracing then M.tracel "priv" "mutex_global %a = %a\n" d_varinfo x d_varinfo r;
    r
end

module PerMutexPrivBase =
struct
  include NewPrivBase
  include MutexGlobals

  module D = Lattice.Unit
  module G = CPA

  let startstate () = ()

  let mutex_inits = RichVarinfo.single ~name:"MUTEX_INITS"

  let get_m_with_mutex_inits ask getg m =
    let get_m = getg (mutex_addr_to_varinfo m) in
    let get_mutex_inits = getg (mutex_inits ()) in
    let is_in_Gm x _ = is_protected_by ask m x in
    let get_mutex_inits' = CPA.filter is_in_Gm get_mutex_inits in
    if M.tracing then M.tracel "priv" "get_m_with_mutex_inits %a:\n  get_m: %a\n  get_mutex_inits: %a\n  get_mutex_inits': %a\n" LockDomain.Addr.pretty m CPA.pretty get_m CPA.pretty get_mutex_inits CPA.pretty get_mutex_inits';
    CPA.join get_m get_mutex_inits'

  (** [get_m_with_mutex_inits] optimized for implementation-specialized [read_global]. *)
  let get_mutex_global_x_with_mutex_inits getg x =
    let get_mutex_global_x = getg (mutex_global x) in
    let get_mutex_inits = getg (mutex_inits ()) in
    match CPA.find_opt x get_mutex_global_x, CPA.find_opt x get_mutex_inits with
      | Some v1, Some v2 -> Some (VD.join v1 v2)
      | Some v, None
      | None, Some v -> Some v
      | None, None -> None

  let escape ask getg sideg (st: BaseComponents (D).t) escaped =
    let escaped_cpa = CPA.filter (fun x _ -> EscapeDomain.EscapedVars.mem x escaped) st.cpa in
    sideg (mutex_inits ()) escaped_cpa;

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

  let enter_multithreaded ask getg sideg (st: BaseComponents (D).t) =
    let global_cpa = CPA.filter (fun x _ -> is_global ask x) st.cpa in
    sideg (mutex_inits ()) global_cpa;

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
    ignore (Pretty.printf "READ GLOBAL %a (%a, %B) = %a\n" d_varinfo x d_loc !Tracing.current_loc (is_unprotected ask x) VD.pretty v);
    r *)
  let write_global ask getg sideg (st: BaseComponents (D).t) x v =
    let cpa' = CPA.add x v st.cpa in
    sideg (mutex_global x) (CPA.add x v (CPA.bot ()));
    {st with cpa = cpa'}
  (* let write_global ask getg sideg cpa x v =
    let cpa' = write_global ask getg sideg cpa x v in
    ignore (Pretty.printf "WRITE GLOBAL %a %a = %a\n" d_varinfo x VD.pretty v CPA.pretty cpa');
    cpa' *)

  let lock ask getg (st: BaseComponents (D).t) m =
    let get_m = get_m_with_mutex_inits ask getg m in
    let is_in_V x _ = is_protected_by ask m x && is_unprotected ask x in
    let cpa' = CPA.filter is_in_V get_m in
    if M.tracing then M.tracel "priv" "PerMutexOplusPriv.lock m=%a cpa'=%a\n" LockDomain.Addr.pretty m CPA.pretty cpa';
    {st with cpa = CPA.fold CPA.add cpa' st.cpa}
  let unlock ask getg sideg (st: BaseComponents (D).t) m =
    let is_in_Gm x _ = is_protected_by ask m x in
    let side_m_cpa = CPA.filter is_in_Gm st.cpa in
    if M.tracing then M.tracel "priv" "PerMutexOplusPriv.unlock m=%a side_m_cpa=%a\n" LockDomain.Addr.pretty m CPA.pretty side_m_cpa;
    sideg (mutex_addr_to_varinfo m) side_m_cpa;
    st

  let sync ask getg (st: BaseComponents (D).t) reason =
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
    if M.tracing then M.tracel "priv" "READ GLOBAL %a %B %a = %a\n" d_varinfo x (is_unprotected ask x) CPA.pretty st.cpa VD.pretty v;
    v
  let write_global ask getg sideg (st: BaseComponents (D).t) x v =
    let cpa' =
      if is_unprotected ask x then
        st.cpa
      else
        CPA.add x v st.cpa
    in
    if M.tracing then M.tracel "priv" "WRITE GLOBAL SIDE %a = %a\n" d_varinfo x VD.pretty v;
    sideg (mutex_global x) (CPA.add x v (CPA.bot ()));
    {st with cpa = cpa'}
  (* let write_global ask getg sideg cpa x v =
    let cpa' = write_global ask getg sideg cpa x v in
    ignore (Pretty.printf "WRITE GLOBAL %a %a = %a\n" d_varinfo x VD.pretty v CPA.pretty cpa');
    cpa' *)

  let lock ask getg (st: BaseComponents (D).t) m =
    let get_m = get_m_with_mutex_inits ask getg m in
    (* Additionally filter get_m in case it contains variables it no longer protects. *)
    let is_in_Gm x _ = is_protected_by ask m x in
    let get_m = CPA.filter is_in_Gm get_m in
    let long_meet m1 m2 = CPA.long_map2 VD.meet m1 m2 in
    let meet = long_meet st.cpa get_m in
    if M.tracing then M.tracel "priv" "LOCK %a:\n  get_m: %a\n  meet: %a\n" LockDomain.Addr.pretty m CPA.pretty get_m CPA.pretty meet;
    {st with cpa = meet}
  let unlock ask getg sideg (st: BaseComponents (D).t) m =
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

  let sync ask getg (st: BaseComponents (D).t) reason =
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

module PerGlobalVesalPriv: S =
struct
  include OldPrivBase

  module D = CachedVars
  module G = BaseDomain.VD

  let startstate () = D.top ()

  let read_global ask getg (st: BaseComponents (D).t) x =
    match CPA.find x st.cpa with
    | `Bot -> (if M.tracing then M.tracec "get" "Using global invariant.\n"; getg x)
    | x -> (if M.tracing then M.tracec "get" "Using privatized version.\n"; x)

  let write_global ask getg sideg (st: BaseComponents (D).t) x v =
    (* Here, an effect should be generated, but we add it to the local
     * state, waiting for the sync function to publish it. *)
    (* Copied from MainFunctor.update_variable *)
    if ((get_bool "exp.volatiles_are_top") && (is_always_unknown x)) then
      {st with cpa = CPA.add x (VD.top ()) st.cpa; priv = CachedVars.add x st.priv}
    else
      {st with cpa = CPA.add x v st.cpa; priv = CachedVars.add x st.priv}

  let is_invisible (a: Q.ask) (v: varinfo): bool =
    (not (ThreadFlag.is_multi a) && is_precious_glob v ||
    match a (Q.MayBePublic {global=v; write=false}) with `MayBool tv -> not tv | _ ->
    if M.tracing then M.tracel "osek" "isPrivate yields top(!!!!)";
    false)
  let is_private = is_invisible

  let is_protected (a: Q.ask) (v: varinfo): bool =
    (not (ThreadFlag.is_multi a) && is_precious_glob v ||
        match a (Q.MayBePublic {global=v; write=true}) with `MayBool tv -> not tv | _ -> false)

  let sync ask getg (st: BaseComponents (D).t) reason =
    let privates = sync_privates reason ask in
    (* For each global variable, we create the diff *)
    let add_var (v: varinfo) (value) ((st: BaseComponents (D).t),acc) =
      if M.tracing then M.traceli "globalize" ~var:v.vname "Tracing for %s\n" v.vname;
      let res =
        if is_global ask v then
          let protected = is_protected ask v in
          if privates && not (is_precious_glob v) || not protected then begin
            if M.tracing then M.tracec "globalize" "Publishing its value: %a\n" VD.pretty value;
            ({ st with cpa = CPA.remove v st.cpa; priv = CachedVars.remove v st.priv} , (v,value) :: acc)
          end else (* protected == true *)
            let (st, acc) = if not (CachedVars.mem v st.priv) then
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

module PerGlobalPriv (Param: PerGlobalPrivParam): S =
struct
  include NewPrivBase

  module D = CachedVars

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

  let startstate () = D.top ()

  let read_global ask getg (st: BaseComponents (D).t) x =
    if CachedVars.mem x st.priv then
      CPA.find x st.cpa
    else if is_unprotected ask x then
      fst (getg x)
    else if CPA.mem x st.cpa then
      VD.join (CPA.find x st.cpa) (snd (getg x))
    else
      snd (getg x)

  let write_global ask getg sideg (st: BaseComponents (D).t) x v =
    sideg x (v, VD.bot ());
    if is_unprotected ask x then
      st
    else
      {st with cpa = CPA.add x v st.cpa; priv = CachedVars.add x st.priv}

  let lock ask getg cpa m = cpa

  let unlock ask getg sideg (st: BaseComponents (D).t) m =
    (* TODO: what about G_m globals in cpa that weren't actually written? *)
    CPA.fold (fun x v (st: BaseComponents (D).t) ->
        if is_protected_by ask m x then ( (* is_in_Gm *)
          (* Extra precision in implementation to pass tests:
             If global is read-protected by multiple locks,
             then inner unlock shouldn't yet publish. *)
          if not Param.check_read_unprotected || is_unprotected_without ask ~write:false x m then
            sideg x (VD.bot (), v);

          if is_unprotected_without ask x m then (* is_in_V' *)
            {st with cpa = CPA.remove x st.cpa; priv = CachedVars.remove x st.priv}
          else
            st
        )
        else
          st
      ) st.cpa st

  let sync ask getg (st: BaseComponents (D).t) reason =
    match reason with
    | `Join -> (* required for branched thread creation *)
      let (st', sidegs) =
        CPA.fold (fun x v (((st: BaseComponents (D).t), sidegs) as acc) ->
            if is_global ask x && is_unprotected ask x then
              ({st with cpa = CPA.remove x st.cpa; priv = CachedVars.remove x st.priv}, (x, (v, VD.bot ())) :: sidegs)
            else
              acc
          ) st.cpa (st, [])
      in
      (st', sidegs)
    | `Return -> (* required for thread return *)
      begin match ThreadId.get_current ask with
        | `Lifted x when CPA.mem x st.cpa ->
          let v = CPA.find x st.cpa in
          ({st with cpa = CPA.remove x st.cpa; priv = CachedVars.remove x st.priv}, [(x, (v, VD.bot ()))])
        | _ ->
          (st, [])
      end
    | `Normal
    | `Init
    | `Thread ->
      (st, [])

  let escape ask getg sideg (st: BaseComponents (D).t) escaped =
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

  let enter_multithreaded ask getg sideg (st: BaseComponents (D).t) =
    CPA.fold (fun x v (st: BaseComponents (D).t) ->
        if is_global ask x then (
          sideg x (v, v);
          {st with cpa = CPA.remove x st.cpa; priv = CachedVars.remove x st.priv}
        )
        else
          st
      ) st.cpa st

  (* ??? *)
  let is_private ask x = true
end

module MinePrivBase =
struct
  include NoInitFinalize
  include MutexGlobals
  let mutex_global x = x (* MutexGlobals.mutex_global not needed here because G is Prod anyway? *)

  module D = Lattice.Unit

  let startstate () = ()

  module Lock = LockDomain.Addr
  module Lockset =
  struct
    include SetDomain.ToppedSet (Lock) (struct let topname = "All locks" end)
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
  let enter_multithreaded ask getg sideg (st: BaseComponents (D).t) = st

  (* ??? *)
  let is_private ask x = true
end

module MinePriv: S =
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

  let global_init_thread = RichVarinfo.single ~name:"global_init"
  let current_thread (ask: Q.ask): Thread.t =
    if !GU.global_initialization then
      global_init_thread ()
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
      ) (fst (getg (mutex_global x))) (CPA.find x st.cpa)

  let write_global ask getg sideg (st: BaseComponents (D).t) x v =
    let s = current_lockset ask in
    let t = current_thread ask in
    let cpa' = CPA.add x v st.cpa in
    if not (!GU.earlyglobs && is_precious_glob x) then
      sideg (mutex_global x) (GWeak.add s (ThreadMap.add t v (ThreadMap.bot ())) (GWeak.bot ()), GSync.bot ());
    {st with cpa = cpa'}

  let lock ask getg (st: BaseComponents (D).t) m =
    let s = current_lockset ask in
    let cpa' = GSync.fold (fun s' cpa' acc ->
        if Lockset.disjoint s s' then
          CPA.join cpa' acc
        else
          acc
      ) (snd (getg (mutex_addr_to_varinfo m))) st.cpa
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
          ) (fst (getg (mutex_global x))) false
      ) st.cpa
    in
    sideg (mutex_addr_to_varinfo m) (GWeak.bot (), GSync.add s side_cpa (GSync.bot ()));
    st

  let sync ask getg (st: BaseComponents (D).t) reason =
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

module MineNoThreadPriv: S =
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

  let read_global ask getg (st: BaseComponents (D).t) x =
    let s = current_lockset ask in
    GWeak.fold (fun s' v acc ->
        if Lockset.disjoint s s' then
          VD.join v acc
        else
          acc
      ) (fst (getg (mutex_global x))) (CPA.find x st.cpa)

  let write_global ask getg sideg (st: BaseComponents (D).t) x v =
    let s = current_lockset ask in
    let cpa' = CPA.add x v st.cpa in
    if not (!GU.earlyglobs && is_precious_glob x) then
      sideg (mutex_global x) (GWeak.add s v (GWeak.bot ()), GSync.bot ());
    {st with cpa = cpa'}

  let lock ask getg (st: BaseComponents (D).t) m =
    let s = current_lockset ask in
    let cpa' = GSync.fold (fun s' cpa' acc ->
        if Lockset.disjoint s s' then
          CPA.join cpa' acc
        else
          acc
      ) (snd (getg (mutex_addr_to_varinfo m))) st.cpa
    in
    {st with cpa = cpa'}

  let unlock ask getg sideg (st: BaseComponents (D).t) m =
    let s = Lockset.remove m (current_lockset ask) in
    let side_cpa = CPA.filter (fun x _ ->
        GWeak.fold (fun s' v acc ->
            (Lockset.mem m s' && not (VD.is_bot v)) || acc
          ) (fst (getg (mutex_global x))) false
      ) st.cpa
    in
    sideg (mutex_addr_to_varinfo m) (GWeak.bot (), GSync.add s side_cpa (GSync.bot ()));
    st

  let sync ask getg (st: BaseComponents (D).t) reason =
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

module MineWPriv: S =
struct
  include MinePrivBase

  module D = SetDomain.ToppedSet (Basetype.Variables) (struct let topname = "All Variables" end)

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

  let startstate () = D.empty ()

  let read_global ask getg (st: BaseComponents (D).t) x =
    let s = current_lockset ask in
    GWeak.fold (fun s' v acc ->
        if Lockset.disjoint s s' then
          VD.join v acc
        else
          acc
      ) (fst (getg (mutex_global x))) (CPA.find x st.cpa)

  let write_global ask getg sideg (st: BaseComponents (D).t) x v =
    let s = current_lockset ask in
    let cpa' = CPA.add x v st.cpa in
    if not (!GU.earlyglobs && is_precious_glob x) then
      sideg (mutex_global x) (GWeak.add s v (GWeak.bot ()), GSync.bot ());
    {st with cpa = cpa'; priv = D.add x st.priv}

  let lock ask getg (st: BaseComponents (D).t) m =
    let s = current_lockset ask in
    let cpa' = GSync.fold (fun s' cpa' acc ->
        if Lockset.disjoint s s' then
          CPA.join cpa' acc
        else
          acc
      ) (snd (getg (mutex_addr_to_varinfo m))) st.cpa
    in
    {st with cpa = cpa'}

  let unlock ask getg sideg (st: BaseComponents (D).t) m =
    let s = Lockset.remove m (current_lockset ask) in
    let is_in_W x _ = D.mem x st.priv in
    let side_cpa = CPA.filter is_in_W st.cpa in
    sideg (mutex_addr_to_varinfo m) (GWeak.bot (), GSync.add s side_cpa (GSync.bot ()));
    st

  let sync ask getg (st: BaseComponents (D).t) reason =
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

  let escape ask getg sideg st escaped = st
  let enter_multithreaded ask getg sideg (st: BaseComponents (D).t) = st
end

module PreciseDomains =
struct
  open MinePrivBase

  module MinLocksets = SetDomain.Hoare (Lattice.Reverse (Lockset)) (struct let topname = "All locksets" end) (* reverse Lockset because Hoare keeps maximal, but we need minimal *)

  module L =
  struct
    include MapDomain.MapBot_LiftTop (Lock) (MinLocksets)
    let name () = "L"
  end

  module W =
  struct
    include MapDomain.MapBot_LiftTop (Basetype.Variables) (MinLocksets)
    let name () = "W"
  end

  module P =
  struct
    (* Note different Map order! *)
    include MapDomain.MapTop_LiftBot (Basetype.Variables) (MinLocksets)
    let name () = "P"

    let mem_V x m p =
      let p_x = find_opt x p |? MinLocksets.singleton (Lockset.empty ()) in (* ensure exists has something to check for thread returns *)
      MinLocksets.for_all (fun s -> Lockset.mem m s) p_x
      (* MinLocksets.leq p_x (MinLocksets.singleton (Lockset.singleton m)) *)
  end
end

module MineLazyPriv: S =
struct
  include MinePrivBase
  include PreciseDomains

  module V =
  struct
    include MapDomain.MapBot_LiftTop (Lock) (CachedVars)
    let name () = "V"
  end
  module D = Lattice.Prod (V) (L)

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

  let startstate () = (V.bot (), L.bot ())

  let lockset_init () = Lockset.All

  let read_global ask getg (st: BaseComponents (D).t) x =
    let s = current_lockset ask in
    let (vv, l) = st.priv in
    let d_cpa = CPA.find x st.cpa in
    let d_sync = L.fold (fun m bs acc ->
        if not (CachedVars.mem x (V.find m vv)) then
          let syncs = snd (getg (mutex_addr_to_varinfo m)) in
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
    let weaks = fst (getg (mutex_global x)) in
    let d_weak = GWeak.fold (fun s' v acc ->
        if Lockset.disjoint s s' then
          VD.join v acc
        else
          acc
      ) weaks (VD.bot ())
    in
    let d_init =
      (* TODO: V.exists *)
      if not (V.for_all (fun m cached -> not (CachedVars.mem x cached)) vv) then
        VD.bot ()
      else
        GWeak.find (lockset_init ()) weaks
    in
    let d_weak = VD.join d_weak d_init in
    let d = VD.join d_cpa (VD.join d_sync d_weak) in
    d

  let write_global ask getg sideg (st: BaseComponents (D).t) x v =
    let s = current_lockset ask in
    let (vv, l) = st.priv in
    let v' = Lockset.fold (fun m acc ->
        V.add m (CachedVars.add x (V.find m acc)) acc
      ) s vv
    in
    let cpa' = CPA.add x v st.cpa in
    if not (!GU.earlyglobs && is_precious_glob x) then
      sideg (mutex_global x) (GWeak.add s v (GWeak.bot ()), GSync.bot ());
    {st with cpa = cpa'; priv = (v', l)}

  let lock ask getg (st: BaseComponents (D).t) m =
    let s = current_lockset ask in
    let (v, l) = st.priv in
    let v' = V.add m (CachedVars.empty ()) v in
    let l' = L.add m (MinLocksets.singleton s) l in
    {st with priv = (v', l')}

  let unlock ask getg sideg (st: BaseComponents (D).t) m =
    let s = Lockset.remove m (current_lockset ask) in
    let is_in_G x _ = is_global ask x in
    let side_cpa = CPA.filter is_in_G st.cpa in
    sideg (mutex_addr_to_varinfo m) (GWeak.bot (), GSync.add s side_cpa (GSync.bot ()));
    (* m stays in v, l *)
    (* TODO: why is it so imprecise now? *)
    st

  let sync ask getg (st: BaseComponents (D).t) reason =
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

  let escape ask getg sideg (st: BaseComponents (D).t) escaped =
    let cpa' = CPA.fold (fun x v acc ->
        if EscapeDomain.EscapedVars.mem x escaped then (
          sideg (mutex_global x) (GWeak.add (lockset_init ()) v (GWeak.bot ()), GSync.bot ());
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
          sideg (mutex_global x) (GWeak.add (lockset_init ()) v (GWeak.bot ()), GSync.bot ());
          {st with cpa = CPA.remove x st.cpa}
        )
        else
          st
      ) st.cpa st
end

module PerGlobalHistoryPriv: S =
struct
  include MinePrivBase
  include PreciseDomains

  module D = Lattice.Prod (W) (P)

  module GWeakW = MapDomain.MapBot (Lockset) (VD)
  module GWeak =
  struct
    include MapDomain.MapBot (Lockset) (GWeakW)
    let name () = "weak"
  end
  module GSyncW = MapDomain.MapBot (Lockset) (CPA)
  module GSync =
  struct
    include MapDomain.MapBot (Lockset) (GSyncW)
    let name () = "sync"
  end
  (* weak: G -> (S:2^M -> (W:2^M -> D)) *)
  (* sync: M -> (S:2^M -> (W:2^M -> (G -> D))) *)
  module G = Lattice.Prod (GWeak) (GSync)

  let startstate () = (W.bot (), P.top ())

  let lockset_init () = Lockset.All

  let read_global ask getg (st: BaseComponents (D).t) x =
    let s = current_lockset ask in
    let (w, p) = st.priv in
    let p_x = P.find_opt x p |? MinLocksets.singleton (Lockset.empty ()) in (* ensure exists has something to check for thread returns *)
    let d_cpa = CPA.find x st.cpa in
    let d_sync = Lockset.fold (fun m acc ->
        if not (P.mem_V x m p) then
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
            ) (snd (getg (mutex_addr_to_varinfo m))) acc
        else
          acc
      ) s (VD.bot ())
    in
    let weaks = fst (getg (mutex_global x)) in
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
    let d = VD.join d_cpa (VD.join d_sync d_weak) in
    d

  let write_global ask getg sideg (st: BaseComponents (D).t) x v =
    let s = current_lockset ask in
    let (w, p) = st.priv in
    let w' = W.add x (MinLocksets.singleton s) w in
    let p' = P.add x (MinLocksets.singleton s) p in
    let p' = P.map (fun s' -> MinLocksets.add s s') p' in
    let cpa' = CPA.add x v st.cpa in
    if not (!GU.earlyglobs && is_precious_glob x) then
      sideg (mutex_global x) (GWeak.add s (GWeakW.add s v (GWeakW.bot ())) (GWeak.bot ()), GSync.bot ());
    (* TODO: publish all g under M_g? *)
    {st with cpa = cpa'; priv = (w', p')}

  let lock ask getg (st: BaseComponents (D).t) m =
    st

  let unlock ask getg sideg (st: BaseComponents (D).t) m =
    let s = Lockset.remove m (current_lockset ask) in
    let (w, p) = st.priv in
    let p' = P.map (fun s' -> MinLocksets.add s s') p in
    if M.tracing then M.traceli "priv" "unlock %a %a\n" Lock.pretty m CPA.pretty st.cpa;
    let side_gsyncw = CPA.fold (fun x v acc ->
        if is_global ask x then (
          let w_x = W.find x w in
          if M.tracing then M.trace "priv" "gsyncw %a %a %a\n" d_varinfo x VD.pretty v MinLocksets.pretty w_x;
          MinLocksets.fold (fun w acc ->
              GSyncW.add w (CPA.add x v (GSyncW.find w acc)) acc
            ) w_x acc
        ) else
          acc
      ) st.cpa (GSyncW.bot ())
    in
    if M.tracing then M.traceu "priv" "unlock %a %a\n" Lock.pretty m GSyncW.pretty side_gsyncw;
    sideg (mutex_addr_to_varinfo m) (GWeak.bot (), GSync.add s side_gsyncw (GSync.bot ()));
    {st with priv = (w, p')}

  let sync ask getg (st: BaseComponents (D).t) reason =
    match reason with
    | `Return -> (* required for thread return *)
      begin match ThreadId.get_current ask with
      | `Lifted x when CPA.mem x st.cpa ->
          let v = CPA.find x st.cpa in
          (st, [(mutex_global x, (GWeak.add (Lockset.empty ()) (GWeakW.add (Lockset.empty ()) v (GWeakW.bot ())) (GWeak.bot ()), GSync.bot ()))])
        | _ ->
          (st, [])
      end
    | `Normal
    | `Join (* TODO: no problem with branched thread creation here? *)
    | `Init
    | `Thread ->
      (st, [])

  let escape ask getg sideg (st: BaseComponents (D).t) escaped =
    let s = current_lockset ask in
    CPA.fold (fun x v acc ->
        if EscapeDomain.EscapedVars.mem x escaped then (
          let (w, p) = st.priv in
          let p' = P.add x (MinLocksets.singleton s) p in
          sideg (mutex_global x) (GWeak.add (Lockset.empty ()) (GWeakW.add (lockset_init ()) v (GWeakW.bot ())) (GWeak.bot ()), GSync.bot ());
          {st with cpa = CPA.remove x st.cpa; priv = (w, p')}
        )
        else
          st
      ) st.cpa st

  let enter_multithreaded ask getg sideg (st: BaseComponents (D).t) =
    CPA.fold (fun x v (st: BaseComponents (D).t) ->
        if is_global ask x then (
          sideg (mutex_global x) (GWeak.add (Lockset.empty ()) (GWeakW.add (lockset_init ()) v (GWeakW.bot ())) (GWeak.bot ()), GSync.bot ());
          {st with cpa = CPA.remove x st.cpa}
        )
        else
          st
      ) st.cpa st
end

module MinePerGlobalPriv: S =
struct
  include MinePrivBase
  include PreciseDomains

  module D = Lattice.Prod3 (L) (W) (P)

  module GWeakW = MapDomain.MapBot (Lockset) (VD)
  module GWeak =
  struct
    include MapDomain.MapBot (Lockset) (GWeakW)
    let name () = "weak"
  end
  module GSyncW = MapDomain.MapBot (Lockset) (CPA)
  module GSync =
  struct
    include MapDomain.MapBot (Lockset) (GSyncW)
    let name () = "sync"
  end
  (* weak: G -> (S:2^M -> (W:2^M -> D)) *)
  (* sync: M -> (S:2^M -> (W:2^M -> (G -> D))) *)
  module G = Lattice.Prod (GWeak) (GSync)

  let startstate () = (L.bot (), W.bot (), P.top ())

  let lockset_init () = Lockset.All

  let read_global ask getg (st: BaseComponents (D).t) x =
    let s = current_lockset ask in
    let (l, w, p) = st.priv in
    let p_x = P.find_opt x p |? MinLocksets.singleton (Lockset.empty ()) in (* ensure exists has something to check for thread returns *)
    let d_cpa = CPA.find x st.cpa in
    let d_m_sync = L.fold (fun m bs acc ->
        if not (P.mem_V x m p) then
          let syncs = snd (getg (mutex_addr_to_varinfo m)) in
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
    let weaks = fst (getg (mutex_global x)) in
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
        if not (P.mem_V x m p) then
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
            ) (snd (getg (mutex_addr_to_varinfo m))) acc
        else
          acc
      ) s (VD.bot ())
    in
    let d_g_weak = d_m_weak in (* happen to coincide *)
    let d_g = VD.join d_g_sync d_g_weak in
    let d = VD.join d_cpa (VD.meet d_m d_g) in
    d

  let write_global ask getg sideg (st: BaseComponents (D).t) x v =
    let s = current_lockset ask in
    let (l, w, p) = st.priv in
    let w' = W.add x (MinLocksets.singleton s) w in
    let p' = P.add x (MinLocksets.singleton s) p in
    let p' = P.map (fun s' -> MinLocksets.add s s') p' in
    let cpa' = CPA.add x v st.cpa in
    if not (!GU.earlyglobs && is_precious_glob x) then
      sideg (mutex_global x) (GWeak.add s (GWeakW.add s v (GWeakW.bot ())) (GWeak.bot ()), GSync.bot ());
    (* TODO: publish all g under M_g? *)
    {st with cpa = cpa'; priv = (l, w', p')}

  let lock ask getg (st: BaseComponents (D).t) m =
    let s = current_lockset ask in
    let (l, w, p) = st.priv in
    let l' = L.add m (MinLocksets.singleton s) l in
    {st with priv = (l', w, p)}

  let unlock ask getg sideg (st: BaseComponents (D).t) m =
    let s = Lockset.remove m (current_lockset ask) in
    let (l, w, p) = st.priv in
    let p' = P.map (fun s' -> MinLocksets.add s s') p in
    let side_gsyncw = CPA.fold (fun x v acc ->
        if is_global ask x then
          MinLocksets.fold (fun w acc ->
              GSyncW.add w (CPA.add x v (GSyncW.find w acc)) acc
            ) (W.find x w) acc
        else
          acc
      ) st.cpa (GSyncW.bot ())
    in
    sideg (mutex_addr_to_varinfo m) (GWeak.bot (), GSync.add s side_gsyncw (GSync.bot ()));
    (* m stays in l *)
    {st with priv = (l, w, p')}

  let sync ask getg (st: BaseComponents (D).t) reason =
    match reason with
    | `Return -> (* required for thread return *)
      begin match ThreadId.get_current ask with
      | `Lifted x when CPA.mem x st.cpa ->
          let v = CPA.find x st.cpa in
          (st, [(mutex_global x, (GWeak.add (Lockset.empty ()) (GWeakW.add (Lockset.empty ()) v (GWeakW.bot ())) (GWeak.bot ()), GSync.bot ()))])
        | _ ->
          (st, [])
      end
    | `Normal
    | `Join (* TODO: no problem with branched thread creation here? *)
    | `Init
    | `Thread ->
      (st, [])

  let escape ask getg sideg (st: BaseComponents (D).t) escaped =
    let s = current_lockset ask in
    CPA.fold (fun x v acc ->
        if EscapeDomain.EscapedVars.mem x escaped then (
          let (l, w, p) = st.priv in
          let p' = P.add x (MinLocksets.singleton s) p in
          sideg (mutex_global x) (GWeak.add (Lockset.empty ()) (GWeakW.add (lockset_init ()) v (GWeakW.bot ())) (GWeak.bot ()), GSync.bot ());
          {st with cpa = CPA.remove x st.cpa; priv = (l, w, p')}
        )
        else
          st
      ) st.cpa st

  let enter_multithreaded ask getg sideg (st: BaseComponents (D).t) =
    CPA.fold (fun x v (st: BaseComponents (D).t) ->
        if is_global ask x then (
          sideg (mutex_global x) (GWeak.add (Lockset.empty ()) (GWeakW.add (lockset_init ()) v (GWeakW.bot ())) (GWeak.bot ()), GSync.bot ());
          {st with cpa = CPA.remove x st.cpa}
        )
        else
          st
      ) st.cpa st
end

module StatsPriv (Priv: S): S with module D = Priv.D =
struct
  module D = Priv.D
  module G = Priv.G

  let time str f arg = Stats.time "priv" (Stats.time str f) arg

  let startstate = Priv.startstate
  let read_global ask getg st x = time "read_global" (Priv.read_global ask getg st) x
  let write_global ask getg sideg st x v = time "write_global" (Priv.write_global ask getg sideg st x) v
  let lock ask getg cpa m = time "lock" (Priv.lock ask getg cpa) m
  let unlock ask getg sideg st m = time "unlock" (Priv.unlock ask getg sideg st) m
  let sync reason ctx = time "sync" (Priv.sync reason) ctx
  let escape ask getg sideg st escaped = time "escape" (Priv.escape ask getg sideg st) escaped
  let enter_multithreaded ask getg sideg st = time "enter_multithreaded" (Priv.enter_multithreaded ask getg sideg) st

  let is_private = Priv.is_private

  let init () = time "init" (Priv.init) ()
  let finalize () = time "finalize" (Priv.finalize) ()
end

module PrecisionDumpPriv (Priv: S): S with module D = Priv.D =
struct
  include Priv

  module LVH = Hashtbl.Make (Printable.Prod (Basetype.ProgLines) (Basetype.Variables))

  let is_dumping = ref false
  let lvh = LVH.create 113

  let init () =
    Priv.init ();
    is_dumping := get_string "exp.priv-prec-dump" <> "";
    LVH.clear lvh

  let read_global ask getg st x =
    let v = Priv.read_global ask getg st x in
    if !GU.in_verifying_stage && !is_dumping then
      LVH.modify_def (VD.bot ()) (!Tracing.current_loc, x) (VD.join v) lvh;
    v

  let dump () =
    let f = open_out_bin (get_string "exp.priv-prec-dump") in
    (* LVH.iter (fun (l, x) v ->
        ignore (Pretty.printf "%a %a = %a\n" d_loc l d_varinfo x VD.pretty v)
      ) lvh; *)
    Marshal.output f lvh;
    close_out_noerr f

  let finalize () =
    if !is_dumping then
      dump ();
    Priv.finalize ()
end

let priv_module: (module S) Lazy.t =
  lazy (
    let module Priv: S =
      (val match get_string "exp.privatization" with
        | "none" -> (module NoPriv: S)
        | "old" -> (module OldPriv)
        | "mutex-oplus" -> (module PerMutexOplusPriv)
        | "mutex-meet" -> (module PerMutexMeetPriv)
        | "global" -> (module PerGlobalPriv (struct let check_read_unprotected = false end))
        | "global-read" -> (module PerGlobalPriv (struct let check_read_unprotected = true end))
        | "global-vesal" -> (module PerGlobalVesalPriv)
        | "mine" -> (module MinePriv)
        | "mine-nothread" -> (module MineNoThreadPriv)
        | "mine-W" -> (module MineWPriv)
        | "mine-lazy" -> (module MineLazyPriv)
        | "global-history" -> (module PerGlobalHistoryPriv)
        | "mine-global" -> (module MinePerGlobalPriv)
        | _ -> failwith "exp.privatization: illegal value"
      )
    in
    let module Priv = PrecisionDumpPriv (Priv) in
    (* let module Priv = StatsPriv (Priv) in *)
    (module Priv)
  )

let get_priv (): (module S) =
  Lazy.force priv_module