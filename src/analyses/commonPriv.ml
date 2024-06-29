(** Thread-modular value analysis utilities for {!BasePriv} and {!RelationPriv}. *)

open Batteries
open GoblintCil
open Analyses
open BaseUtil
module Q = Queries

module IdxDom = ValueDomain.IndexDomain
module VD     = BaseDomain.VD

module type AtomicParam =
sig
  val handle_atomic: bool
  (** Whether to handle SV-COMP atomic blocks (experimental). *)
end

module NoAtomic: AtomicParam =
struct
  let handle_atomic = false
end

module ConfCheck =
struct
  module RequireMutexActivatedInit =
  struct
    let init () =
      let analyses = GobConfig.get_string_list "ana.activated" in
      let mutex_active = List.mem "mutex" analyses || not (List.mem "base" analyses) in
      if not mutex_active then failwith "Privatization (to be useful) requires the 'mutex' analysis to be enabled (it is currently disabled)"
  end

  module RequireMutexPathSensOneMainInit =
  struct
    let init () =
      RequireMutexActivatedInit.init ();
      let mutex_path_sens = List.mem "mutex" (GobConfig.get_string_list "ana.path_sens") in
      if not mutex_path_sens then failwith "The activated privatization requires the 'mutex' analysis to be enabled & path sensitive (it is currently enabled, but not path sensitive)";
      let mainfuns = List.length @@ GobConfig.get_list "mainfun" in
      if not (mainfuns = 1) then failwith "The activated privatization requires exactly one main function to be specified";
      ()
  end

  module RequireThreadFlagPathSensInit =
  struct
    let init () =
      let threadflag_active = List.mem "threadflag" (GobConfig.get_string_list "ana.activated") in
      if threadflag_active then
        let threadflag_path_sens = List.mem "threadflag" (GobConfig.get_string_list "ana.path_sens") in
        if not threadflag_path_sens then failwith "The activated privatization requires the 'threadflag' analysis to be path sensitive if it is enabled (it is currently enabled, but not path sensitive)";
        ()
  end

  (** Whether branched thread creation needs to be handled by [sync `Join] of privatization. *)
  let branched_thread_creation () =
    let threadflag_active = List.mem "threadflag" (GobConfig.get_string_list "ana.activated") in
    if threadflag_active then
      let threadflag_path_sens = List.mem "threadflag" (GobConfig.get_string_list "ana.path_sens") in
      not threadflag_path_sens
    else
      true

  (** Whether branched thread creation at start nodes of procedures needs to be handled by [sync `JoinCall] of privatization. *)
  let branched_thread_creation_at_call (ask:Queries.ask) =
    let threadflag_active = List.mem "threadflag" (GobConfig.get_string_list "ana.activated") in
    if threadflag_active then
      let sens = GobConfig.get_string_list "ana.ctx_sens" in
      let threadflag_ctx_sens = match sens with
        | [] -> (* use values of "ana.ctx_insens" (blacklist) *)
          not (List.mem "threadflag" @@ GobConfig.get_string_list "ana.ctx_insens")
        | sens -> (* use values of "ana.ctx_sens" (whitelist) *)
          List.mem "threadflag" sens
      in
      if not threadflag_ctx_sens then
        true
      else
        ask.f (Queries.GasExhausted)
    else
      true
end

module Protection =
struct
  open Q.Protection
  let is_unprotected ask ?(protection=Strong) x: bool =
    let multi = if protection = Weak then ThreadFlag.is_currently_multi ask else ThreadFlag.has_ever_been_multi ask in
    (!GobConfig.earlyglobs && not multi && not (is_excluded_from_earlyglobs x)) ||
    (
      multi &&
      ask.f (Q.MayBePublic {global=x; write=true; protection})
    )

  let is_unprotected_without ask ?(write=true) ?(protection=Strong) x m: bool =
    (if protection = Weak then ThreadFlag.is_currently_multi ask else ThreadFlag.has_ever_been_multi ask) &&
    ask.f (Q.MayBePublicWithout {global=x; write; without_mutex=m; protection})

  let is_protected_by ask ?(protection=Strong) m x: bool =
    is_global ask x &&
    not (VD.is_immediate_type x.vtype) &&
    ask.f (Q.MustBeProtectedBy {mutex=m; global=x; write=true; protection})

  let protected_vars (ask: Q.ask): varinfo list =
    LockDomain.MustLockset.fold (fun ml acc ->
        Q.VS.join (ask.f (Q.MustProtectedVars {mutex = ml; write = true})) acc
      ) (ask.f Q.MustLockset) (Q.VS.empty ())
    |> Q.VS.elements
end

module MutexGlobals =
struct
  module VMutex =
  struct
    include LockDomain.Addr
    let name () = "mutex"
  end
  module VMutexInits = Printable.UnitConf (struct let name = "MUTEX_INITS" end)
  module VGlobal =
  struct
    include VarinfoV
    let name () = "global"
  end
  module V =
  struct
    include Printable.Either3Conf (struct include Printable.DefaultConf let expand2 = false end) (VMutex) (VMutexInits) (VGlobal)
    let name () = "MutexGlobals"
    let mutex x: t = `Left x
    let mutex_inits: t = `Middle ()
    let global x: t = `Right x
  end

  let iter_sys_vars getg vq vf =
    match vq with
    | VarQuery.Global g -> vf (V.global g)
    | _ -> ()
end

module MayVars =
struct
  include SetDomain.ToppedSet (Basetype.Variables) (struct let topname = "All Variables" end)
  let name () = "may variables"
end

module MustVars =
struct
  include SetDomain.Reverse (MayVars)
  let name () = "must variables"
end

module Locksets =
struct
  module Lock =
  struct
    include LockDomain.Addr
    let name () = "lock"
  end

  module Lockset = SetDomain.ToppedSet (Lock) (struct let topname = "All locks" end)

  module MustLockset = SetDomain.Reverse (Lockset)

  let current_lockset (ask: Q.ask): Lockset.t =
    (* TODO: remove this global_init workaround *)
    if !AnalysisState.global_initialization then
      Lockset.empty ()
    else
      let mls = ask.f Queries.MustLockset in
      LockDomain.MustLockset.fold (fun ml acc -> Lockset.add (Addr (LockDomain.MustLock.to_mval ml)) acc) mls (Lockset.empty ()) (* TODO: use MustLockset as Lockset *)

  (* TODO: reversed SetDomain.Hoare *)
  module MinLocksets = HoareDomain.Set_LiftTop (MustLockset) (struct let topname = "All locksets" end) (* reverse Lockset because Hoare keeps maximal, but we need minimal *)
end

module WriteCenteredD =
struct
  open Locksets

  module W =
  struct
    include MapDomain.MapBot_LiftTop (Basetype.Variables) (MinLocksets)
    let name () = "W"
  end

  module P =
  struct
    (* Note different Map order! *)
    (* MapTop because default value in P must be top of MinLocksets,
       as opposed to bottom in W. *)
    include MapDomain.MapTop_LiftBot (Basetype.Variables) (MinLocksets)
    let name () = "P"

    (* TODO: change MinLocksets.exists/top instead? *)
    let find x p = find_opt x p |? MinLocksets.singleton (Lockset.empty ()) (* ensure exists has something to check for thread returns *)
  end
end

module type Digest =
sig
  include Printable.S

  val current: Q.ask -> t
  val accounted_for: Q.ask -> current:t -> other:t -> bool
end

(** Digest to be used for analyses that account for all join-local contributions in some locally tracked datastructure, akin to the L component from the analyses in
    @see <https://doi.org/10.1007/978-3-031-30044-8_2> Schwarz, M., Saan, S., Seidl, H., Erhard, J., Vojdani, V. Clustered Relational Thread-Modular Abstract Interpretation with Local Traces.
*)
module ThreadDigest: Digest =
struct
  include ThreadIdDomain.ThreadLifted

  module TID = ThreadIdDomain.Thread

  let current (ask: Q.ask) =
    ThreadId.get_current ask

  let accounted_for (ask: Q.ask) ~(current: t) ~(other: t) =
    match current, other with
    | `Lifted current, `Lifted other ->
      if TID.is_unique current && TID.equal current other then
        true (* self-read *)
      else if GobConfig.get_bool "ana.relation.priv.not-started" && MHP.definitely_not_started (current, ask.f Q.CreatedThreads) other then
        true (* other is not started yet *)
      else if GobConfig.get_bool "ana.relation.priv.must-joined" && MHP.must_be_joined other (ask.f Queries.MustJoinedThreads) then
        true (* accounted for in local information *)
      else
        false
    | _ -> false
end

(** Ego-Lane Derived digest based on whether given threads have been started yet, can be used to refine any analysis
    @see PhD thesis of M. Schwarz once it is published ;)
*)
module ThreadNotStartedDigest:Digest =
struct
  include ThreadIdDomain.ThreadLifted

  module TID = ThreadIdDomain.Thread

  let current (ask: Q.ask) =
    ThreadId.get_current ask

  let accounted_for (ask: Q.ask) ~(current: t) ~(other: t) =
    match current, other with
    | `Lifted current, `Lifted other ->
      MHP.definitely_not_started (current, ask.f Q.CreatedThreads) other
    | _ -> false
end

module PerMutexTidCommon (Digest: Digest) (LD:Lattice.S) =
struct
  include ConfCheck.RequireThreadFlagPathSensInit

  module TID = ThreadIdDomain.Thread

  (** May written variables. *)
  module W =
  struct
    include MayVars
    let name () = "W"
  end

  module V =
  struct
    include Printable.EitherConf (struct let expand1 = false let expand2 = true end) (MutexGlobals.V) (TID)
    let mutex x = `Left (MutexGlobals.V.mutex x)
    let mutex_inits = `Left MutexGlobals.V.mutex_inits
    let global x = `Left (MutexGlobals.V.global x)
    let thread x = `Right x
  end

  module LLock =
  struct
    include Printable.Either (Locksets.Lock) (struct include CilType.Varinfo let name () = "global" end)
    let mutex m = `Left m
    let global x = `Right x
  end

  (** Mutexes / globals to which values have been published, i.e. for which the initializers need not be read **)
  module LMust = struct
    include SetDomain.Reverse (SetDomain.ToppedSet (LLock) (struct let topname = "All locks" end))
    let name () = "LMust"
  end

  (* Map from locks to last written values thread-locally *)
  module L =
  struct
    include MapDomain.MapBot_LiftTop (LLock) (LD)
    let name () = "L"
  end
  module GMutex = MapDomain.MapBot_LiftTop (Digest) (LD)
  module GThread = Lattice.Prod (LMust) (L)

  module G =
  struct
    include Lattice.Lift2Conf (struct include Printable.DefaultConf let expand1 = false let expand2 = false end) (GMutex) (GThread)

    let mutex = function
      | `Bot -> GMutex.bot ()
      | `Lifted1 x -> x
      | _ -> failwith "PerMutexMeetPrivTID.mutex"
    let thread = function
      | `Bot -> GThread.bot ()
      | `Lifted2 x -> x
      | _ -> failwith "PerMutexMeetPrivTID.thread"
    let create_mutex mutex = `Lifted1 mutex
    let create_global global = `Lifted1 global
    let create_thread thread = `Lifted2 thread
  end

  module D = Lattice.Prod3 (W) (LMust) (L)

  let get_relevant_writes_nofilter (ask:Q.ask) v =
    let current = Digest.current ask in
    GMutex.fold (fun k v acc ->
        if not (Digest.accounted_for ask ~current ~other:k) then
          LD.join acc v
        else
          acc
      ) v (LD.bot ())

  let merge_all v =
    GMutex.fold (fun _ v acc -> LD.join acc v) v (LD.bot ())

  let startstate () = W.bot (), LMust.top (), L.bot ()
end


let lift_lock (ask: Q.ask) f st (addr: LockDomain.Addr.t) =
  (* Should be in sync with:
     1. LocksetAnalysis.MakeMust.event
     2. MutexAnalysis.Spec.Arg.add
     3. LockDomain.MustLocksetRW.add_mval_rw *)
  match addr with
  | UnknownPtr -> st
  | Addr (v, _) when ask.f (IsMultiple v) -> st
  | Addr mv when LockDomain.Mval.is_definite mv -> f st addr
  | Addr _
  | NullPtr
  | StrPtr _ -> st

let lift_unlock (ask: Q.ask) f st (addr: LockDomain.Addr.t) =
  (* Should be in sync with:
     1. LocksetAnalysis.MakeMust.event
     2. MutexAnalysis.Spec.Arg.remove
     3. MutexAnalysis.Spec.Arg.remove_all
     4. LockDomain.MustLocksetRW.remove_mval_rw *)
  match addr with
  | UnknownPtr ->
    LockDomain.MustLockset.fold (fun ml st ->
        (* call privatization's unlock only with definite lock *)
        f st (LockDomain.Addr.Addr (LockDomain.MustLock.to_mval ml)) (* TODO: no conversion *)
      ) (ask.f MustLockset) st
  | StrPtr _
  | NullPtr -> st
  | Addr mv when LockDomain.Mval.is_definite mv -> f st addr
  | Addr mv ->
    LockDomain.MustLockset.fold (fun ml st ->
        if LockDomain.MustLock.semantic_equal_mval ml mv = Some false then
          st
        else
          (* call privatization's unlock only with definite lock *)
          f st (Addr (LockDomain.MustLock.to_mval ml)) (* TODO: no conversion *)
      ) (ask.f MustLockset) st
