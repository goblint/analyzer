open Batteries
open GoblintCil
open Analyses
open BaseUtil
module Q = Queries

module IdxDom = ValueDomain.IndexDomain
module VD     = BaseDomain.VD

module ConfCheck =
struct
  module RequireMutexActivatedInit =
  struct
    let init () =
      let analyses = GobConfig.get_string_list "ana.activated" in
      let mutex_active = List.mem "mutex" analyses || not (List.mem "base" analyses) in
      if not mutex_active then failwith "Privatization (to be useful) requires the 'mutex' analysis to be enabled (it is currently disabled)"
  end

  module RequireMutexPathSensInit =
  struct
    let init () =
      RequireMutexActivatedInit.init ();
      let mutex_path_sens = List.mem "mutex" (GobConfig.get_string_list "ana.path_sens") in
      if not mutex_path_sens then failwith "The activated privatization requires the 'mutex' analysis to be enabled & path sensitive (it is currently enabled, but not path sensitive)";
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
end

module Protection =
struct
  let is_unprotected ask x: bool =
    let multi = ThreadFlag.has_ever_been_multi ask in
    (!GU.earlyglobs && not multi && not (is_excluded_from_earlyglobs x)) ||
    (
      multi &&
      ask.f (Q.MayBePublic {global=x; write=true})
    )

  let is_unprotected_without ask ?(write=true) x m: bool =
    ThreadFlag.has_ever_been_multi ask &&
    ask.f (Q.MayBePublicWithout {global=x; write; without_mutex=m})

  let is_protected_by ask m x: bool =
    is_global ask x &&
    not (VD.is_immediate_type x.vtype) &&
    ask.f (Q.MustBeProtectedBy {mutex=m; global=x; write=true})

  let protected_vars (ask: Q.ask): varinfo list =
    let module VS = Set.Make (CilType.Varinfo) in
    Q.LS.fold (fun (v, _) acc ->
        let m = ValueDomain.Addr.from_var ~is_modular:(ask.f IsModular) v in (* TODO: don't ignore offsets *)
        Q.LS.fold (fun l acc ->
            VS.add (fst l) acc (* always `NoOffset from mutex analysis *)
          ) (ask.f (Q.MustProtectedVars {mutex = m; write = true})) acc
      ) (ask.f Q.MustLockset) VS.empty
    |> VS.elements
end

module MutexGlobals =
struct
  module VMutex =
  struct
    include LockDomain.Addr
    let name () = "mutex"
    let show x = show x ^ ":mutex" (* distinguishable variant names for html *)
  end
  module VMutexInits = Printable.UnitConf (struct let name = "MUTEX_INITS" end)
  module VGlobal =
  struct
    include VarinfoV
    let name () = "global"
    let show x = show x ^ ":global" (* distinguishable variant names for html *)
  end
  module V =
  struct
    (* TODO: Either3? *)
    include Printable.Either (Printable.Either (VMutex) (VMutexInits)) (VGlobal)
    let name () = "MutexGlobals"
    let mutex x: t = `Left (`Left x)
    let mutex_inits: t = `Left (`Right ())
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
  module Lock = LockDomain.Addr

  module Lockset =
  struct
    include Printable.Std (* To make it Groupable *)
    include SetDomain.ToppedSet (Lock) (struct let topname = "All locks" end)
  end

  module MustLockset = SetDomain.Reverse (Lockset)

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
      let ls = ask.f Queries.MustLockset in
      Q.LS.fold (fun (var, offs) acc ->
          Lockset.add (Lock.from_var_offset ~is_modular:(ask.f IsModular)  (var, conv_offset offs)) acc
        ) ls (Lockset.empty ())

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

module type PerMutexTidCommonArg = sig
  val exclude_not_started: unit -> bool
  val exclude_must_joined: unit -> bool
end

module PerMutexTidCommon (Conf:PerMutexTidCommonArg) (LD:Lattice.S) =
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
    include Printable.Either (MutexGlobals.V) (TID)
    let mutex x = `Left (MutexGlobals.V.mutex x)
    let mutex_inits = `Left MutexGlobals.V.mutex_inits
    let global x = `Left (MutexGlobals.V.global x)
    let thread x = `Right x
  end

  module LLock =
  struct
    include Printable.Either (Locksets.Lock) (CilType.Varinfo)
    let mutex m = `Left m
    let global x = `Right x
  end

  (** Mutexes / globals to which values have been published, i.e. for which the initializers need not be read **)
  module LMust = struct
    include SetDomain.Reverse (SetDomain.ToppedSet (LLock) (struct let topname = "All locks" end))
    let name () = "LMust"
  end

  (* Map from locks to last written values thread-locally *)
  module L = MapDomain.MapBot_LiftTop (LLock) (LD)
  module GMutex = MapDomain.MapBot_LiftTop (ThreadIdDomain.ThreadLifted) (LD)
  module GThread = Lattice.Prod (LMust) (L)

  module G =
  struct
    include Lattice.Lift2 (GMutex) (GThread) (Printable.DefaultNames)

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

  let compatible (ask:Q.ask) current must_joined other =
    match current, other with
    | `Lifted current, `Lifted other ->
      if (TID.is_unique current) && (TID.equal current other) then
        false (* self-read *)
      else if Conf.exclude_not_started () && MHP.definitely_not_started (current, ask.f Q.CreatedThreads) other then
        false (* other is not started yet *)
      else if Conf.exclude_must_joined () && MHP.must_be_joined other must_joined then
        false (* accounted for in local information *)
      else
        true
    | _ -> true

  let get_relevant_writes_nofilter (ask:Q.ask) v =
    let current = ThreadId.get_current ask in
    let must_joined = ask.f Queries.MustJoinedThreads in
    GMutex.fold (fun k v acc ->
        if compatible ask current must_joined k then
          LD.join acc v
        else
          acc
      ) v (LD.bot ())

  let merge_all v =
    GMutex.fold (fun _ v acc -> LD.join acc v) v (LD.bot ())

  let startstate () = W.bot (), LMust.top (), L.bot ()
end
