open Prelude.Ana
open Analyses
open BaseUtil
module Q = Queries

module IdxDom = ValueDomain.IndexDomain
module VD     = BaseDomain.VD


module Protection =
struct
  let is_unprotected ask x: bool =
    let multi = ThreadFlag.is_multi ask in
    (!GU.earlyglobs && not multi && not (is_excluded_from_earlyglobs x)) ||
    (
      multi &&
      ask.f (Q.MayBePublic {global=x; write=true})
    )

  let is_unprotected_without ask ?(write=true) x m: bool =
    ThreadFlag.is_multi ask &&
    ask.f (Q.MayBePublicWithout {global=x; write; without_mutex=m})

  let is_protected_by ask m x: bool =
    is_global ask x &&
    not (VD.is_immediate_type x.vtype) &&
    ask.f (Q.MustBeProtectedBy {mutex=m; global=x; write=true})

  let protected_vars (ask: Q.ask): varinfo list =
    Q.LS.fold (fun (v, _) acc ->
        let m = ValueDomain.Addr.from_var v in (* TODO: don't ignore offsets *)
        Q.LS.fold (fun l acc ->
            fst l :: acc (* TODO: don't ignore offsets *)
          ) (ask.f (Q.MustProtectedVars m)) acc
      ) (ask.f Q.MustLockset) []
    |> List.unique_cmp ~cmp:CilType.Varinfo.compare (* TODO: use set *)
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
          Lockset.add (Lock.from_var_offset (var, conv_offset offs)) acc
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
