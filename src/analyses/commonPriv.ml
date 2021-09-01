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
    (!GU.earlyglobs && not multi && not (is_precious_glob x)) ||
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

  let is_atomic ask: bool =
    ask Q.MustBeAtomic
end

module MutexGlobalsBase =
struct
  let mutex_addr_to_varinfo = function
    | LockDomain.Addr.Addr (v, `NoOffset) -> v
    | LockDomain.Addr.Addr (v, offs) ->
      M.warn_each ~msg:(Pretty.sprint ~width:800 @@ Pretty.dprintf "MutexGlobalsBase: ignoring offset %a%a" d_varinfo v LockDomain.Addr.Offs.pretty offs) ();
      v
    | _ -> failwith "MutexGlobalsBase.mutex_addr_to_varinfo"
end

module ImplicitMutexGlobals =
struct
  include MutexGlobalsBase
  let mutex_global x = x
end

module ExplicitMutexGlobals =
struct
  include MutexGlobalsBase
  let mutex_global: varinfo -> varinfo = RichVarinfo.Variables.map ~name:(fun x -> "MUTEX_GLOBAL_" ^ x.vname) (* explicit type to force call without ?size *)
  let mutex_global x =
    let r = mutex_global x in
    if M.tracing then M.tracel "priv" "mutex_global %a = %a\n" d_varinfo x d_varinfo r;
    r
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
      let ls = ask.f Queries.CurrentLockset in
      Q.LS.fold (fun (var, offs) acc ->
          Lockset.add (Lock.from_var_offset (var, conv_offset offs)) acc
        ) ls (Lockset.empty ())

  (* TODO: reversed SetDomain.Hoare *)
  module MinLocksets = HoareDomain.Set_LiftTop (Lattice.Reverse (Lockset)) (struct let topname = "All locksets" end) (* reverse Lockset because Hoare keeps maximal, but we need minimal *)
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
      let analyses = GobConfig.get_list "ana.activated" in
      let mutex_active = List.exists (fun x -> Json.string x="mutex") analyses || List.for_all (fun x -> Json.string x<>"base") analyses in
      if not mutex_active then failwith "Privatization (to be useful) requires the 'mutex' analysis to be enabled (it is currently disabled)"
  end

  module RequireMutexPathSensInit =
  struct
    let init () =
      RequireMutexActivatedInit.init ();
      let mutex_path_sens = List.exists (fun x -> Json.string x="mutex") (GobConfig.get_list "ana.path_sens") in
      if not mutex_path_sens then failwith "The activated privatization requires the 'mutex' analysis to be enabled & path sensitive (it is currently enabled, but not path sensitive)";
      ()
  end

end
