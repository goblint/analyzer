(** Must lockset and protecting lockset analysis ([mutex]). *)

module M = Messages
module Mval = ValueDomain.Mval
module Addr = ValueDomain.Addr
module AddrRW = LockDomain.AddrRW
module MustLockset = LockDomain.MustLockset
module MustLocksetRW = LockDomain.MustLocksetRW
module MustMultiplicity = LockDomain.MustMultiplicity
module LF = LibraryFunctions
open GoblintCil
open Analyses
open Batteries

module VarSet = SetDomain.Make (Basetype.Variables)

module Spec =
struct
  module Arg =
  struct
    module D =
    struct
      include Lattice.Prod (MustLocksetRW) (MustMultiplicity)
      let empty () = (MustLocksetRW.empty (), MustMultiplicity.empty ())
    end


    (** Global data is collected using dirty side-effecting. *)

    (* Two global invariants:
       1. varinfo -> set of mutexes  --  used for protecting locksets (M[g])
       2. mutex -> set of varinfos  --  used for protected variables (G_m), only collected during postsolving (!) *)

    module V =
    struct
      include Printable.Either (struct include CilType.Varinfo let name () = "protecting" end) (struct include LockDomain.MustLock let name () = "protected" end)
      let name () = "mutex"
      let protecting x = `Left x
      let protected x = `Right x
      let is_write_only = function
        | `Left _ -> false
        | `Right _ -> true
    end

    module MakeP (G0: Lattice.S) = struct
      module ReadWrite =
      struct
        include G0
        let name () = "readwrite"
      end

      module Write =
      struct
        include G0
        let name () = "write"
      end

      module P = Lattice.Prod (ReadWrite) (Write)
      include Lattice.Prod (P) (P)

      let name () = "strong protection * weak protection"

      let get ~write protection (s,w) =
        let (rw, w) = match protection with
          | Queries.Protection.Strong -> s
          | Weak -> w
        in
        if write then w else rw
    end

    (** Collects information about which variables are protected by which mutexes *)
    module GProtecting: sig
      include Lattice.S
      val make: write:bool -> recovered:bool -> MustLockset.t -> t
      val get: write:bool -> Queries.Protection.t -> t -> MustLockset.t
    end = struct
      include MakeP (MustLockset)

      let make ~write ~recovered locks =
        (* If the access is not a write, set to T so intersection with current write-protecting is identity *)
        let wlocks = if write then locks else MustLockset.all () in
        if recovered then
          (* If we are in single-threaded mode again, this does not need to be added to set of mutexes protecting in mt-mode only *)
          ((locks, wlocks), (MustLockset.all (), MustLockset.all ()))
        else
          ((locks, wlocks), (locks, wlocks))
    end


    (** Collects information about which mutex protects which variable *)
    module GProtected: sig
      include Lattice.S
      val make: write:bool -> VarSet.t -> t
      val get: write:bool -> Queries.Protection.t -> t -> VarSet.t
    end = struct
      include MakeP (VarSet)

      let make ~write vs =
        let vs_empty = VarSet.empty () in
        if write then
          ((vs_empty, vs), (vs_empty, vs))
        else
          ((vs, vs_empty), (vs, vs_empty))
    end

    module G =
    struct
      include Lattice.Lift2Conf (struct include Printable.DefaultConf let expand1 = false let expand2 = false end) (GProtecting) (GProtected)

      let protecting = function
        | `Bot -> GProtecting.bot ()
        | `Lifted1 x -> x
        | _ -> failwith "Mutex.protecting"
      let protected = function
        | `Bot -> GProtected.bot ()
        | `Lifted2 x -> x
        | _ -> failwith "Mutex.protected"
      let create_protecting protecting = `Lifted1 protecting
      let create_protected protected = `Lifted2 protected
    end

    let add man ((addr, rw): AddrRW.t): D.t =
      match addr with
      | Addr ((v, o) as mv) ->
        let (s, m) = man.local in
        let s' = MustLocksetRW.add_mval_rw (mv, rw) s in
        let m' =
          match man.ask (Queries.MutexType (v, Offset.Unit.of_offs o)) with
          | `Lifted Recursive -> MustMultiplicity.increment mv m
          | `Lifted NonRec ->
            if MustLocksetRW.mem_mval mv s then
              M.error ~category:M.Category.Behavior.Undefined.double_locking "Acquiring a non-recursive mutex that is already held";
            m
          | `Bot | `Top -> m
        in
        (s', m')
      | NullPtr ->
        M.warn "locking NULL mutex";
        man.local
      | StrPtr _
      | UnknownPtr -> man.local

    let remove' man ~warn (addr: Addr.t): D.t =
      match addr with
      | StrPtr _
      | UnknownPtr -> man.local
      | NullPtr ->
        if warn then
          M.warn "unlocking NULL mutex";
        man.local
      | Addr mv ->
        let (s, m) = man.local in
        if warn && not (MustLocksetRW.mem_mval mv s) then
          M.warn "unlocking mutex (%a) which may not be held" Mval.pretty mv;
        if MutexTypeAnalysis.must_be_recursive man mv then (
          let (m', rmed) = MustMultiplicity.decrement mv m in
          if rmed then
            (* TODO: don't repeat the same semantic_equal checks *)
            (* TODO: rmed per lockset element, not aggregated *)
            (MustLocksetRW.remove_mval mv s, m')
          else
            (s, m')
        )
        else
          (MustLocksetRW.remove_mval mv s, m) (* Should decrement something if may be recursive? No: https://github.com/goblint/analyzer/pull/1430#discussion_r1615266081. *)

    let remove = remove' ~warn:true

    let remove_all man: D.t =
      (* Mutexes.iter (fun m ->
           man.emit (MustUnlock m)
         ) (D.export_locks man.local); *)
      (* TODO: used to have remove_nonspecial, which kept v.vname.[0] = '{' variables *)
      M.warn "unlocking unknown mutex which may not be held";
      D.empty ()
  end
  include LocksetAnalysis.MakeMust (Arg)
  let name () = "mutex"

  module D = Arg.D (* help type checker using explicit constraint *)
  module P = IdentityP (D)

  module V = Arg.V
  module GProtecting = Arg.GProtecting
  module GProtected = Arg.GProtected
  module G = Arg.G

  module GM = Hashtbl.Make (LockDomain.MustLock)

  let max_protected = ref 0
  let num_mutexes = ref 0
  let sum_protected = ref 0

  let init _ =
    max_protected := 0;
    num_mutexes := 0;
    sum_protected := 0

  let query (man: (D.t, _, _, V.t) man) (type a) (q: a Queries.t): a Queries.result =
    let ls, m = man.local in
    (* get the set of mutexes protecting the variable v in the given mode *)
    let protecting ~write mode v = GProtecting.get ~write mode (G.protecting (man.global (V.protecting v))) in
    match q with
    | Queries.MayBePublic _ when MustLocksetRW.is_all ls -> false
    | Queries.MayBePublic {global=v; write; protection} ->
      let held_locks = MustLocksetRW.to_must_lockset (MustLocksetRW.filter snd ls) in
      let protecting = protecting ~write protection v in
      (* TODO: unsound in 29/24, why did we do this before? *)
      (* if Mutexes.mem verifier_atomic (Lockset.export_locks man.local) then
        false
      else *)
      MustLockset.disjoint held_locks protecting
    | Queries.MayBePublicWithout _ when MustLocksetRW.is_all ls -> false
    | Queries.MayBePublicWithout {global=v; write; without_mutex; protection} ->
      let held_locks = MustLockset.remove without_mutex (MustLocksetRW.to_must_lockset ls) in
      let protecting = protecting ~write protection v in
      (* TODO: unsound in 29/24, why did we do this before? *)
      (* if Mutexes.mem verifier_atomic (Lockset.export_locks (Lockset.remove (without_mutex, true) man.local)) then
        false
      else *)
      MustLockset.disjoint held_locks protecting
    | Queries.MustBeProtectedBy {mutex = ml; global=v; write; protection} ->
      let protecting = protecting ~write protection v in
      (* TODO: unsound in 29/24, why did we do this before? *)
      (* if LockDomain.Addr.equal mutex (LockDomain.Addr.of_var LF.verifier_atomic_var) then
        true
      else *)
      MustLockset.mem ml protecting
    | Queries.MustProtectingLocks {global; write} ->
      protecting ~write Strong global
    | Queries.MustLockset ->
      let held_locks = MustLocksetRW.to_must_lockset (MustLocksetRW.filter snd ls) in
      held_locks
    | Queries.MustBeAtomic ->
      let held_locks = MustLocksetRW.to_must_lockset (MustLocksetRW.filter snd ls) in
      MustLockset.mem (LF.verifier_atomic_var, `NoOffset) held_locks (* TODO: Mval.of_var *)
    | Queries.MustProtectedVars {mutex; write} ->
      let protected = GProtected.get ~write Strong (G.protected (man.global (V.protected mutex))) in
      VarSet.fold (fun v acc ->
          Queries.VS.add v acc
        ) protected (Queries.VS.empty ())
    | Queries.IterSysVars (Global g, f) ->
      f (Obj.repr (V.protecting g)) (* TODO: something about V.protected? *)
    | WarnGlobal g ->
      let g: V.t = Obj.obj g in
      begin match g with
        | `Left g' -> (* protecting *)
          if GobConfig.get_bool "dbg.print_protection" then (
            let protecting = GProtecting.get ~write:false Strong (G.protecting (man.global g)) in (* readwrite protecting *)
            let s = MustLockset.cardinal protecting in
            M.info_noloc ~category:Race "Variable %a read-write protected by %d mutex(es): %a" CilType.Varinfo.pretty g' s MustLockset.pretty protecting
          )
        | `Right m -> (* protected *)
          if GobConfig.get_bool "dbg.print_protection" then (
            let protected = GProtected.get ~write:false Strong (G.protected (man.global g)) in (* readwrite protected *)
            let s = VarSet.cardinal protected in
            max_protected := max !max_protected s;
            sum_protected := !sum_protected + s;
            incr num_mutexes;
            M.info_noloc ~category:Race "Mutex %a read-write protects %d variable(s): %a" LockDomain.MustLock.pretty m s VarSet.pretty protected
          )
      end
    | _ -> Queries.Result.top q

  module A =
  struct
    include MustLocksetRW
    let name () = "lock"
    let may_race ls1 ls2 =
      (* not mutually exclusive *)
      not @@ exists (fun ((m1, w1) as l1) ->
          if w1 then
            (* write lock is exclusive with write lock or read lock *)
            mem l1 ls2 || mem (m1, false) ls2
          else
            (* read lock is exclusive with just write lock *)
            mem (m1, true) ls2
        ) ls1
    let should_print ls = not (is_empty ls)
  end

  let access man (a: Queries.access) =
    fst man.local

  let event (man: (D.t, _, _, V.t) man) e (oman: (D.t, _, _, _) man) =
    match e with
    | Events.Access {exp; ad; kind; _} when ThreadFlag.has_ever_been_multi (Analyses.ask_of_man man) -> (* threadflag query in post-threadspawn man *)
      let is_recovered_to_st = not (ThreadFlag.is_currently_multi (Analyses.ask_of_man man)) in
      (* must use original (pre-assign, etc) man queries *)
      let old_access var_opt =
        (* TODO: this used to use man instead of oman, why? *)
        (*privatization*)
        match var_opt with
        | Some v ->
          if not (MustLocksetRW.is_all (fst oman.local)) then
            let locks = MustLocksetRW.to_must_lockset (MustLocksetRW.filter snd (fst oman.local)) in
            let write = match kind with
              | Write | Free -> true
              | Read -> false
              | Call
              | Spawn -> false (* TODO: nonsense? *)
            in
            let s = GProtecting.make ~write ~recovered:is_recovered_to_st locks in
            man.sideg (V.protecting v) (G.create_protecting s);

            if !AnalysisState.postsolving then (
              let protecting mode = GProtecting.get ~write mode (G.protecting (man.global (V.protecting v))) in
              let held_strong = protecting Strong in
              let held_weak = protecting Weak in
              let vs = VarSet.singleton v in
              let protected = G.create_protected @@ GProtected.make ~write vs in
              MustLockset.iter (fun ml -> man.sideg (V.protected ml) protected) held_strong;
              (* If the mutex set here is top, it is actually not accessed *)
              if is_recovered_to_st && not @@ MustLockset.is_all held_weak then
                MustLockset.iter (fun ml -> man.sideg (V.protected ml) protected) held_weak;
            )
        | None -> M.info ~category:Unsound "Write to unknown address: privatization is unsound."
      in
      let module AD = Queries.AD in
      let has_escaped g = oman.ask (Queries.MayEscape g) in
      let on_ad ad =
        let f = function
          | AD.Addr.Addr (g,_) when g.vglob || has_escaped g -> old_access (Some g)
          | UnknownPtr -> old_access None
          | _ -> ()
        in
        AD.iter f ad
      in
      begin match ad with
        | ad when not (AD.is_top ad) ->
          (* the case where the points-to set is non top and does not contain unknown values *)
          on_ad ad
        | ad ->
          (* the case where the points-to set is non top and contains unknown values *)
          (* now we need to access all fields that might be pointed to: is this correct? *)
          begin match oman.ask (ReachableUkTypes exp) with
            | ts when Queries.TS.is_top ts ->
              ()
            | ts ->
              let f = function
                | TComp (_, _) -> true
                | _ -> false
              in
              if Queries.TS.exists f ts then
                old_access None
          end;
          on_ad ad
          (* | _ ->
             old_access None None *) (* TODO: what about this case? *)
      end;
      man.local
    | _ ->
      event man e oman (* delegate to must lockset analysis *)

  let finalize () =
    if GobConfig.get_bool "dbg.print_protection" then (
      M.msg_group Info ~category:Race "Mutex read-write protection summary" [
        (Pretty.dprintf "Number of mutexes: %d" !num_mutexes, None);
        (Pretty.dprintf "Max number variables of protected by a mutex: %d" !max_protected, None);
        (Pretty.dprintf "Total number of protected variables (including duplicates): %d" !sum_protected, None);
      ]
    )
end

let _ =
  MCP.register_analysis ~dep:["mutexEvents"; "access"] (module Spec : MCPSpec)
