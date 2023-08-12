(** Must lockset and protecting lockset analysis ([mutex]). *)

module M = Messages
module Addr = ValueDomain.Addr
module Lockset = LockDomain.Lockset
module Mutexes = LockDomain.Mutexes
module LF = LibraryFunctions
open GoblintCil
open Analyses
open Batteries

module VarSet = SetDomain.Make (Basetype.Variables)

module Spec =
struct
  module Arg =
  struct
    module Multiplicity = struct
      (* the maximum multiplicity which we keep track of precisely *)
      let max_count () = 4

      module Count = Lattice.Reverse (
          Lattice.Chain (
          struct
            let n () = max_count () + 1
            let names x = if x = max_count () then Format.asprintf ">= %d" x else Format.asprintf "%d" x
          end
          )
        )

      include MapDomain.MapTop_LiftBot (ValueDomain.Addr) (Count)

      let increment v x =
        let current = find v x in
        if current = max_count () then
          x
        else
          add v (current + 1) x

      let decrement v x =
        let current = find v x in
        if current = 0 then
          (x, true)
        else
          (add v (current - 1) x, current - 1 = 0)
    end

    module D = struct include Lattice.Prod(Lockset)(Multiplicity)
      let empty () = (Lockset.empty (), Multiplicity.empty ())
    end


    (** Global data is collected using dirty side-effecting. *)

    (* Two global invariants:
       1. varinfo -> set of mutexes  --  used for protecting locksets (M[g])
       2. mutex -> set of varinfos  --  used for protected variables (G_m), only collected during postsolving (!) *)

    module V =
    struct
      include Printable.Either (struct include CilType.Varinfo let name () = "protecting" end) (struct include ValueDomain.Addr let name () = "protected" end)
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
      val make: write:bool -> recovered:bool -> Mutexes.t -> t
      val get: write:bool -> Queries.Protection.t -> t -> Mutexes.t
    end = struct
      include MakeP (LockDomain.Simple)

      let make ~write ~recovered locks =
        (* If the access is not a write, set to T so intersection with current write-protecting is identity *)
        let wlocks = if write then locks else Mutexes.top () in
        if recovered then
          (* If we are in single-threaded mode again, this does not need to be added to set of mutexes protecting in mt-mode only *)
          ((locks, wlocks), (Mutexes.top (), Mutexes.top ()))
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
      include Lattice.Lift2 (GProtecting) (GProtected) (Printable.DefaultNames)

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

    let add ctx (l:Mutexes.elt*bool) =
      let s,m = ctx.local in
      let s' = Lockset.add l s in
      match Addr.to_mval (fst l) with
      | Some mval when MutexTypeAnalysis.must_be_recursive ctx mval ->
        (s', Multiplicity.increment (fst l) m)
      | _ -> (s', m)

    let remove' ctx ~warn l =
      let s, m = ctx.local in
      let rm s = Lockset.remove (l, true) (Lockset.remove (l, false) s) in
      if warn &&  (not (Lockset.mem (l,true) s || Lockset.mem (l,false) s)) then M.warn "unlocking mutex which may not be held";
      match Addr.to_mval l with
      | Some mval when MutexTypeAnalysis.must_be_recursive ctx mval ->
        let m',rmed = Multiplicity.decrement l m in
        if rmed then
          (rm s, m')
        else
          (s, m')
      | _ -> (rm s, m)

    let remove = remove' ~warn:true

    let remove_all ctx =
      (* Mutexes.iter (fun m ->
           ctx.emit (MustUnlock m)
         ) (D.export_locks ctx.local); *)
      (* TODO: used to have remove_nonspecial, which kept v.vname.[0] = '{' variables *)
      M.warn "unlocking unknown mutex which may not be held";
      (Lockset.empty (), Multiplicity.empty ())

    let empty () = (Lockset.empty (), Multiplicity.empty ())
  end
  include LocksetAnalysis.MakeMust (Arg)
  let name () = "mutex"

  module D = Arg.D (* help type checker using explicit constraint *)
  module P = IdentityP (D)

  module V = Arg.V
  module GProtecting = Arg.GProtecting
  module GProtected = Arg.GProtected
  module G = Arg.G

  module GM = Hashtbl.Make (ValueDomain.Addr)

  let max_protected = ref 0
  let num_mutexes = ref 0
  let sum_protected = ref 0

  let init _ =
    max_protected := 0;
    num_mutexes := 0;
    sum_protected := 0

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let ls, m = ctx.local in
    (* get the set of mutexes protecting the variable v in the given mode *)
    let protecting ~write mode v = GProtecting.get ~write mode (G.protecting (ctx.global (V.protecting v))) in
    let non_overlapping locks1 locks2 = Mutexes.is_empty @@ Mutexes.inter locks1 locks2 in
    match q with
    | Queries.MayBePublic _ when Lockset.is_bot ls -> false
    | Queries.MayBePublic {global=v; write; protection} ->
      let held_locks = Lockset.export_locks (Lockset.filter snd ls) in
      let protecting = protecting ~write protection v in
      (* TODO: unsound in 29/24, why did we do this before? *)
      (* if Mutexes.mem verifier_atomic (Lockset.export_locks ctx.local) then
        false
      else *)
      non_overlapping held_locks protecting
    | Queries.MayBePublicWithout _ when Lockset.is_bot ls -> false
    | Queries.MayBePublicWithout {global=v; write; without_mutex; protection} ->
      let held_locks = Lockset.export_locks @@ fst @@ Arg.remove' ctx ~warn:false without_mutex in
      let protecting = protecting ~write protection v in
      (* TODO: unsound in 29/24, why did we do this before? *)
      (* if Mutexes.mem verifier_atomic (Lockset.export_locks (Lockset.remove (without_mutex, true) ctx.local)) then
        false
      else *)
      non_overlapping held_locks protecting
    | Queries.MustBeProtectedBy {mutex; global=v; write; protection} ->
      let mutex_lockset = Lockset.export_locks @@ Lockset.singleton (mutex, true) in
      let protecting = protecting ~write protection v in
      (* TODO: unsound in 29/24, why did we do this before? *)
      (* if LockDomain.Addr.equal mutex verifier_atomic then
        true
      else *)
      Mutexes.leq mutex_lockset protecting
    | Queries.MustLockset ->
      let held_locks = Lockset.export_locks (Lockset.filter snd ls) in
      let ls = Mutexes.fold (fun addr ls ->
          match Addr.to_mval addr with
          | Some (var, offs) -> Queries.LS.add (var, Addr.Offs.to_exp offs) ls
          | None -> ls
        ) held_locks (Queries.LS.empty ())
      in
      ls
    | Queries.MustBeAtomic ->
      let held_locks = Lockset.export_locks (Lockset.filter snd ls) in
      Mutexes.mem (LockDomain.Addr.of_var LF.verifier_atomic_var) held_locks
    | Queries.MustProtectedVars {mutex = m; write} ->
      let protected = GProtected.get ~write Strong (G.protected (ctx.global (V.protected m))) in
      VarSet.fold (fun v acc ->
          Queries.LS.add (v, `NoOffset) acc
        ) protected (Queries.LS.empty ())
    | Queries.IterSysVars (Global g, f) ->
      f (Obj.repr (V.protecting g)) (* TODO: something about V.protected? *)
    | WarnGlobal g ->
      let g: V.t = Obj.obj g in
      begin match g with
        | `Left g' -> (* protecting *)
          if GobConfig.get_bool "dbg.print_protection" then (
            let protecting = GProtecting.get ~write:false Strong (G.protecting (ctx.global g)) in (* readwrite protecting *)
            let s = Mutexes.cardinal protecting in
            M.info_noloc ~category:Race "Variable %a read-write protected by %d mutex(es): %a" CilType.Varinfo.pretty g' s Mutexes.pretty protecting
          )
        | `Right m -> (* protected *)
          if GobConfig.get_bool "dbg.print_protection" then (
            let protected = GProtected.get ~write:false Strong (G.protected (ctx.global g)) in (* readwrite protected *)
            let s = VarSet.cardinal protected in
            max_protected := max !max_protected s;
            sum_protected := !sum_protected + s;
            incr num_mutexes;
            M.info_noloc ~category:Race "Mutex %a read-write protects %d variable(s): %a" ValueDomain.Addr.pretty m s VarSet.pretty protected
          )
      end
    | _ -> Queries.Result.top q

  module A =
  struct
    include Lockset
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

  let access ctx (a: Queries.access) =
    fst ctx.local

  let event ctx e octx =
    match e with
    | Events.Access {exp; lvals; kind; _} when ThreadFlag.has_ever_been_multi (Analyses.ask_of_ctx ctx) -> (* threadflag query in post-threadspawn ctx *)
      let is_recovered_to_st = not (ThreadFlag.is_currently_multi (Analyses.ask_of_ctx ctx)) in
      (* must use original (pre-assign, etc) ctx queries *)
      let old_access var_opt offs_opt =
        (* TODO: this used to use ctx instead of octx, why? *)
        (*privatization*)
        match var_opt with
        | Some v ->
          if not (Lockset.is_bot (fst octx.local)) then
            let locks = Lockset.export_locks (Lockset.filter snd (fst octx.local)) in
            let write = match kind with
              | Write | Free -> true
              | Read -> false
              | Spawn -> false (* TODO: nonsense? *)
            in
            let s = GProtecting.make ~write ~recovered:is_recovered_to_st locks in
            ctx.sideg (V.protecting v) (G.create_protecting s);

            if !AnalysisState.postsolving then (
              let protecting mode = GProtecting.get ~write mode (G.protecting (ctx.global (V.protecting v))) in
              let held_strong = protecting Strong in
              let held_weak = protecting Weak in
              let vs = VarSet.singleton v in
              let protected = G.create_protected @@ GProtected.make ~write vs in
              Mutexes.iter (fun addr -> ctx.sideg (V.protected addr) protected) held_strong;
              (* If the mutex set here is top, it is actually not accessed *)
              if is_recovered_to_st && not @@ Mutexes.is_top held_weak then
                Mutexes.iter (fun addr -> ctx.sideg (V.protected addr) protected) held_weak;
            )
        | None -> M.info ~category:Unsound "Write to unknown address: privatization is unsound."
      in
      let module LS = Queries.LS in
      let has_escaped g = octx.ask (Queries.MayEscape g) in
      let on_lvals ls =
        let ls = LS.filter (fun (g,_) -> g.vglob || has_escaped g) ls in
        let f (var, offs) =
          let coffs = Offset.Exp.to_cil offs in
          if CilType.Varinfo.equal var dummyFunDec.svar then
            old_access None (Some coffs)
          else
            old_access (Some var) (Some coffs)
        in
        LS.iter f ls
      in
      begin match lvals with
        | ls when not (LS.is_top ls) && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) ls) ->
          (* the case where the points-to set is non top and does not contain unknown values *)
          on_lvals ls
        | ls when not (LS.is_top ls) ->
          (* the case where the points-to set is non top and contains unknown values *)
          (* now we need to access all fields that might be pointed to: is this correct? *)
          begin match octx.ask (ReachableUkTypes exp) with
            | ts when Queries.TS.is_top ts ->
              ()
            | ts ->
              let f = function
                | TComp (_, _) -> true
                | _ -> false
              in
              if Queries.TS.exists f ts then
                old_access None None
          end;
          on_lvals ls
        | _ ->
          old_access None None
      end;
      ctx.local
    | _ ->
      event ctx e octx (* delegate to must lockset analysis *)

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
