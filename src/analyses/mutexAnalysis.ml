(** Protecting mutex analysis. Must locksets locally and for globals. *)

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
    module D = Lockset

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

    module MakeG (G0: Lattice.S) =
    struct
      module ReadWriteNoRecover =
      struct
        include G0
        let name () = "readwriteNoRecover"
      end
      module WriteNoRecover =
      struct
        include G0
        let name () = "writeNoRecover"
      end
      module ReadWriteRecover =
      struct
        include G0
        let name () = "readwriteRecover"
      end
      module WriteRecover =
      struct
        include G0
        let name () = "writeRecover"
      end
      include Lattice.Prod4 (ReadWriteNoRecover) (WriteNoRecover) (ReadWriteRecover) (WriteRecover)
    end

    module GProtecting = MakeG (LockDomain.Simple)
    module GProtected = MakeG (VarSet)
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

    let add ctx l =
      D.add l ctx.local

    let remove ctx l =
      D.remove (l, true) (D.remove (l, false) ctx.local)

    let remove_all ctx =
      (* Mutexes.iter (fun m ->
           ctx.emit (MustUnlock m)
         ) (D.export_locks ctx.local); *)
      (* TODO: used to have remove_nonspecial, which kept v.vname.[0] = '{' variables *)
      D.empty ()
  end
  include LocksetAnalysis.MakeMust (Arg)
  let name () = "mutex"

  module D = Arg.D (* help type checker using explicit constraint *)
  let should_join x y = D.equal x y

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

  let rec conv_offset_inv = function
    | `NoOffset -> `NoOffset
    | `Field (f, o) -> `Field (f, conv_offset_inv o)
    | `Index (i, o) ->
      let i_exp =
        match ValueDomain.IndexDomain.to_int i with
        | Some i -> Const (CInt (i, Cilfacade.ptrdiff_ikind (), Some (Z.to_string i)))
        | None -> Lval.any_index_exp
      in
      `Index (i_exp, conv_offset_inv o)

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let check_fun ~write ~recover ls =
      let locks = Lockset.export_locks ls in
      let rw,w = if write then (Mutexes.bot (),locks) else (locks, Mutexes.bot ()) in
      if recover then
        (Mutexes.bot (), Mutexes.bot (), rw, w)
      else
        (rw, w, Mutexes.bot (), Mutexes.bot ())
    in
    let non_overlapping locks1 locks2 =
      let intersect = GProtecting.join locks1 locks2 in
      GProtecting.is_top intersect
    in
    match q with
    | Queries.MayBePublic _ when Lockset.is_bot ctx.local -> false
    | Queries.MayBePublic {global=v; write; recoverable} ->
      let held_locks: GProtecting.t = check_fun ~write ~recover:recoverable (Lockset.filter snd ctx.local) in
      (* TODO: unsound in 29/24, why did we do this before? *)
      (* if Mutexes.mem verifier_atomic (Lockset.export_locks ctx.local) then
        false
      else *)
      non_overlapping held_locks (G.protecting (ctx.global (V.protecting v)))
    | Queries.MayBePublicWithout _ when Lockset.is_bot ctx.local -> false
    | Queries.MayBePublicWithout {global=v; write; without_mutex; recoverable} ->
      let held_locks: GProtecting.t = check_fun ~write ~recover:recoverable (Lockset.remove (without_mutex, true) (Lockset.filter snd ctx.local)) in
      (* TODO: unsound in 29/24, why did we do this before? *)
      (* if Mutexes.mem verifier_atomic (Lockset.export_locks (Lockset.remove (without_mutex, true) ctx.local)) then
        false
      else *)
      non_overlapping held_locks (G.protecting (ctx.global (V.protecting v)))
    | Queries.MustBeProtectedBy {mutex; global; write; recoverable} ->
      let mutex_lockset = Lockset.singleton (mutex, true) in
      let held_locks: GProtecting.t = check_fun ~write ~recover:recoverable mutex_lockset in
      (* TODO: unsound in 29/24, why did we do this before? *)
      (* if LockDomain.Addr.equal mutex verifier_atomic then
        true
      else *)
      GProtecting.leq (G.protecting (ctx.global (V.protecting global))) held_locks
    | Queries.MustLockset ->
      let held_locks = Lockset.export_locks (Lockset.filter snd ctx.local) in
      let ls = Mutexes.fold (fun addr ls ->
          match Addr.to_var_offset addr with
          | Some (var, offs) -> Queries.LS.add (var, conv_offset_inv offs) ls
          | None -> ls
        ) held_locks (Queries.LS.empty ())
      in
      ls
    | Queries.MustBeAtomic ->
      let held_locks = Lockset.export_locks (Lockset.filter snd ctx.local) in
      Mutexes.mem (LockDomain.Addr.from_var LF.verifier_atomic_var) held_locks
    | Queries.MustProtectedVars {mutex = m; write} ->
      let protected = (if write then Tuple4.second else Tuple4.first) (G.protected (ctx.global (V.protected m))) in
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
            let (protecting, _, _, _) = G.protecting (ctx.global g) in (* readwrite protecting *)
            let s = Mutexes.cardinal protecting in
            M.info_noloc ~category:Race "Variable %a read-write protected by %d mutex(es): %a" CilType.Varinfo.pretty g' s Mutexes.pretty protecting
          )
        | `Right m -> (* protected *)
          if GobConfig.get_bool "dbg.print_protection" then (
            let (protected, _, _ ,_) = G.protected (ctx.global g) in (* readwrite protected *)
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
    include D
    let name () = "lock"
    let may_race ls1 ls2 =
      (* not mutually exclusive *)
      not @@ D.exists (fun ((m1, w1) as l1) ->
          if w1 then
            (* write lock is exclusive with write lock or read lock *)
            D.mem l1 ls2 || D.mem (m1, false) ls2
          else
            (* read lock is exclusive with just write lock *)
            D.mem (m1, true) ls2
        ) ls1
    let should_print ls = not (is_empty ls)
  end

  let access ctx (a: Queries.access) =
    ctx.local

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
          if not (Lockset.is_bot octx.local) then
            let locks = Lockset.export_locks (Lockset.filter snd octx.local) in
            let write = match kind with
              | Write | Free -> true
              | Read -> false
              | Spawn -> false (* TODO: nonsense? *)
            in
            (* If the access is not a write, set to T so intersection with current write-protecting is identity *)
            let wlocks = if write then locks else Mutexes.top () in
            let el =
              if is_recovered_to_st then
                (* If we are in single-threaded mode again, this does not need to be added to set of mutexes protecting in mt-mode only *)
                (locks, wlocks, Mutexes.top (), Mutexes.top ())
              else
                (locks, wlocks, locks, wlocks)
            in
            ctx.sideg (V.protecting v) (G.create_protecting el);

            if !AnalysisState.postsolving then (
              let protecting = G.protecting (ctx.global (V.protecting v)) in
              let vs_empty = VarSet.empty () in
              let vs = VarSet.singleton v in
              let held_norecovery = (if write then Tuple4.second else Tuple4.first) protecting in
              let held_recovery = (if write then Tuple4.fourth else Tuple4.third) protecting in
              Mutexes.iter (fun addr ->
                  let protected =
                    if write then
                      (vs_empty, vs, vs_empty, vs)
                    else
                      (vs, vs_empty, vs, vs_empty)
                  in
                  ctx.sideg (V.protected addr) (G.create_protected protected)
                ) held_norecovery;
              (* If the mutex set here is top, it is actually not accessed *)
              if is_recovered_to_st && not @@ Mutexes.is_top held_recovery then
                Mutexes.iter (fun addr ->
                    let protected =
                      if write then
                        (vs_empty, vs_empty, vs_empty, vs)
                    else
                      (vs_empty, vs_empty, vs, vs_empty)
                  in
                  ctx.sideg (V.protected addr) (G.create_protected protected)
                  ) held_recovery;
            )
        | None -> M.info ~category:Unsound "Write to unknown address: privatization is unsound."
      in
      let module LS = Queries.LS in
      let has_escaped g = octx.ask (Queries.MayEscape g) in
      let on_lvals ls =
        let ls = LS.filter (fun (g,_) -> g.vglob || has_escaped g) ls in
        let f (var, offs) =
          let coffs = Lval.CilLval.to_ciloffs offs in
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
