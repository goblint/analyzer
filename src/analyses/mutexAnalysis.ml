(** Protecting mutex analysis. Must locksets locally and for globals. *)

module M = Messages
module Addr = ValueDomain.Addr
module Lockset = LockDomain.Lockset
module Mutexes = LockDomain.Mutexes
module LF = LibraryFunctions
open Prelude.Ana
open Analyses


module VarSet = SetDomain.Make (Basetype.Variables)

module Spec =
struct
  module Arg =
  struct
    module D = Lockset

    (** Global data is collected using dirty side-effecting. *)

    (* Two global invariants:
       1. varinfo -> set of mutexes  --  used for protecting locksets (M[g])
       2. mutex -> set of varinfos  --  used for protected variables (G_m), only collected during postsolving *)

    module V =
    struct
      include Printable.Either (CilType.Varinfo) (ValueDomain.Addr)
      let name () = "mutex"
      let protecting x = `Left x
      let protected x = `Right x
      let is_write_only = function
        | `Left _ -> false
        | `Right _ -> true
    end

    module MakeG (G0: Lattice.S) =
    struct
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
      include Lattice.Prod (ReadWrite) (Write)
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

  let max_cluster = ref 0
  let num_mutexes = ref 0
  let sum_protected = ref 0

  let init _ =
    max_cluster := 0;
    num_mutexes := 0;
    sum_protected := 0

  let rec conv_offset_inv = function
    | `NoOffset -> `NoOffset
    | `Field (f, o) -> `Field (f, conv_offset_inv o)
    | `Index (i, o) ->
      let i_exp =
        match ValueDomain.IndexDomain.to_int i with
        | Some i -> Const (CInt (i, Cilfacade.ptrdiff_ikind (), Some (Z.to_string i)))
        | None -> MyCFG.unknown_exp
      in
      `Index (i_exp, conv_offset_inv o)

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let check_fun ~write ls =
      let locks = Lockset.export_locks ls in
      if write then (Mutexes.bot (), locks) else (locks, Mutexes.bot ())
    in
    let non_overlapping locks1 locks2 =
      let intersect = GProtecting.join locks1 locks2 in
      GProtecting.is_top intersect
    in
    match q with
    | Queries.MayBePublic _ when Lockset.is_bot ctx.local -> false
    | Queries.MayBePublic {global=v; write} ->
      let held_locks: GProtecting.t = check_fun ~write (Lockset.filter snd ctx.local) in
      (* TODO: unsound in 29/24, why did we do this before? *)
      (* if Mutexes.mem verifier_atomic (Lockset.export_locks ctx.local) then
        false
      else *)
        non_overlapping held_locks (G.protecting (ctx.global (V.protecting v)))
    | Queries.MayBePublicWithout _ when Lockset.is_bot ctx.local -> false
    | Queries.MayBePublicWithout {global=v; write; without_mutex} ->
      let held_locks: GProtecting.t = check_fun ~write (Lockset.remove (without_mutex, true) (Lockset.filter snd ctx.local)) in
      (* TODO: unsound in 29/24, why did we do this before? *)
      (* if Mutexes.mem verifier_atomic (Lockset.export_locks (Lockset.remove (without_mutex, true) ctx.local)) then
        false
      else *)
         non_overlapping held_locks (G.protecting (ctx.global (V.protecting v)))
    | Queries.MustBeProtectedBy {mutex; global; write} ->
      let mutex_lockset = Lockset.singleton (mutex, true) in
      let held_locks: GProtecting.t = check_fun ~write mutex_lockset in
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
      Mutexes.mem MutexEventsAnalysis.verifier_atomic held_locks
    | Queries.IterSysVars (Global g, f) ->
      f (Obj.repr (V.protecting g))
      (* TODO: something about V.protected? *)
    | WarnGlobal g ->
      let g: V.t = Obj.obj g in
      begin match g with
        | `Left g' -> (* protecting *)
          if GobConfig.get_bool "dbg.print_protection" then (
            let (protecting, _) = G.protecting (ctx.global g) in (* readwrite protecting *)
            let s = Mutexes.cardinal protecting in
            M.info_noloc ~category:Race "Variable %a read-write protected by %d mutex(es): %a" CilType.Varinfo.pretty g' s Mutexes.pretty protecting
          )
        | `Right m -> (* protected *)
          if GobConfig.get_bool "dbg.print_protection" then (
            let (protected, _) = G.protected (ctx.global g) in (* readwrite protected *)
            let s = VarSet.cardinal protected in
            max_cluster := max !max_cluster s;
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
    | Events.Access {var_opt; kind} ->
      (*privatization*)
      begin match var_opt with
        | Some v ->
          if not (Lockset.is_bot ctx.local) then
            let locks = Lockset.export_locks (Lockset.filter snd ctx.local) in
            let write = match kind with
              | Write | Free -> true
              | Read -> false
              | Spawn -> false (* TODO: nonsense? *)
            in
            let el = (locks, if write then locks else Mutexes.top ()) in
            ctx.sideg (V.protecting v) (G.create_protecting el);

            if !GU.postsolving && GobConfig.get_bool "dbg.print_protection" then (
              let held_locks = (if write then snd else fst) (G.protecting (ctx.global (V.protecting v))) in
              let vs_empty = VarSet.empty () in
              Mutexes.iter (fun addr ->
                  let vs = VarSet.singleton v in
                  let protected =
                    if write then
                      (vs_empty, vs)
                    else
                      (vs, vs_empty)
                  in
                  ctx.sideg (V.protected addr) (G.create_protected protected)
                ) held_locks
            )
        | None -> M.info ~category:Unsound "Write to unknown address: privatization is unsound."
      end;
      ctx.local
    | _ ->
      event ctx e octx (* delegate to must lockset analysis *)

  let finalize () =
    if GobConfig.get_bool "dbg.print_protection" then (
      M.msg_group Info ~category:Race "Mutex read-write protection summary" [
        (Pretty.dprintf "Number of mutexes: %d" !num_mutexes, None);
        (Pretty.dprintf "Max number variables of protected by a mutex: %d" !max_cluster, None);
        (Pretty.dprintf "Total number of protected variables (including duplicates): %d" !sum_protected, None);
      ]
    )
end

let _ =
  MCP.register_analysis ~dep:["mutexEvents"; "access"] (module Spec : MCPSpec)
