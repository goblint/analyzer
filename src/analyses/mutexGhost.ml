(** Analysis for checking whether ghost globals are only accessed by one unique thread ([mutexGhost]). *)

open Analyses
open GoblintCil

module TID = ThreadIdDomain.Thread
module TIDs = ConcDomain.ThreadSet

module Spec =
struct
  include IdentitySpec

  let name () = "mutexGhost"

  module D = 
  struct 
    module Mutex = Lattice.Flat (Printable.Prod (LockDomain.MustLock) (BoolDomain.Bool))
    module MayGhostVarSet = SetDomain.ToppedSet (Basetype.Variables) (struct let topname = "All variables" end)
    (* Triple:
        1. if the current location is inside an instrument atomic block,
        2. if locking/unlocking a mutex is instrumented here,
        3. set of ghost variable updates in this instrument atomic block  *)
    include Lattice.Prod3 (BoolDomain.MustBool) (Mutex) (MayGhostVarSet)

    let isGhostAtomic (x, _, _) = x
    let mutex (_, y, _) = y
    let ghosts (_, _, z) = z
    let init = (false, Mutex.bot(), MayGhostVarSet.bot())
    let create_annotation = (true, Mutex.bot(), MayGhostVarSet.bot())
    let create_lock_mutex l = (true, `Lifted (l, true), MayGhostVarSet.bot())
    let create_unlock_mutex l = (true, `Lifted (l, false), MayGhostVarSet.bot())
    let create_ghosts v = (true, Mutex.bot(), MayGhostVarSet.singleton v)
  end  
  include ValueContexts (D)
  module P = UnitP

  (* Global Unknowns: a pair of maps *)
  (* mutex -> set of ghost variables
          When locking or unlocking this mutex, 
          these ghost variables are **always** correctly updated. *)
  (* variable -> mutex
          When this ghost variable is updated, it marks this mutex.
          Result is bot when ghost variable marks wrongly (1-unlock or 0-lock) or marks a non-mutex operation *)
  module V = 
  struct
    include Printable.Either 
        (struct include LockDomain.MustLock end) 
        (struct include Basetype.Variables end)
    let mutex x = `Left x
    let ghost x = `Right x
    include StdV
  end
  module G = 
  struct 
    module GhostSet = Set.Make (Basetype.Variables)
    module MustGhostSet = Lattice.Reverse (SetDomain.ToppedSet (Basetype.Variables) (struct let topname = "All variables" end))
    module MutexDomain = Lattice.Flat (LockDomain.MustLock)
    include Lattice.Lift2Conf (struct include Printable.DefaultConf let expand1 = false let expand2 = false end) (MustGhostSet) (MutexDomain)

    let ghost_set = function 
      | `Bot -> MustGhostSet.bot()
      | `Lifted1 x -> x
      | _ -> failwith "ghost_set"
    let mutex = function 
      | `Bot -> MutexDomain.bot()
      | `Lifted2 x -> x 
      | _ -> failwith "mutex"
    let create_ghost_set g_set = `Lifted1 g_set 
    let create_mutex mutex = `Lifted2 mutex 
  end

  (* the first field should be top *)
  let startstate _ = D.init
  let exitstate _ = D.bot()

  let eval_const man e =
    match man.ask (Queries.EvalInt e) with 
    | `Lifted n -> IntDomain.IntDomTuple.to_int n
    | _ -> None

  let is_one man rval = GobOption.exists (Z.equal Z.one) (eval_const man rval)

  let is_zero man rval = GobOption.exists (Z.equal Z.zero) (eval_const man rval)

  let event (man : (D.t, G.t, C.t, V.t) man) e oman : D.t =
    let verifier_atomic_instrument_addr = LockDomain.Addr.of_var LibraryFunctions.verifier_atomic_instrument_var in
    let is_atomic = D.isGhostAtomic man.local in
    match e with
    | Events.Lock (addr, _) when (LockDomain.Addr.equal addr verifier_atomic_instrument_addr) ->
      D.create_annotation
    | Events.Unlock addr when (LockDomain.Addr.equal addr verifier_atomic_instrument_addr) ->  
      ((match (D.mutex man.local) with 
          | `Lifted (lock, _) -> 
            (man.sideg (V.mutex lock) (G.create_ghost_set (D.ghosts man.local)))
          | _ -> ());
       D.init)
    | Events.Lock (addr, _) -> 
      ( match LockDomain.MustLock.of_addr addr with
        | Some lock -> 
          if is_atomic then
            (D.join man.local (D.create_lock_mutex lock))
          else 
            (man.sideg (V.mutex lock) (G.create_ghost_set (G.MustGhostSet.top()));
             man.local)
        | None -> 
          man.local
      )
    | Events.Unlock addr -> 
      ( match LockDomain.MustLock.of_addr addr with
        | Some lock -> 
          if is_atomic then
            D.join man.local (D.create_unlock_mutex lock)
          else 
            (man.sideg (V.mutex lock) (G.create_ghost_set (G.MustGhostSet.top()));
             man.local)
        | None -> 
          man.local
      )
    | _ ->
      man.local

  let assign (man :(D.t, G.t, C.t, V.t) man) (lval : lval) (rval : exp) : D.t =
    if !AnalysisState.global_initialization then
      man.local
    else
      match lval with
      (* These ghost variables only appear in this shape. *)
      | Var var, NoOffset when YamlWitness.VarSet.mem var !(YamlWitness.ghostVars) ->
        let is_one = is_one man rval in 
        let is_zero = is_zero man rval in
        (* whether it marks a mutex locking/unlocking *)
        (* This relies on that intrumentations are placed after the annotated instruction. *)
        let mutex_arg, new_local =
          match D.mutex man.local with
          | `Lifted (lock, b) when (b && is_one) || (not b && is_zero) ->
            `Lifted lock, D.join man.local (D.create_ghosts var)
          | _ ->
            G.MutexDomain.top(), man.local
        in
        man.sideg (V.ghost var) (G.create_mutex mutex_arg);
        new_local
      | _ ->
        man.local

  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.IsMutexGhost var when YamlWitness.VarSet.mem var !(YamlWitness.ghostVars) ->
      ( match G.mutex (man.global (V.ghost var)) with
        | `Lifted (lock : LockDomain.MustLock.t) ->
          if LockDomain.MustLock.is_definite lock then
            (match G.ghost_set (man.global (V.mutex lock)) with
             | `Lifted gset ->
               if G.GhostSet.mem var gset then `Lifted lock else Queries.Result.top q
             | _ -> Queries.Result.top q)
          else 
            Queries.Result.top q
        | _ -> Queries.Result.top q)
    | Queries.WarnGlobal g ->
      let v: V.t = Obj.obj g in
      (match v with 
       | `Left lock -> ()
       | `Right ghost -> 
         (match G.mutex (man.global (V.ghost ghost)) with 
          | `Lifted l -> 
            (match G.ghost_set (man.global (V.mutex l)) with 
             | `Lifted gset -> 
               (if G.GhostSet.mem ghost gset then 
                  M.info_noloc ~category:Witness "mutexGhost: global %a is only used to mark the boundary of all of the critical sections protected by mutex %a" Basetype.Variables.pretty ghost LockDomain.MustLock.pretty l
                else 
                  M.info_noloc ~category:Witness "mutexGhost: global %a is only used to mark the boundary of part (not all) of the critical sections protected by mutex %a" Basetype.Variables.pretty ghost LockDomain.MustLock.pretty l) 
             | _ -> M.info_noloc ~category:Witness "mutexGhost: global %a is only used to mark the boundary of part (not all) of the critical sections protected by mutex %a" Basetype.Variables.pretty ghost LockDomain.MustLock.pretty l)
          | _ -> 
            M.info_noloc ~category:Witness "mutexGhost: global %a is used incorrectly, either matching several mutex, or non-mutex events" Basetype.Variables.pretty ghost
         ))
    | _ ->
      Queries.Result.top q
end

let _ =
  MCP.register_analysis ~dep:["mutexEvents"; "threadid"] (module Spec : MCPSpec)
