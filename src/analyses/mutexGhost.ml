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
  (* lock -> Variable Set, intersection as join *)
  (* variable -> Flat (Lock) *)
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

  let is_one man rval = 
    match eval_const man rval with 
    | Some value -> Z.equal value (Z.one)
    | _ -> false

  let is_zero man rval = 
    match eval_const man rval with 
    | Some value -> Z.equal value (Z.zero)
    | _ -> false

  let event (man : (D.t, G.t, C.t, V.t) man) e oman : D.t =
    let verifier_atomic_instrument_addr = LockDomain.Addr.of_var LibraryFunctions.verifier_atomic_instrument_var in
    (* TODO: MustBeGhostAtomic *)
    let is_atomic = D.isGhostAtomic man.local in
    match e with
    | Events.Lock (addr, _) when (LockDomain.Addr.equal addr verifier_atomic_instrument_addr) ->
      D.create_annotation
    | Events.Unlock addr when (LockDomain.Addr.equal addr verifier_atomic_instrument_addr) ->  
      ((match (D.mutex man.local) with 
          | `Lifted (lock, _) -> 
            (man.sideg (V.mutex lock) (G.create_ghost_set (D.ghosts man.local)))
          | _ -> ());
       D.bot())
    | Events.Lock (addr, _) -> 
      ( match LockDomain.MustLock.of_addr addr with
        | Some lock -> 
          if not is_atomic then man.sideg (V.mutex lock) (G.create_ghost_set (G.MustGhostSet.top()));
          if is_atomic then
            (D.join man.local (D.create_lock_mutex lock))
          else 
            man.local
        | None -> 
          man.local
      )
    | Events.Unlock addr -> 
      ( match LockDomain.MustLock.of_addr addr with
        | Some lock -> 
          if not is_atomic then man.sideg (V.mutex lock) (G.create_ghost_set (G.MustGhostSet.top()));
          if is_atomic then
            D.join man.local (D.create_unlock_mutex lock)
          else 
            man.local
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
      | Var var, NoOffset when YamlWitness.VarSet.mem var !(YamlWitness.ghostVars) ->
        let is_one = is_one man rval in 
        let is_zero = is_zero man rval in
        let lock, is_lock, is_unlock = match (D.mutex man.local) with 
          | `Lifted (lock, true) -> Some lock, true, false 
          | `Lifted (lock, false) -> Some lock, false, true
          | _ -> None, false, false
        in
        let valid = (D.isGhostAtomic man.local) && (is_one || is_zero) &&
                    ((is_zero && is_unlock) || (is_one && is_lock)) in 
        if valid then 
          ( (match lock with 
                | Some lock -> 
                  man.sideg (V.ghost var) (G.create_mutex (`Lifted lock))
                | None -> ());
            (D.join man.local (D.create_ghosts var)))
        else
          (man.sideg (V.ghost var) (G.create_mutex (G.MutexDomain.top()));
           man.local)
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
                  M.warn_noloc ~category:Witness "mutexGhost: global %a is only used to mark the boundary of part (not all) of the critical sections protected by mutex %a" Basetype.Variables.pretty ghost LockDomain.MustLock.pretty l) 
             | _ -> M.warn_noloc ~category:Witness "mutexGhost: global %a is only used to mark the boundary of part (not all) of the critical sections protected by mutex %a" Basetype.Variables.pretty ghost LockDomain.MustLock.pretty l)
          | _ -> 
            M.warn_noloc ~category:Witness "mutexGhost: global %a is used incorrectly, either matching several mutex, or non-mutex events" Basetype.Variables.pretty ghost
         ))
    | _ ->
      Queries.Result.top q
end

let _ =
  MCP.register_analysis ~dep:["mutexEvents"; "threadid"] (module Spec : MCPSpec)
