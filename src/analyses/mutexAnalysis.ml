(** Mutex analysis. *)

module M = Messages
module Addr = ValueDomain.Addr
module Lockset = LockDomain.Lockset
module Mutexes = LockDomain.Mutexes
module LF = LibraryFunctions
open Prelude.Ana
open Analyses
open GobConfig

let big_kernel_lock = LockDomain.Addr.from_var (Goblintutil.create_var (makeGlobalVar "[big kernel lock]" intType))
let console_sem = LockDomain.Addr.from_var (Goblintutil.create_var (makeGlobalVar "[console semaphore]" intType))
let verifier_atomic = LockDomain.Addr.from_var (Goblintutil.create_var (makeGlobalVar "[__VERIFIER_atomic]" intType))

module type SpecParam =
sig
  module G: Lattice.S
  val effect_fun: ?write:bool -> Lockset.t -> G.t
  val check_fun: ?write:bool -> Lockset.t -> G.t
end

(** Mutex analyzer without base --- this is the new standard *)
module MakeSpec (P: SpecParam) =
struct
  include Analyses.DefaultSpec

  (** name for the analysis (btw, it's "Only Mutex Must") *)
  let name () = "mutex"

  (** Add current lockset alongside to the base analysis domain. Global data is collected using dirty side-effecting. *)
  module D = Lockset
  module C = Lockset

  (** We do not add global state, so just lift from [BS]*)
  module G = P.G
  module V = VarinfoV

  let should_join x y = D.equal x y

  (* NB! Currently we care only about concrete indexes. Base (seeing only a int domain
     element) answers with the string "unknown" on all non-concrete cases. *)
  let rec conv_offset x =
    match x with
    | `NoOffset    -> `NoOffset
    | `Index (Const (CInt (i,_,s)),o) -> `Index (IntDomain.of_const (i,Cilfacade.ptrdiff_ikind (),s), conv_offset o)
    | `Index (_,o) -> `Index (ValueDomain.IndexDomain.top (), conv_offset o)
    | `Field (f,o) -> `Field (f, conv_offset o)

  let rec conv_offset_inv = function
    | `NoOffset -> `NoOffset
    | `Field (f, o) -> `Field (f, conv_offset_inv o)
    (* TODO: better indices handling *)
    | `Index (_, o) -> `Index (MyCFG.unknown_exp, conv_offset_inv o)


  let eval_exp_addr (a: Queries.ask) exp =
    let gather_addr (v,o) b = ValueDomain.Addr.from_var_offset (v,conv_offset o) :: b in
    match a.f (Queries.MayPointTo exp) with
    | a when not (Queries.LS.is_top a)
                   && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) a) ->
      Queries.LS.fold gather_addr (Queries.LS.remove (dummyFunDec.svar, `NoOffset) a) []
    | _ -> []

  let lock ctx rw may_fail nonzero_return_when_aquired a lv arglist ls =
    let is_a_blob addr =
      match LockDomain.Addr.to_var addr with
      | Some a -> a.vname.[0] = '('
      | None -> false
    in
    let lock_one (e:LockDomain.Addr.t) =
      if is_a_blob e then
        ls
      else begin
        let nls = Lockset.add (e,rw) ls in
        let changed = Lockset.compare ls nls <> 0 in
        match lv with
        | None ->
          if may_fail then
            ls
          else (
            (* If the lockset did not change, do not emit Lock event *)
            if changed then ctx.emit (Events.Lock e);
            nls
          )
        | Some lv ->
          let sb = Events.SplitBranch (Lval lv, nonzero_return_when_aquired) in
          if changed then
            ctx.split nls [sb; Events.Lock e]
          else
            ctx.split nls [sb];
          if may_fail then (
            let fail_exp = if nonzero_return_when_aquired then Lval lv else BinOp(Gt, Lval lv, zero, intType) in
            ctx.split ls [Events.SplitBranch (fail_exp, not nonzero_return_when_aquired)]
          );
          raise Analyses.Deadcode
      end
    in
    match arglist with
    | [x] -> begin match  (eval_exp_addr a x) with
        | [e]  -> lock_one e
        | _ -> ls
      end
    | _ -> Lockset.top ()


  (** We just lift start state, global and dependency functions: *)
  let startstate v = Lockset.empty ()
  let threadenter ctx lval f args = [Lockset.empty ()]
  let exitstate  v = Lockset.empty ()

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let non_overlapping locks1 locks2 =
      let intersect = G.join locks1 locks2 in
      G.is_top intersect
    in
    match q with
    | Queries.MayBePublic _ when Lockset.is_bot ctx.local -> false
    | Queries.MayBePublic {global=v; write} ->
      let held_locks: G.t = P.check_fun ~write (Lockset.filter snd ctx.local) in
      (* TODO: unsound in 29/24, why did we do this before? *)
      (* if Mutexes.mem verifier_atomic (Lockset.export_locks ctx.local) then
        false
      else *)
        non_overlapping held_locks (ctx.global v)
    | Queries.MayBePublicWithout _ when Lockset.is_bot ctx.local -> false
    | Queries.MayBePublicWithout {global=v; write; without_mutex} ->
      let held_locks: G.t = P.check_fun ~write (Lockset.remove (without_mutex, true) (Lockset.filter snd ctx.local)) in
      (* TODO: unsound in 29/24, why did we do this before? *)
      (* if Mutexes.mem verifier_atomic (Lockset.export_locks (Lockset.remove (without_mutex, true) ctx.local)) then
        false
      else *)
         non_overlapping held_locks (ctx.global v)
    | Queries.MustBeProtectedBy {mutex; global; write} ->
      let mutex_lockset = Lockset.singleton (mutex, true) in
      let held_locks: G.t = P.check_fun ~write mutex_lockset in
      (* TODO: unsound in 29/24, why did we do this before? *)
      (* if LockDomain.Addr.equal mutex verifier_atomic then
        true
      else *)
        G.leq (ctx.global global) held_locks
    | Queries.CurrentLockset ->
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
      Mutexes.mem verifier_atomic held_locks
    | _ -> Queries.Result.top q

  module A =
  struct
    include D
    let name () = "lock"
    let may_race ls1 ls2 =
      (* not mutually exclusive *)
      not @@ D.ReverseAddrSet.exists (fun ((m1, w1) as l1) ->
          if w1 then
            (* write lock is exclusive with write lock or read lock *)
            D.ReverseAddrSet.mem l1 ls2 || D.ReverseAddrSet.mem (m1, false) ls2
          else
            (* read lock is exclusive with just write lock *)
            D.ReverseAddrSet.mem (m1, true) ls2
        ) ls1
    let should_print ls = not (is_empty ls)
  end

  let access ctx e vo w =
    ctx.local

  (** Transfer functions: *)

  let assign ctx lval rval : D.t =
    ctx.local

  let branch ctx exp tv : D.t =
    ctx.local

  let return ctx exp fundec : D.t =
    (* deprecated but still valid SV-COMP convention for atomic block *)
    if get_bool "ana.sv-comp.functions" && String.starts_with fundec.svar.vname "__VERIFIER_atomic_" then (
      ctx.emit (Events.Unlock verifier_atomic);
      Lockset.remove (verifier_atomic, true) ctx.local
    )
    else
      ctx.local

  let body ctx f : D.t =
    (* deprecated but still valid SV-COMP convention for atomic block *)
    if get_bool "ana.sv-comp.functions" && String.starts_with f.svar.vname "__VERIFIER_atomic_" then (
      ctx.emit (Events.Lock verifier_atomic);
      Lockset.add (verifier_atomic, true) ctx.local
    )
    else
      ctx.local

  let special ctx lv f arglist : D.t =
    let remove_rw x st =
      ctx.emit (Events.Unlock x);
      Lockset.remove (x,true) (Lockset.remove (x,false) st)
    in
    let unlock remove_fn =
      let remove_nonspecial x =
        if Lockset.is_top x then x else
          Lockset.filter (fun (v,_) -> match LockDomain.Addr.to_var v with
              | Some v when v.vname.[0] = '{' -> true
              | _ -> false
            ) x
      in
      match arglist with
      | x::xs -> begin match  (eval_exp_addr (Analyses.ask_of_ctx ctx) x) with
          | [] -> remove_nonspecial ctx.local
          | es -> List.fold_right remove_fn es ctx.local
        end
      | _ -> ctx.local
    in
    match (LF.classify f.vname arglist, f.vname) with
    | _, "_lock_kernel" ->
      ctx.emit (Events.Lock big_kernel_lock);
      Lockset.add (big_kernel_lock,true) ctx.local
    | _, "_unlock_kernel" ->
      ctx.emit (Events.Unlock big_kernel_lock);
      Lockset.remove (big_kernel_lock,true) ctx.local
    | `Lock (failing, rw, nonzero_return_when_aquired), _
      -> let arglist = if f.vname = "LAP_Se_WaitSemaphore" then [List.hd arglist] else arglist in
      (*print_endline @@ "Mutex `Lock "^f.vname;*)
      lock ctx rw failing nonzero_return_when_aquired (Analyses.ask_of_ctx ctx) lv arglist ctx.local
    | `Unlock, "__raw_read_unlock"
    | `Unlock, "__raw_write_unlock"  ->
      let drop_raw_lock x =
        let rec drop_offs o =
          match o with
          | `Field ({fname="raw_lock"; _},`NoOffset) -> `NoOffset
          | `Field (f1,o1) -> `Field (f1, drop_offs o1)
          | `Index (i1,o1) -> `Index (i1, drop_offs o1)
          | `NoOffset -> `NoOffset
        in
        match Addr.to_var_offset x with
        | Some (v,o) -> Addr.from_var_offset (v, drop_offs o)
        | None -> x
      in
      unlock (fun l -> remove_rw (drop_raw_lock l))
    | `Unlock, _ ->
      (*print_endline @@ "Mutex `Unlock "^f.vname;*)
      unlock remove_rw
    | _, "spinlock_check" -> ctx.local
    | _, "acquire_console_sem" when get_bool "kernel" ->
      ctx.emit (Events.Lock console_sem);
      Lockset.add (console_sem,true) ctx.local
    | _, "release_console_sem" when get_bool "kernel" ->
      ctx.emit (Events.Unlock console_sem);
      Lockset.remove (console_sem,true) ctx.local
    | _, "__builtin_prefetch" | _, "misc_deregister" ->
      ctx.local
    | _, "__VERIFIER_atomic_begin" when get_bool "ana.sv-comp.functions" ->
      ctx.emit (Events.Lock verifier_atomic);
      Lockset.add (verifier_atomic, true) ctx.local
    | _, "__VERIFIER_atomic_end" when get_bool "ana.sv-comp.functions" ->
      ctx.emit (Events.Unlock verifier_atomic);
      Lockset.remove (verifier_atomic, true) ctx.local
    | _, "pthread_cond_wait"
    | _, "pthread_cond_timedwait" ->
      (* mutex is unlocked while waiting but relocked when returns *)
      (* emit unlock-lock events for privatization *)
      let m_arg = List.nth arglist 1 in
      let ms = eval_exp_addr (Analyses.ask_of_ctx ctx) m_arg in
      List.iter (fun m ->
          (* unlock-lock each possible mutex as a split to be dependent *)
          (* otherwise may-point-to {a, b} might unlock a, but relock b *)
          ctx.split ctx.local [Events.Unlock m; Events.Lock m];
        ) ms;
      raise Deadcode (* splits cover all cases *)
    | _, x ->
      ctx.local

  let enter ctx lv f args : (D.t * D.t) list =
    [(ctx.local,ctx.local)]

  let combine ctx lv fexp f args fc al =
    al


  let threadspawn ctx lval f args fctx =
    ctx.local

  let event ctx e octx =
    match e with
    | Events.Access {var_opt; write} ->
      (*privatization*)
      begin match var_opt with
        | Some v ->
          if not (Lockset.is_bot ctx.local) then
            let ls = Lockset.filter snd ctx.local in
            let el = P.effect_fun ~write ls in
            ctx.sideg v el
        | None -> M.info ~category:Unsound "Write to unknown address: privatization is unsound."
      end;
      ctx.local
    | _ ->
      ctx.local

end

module MyParam =
struct
  module G = LockDomain.Simple
  let effect_fun ?write:(w=false) ls = Lockset.export_locks ls
  let check_fun = effect_fun
end

module WriteBased =
struct
  module GReadWrite =
  struct
    include LockDomain.Simple
    let name () = "readwrite"
  end
  module GWrite =
  struct
    include LockDomain.Simple
    let name () = "write"
  end
  module G = Lattice.Prod (GReadWrite) (GWrite)
  let effect_fun ?write:(w=false) ls =
    let locks = Lockset.export_locks ls in
    (locks, if w then locks else Mutexes.top ())
  let check_fun ?write:(w=false) ls =
    let locks = Lockset.export_locks ls in
    if w then (Mutexes.bot (), locks) else (locks, Mutexes.bot ())
end

module Spec = MakeSpec (WriteBased)

let _ =
  MCP.register_analysis ~dep:["access"] (module Spec : MCPSpec)
