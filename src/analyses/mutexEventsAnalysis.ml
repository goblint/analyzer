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

module Spec: MCPSpec =
struct
  include UnitAnalysis.Spec
  let name () = "mutexEvents"


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
    | a when not (Queries.LS.is_top a) && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) a) ->
      Queries.LS.fold gather_addr (Queries.LS.remove (dummyFunDec.svar, `NoOffset) a) []
    | _ -> []

  let lock ctx rw may_fail nonzero_return_when_aquired a lv arg =
    let is_a_blob addr =
      match LockDomain.Addr.to_var addr with
      | Some a -> a.vname.[0] = '('
      | None -> false
    in
    let lock_one (e:LockDomain.Addr.t) =
      if not (is_a_blob e) then begin
        match lv with
        | None ->
          if not may_fail then
            ctx.emit (Events.Lock (e, rw))
        | Some lv ->
          let sb = Events.SplitBranch (Lval lv, nonzero_return_when_aquired) in
          ctx.split () [sb; Events.Lock (e, rw)];
          if may_fail then (
            let fail_exp = if nonzero_return_when_aquired then Lval lv else BinOp(Gt, Lval lv, zero, intType) in
            ctx.split () [Events.SplitBranch (fail_exp, not nonzero_return_when_aquired)]
          );
          raise Analyses.Deadcode
      end
    in
    match eval_exp_addr a arg with
    | [e] -> lock_one e
    | _ -> ()



  let return ctx exp fundec : D.t =
    (* deprecated but still valid SV-COMP convention for atomic block *)
    if get_bool "ana.sv-comp.functions" && String.starts_with fundec.svar.vname "__VERIFIER_atomic_" then
      ctx.emit (Events.Unlock verifier_atomic)

  let body ctx f : D.t =
    (* deprecated but still valid SV-COMP convention for atomic block *)
    if get_bool "ana.sv-comp.functions" && String.starts_with f.svar.vname "__VERIFIER_atomic_" then
      ctx.emit (Events.Lock (verifier_atomic, true))

  let special (ctx: (unit, _, _, _) ctx) lv f arglist : D.t =
    let remove_rw x =
      ctx.emit (Events.Unlock x)
    in
    let unlock remove_fn =
      let remove_nonspecial x =
        (* if Lockset.is_top x then x else
           Lockset.filter (fun (v,_) -> match LockDomain.Addr.to_var v with
               | Some v when v.vname.[0] = '{' -> true
               | _ -> false
             ) x *)
        ()
      in
      match arglist with
      | [arg] ->
        begin match eval_exp_addr (Analyses.ask_of_ctx ctx) arg with
          | [] -> remove_nonspecial ctx.local
          | es -> List.iter remove_fn es
        end
      | _ -> failwith "unlock has multiple arguments"
    in
    match (LF.classify f.vname arglist, f.vname) with
    | _, "_lock_kernel" ->
      ctx.emit (Events.Lock (big_kernel_lock, true))
    | _, "_unlock_kernel" ->
      ctx.emit (Events.Unlock big_kernel_lock)
    | `Lock (failing, rw, nonzero_return_when_aquired), _ ->
      begin match arglist with
        | [arg] ->
          (*print_endline @@ "Mutex `Lock "^f.vname;*)
          lock ctx rw failing nonzero_return_when_aquired (Analyses.ask_of_ctx ctx) lv arg
        | _ -> failwith "lock has multiple arguments"
      end
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
    | _, "spinlock_check" -> ()
    | _, "acquire_console_sem" when get_bool "kernel" ->
      ctx.emit (Events.Lock (console_sem, true))
    | _, "release_console_sem" when get_bool "kernel" ->
      ctx.emit (Events.Unlock console_sem)
    | _, "__builtin_prefetch" | _, "misc_deregister" ->
      ()
    | _, "__VERIFIER_atomic_begin" when get_bool "ana.sv-comp.functions" ->
      ctx.emit (Events.Lock (verifier_atomic, true))
    | _, "__VERIFIER_atomic_end" when get_bool "ana.sv-comp.functions" ->
      ctx.emit (Events.Unlock verifier_atomic)
    | _, "pthread_cond_wait"
    | _, "pthread_cond_timedwait" ->
      (* mutex is unlocked while waiting but relocked when returns *)
      (* emit unlock-lock events for privatization *)
      let m_arg = List.nth arglist 1 in
      let ms = eval_exp_addr (Analyses.ask_of_ctx ctx) m_arg in
      List.iter (fun m ->
          (* unlock-lock each possible mutex as a split to be dependent *)
          (* otherwise may-point-to {a, b} might unlock a, but relock b *)
          ctx.split () [Events.Unlock m; Events.Lock (m, true)];
        ) ms;
      raise Deadcode (* splits cover all cases *)
    | _, x ->
      ()

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
