(** Mutex locking and unlocking analysis ([mutexEvents]).

    Emits {!Events.Lock} and {!Events.Unlock} to other analyses. *)

module M = Messages
module Addr = ValueDomain.Addr
module Lockset = LockDomain.Lockset
module Mutexes = LockDomain.Mutexes
module LF = LibraryFunctions
open Batteries
open GoblintCil
open Analyses
open GobConfig


module Spec: MCPSpec =
struct
  include UnitAnalysis.Spec
  let name () = "mutexEvents"

  let eval_exp_addr (a: Queries.ask) exp = a.f (Queries.MayPointTo exp)

  let lock ctx rw may_fail nonzero_return_when_aquired a lv_opt arg =
    match lv_opt with
    | None ->
      Queries.AD.iter (fun e ->
          ctx.split () [Events.Lock (e, rw)]
        ) (eval_exp_addr a arg);
      if may_fail then
        ctx.split () [];
      raise Analyses.Deadcode
    | Some lv ->
      let sb = Events.SplitBranch (Lval lv, nonzero_return_when_aquired) in
      Queries.AD.iter (fun e ->
          ctx.split () [sb; Events.Lock (e, rw)];
        ) (eval_exp_addr a arg);
      if may_fail then (
        let fail_exp = if nonzero_return_when_aquired then Lval lv else BinOp(Gt, Lval lv, zero, intType) in
        ctx.split () [Events.SplitBranch (fail_exp, not nonzero_return_when_aquired)]
      );
      raise Analyses.Deadcode



  let return ctx exp fundec : D.t =
    (* deprecated but still valid SV-COMP convention for atomic block *)
    if get_bool "ana.sv-comp.functions" && String.starts_with fundec.svar.vname "__VERIFIER_atomic_" then
      ctx.emit (Events.Unlock (LockDomain.Addr.of_var LF.verifier_atomic_var))

  let body ctx f : D.t =
    (* deprecated but still valid SV-COMP convention for atomic block *)
    if get_bool "ana.sv-comp.functions" && String.starts_with f.svar.vname "__VERIFIER_atomic_" then
      ctx.emit (Events.Lock (LockDomain.Addr.of_var LF.verifier_atomic_var, true))

  let special (ctx: (unit, _, _, _) ctx) lv f arglist : D.t =
    let remove_rw x = x in
    let unlock arg remove_fn =
      Queries.AD.iter (fun e ->
          ctx.split () [Events.Unlock (remove_fn e)]
        ) (eval_exp_addr (Analyses.ask_of_ctx ctx) arg);
      raise Analyses.Deadcode
    in
    let desc = LF.find f in
    match desc.special arglist with
    | Lock { lock = arg; try_ = failing; write = rw; return_on_success = nonzero_return_when_aquired } ->
      lock ctx rw failing nonzero_return_when_aquired (Analyses.ask_of_ctx ctx) lv arg
    | Unlock arg ->
      unlock arg remove_rw
    | Wait { mutex = m_arg; _}
    | TimedWait { mutex = m_arg; _} ->
      (* mutex is unlocked while waiting but relocked when returns *)
      (* emit unlock-lock events for privatization *)
      let ms = eval_exp_addr (Analyses.ask_of_ctx ctx) m_arg in
      Queries.AD.iter (fun m ->
          (* unlock-lock each possible mutex as a split to be dependent *)
          (* otherwise may-point-to {a, b} might unlock a, but relock b *)
          ctx.split () [Events.Unlock m; Events.Lock (m, true)];
        ) ms;
      raise Deadcode (* splits cover all cases *)
    | _ ->
      ()

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
