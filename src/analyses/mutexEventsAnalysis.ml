(** Mutex locking and unlocking analysis ([mutexEvents]).

    Emits {!Events.Lock} and {!Events.Unlock} to other analyses. *)

module M = Messages
module Addr = ValueDomain.Addr
module LF = LibraryFunctions
open GoblintCil
open Analyses
open GobConfig


module Spec: MCPSpec =
struct
  include UnitAnalysis.Spec
  let name () = "mutexEvents"

  let eval_exp_addr (a: Queries.ask) exp = a.f (Queries.MayPointTo exp)

  let lock man rw may_fail nonzero_return_when_aquired a lv_opt arg =
    let compute_refine_split (e: Addr.t) = match e with
      | Addr a ->
        let arg_e = AddrOf (PreValueDomain.Mval.to_cil a) in
        if not (CilType.Exp.equal arg arg_e) then
          let e' = BinOp (Eq, arg, arg_e, intType) in
          [Events.SplitBranch (e', true)]
        else
          []
      | _ -> []
    in
    match lv_opt with
    | None ->
      Queries.AD.iter (fun e ->
          man.split () (Events.Lock (e, rw) :: compute_refine_split e)
        ) (eval_exp_addr a arg);
      if may_fail then
        man.split () [];
      raise Analyses.Deadcode
    | Some lv ->
      let sb = Events.SplitBranch (Lval lv, nonzero_return_when_aquired) in
      Queries.AD.iter (fun e ->
          man.split () (sb :: Events.Lock (e, rw) :: compute_refine_split e);
        ) (eval_exp_addr a arg);
      if may_fail then (
        let fail_exp = if nonzero_return_when_aquired then Lval lv else BinOp(Gt, Lval lv, zero, intType) in
        man.split () [Events.SplitBranch (fail_exp, not nonzero_return_when_aquired)]
      );
      raise Analyses.Deadcode



  let return man exp fundec : D.t =
    (* deprecated but still valid SV-COMP convention for atomic block *)
    if get_bool "ana.sv-comp.functions" && String.starts_with fundec.svar.vname ~prefix:"__VERIFIER_atomic_" then
      man.emit (Events.Unlock (LockDomain.Addr.of_var LF.verifier_atomic_var))

  let body man f : D.t =
    (* deprecated but still valid SV-COMP convention for atomic block *)
    if get_bool "ana.sv-comp.functions" && String.starts_with f.svar.vname ~prefix:"__VERIFIER_atomic_" then
      man.emit (Events.Lock (LockDomain.Addr.of_var LF.verifier_atomic_var, true))

  let special (man: (unit, _, _, _) man) lv f arglist : D.t =
    let remove_rw x = x in
    let unlock arg remove_fn =
      Queries.AD.iter (fun e ->
          man.split () [Events.Unlock (remove_fn e)]
        ) (eval_exp_addr (Analyses.ask_of_man man) arg);
      raise Analyses.Deadcode
    in
    let desc = LF.find f in
    match desc.special arglist with
    | Lock { lock = arg; try_ = failing; write = rw; return_on_success = nonzero_return_when_aquired } ->
      lock man rw failing nonzero_return_when_aquired (Analyses.ask_of_man man) lv arg
    | Unlock arg ->
      unlock arg remove_rw
    | Wait { mutex = m_arg; _}
    | TimedWait { mutex = m_arg; _} ->
      (* mutex is unlocked while waiting but relocked when returns *)
      (* emit unlock-lock events for privatization *)
      let ms = eval_exp_addr (Analyses.ask_of_man man) m_arg in
      Queries.AD.iter (fun m ->
          (* unlock-lock each possible mutex as a split to be dependent *)
          (* otherwise may-point-to {a, b} might unlock a, but relock b *)
          man.split () [Events.Unlock m; Events.Lock (m, true)];
        ) ms;
      raise Deadcode (* splits cover all cases *)
    | _ ->
      ()

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
