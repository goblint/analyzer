(** Deadlock analysis. *)

open Prelude.Ana
open Analyses
open DeadlockDomain
open ValueDomain

let forbiddenList : ( (myowntypeEntry*myowntypeEntry) list ref) = ref []

module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "deadlock"

  (* The domain for the analysis *)
  module D = DeadlockDomain.Lockset (* MayLockset *)
  module C = DeadlockDomain.Lockset

  let addLockingInfo newLock lockList =
    let add_comb a b =
      if List.exists (fun (x,y) -> MyLock.equal x a && MyLock.equal y b) !forbiddenList then ()
      else forbiddenList := (a,b)::!forbiddenList
    in

    let may_equal l1 l2 = match l1, l2 with
      | {addr = a; _}, _ when Addr.equal a (Addr.from_var_offset (dummyFunDec.svar, `NoOffset)) ->
        true
      | _, {addr = a; _} when Addr.equal a (Addr.from_var_offset (dummyFunDec.svar, `NoOffset)) ->
        true
      | _, _ -> MyLock.equal l1 l2
    in

    (* Check forbidden list *)
    if !Goblintutil.postsolving then begin
      D.iter (fun e -> List.iter (fun (a,b) ->
          if ((may_equal a e) && (may_equal b newLock)) then (
            Messages.warn "Deadlock warning: Locking order %a, %a at %a, %a violates order at %a, %a." ValueDomain.Addr.pretty e.addr ValueDomain.Addr.pretty newLock.addr CilType.Location.pretty e.loc CilType.Location.pretty newLock.loc CilType.Location.pretty b.loc CilType.Location.pretty a.loc;
            Messages.warn ~loc:a.loc "Deadlock warning: Locking order %a, %a at %a, %a violates order at %a, %a." ValueDomain.Addr.pretty newLock.addr ValueDomain.Addr.pretty e.addr CilType.Location.pretty b.loc CilType.Location.pretty a.loc CilType.Location.pretty e.loc CilType.Location.pretty newLock.loc;
          )
          else () ) !forbiddenList ) lockList;

      (* Add forbidden order *)
      D.iter (
        fun lock ->
          add_comb newLock lock;
          let transAddList = List.find_all (fun (a,b) -> MyLock.equal a lock) !forbiddenList in
          List.iter (fun (a,b) -> add_comb newLock b) transAddList
      ) lockList
    end


  (* Some required states *)
  let startstate _ : D.t = D.empty ()
  let threadenter ctx lval f args = [D.empty ()]
  let exitstate  _ : D.t = D.empty ()

  let event ctx e octx =
    match e with
    | Events.Lock l ->
      let lockAddr = fst l in
      addLockingInfo {addr = lockAddr; loc = !Tracing.current_loc } ctx.local;
      D.add {addr = lockAddr; loc = !Tracing.current_loc } ctx.local
    | Events.Unlock l when Addr.equal l (Addr.from_var_offset (dummyFunDec.svar, `NoOffset)) ->
      (* unlock nothing *)
      ctx.local
    | Events.Unlock l ->
      let inLockAddrs e = ValueDomain.Addr.equal l e.addr in
      D.filter (neg inLockAddrs) ctx.local
    | _ ->
      ctx.local
end

let _ =
  MCP.register_analysis ~dep:["mutexEvents"] (module Spec : MCPSpec)
