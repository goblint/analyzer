(** Deadlock analysis. *)

open Prelude.Ana
open Analyses
open DeadlockDomain

let forbiddenList : ( (myowntypeEntry*myowntypeEntry) list ref) = ref []

module Arg =
struct
  module D = DeadlockDomain.Lockset

  let addLockingInfo newLock lockList =
    let add_comb a b =
      if List.exists (fun (x,y) -> MyLock.equal x a && MyLock.equal y b) !forbiddenList then ()
      else forbiddenList := (a,b)::!forbiddenList
    in

    let may_equal l1 l2 = match l1, l2 with
      | {addr = UnknownPtr; _}, _
      | _, {addr = UnknownPtr; _} ->
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

  let add ctx l =
    let lockAddr = fst l in
    addLockingInfo {addr = lockAddr; loc = !Tracing.current_loc } ctx.local;
    D.add {addr = lockAddr; loc = !Tracing.current_loc } ctx.local

  let remove ctx l =
    let inLockAddrs e = ValueDomain.Addr.equal l e.addr in
    D.filter (neg inLockAddrs) ctx.local
end

module Spec =
struct
  include LocksetAnalysis.MakeMay (Arg)
  let name () = "deadlock"
end

let _ =
  MCP.register_analysis ~dep:["mutexEvents"] (module Spec : MCPSpec)
