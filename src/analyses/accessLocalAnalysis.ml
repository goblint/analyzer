(** Access and data race analysis. *)

module M = Messages
module LF = LibraryFunctions
open Prelude.Ana
open Analyses
open GobConfig


(** Access and data race analyzer without base --- this is the new standard *)
module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "accessLocal"

  module D = AccessDomain.EventSet
  module C = Lattice.Unit

  let do_access (ctx: (D.t, G.t, C.t, V.t) ctx) (kind:AccessKind.t) (reach:bool) (e:exp) =
    if M.tracing then M.trace "accessLocal" "do_access %a %a %B\n" d_exp e AccessKind.pretty kind reach;
    let emit_access vo = ctx.emit (AccessLocal {var_opt=vo; kind}) in
    let reach_or_mpt: _ Queries.t = if reach then ReachableFrom e else MayPointTo e in
    match ctx.ask reach_or_mpt with
    | ls when Queries.LS.is_top ls -> emit_access None
    | ls ->
      Queries.LS.iter (fun (var, _) ->
          if CilType.Varinfo.equal var dummyFunDec.svar then
            emit_access None
          else
            emit_access (Some var)
        ) ls

  (** Three access levels:
      + [deref=false], [reach=false] - Access [exp] without dereferencing, used for all normal reads and all function call arguments.
      + [deref=true], [reach=false] - Access [exp] by dereferencing once (may-point-to), used for lval writes and shallow special accesses.
      + [deref=true], [reach=true] - Access [exp] by dereferencing transitively (reachable), used for deep special accesses. *)
  let access_one_top ?(force=false) ?(deref=false) ctx (kind: AccessKind.t) reach exp =
    if M.tracing then M.traceli "accessLocal" "access_one_top %a %b %a:\n" AccessKind.pretty kind reach d_exp exp;
    if deref then do_access ctx kind reach exp;
    Access.distribute_access_exp (do_access ctx Read false) exp;
    if M.tracing then M.traceu "accessLocal" "access_one_top %a %b %a\n" AccessKind.pretty kind reach d_exp exp

  (** We just lift start state, global and dependency functions: *)
  let startstate v = D.empty ()
  let threadenter ctx lval f args = [D.empty ()]
  let exitstate  v = D.empty ()
  let context fd d = ()


  (** Transfer functions: *)

  let assign ctx lval rval : D.t =
    (* ignore global inits *)
    if !GU.global_initialization then ctx.local else begin
      access_one_top ~deref:true ctx Write false (AddrOf lval);
      access_one_top ctx Read false rval;
      ctx.local
    end

  let branch ctx exp tv : D.t =
    access_one_top ctx Read false exp;
    ctx.local

  let return ctx exp fundec : D.t =
    begin match exp with
      | Some exp -> access_one_top ctx Read false exp
      | None -> ()
    end;
    ctx.local

  let body ctx f : D.t =
    ctx.local

  let special ctx lv f arglist : D.t =
    let desc = LF.find f in
    match desc.special arglist, f.vname with
    (* TODO: remove cases *)
    | _, "_lock_kernel" ->
      ctx.local
    | _, "_unlock_kernel" ->
      ctx.local
    | Lock { try_ = failing; write = rw; return_on_success = nonzero_return_when_aquired; _ }, _
      -> ctx.local
    | Unlock _, "__raw_read_unlock"
    | Unlock _, "__raw_write_unlock"  ->
      ctx.local
    | Unlock _, _ ->
      ctx.local
    | _, "spinlock_check" -> ctx.local
    | _, "acquire_console_sem" when get_bool "kernel" ->
      ctx.local
    | _, "release_console_sem" when get_bool "kernel" ->
      ctx.local
    | _, "__builtin_prefetch" | _, "misc_deregister" ->
      ctx.local
    | _, "__VERIFIER_atomic_begin" when get_bool "ana.sv-comp.functions" ->
      ctx.local
    | _, "__VERIFIER_atomic_end" when get_bool "ana.sv-comp.functions" ->
      ctx.local
    | _, "pthread_cond_wait"
    | _, "pthread_cond_timedwait" ->
      ctx.local
    | _, _ ->
      LibraryDesc.Accesses.iter desc.accs (fun {kind; deep = reach} exp ->
          access_one_top ~deref:true ctx kind reach exp (* access dereferenced using special accesses *)
        ) arglist;
      (match lv with
       | Some x -> access_one_top ~deref:true ctx Write false (AddrOf x)
       | None -> ());
      List.iter (access_one_top ctx Read false) arglist; (* always read all argument expressions without dereferencing *)
      ctx.local

  let enter ctx lv f args : (D.t * D.t) list =
    [(ctx.local,ctx.local)]

  let combine ctx lv fexp f args fc al =
    access_one_top ctx Read false fexp;
    begin match lv with
      | None      -> ()
      | Some lval -> access_one_top ~deref:true ctx Write false (AddrOf lval)
    end;
    List.iter (access_one_top ctx Read false) args;
    al


  let threadspawn ctx lval f args fctx =
    (* must explicitly access thread ID lval because special to pthread_create doesn't if singlethreaded before *)
    begin match lval with
      | None -> ()
      | Some lval -> access_one_top ~force:true ~deref:true ctx Write false (AddrOf lval) (* must force because otherwise doesn't if singlethreaded before *)
    end;
    ctx.local

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | MayAccessed ->
      (ctx.local: D.t)
    | _ -> Queries.Result.top q

  let sync reason ctx: D.t =
    D.empty ()

  let event ctx e octx =
    match e with
    | Events.AccessLocal access ->
      D.add access ctx.local
    | _ ->
      ctx.local
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
