(** Analysis of memory accesses ([access]). *)

module LF = LibraryFunctions
open GoblintCil
open Analyses
open GobConfig


(** Access analyzer without base --- this is the new standard *)
module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "access"

  module D = Lattice.Unit
  include Analyses.ValueContexts(D)

  module V =
  struct
    include Node
    let is_write_only _ = true
    let is_category x c = false
  end
  module G = AccessDomain.EventSet

  let collect_local = ref false
  let emit_single_threaded = ref false

  let init _ =
    collect_local := get_bool "witness.yaml.enabled" && get_bool "witness.invariant.accessed";
    let activated = get_string_list "ana.activated" in
    emit_single_threaded := List.mem (ModifiedSinceSetjmp.Spec.name ()) activated || List.mem (PoisonVariables.Spec.name ()) activated

  let do_access (ctx: (D.t, G.t, C.t, V.t) ctx) (kind:AccessKind.t) (reach:bool) (e:exp) =
    if M.tracing then M.trace "access" "do_access %a %a %B" d_exp e AccessKind.pretty kind reach;
    let reach_or_mpt: _ Queries.t = if reach then ReachableFrom e else MayPointTo e in
    let ad = ctx.ask reach_or_mpt in
    ctx.emit (Access {exp=e; ad; kind; reach})

  (** Three access levels:
      + [deref=false], [reach=false] - Access [exp] without dereferencing, used for all normal reads and all function call arguments.
      + [deref=true], [reach=false] - Access [exp] by dereferencing once (may-point-to), used for lval writes and shallow special accesses.
      + [deref=true], [reach=true] - Access [exp] by dereferencing transitively (reachable), used for deep special accesses. *)
  let access_one_top ?(force=false) ?(deref=false) ctx (kind: AccessKind.t) reach exp =
    if M.tracing then M.traceli "access" "access_one_top %a (kind = %a, reach = %B, deref = %B)" CilType.Exp.pretty exp AccessKind.pretty kind reach deref;
    if force || !collect_local || !emit_single_threaded || ThreadFlag.has_ever_been_multi (Analyses.ask_of_ctx ctx) then (
      if deref && Cil.isPointerType (Cilfacade.typeOf exp) then (* avoid dereferencing integers to unknown pointers, which cause many spurious type-based accesses *)
        do_access ctx kind reach exp;
      if M.tracing then M.tracei "access" "distribute_access_exp";
      Access.distribute_access_exp (do_access ctx Read false) exp;
      if M.tracing then M.traceu "access" "distribute_access_exp";
    );
    if M.tracing then M.traceu "access" "access_one_top"


  (** We just lift start state, global and dependency functions: *)
  let startstate v = ()
  let threadenter ctx ~multiple lval f args = [()]
  let exitstate  v = ()
  let context ctx fd d = ()


  (** Transfer functions: *)

  let vdecl ctx v =
    access_one_top ctx Read false (SizeOf v.vtype);
    ctx.local

  let assign ctx lval rval : D.t =
    (* ignore global inits *)
    if !AnalysisState.global_initialization then ctx.local else begin
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
    match desc.special arglist with
    (* TODO: remove Lock/Unlock cases when all those libraryfunctions use librarydescs and don't read mutex contents *)
    | Lock _
    | Unlock _ ->
      ctx.local
    | _ ->
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

  let combine_env ctx lval fexp f args fc au f_ask =
    (* These should be in enter, but enter cannot emit events, nor has fexp argument *)
    access_one_top ctx Read false fexp;
    List.iter (access_one_top ctx Read false) args;
    au

  let combine_assign ctx lv fexp f args fc al f_ask =
    begin match lv with
      | None      -> ()
      | Some lval -> access_one_top ~deref:true ctx Write false (AddrOf lval)
    end;
    ctx.local


  let threadspawn ctx  ~multiple lval f args fctx =
    (* must explicitly access thread ID lval because special to pthread_create doesn't if singlethreaded before *)
    begin match lval with
      | None -> ()
      | Some lval -> access_one_top ~force:true ~deref:true ctx Write false (AddrOf lval) (* must force because otherwise doesn't if singlethreaded before *)
    end;
    ctx.local

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | MayAccessed ->
      (ctx.global ctx.node: G.t)
    | _ -> Queries.Result.top q

  let event ctx e octx =
    match e with
    | Events.Access {ad; kind; _} when !collect_local && !AnalysisState.postsolving ->
      let events = Queries.AD.fold (fun addr es ->
          match addr with
          | Queries.AD.Addr.Addr (var, offs) ->
            let coffs = ValueDomain.Offs.to_cil offs in
            let access: AccessDomain.Event.t = {var_opt = (Some var); offs_opt = (Some coffs); kind} in
            G.add access es
          | UnknownPtr ->
            let access: AccessDomain.Event.t = {var_opt = None; offs_opt = None; kind} in
            G.add access es
          | _ -> es
        ) ad (G.empty ())
      in
      ctx.sideg ctx.node events
    | _ ->
      ctx.local
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
