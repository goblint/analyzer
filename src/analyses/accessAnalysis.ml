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
  module C = Lattice.Unit

  module V =
  struct
    include Node
    let is_write_only _ = true
  end
  module G = AccessDomain.EventSet

  let collect_local = ref false
  let emit_single_threaded = ref false

  let init _ =
    collect_local := get_bool "witness.yaml.enabled" && get_bool "witness.invariant.accessed";
    let activated = get_string_list "ana.activated" in
    emit_single_threaded := List.mem (ModifiedSinceLongjmp.Spec.name ()) activated || List.mem (PoisonVariables.Spec.name ()) activated

  let do_access ?(lval=None) ?(resolve=true) (ctx: (D.t, G.t, C.t, V.t) ctx) (kind:AccessKind.t) (reach:bool) (e:exp) =
    if M.tracing then M.trace "access" "do_access %a %a %B\n" d_exp e AccessKind.pretty kind reach;
    let reach_or_mpt: _ Queries.t = if reach then ReachableFrom e else MayPointTo e in
    let ls = BatOption.default (ctx.ask reach_or_mpt) lval in
    ctx.emit (Access {exp=e; lvals=ls; kind; reach})

  (** Three access levels:
      + [deref=false], [reach=false] - Access [exp] without dereferencing, used for all normal reads and all function call arguments.
      + [deref=true], [reach=false] - Access [exp] by dereferencing once (may-point-to), used for lval writes and shallow special accesses.
      + [deref=true], [reach=true] - Access [exp] by dereferencing transitively (reachable), used for deep special accesses.
    If [lval] is passed, access is no ReachableFrom and MayPointTo queries are performed. *)
  let access_one_top ?lval ?(force=false) ?(deref=false) ctx (kind: AccessKind.t) reach exp =
    if M.tracing then M.traceli "access" "access_one_top %a %b %a:\n" AccessKind.pretty kind reach d_exp exp;
    if force || !collect_local || !emit_single_threaded || ThreadFlag.has_ever_been_multi (Analyses.ask_of_ctx ctx) then (
      if deref then
        do_access ~lval ctx kind reach exp;
      Access.distribute_access_exp (do_access ~lval ctx Read false) exp
    );
    if M.tracing then M.traceu "access" "access_one_top %a %b %a\n" AccessKind.pretty kind reach d_exp exp

  (** We just lift start state, global and dependency functions: *)
  let startstate v = ()
  let threadenter ctx lval f args = [()]
  let exitstate  v = ()
  let context fd d = ()


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

  let modular_combine_env ctx lv f args (f_ask: Queries.ask) : D.t =
    let open ValueDomain in
    let open WrittenDomain in
    (* TODO: Handle values that are read in modular functions *)
    let written = f_ask.f Queries.Written in
    (* Collect all addreses that were written to. *)
    let addresses =
      let list = ref [] in
      Written.iter (fun a _ -> list := a :: !list) written;
      !list
    in
    let get_reachable e =
      match ctx.ask (Queries.ReachableAddressesFrom e) with
      | `Top -> failwith "ReachableAddressesFrom returned `Top."
      | `Lifted a -> a
    in
    let used_globals = ModularUtil.get_callee_globals f_ask in
    let effective_args = args @ used_globals in
    let reachable =
      List.fold_left (fun acc arg -> AD.join acc (get_reachable arg) ) (AD.bot ()) effective_args
    in
    let map_back a = ModularUtil.ValueDomainExtension.map_back a ~reachable in
    let map_back a =
      let address = VD.Address a in
      match map_back address with
      | Address maped_back ->
        maped_back
      | _ ->
        M.warn "map_back failed for %a" AD.pretty a;
        AD.bot ()
    in
    let addresses = List.map map_back addresses in
    (* Convert to lvals *)
    let address_to_lvals (ad: AD.t) =
      let addr_to_lval_and_exp (v, offs) =
        let lhost = Var v in
        let offset = Offs.to_cil_offset offs in
        let exp = AddrOf (lhost, offset) in

        let offs = Lval.CilLval.of_ciloffs offset in
        let lval = v, offs in

        lval, exp
      in
      let addrs = AD.to_var_offset ad in
      List.map addr_to_lval_and_exp addrs
    in
    let lvs_exps = List.concat_map address_to_lvals addresses in
    (* Trigger access events *)
    let add_lval_event ((v, offs), exp) =
      let ls = LS.of_list [v, offs] in
      access_one_top ~lval:ls ~deref:true ctx AccessKind.Write false exp
    in
    List.iter add_lval_event lvs_exps

  let write_lval_option ctx lval =
    match lval with
    | None -> ()
    | Some lval -> access_one_top ~deref:true ctx Write false (AddrOf lval);
    ctx.local

  let modular_combine_assign ctx lv f arglist (f_ask: Queries.ask) : D.t =
    write_lval_option ctx lv

  let enter ctx lv f args : (D.t * D.t) list =
    [(ctx.local,ctx.local)]

  let combine_env ctx lval fexp f args fc au f_ask =
    access_one_top ctx Read false fexp;
    List.iter (access_one_top ctx Read false) args;
    au

  let combine_assign ctx lv fexp f args fc al f_ask =
    write_lval_option ctx lv

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
      (ctx.global ctx.node: G.t)
    | _ -> Queries.Result.top q

  let event ctx e octx =
    match e with
    | Events.Access {lvals; kind; _} when !collect_local && !AnalysisState.postsolving ->
      begin match lvals with
        | ls when Queries.LS.is_top ls ->
          let access: AccessDomain.Event.t = {var_opt = None; offs_opt = None; kind} in
          ctx.sideg ctx.node (G.singleton access)
        | ls ->
          let events = Queries.LS.fold (fun (var, offs) acc ->
              let coffs = Lval.CilLval.to_ciloffs offs in
              let access: AccessDomain.Event.t =
                if CilType.Varinfo.equal var dummyFunDec.svar then
                  {var_opt = None; offs_opt = (Some coffs); kind}
                else
                  {var_opt = (Some var); offs_opt = (Some coffs); kind}
              in
              G.add access acc
            ) ls (G.empty ())
          in
          ctx.sideg ctx.node events
      end
    | _ ->
      ctx.local
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
