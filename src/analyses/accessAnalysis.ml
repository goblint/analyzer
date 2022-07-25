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

  let name () = "access"

  module EventAccess =
  struct
    include Printable.Std
    type t = Events.access [@@deriving eq, ord, hash]
    let name () = "eventaccess"

    let pretty () ({var_opt; kind}: t) =
      dprintf "{var_opt=%a, kind=%a}" (docOpt (CilType.Varinfo.pretty ())) var_opt AccessKind.pretty kind

    include Printable.SimplePretty (
      struct
        type nonrec t = t
        let pretty = pretty
      end
      )
  end

  module D = SetDomain.Make (EventAccess)
  module C = Lattice.Unit

  module G =
  struct
    include Access.AS

    let leq x y = !GU.postsolving || leq x y (* HACK: to pass verify*)
  end
  module V = Printable.Prod (Access.LVOpt) (Access.T)

  let safe       = ref 0
  let vulnerable = ref 0
  let unsafe     = ref 0

  let init marshal =
    safe := 0;
    vulnerable := 0;
    unsafe := 0

  let side_access ctx ty lv_opt (conf, w, loc, e, a) =
    let ty =
      if Option.is_some lv_opt then
        `Type Cil.voidType (* avoid unsound type split for alloc variables *)
      else
        ty
    in
    let d =
      if !GU.should_warn then
        Access.AS.singleton (conf, w, loc, e, a)
      else
        G.bot () (* HACK: just to pass validation with MCP DomVariantLattice *)
    in
    ctx.sideg (lv_opt, ty) d

  let do_access (ctx: (D.t, G.t, C.t, V.t) ctx) (kind:AccessKind.t) (reach:bool) (conf:int) (e:exp) =
    if M.tracing then M.trace "access" "do_access %a %a %B\n" d_exp e AccessKind.pretty kind reach;
    let open Queries in
    let part_access ctx (e:exp) (vo:varinfo option) (kind: AccessKind.t): MCPAccess.A.t =
      ctx.emit (Access {var_opt=vo; kind});
      (*partitions & locks*)
      Obj.obj (ctx.ask (PartAccess (Memory {exp=e; var_opt=vo; kind})))
    in
    let add_access conf vo oo =
      let a = part_access ctx e vo kind in
      Access.add (side_access ctx) e kind conf vo oo a;
    in
    let add_access_struct conf ci =
      let a = part_access ctx e None kind in
      Access.add_struct (side_access ctx) e kind conf (`Struct (ci,`NoOffset)) None a
    in
    let has_escaped g = ctx.ask (Queries.MayEscape g) in
    (* The following function adds accesses to the lval-set ls
       -- this is the common case if we have a sound points-to set. *)
    let on_lvals ls includes_uk =
      let ls = LS.filter (fun (g,_) -> g.vglob || has_escaped g) ls in
      let conf = if reach then conf - 20 else conf in
      let conf = if includes_uk then conf - 10 else conf in
      let f (var, offs) =
        let coffs = Lval.CilLval.to_ciloffs offs in
        if CilType.Varinfo.equal var dummyFunDec.svar then
          add_access conf None (Some coffs)
        else
          add_access conf (Some var) (Some coffs)
      in
      LS.iter f ls
    in
    let reach_or_mpt = if reach then ReachableFrom e else MayPointTo e in
    match ctx.ask reach_or_mpt with
    | ls when not (LS.is_top ls) && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) ls) ->
      (* the case where the points-to set is non top and does not contain unknown values *)
      on_lvals ls false
    | ls when not (LS.is_top ls) ->
      (* the case where the points-to set is non top and contains unknown values *)
      let includes_uk = ref false in
      (* now we need to access all fields that might be pointed to: is this correct? *)
      begin match ctx.ask (ReachableUkTypes e) with
        | ts when Queries.TS.is_top ts ->
          includes_uk := true
        | ts ->
          if Queries.TS.is_empty ts = false then
            includes_uk := true;
          let f = function
            | TComp (ci, _) ->
              add_access_struct (conf - 50) ci
            | _ -> ()
          in
          Queries.TS.iter f ts
      end;
      on_lvals ls !includes_uk
    | _ ->
      add_access (conf - 60) None None

  (** Three access levels:
      + [deref=false], [reach=false] - Access [exp] without dereferencing, used for all normal reads and all function call arguments.
      + [deref=true], [reach=false] - Access [exp] by dereferencing once (may-point-to), used for lval writes and shallow special accesses.
      + [deref=true], [reach=true] - Access [exp] by dereferencing transitively (reachable), used for deep special accesses. *)
  let access_one_top ?(force=false) ?(deref=false) ctx (kind: AccessKind.t) reach exp =
    if M.tracing then M.traceli "access" "access_one_top %a %b %a:\n" AccessKind.pretty kind reach d_exp exp;
    if force || ThreadFlag.is_multi (Analyses.ask_of_ctx ctx) then (
      let conf = 110 in
      if deref then do_access ctx kind reach conf exp;
      Access.distribute_access_exp (do_access ctx Read false) conf exp;
    );
    if M.tracing then M.traceu "access" "access_one_top %a %b %a\n" AccessKind.pretty kind reach d_exp exp

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
    | WarnGlobal g ->
      let g: V.t = Obj.obj g in
      (* ignore (Pretty.printf "WarnGlobal %a\n" CilType.Varinfo.pretty g); *)
      let accs = ctx.global g in
      Stats.time "access" (Access.warn_global safe vulnerable unsafe g) accs
    | _ -> Queries.Result.top q

  let sync reason ctx: D.t =
    D.empty ()

  let event ctx e octx =
    match e with
    | Events.Access access ->
      D.add access ctx.local
    | _ ->
      ctx.local

  let finalize () =
    let total = !safe + !unsafe + !vulnerable in
    if total > 0 then (
      ignore (Pretty.printf "\nSummary for all memory locations:\n");
      ignore (Pretty.printf "\tsafe:        %5d\n" !safe);
      ignore (Pretty.printf "\tvulnerable:  %5d\n" !vulnerable);
      ignore (Pretty.printf "\tunsafe:      %5d\n" !unsafe);
      ignore (Pretty.printf "\t-------------------\n");
      ignore (Pretty.printf "\ttotal:       %5d\n" total)
    )
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
