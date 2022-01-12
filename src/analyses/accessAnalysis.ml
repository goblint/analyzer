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

  module D = Lattice.Unit
  module C = Lattice.Unit

  module G =
  struct
    include Access.PM

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

  let side_access ctx ty lv_opt ls_opt (conf, mhp, w, loc, e, lp) =
    if !GU.should_warn then (
      let d =
        let open Access in
        PM.singleton ls_opt (
          AS.singleton (conf, mhp, w, loc, e, lp)
        )
      in
      ctx.sideg (lv_opt, ty) d
    )
    else
      ctx.sideg (lv_opt, ty) (G.bot ()) (* HACK: just to pass validation with MCP DomVariantLattice *)

  let do_access (ctx: (D.t, G.t, C.t, V.t) ctx) (w:bool) (reach:bool) (conf:int) (e:exp) =
    let tid = ctx.ask CurrentThreadId in
    let created = ctx.ask CreatedThreads in
    let joined = ctx.ask MustJoinedThreads in
    let mhp:MHP.t = {tid=tid; created=created; must_joined=joined } in
    let open Queries in
    let part_access ctx (e:exp) (vo:varinfo option) (w: bool): MCPAccess.A.t =
      ctx.emit (Access {var_opt=vo; write=w});
      (*partitions & locks*)
      Obj.obj (ctx.ask (PartAccess {exp=e; var_opt=vo; write=w}))
    in
    let add_access conf vo oo =
      let a = part_access ctx e vo w in
      Access.add (side_access ctx) e w conf mhp vo oo a;
    in
    let add_access_struct conf ci =
      let a = part_access ctx e None w in
      Access.add_struct (side_access ctx) e w conf mhp (`Struct (ci,`NoOffset)) None a
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

  let access_one_top ?(force=false) ctx write reach exp =
    (* ignore (Pretty.printf "access_one_top %b %b %a:\n" write reach d_exp exp); *)
    if force || ThreadFlag.is_multi (Analyses.ask_of_ctx ctx) then (
      let conf = 110 in
      if reach || write then do_access ctx write reach conf exp;
      Access.distribute_access_exp (do_access ctx) false false conf exp;
    )

  (** We just lift start state, global and dependency functions: *)
  let startstate v = ()
  let threadenter ctx lval f args = [()]
  let exitstate  v = ()


  (** Transfer functions: *)

  let assign ctx lval rval : D.t =
    (* ignore global inits *)
    if !GU.global_initialization then ctx.local else begin
      access_one_top ctx true  false (AddrOf lval);
      access_one_top ctx false false rval;
      ctx.local
    end

  let branch ctx exp tv : D.t =
    access_one_top ctx false false exp;
    ctx.local

  let return ctx exp fundec : D.t =
    begin match exp with
      | Some exp -> access_one_top ctx false false exp
      | None -> ()
    end;
    ctx.local

  let body ctx f : D.t =
    ctx.local

  let special ctx lv f arglist : D.t =
    match (LF.classify f.vname arglist, f.vname) with
    (* TODO: remove cases *)
    | _, "_lock_kernel" ->
      ctx.local
    | _, "_unlock_kernel" ->
      ctx.local
    | `Lock (failing, rw, nonzero_return_when_aquired), _
      -> ctx.local
    | `Unlock, "__raw_read_unlock"
    | `Unlock, "__raw_write_unlock"  ->
      ctx.local
    | `Unlock, _ ->
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
    | _, x ->
      let arg_acc act =
        match LF.get_threadsafe_inv_ac x with
        | Some fnc -> (fnc act arglist)
        | _ -> arglist
      in
      List.iter (access_one_top ctx false true) (arg_acc `Read);
      List.iter (access_one_top ctx true  true ) (arg_acc `Write);
      (match lv with
       | Some x -> access_one_top ctx true false (AddrOf x)
       | None -> ());
      ctx.local

  let enter ctx lv f args : (D.t * D.t) list =
    [(ctx.local,ctx.local)]

  let combine ctx lv fexp f args fc al =
    access_one_top ctx false false fexp;
    begin match lv with
      | None      -> ()
      | Some lval -> access_one_top ctx true false (AddrOf lval)
    end;
    List.iter (access_one_top ctx false false) args;
    al


  let threadspawn ctx lval f args fctx =
    (* must explicitly access thread ID lval because special to pthread_create doesn't if singlethreaded before *)
    begin match lval with
      | None -> ()
      | Some lval -> access_one_top ~force:true ctx true false (AddrOf lval) (* must force because otherwise doesn't if singlethreaded before *)
    end;
    ctx.local

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | WarnGlobal g ->
      let g: V.t = Obj.obj g in
      (* ignore (Pretty.printf "WarnGlobal %a\n" CilType.Varinfo.pretty g); *)
      let pm = ctx.global g in
      Access.print_accesses g pm;
      Access.incr_summary safe vulnerable unsafe g pm
    | _ -> Queries.Result.top q

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
