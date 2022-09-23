(** Analysis using Apron for integer variables. *)

open Prelude.Ana
open Analyses

include RelationAnalysis

module ExtendedSpecFunctor (AD: RelationDomain.RD) (Priv: RelationPriv.S) =
struct

  module OctApron = ApronPrecCompareUtil.OctagonD
  include SpecFunctor (Priv) (AD) (ApronPrecCompareUtil.Util)
  module AD = ApronDomain.D2(OctApron.Man)
  module PCU = ApronPrecCompareUtil.Util(OctApron)

<<<<<<< HEAD
  let results = PCU.RH.create 103 (*ToDo This should not be created again!*)
=======
  module AD =
  struct
    include AD
    include ApronDomain.Tracked
  end

  module Priv = Priv(AD)
  module D = ApronComponents (AD) (Priv.D)
  module G = Priv.G
  module C = D
  module V =
  struct
    include Priv.V
    include StdV
  end

  open AD
  open (ApronDomain: (sig module V: (module type of ApronDomain.V) end)) (* open only V from ApronDomain (to shadow V of Spec), but don't open D (to not shadow D here) *)

  open ApronPrecCompareUtil
  (* Result map used for comparison of results *)
  let results = RH.create 103

  let should_join = Priv.should_join

  let context fd x =
    if ContextUtil.should_keep ~isAttr:GobContext ~keepOption:"ana.apron.context" ~removeAttr:"apron.no-context" ~keepAttr:"apron.context" fd then
      x
    else
      D.bot () (* just like startstate, heterogeneous AD.bot () means top over empty set of variables *)

  let exitstate  _ = { apr = AD.bot (); priv = Priv.startstate () }
  let startstate _ = { apr = AD.bot (); priv = Priv.startstate () }

  (* Functions for manipulating globals as temporary locals. *)

  let read_global ask getg st g x =
    if ThreadFlag.is_multi ask then
      Priv.read_global ask getg st g x
    else (
      let apr = st.apr in
      let g_var = V.global g in
      let x_var = V.local x in
      let apr' = AD.add_vars apr [g_var] in
      let apr' = AD.assign_var apr' x_var g_var in
      apr'
    )

  module VH = BatHashtbl.Make (Basetype.Variables)

  let read_globals_to_locals (ask:Queries.ask) getg st e =
    let v_ins = VH.create 10 in
    let visitor = object
      inherit nopCilVisitor
      method! vvrbl (v: varinfo) =
        if v.vglob || ThreadEscape.has_escaped ask v then (
          let v_in =
            if VH.mem v_ins v then
              VH.find v_ins v
            else
              let v_in = Goblintutil.create_var @@ makeVarinfo false (v.vname ^ "#in") v.vtype in (* temporary local g#in for global g *)
              VH.replace v_ins v v_in;
              v_in
          in
          ChangeTo v_in
        )
        else
          SkipChildren
    end
    in
    let e' = visitCilExpr visitor e in
    let apr = AD.add_vars st.apr (List.map V.local (VH.values v_ins |> List.of_enum)) in (* add temporary g#in-s *)
    let apr' = VH.fold (fun v v_in apr ->
        if M.tracing then M.trace "apron" "read_global %a %a\n" d_varinfo v d_varinfo v_in;
        read_global ask getg {st with apr = apr} v v_in (* g#in = g; *)
      ) v_ins apr
    in
    (apr', e', v_ins)

  let read_from_globals_wrapper ask getg st e f =
    let (apr', e', _) = read_globals_to_locals ask getg st e in
    f apr' e' (* no need to remove g#in-s *)

  let assign_from_globals_wrapper ask getg st e f =
    let (apr', e', v_ins) = read_globals_to_locals ask getg st e in
    if M.tracing then M.trace "apron" "assign_from_globals_wrapper %a\n" d_exp e';
    let apr' = f apr' e' in (* x = e; *)
    let apr'' = AD.remove_vars apr' (List.map V.local (VH.values v_ins |> List.of_enum)) in (* remove temporary g#in-s *)
    apr''

  let write_global ask getg sideg st g x =
    if ThreadFlag.is_multi ask then
      Priv.write_global ask getg sideg st g x
    else (
      let apr = st.apr in
      let g_var = V.global g in
      let x_var = V.local x in
      let apr' = AD.add_vars apr [g_var] in
      let apr' = AD.assign_var apr' g_var x_var in
      {st with apr = apr'}
    )

  let rec assign_to_global_wrapper (ask:Queries.ask) getg sideg st lv f =
    match lv with
    | (Var v, NoOffset) when AD.varinfo_tracked v ->
      if not v.vglob && not (ThreadEscape.has_escaped ask v) then (
        if ask.f (Queries.IsMultiple v) then
          {st with apr = AD.join (f st v) st.apr}
        else
          {st with apr = f st v}
      )
      else (
        let v_out = Goblintutil.create_var @@ makeVarinfo false (v.vname ^ "#out") v.vtype in (* temporary local g#out for global g *)
        let st = {st with apr = AD.add_vars st.apr [V.local v_out]} in (* add temporary g#out *)
        let st' = {st with apr = f st v_out} in (* g#out = e; *)
        if M.tracing then M.trace "apron" "write_global %a %a\n" d_varinfo v d_varinfo v_out;
        let st' = write_global ask getg sideg st' v v_out in (* g = g#out; *)
        let apr'' = AD.remove_vars st'.apr [V.local v_out] in (* remove temporary g#out *)
        {st' with apr = apr''}
      )
    | (Mem v, NoOffset) ->
      (let r = ask.f (Queries.MayPointTo v) in
       match r with
       | `Top ->
         st
       | `Lifted s ->
         let lvals = Queries.LS.elements r in
         let ass' = List.map (fun lv -> assign_to_global_wrapper ask getg sideg st (Lval.CilLval.to_lval lv) f) lvals in
         List.fold_right D.join ass' (D.bot ())
      )
    (* Ignoring all other assigns *)
    | _ ->
      st

  let assert_type_bounds apr x =
    assert (AD.varinfo_tracked x);
    match Cilfacade.get_ikind x.vtype with
    | ik when not (IntDomain.should_ignore_overflow ik) -> (* don't add type bounds for signed when assume_none *)
      let (type_min, type_max) = IntDomain.Size.range ik in
      (* TODO: don't go through CIL exp? *)
      let apr = AD.assert_inv apr (BinOp (Le, Lval (Cil.var x), (Cil.kintegerCilint ik (Cilint.cilint_of_big_int type_max)), intType)) false in
      let apr = AD.assert_inv apr (BinOp (Ge, Lval (Cil.var x), (Cil.kintegerCilint ik (Cilint.cilint_of_big_int type_min)), intType)) false in
      apr
    | _
    | exception Invalid_argument _ ->
      apr


  let replace_deref_exps ask e =
    let rec inner e = match e with
      | Const x -> e
      | UnOp (unop, e, typ) -> UnOp(unop, inner e, typ)
      | BinOp (binop, e1, e2, typ) -> BinOp (binop, inner e1, inner e2, typ)
      | CastE (t,e) -> CastE (t, inner e)
      | Lval (Var v, off) -> Lval (Var v, off)
      | Lval (Mem e, NoOffset) ->
        (match ask (Queries.MayPointTo e) with
         | a when not (Queries.LS.is_top a || Queries.LS.mem (dummyFunDec.svar, `NoOffset) a) && (Queries.LS.cardinal a) = 1 ->
           let lval = Lval.CilLval.to_lval (Queries.LS.choose a) in
           Lval lval
         (* It would be possible to do better here, exploiting e.g. that the things pointed to are known to be equal *)
         (* see: https://github.com/goblint/analyzer/pull/742#discussion_r879099745 *)
         | _ -> Lval (Mem e, NoOffset))
      | e -> e (* TODO: Potentially recurse further? *)
    in
    inner e

  (* Basic transfer functions. *)

  let assign ctx (lv:lval) e =
    let st = ctx.local in
    if !GU.global_initialization && e = MyCFG.unknown_exp then
      st (* ignore extern inits because there's no body before assign, so the apron env is empty... *)
    else (
      let simplified_e = replace_deref_exps ctx.ask e in
      if M.tracing then M.traceli "apron" "assign %a = %a (simplified to %a)\n" d_lval lv  d_exp e d_exp simplified_e;
      let ask = Analyses.ask_of_ctx ctx in
      let r = assign_to_global_wrapper ask ctx.global ctx.sideg st lv (fun st v ->
          assign_from_globals_wrapper ask ctx.global st simplified_e (fun apr' e' ->
              if M.tracing then M.traceli "apron" "assign inner %a = %a (%a)\n" d_varinfo v d_exp e' d_plainexp e';
              if M.tracing then M.trace "apron" "st: %a\n" AD.pretty apr';
              let r = AD.assign_exp apr' (V.local v) e' in
              if M.tracing then M.traceu "apron" "-> %a\n" AD.pretty r;
              r
            )
        )
      in
      if M.tracing then M.traceu "apron" "-> %a\n" D.pretty r;
      r
    )

  let branch ctx e b =
    let st = ctx.local in
    let res = assign_from_globals_wrapper (Analyses.ask_of_ctx ctx) ctx.global st e (fun apr' e' ->
        (* not an assign, but must remove g#in-s still *)
        AD.assert_inv apr' e' (not b)
      )
    in
    if AD.is_bot_env res then raise Deadcode;
    {st with apr = res}


  (* Function call transfer functions. *)

  let any_local_reachable fundec reachable_from_args =
    let locals = fundec.sformals @ fundec.slocals in
    let locals_id = List.map (fun v -> v.vid) locals in
    Queries.LS.exists (fun (v',_) -> List.mem v'.vid locals_id && AD.varinfo_tracked v') reachable_from_args

  let pass_to_callee fundec any_local_reachable var =
    (* TODO: currently, we pass all locals of the caller to the callee, provided one of them is reachbale to preserve relationality *)
    (* there should be smarter ways to do this, e.g. by keeping track of which values are written etc. ... *)
    (* Also, a local *)
    let vname = Var.to_string var in
    let locals = fundec.sformals @ fundec.slocals in
    match List.find_opt (fun v -> V.local_name v = vname) locals with
    | None -> true
    | Some v -> any_local_reachable

  let enter ctx r f args =
    let fundec = Node.find_fundec ctx.node in
    let st = ctx.local in
    if M.tracing then M.tracel "combine" "apron enter f: %a\n" d_varinfo f.svar;
    if M.tracing then M.tracel "combine" "apron enter formals: %a\n" (d_list "," d_varinfo) f.sformals;
    if M.tracing then M.tracel "combine" "apron enter local: %a\n" D.pretty ctx.local;
    let arg_assigns =
      GobList.combine_short f.sformals args (* TODO: is it right to ignore missing formals/args? *)
      |> List.filter (fun (x, _) -> AD.varinfo_tracked x)
      |> List.map (Tuple2.map1 V.arg)
    in
    let reachable_from_args = List.fold (fun ls e -> Queries.LS.join ls (ctx.ask (ReachableFrom e))) (Queries.LS.empty ()) args in
    let arg_vars = List.map fst arg_assigns in
    let new_apr = AD.add_vars st.apr arg_vars in
    (* AD.assign_exp_parallel_with new_oct arg_assigns; (* doesn't need to be parallel since exps aren't arg vars directly *) *)
    (* TODO: parallel version of assign_from_globals_wrapper? *)
    let ask = Analyses.ask_of_ctx ctx in
    let new_apr = List.fold_left (fun new_apr (var, e) ->
        assign_from_globals_wrapper ask ctx.global {st with apr = new_apr} e (fun apr' e' ->
            AD.assign_exp apr' var e'
          )
      ) new_apr arg_assigns
    in
    let any_local_reachable = any_local_reachable fundec reachable_from_args in
    AD.remove_filter_with new_apr (fun var ->
        match V.find_metadata var with
        | Some Local when not (pass_to_callee fundec any_local_reachable var) -> true (* remove caller locals provided they are unreachable *)
        | Some Arg when not (List.mem_cmp Var.compare var arg_vars) -> true (* remove caller args, but keep just added args *)
        | _ -> false (* keep everything else (just added args, globals, global privs) *)
      );
    if M.tracing then M.tracel "combine" "apron enter newd: %a\n" AD.pretty new_apr;
    [st, {st with apr = new_apr}]

  let body ctx f =
    let st = ctx.local in
    let formals = List.filter AD.varinfo_tracked f.sformals in
    let locals = List.filter AD.varinfo_tracked f.slocals in
    let new_apr = AD.add_vars st.apr (List.map V.local (formals @ locals)) in
    (* TODO: do this after local_assigns? *)
    let new_apr = List.fold_left (fun new_apr x ->
        assert_type_bounds new_apr x
      ) new_apr (formals @ locals)
    in
    let local_assigns = List.map (fun x -> (V.local x, V.arg x)) formals in
    AD.assign_var_parallel_with new_apr local_assigns; (* doesn't need to be parallel since arg vars aren't local vars *)
    {st with apr = new_apr}

  let return ctx e f =
    let st = ctx.local in
    let ask = Analyses.ask_of_ctx ctx in
    let new_apr =
      if AD.type_tracked (Cilfacade.fundec_return_type f) then (
        let apr' = AD.add_vars st.apr [V.return] in
        match e with
        | Some e ->
          assign_from_globals_wrapper (Analyses.ask_of_ctx ctx) ctx.global {st with apr = apr'} e (fun apr' e' ->
              AD.assign_exp apr' V.return e'
            )
        | None ->
          apr' (* leaves V.return unconstrained *)
      )
      else
        AD.copy st.apr
    in
    let local_vars =
      f.sformals @ f.slocals
      |> List.filter AD.varinfo_tracked
      |> List.map V.local
    in

    AD.remove_vars_with new_apr local_vars;
    let st' = {st with apr = new_apr} in
    begin match ThreadId.get_current ask with
      | `Lifted tid when ThreadReturn.is_current ask ->
        Priv.thread_return ask ctx.global ctx.sideg tid st'
      | _ ->
        st'
    end

  let combine ctx r fe f args fc fun_st =
    let st = ctx.local in
    let reachable_from_args = List.fold (fun ls e -> Queries.LS.join ls (ctx.ask (ReachableFrom e))) (Queries.LS.empty ()) args in
    let fundec = Node.find_fundec ctx.node in
    if M.tracing then M.tracel "combine" "apron f: %a\n" d_varinfo f.svar;
    if M.tracing then M.tracel "combine" "apron formals: %a\n" (d_list "," d_varinfo) f.sformals;
    if M.tracing then M.tracel "combine" "apron args: %a\n" (d_list "," d_exp) args;
    let new_fun_apr = AD.add_vars fun_st.apr (AD.vars st.apr) in
    let arg_substitutes =
      GobList.combine_short f.sformals args (* TODO: is it right to ignore missing formals/args? *)
      (* Do not do replacement for actuals whose value may be modified after the call *)
      |> List.filter (fun (x, e) -> AD.varinfo_tracked x && List.for_all (fun v -> not (Queries.LS.exists (fun (v',_) -> v'.vid = v.vid) reachable_from_args)) (Basetype.CilExp.get_vars e))
      |> List.map (Tuple2.map1 V.arg)
    in
    (* AD.substitute_exp_parallel_with new_fun_oct arg_substitutes; (* doesn't need to be parallel since exps aren't arg vars directly *) *)
    (* TODO: parallel version of assign_from_globals_wrapper? *)
    let ask = Analyses.ask_of_ctx ctx in
    let new_fun_apr = List.fold_left (fun new_fun_apr (var, e) ->
        assign_from_globals_wrapper ask ctx.global {st with apr = new_fun_apr} e (fun apr' e' ->
            (* not an assign, but still works? *)
            AD.substitute_exp apr' var e'
          )
      ) new_fun_apr arg_substitutes
    in
    let any_local_reachable = any_local_reachable fundec reachable_from_args in
    let arg_vars = f.sformals |> List.filter (AD.varinfo_tracked) |> List.map V.arg in
    if M.tracing then M.tracel "combine" "apron remove vars: %a\n" (docList (fun v -> Pretty.text (Var.to_string v))) arg_vars;
    AD.remove_vars_with new_fun_apr arg_vars; (* fine to remove arg vars that also exist in caller because unify from new_apr adds them back with proper constraints *)
    let new_apr = AD.keep_filter st.apr (fun var ->
        match V.find_metadata var with
        | Some Local when not (pass_to_callee fundec any_local_reachable var) -> true (* keep caller locals, provided they were not passed to the function *)
        | Some Arg -> true (* keep caller args *)
        | _ -> false (* remove everything else (globals, global privs, reachable things from the caller) *)
      )
    in
    let unify_apr = AD.unify new_apr new_fun_apr in (* TODO: unify_with *)
    if M.tracing then M.tracel "combine" "apron unifying %a %a = %a\n" AD.pretty new_apr AD.pretty new_fun_apr AD.pretty unify_apr;
    let unify_st = {fun_st with apr = unify_apr} in
    if AD.type_tracked (Cilfacade.fundec_return_type f) then (
      let unify_st' = match r with
        | Some lv ->
          assign_to_global_wrapper (Analyses.ask_of_ctx ctx) ctx.global ctx.sideg unify_st lv (fun st v ->
              AD.assign_var st.apr (V.local v) V.return
            )
        | None ->
          unify_st
      in
      AD.remove_vars_with unify_st'.apr [V.return]; (* mutates! *)
      unify_st'
    )
    else
      unify_st


  let invalidate_one ask ctx st lv =
    assign_to_global_wrapper ask ctx.global ctx.sideg st lv (fun st v ->
        let apr' = AD.forget_vars st.apr [V.local v] in
        assert_type_bounds apr' v (* re-establish type bounds after forget *)
      )


  (* Give the set of reachables from argument. *)
  let reachables (ask: Queries.ask) es =
    let reachable e st =
      match st with
      | None -> None
      | Some st ->
        let vs = ask.f (Queries.ReachableFrom e) in
        if Queries.LS.is_top vs then
          None
        else
          Some (Queries.LS.join vs st)
    in
    List.fold_right reachable es (Some (Queries.LS.empty ()))


  let forget_reachable ctx st es =
    let ask = Analyses.ask_of_ctx ctx in
    let rs =
      match reachables ask es with
      | None ->
        (* top reachable, so try to invalidate everything *)
        let fd = Node.find_fundec ctx.node in
        AD.vars st.apr
        |> List.filter_map (V.to_cil_varinfo fd)
        |> List.map Cil.var
      | Some rs ->
        Queries.LS.elements rs
        |> List.map Lval.CilLval.to_lval
    in
    List.fold_left (fun st lval ->
        invalidate_one ask ctx st lval
      ) st rs

  let assert_fn ctx e refine =
    if not refine then
      ctx.local
    else
      (* copied from branch *)
      let st = ctx.local in
      let res = assign_from_globals_wrapper (Analyses.ask_of_ctx ctx) ctx.global st e (fun apr' e' ->
          (* not an assign, but must remove g#in-s still *)
          AD.assert_inv apr' e' false
        )
      in
      if AD.is_bot_env res then raise Deadcode;
      {st with apr = res}

  let special ctx r f args =
    let ask = Analyses.ask_of_ctx ctx in
    let st = ctx.local in
    let desc = LibraryFunctions.find f in
    match desc.special args, f.vname with
    | Assert { exp; refine; _ }, _ -> assert_fn ctx exp refine
    | ThreadJoin { thread = id; ret_var = retvar }, _ ->
      (
        (* Forget value that thread return is assigned to *)
        let st' = forget_reachable ctx st [retvar] in
        let st' = Priv.thread_join ask ctx.global id st' in
        match r with
        | Some lv -> invalidate_one ask ctx st' lv
        | None -> st'
      )
    | ThreadExit _, _ ->
      begin match ThreadId.get_current ask with
        | `Lifted tid ->
          (* value returned from the thread is not used in thread_join or any Priv.thread_join, *)
          (* thus no handling like for returning from functions required *)
          ignore @@ Priv.thread_return ask ctx.global ctx.sideg tid st;
          raise Deadcode
        | _ ->
          raise Deadcode
      end
    | Unknown, "__goblint_assume_join" ->
      let id = List.hd args in
      Priv.thread_join ~force:true ask ctx.global id st
    | _, _ ->
      let lvallist e =
        let s = ask.f (Queries.MayPointTo e) in
        match s with
        | `Top -> []
        | `Lifted _ -> List.map (Lval.CilLval.to_lval) (Queries.LS.elements s)
      in
      let shallow_addrs = LibraryDesc.Accesses.find desc.accs { kind = Write; deep = false } args in
      let deep_addrs = LibraryDesc.Accesses.find desc.accs { kind = Write; deep = true } args in
      let deep_addrs =
        if List.mem LibraryDesc.InvalidateGlobals desc.attrs then (
          foldGlobals !Cilfacade.current_file (fun acc global ->
              match global with
              | GVar (vi, _, _) when not (BaseUtil.is_static vi) ->
                mkAddrOf (Var vi, NoOffset) :: acc
              (* TODO: what about GVarDecl? *)
              | _ -> acc
            ) deep_addrs
        )
        else
          deep_addrs
      in
      let st' = forget_reachable ctx st deep_addrs in
      let shallow_lvals = List.concat_map lvallist shallow_addrs in
      let st' = List.fold_left (invalidate_one ask ctx) st' shallow_lvals in
      (* invalidate lval if present *)
      match r with
      | Some lv -> invalidate_one ask ctx st' lv
      | None -> st'


  let query_invariant ctx context =
    let keep_local = GobConfig.get_bool "ana.apron.invariant.local" in
    let keep_global = GobConfig.get_bool "ana.apron.invariant.global" in

    let apr = ctx.local.apr in
    (* filter variables *)
    let var_filter v = match V.find_metadata v with
      | Some (Global _) -> keep_global
      | Some Local -> keep_local
      | _ -> false
    in
    let apr = AD.keep_filter apr var_filter in

    let one_var = GobConfig.get_bool "ana.apron.invariant.one-var" in
    let scope = Node.find_fundec ctx.node in

    AD.invariant ~scope apr
    |> List.enum
    |> Enum.filter_map (fun (lincons1: Lincons1.t) ->
        (* filter one-vars *)
        if one_var || Apron.Linexpr0.get_size lincons1.lincons0.linexpr0 >= 2 then
          CilOfApron.cil_exp_of_lincons1 scope lincons1
          |> Option.filter (fun exp -> not (InvariantCil.exp_contains_tmp exp) && InvariantCil.exp_is_in_scope scope exp)
        else
          None
      )
    |> Enum.fold (fun acc x -> Invariant.(acc && of_exp x)) Invariant.none

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    let st = ctx.local in
    let eval_int e =
      let esimple = replace_deref_exps ctx.ask e in
      read_from_globals_wrapper
        (Analyses.ask_of_ctx ctx)
        ctx.global st esimple
        (fun apr' e' -> AD.eval_int apr' e')
    in
    match q with
    | EvalInt e ->
      if M.tracing then M.traceli "evalint" "apron query %a (%a)\n" d_exp e d_plainexp e;
      if M.tracing then M.trace "evalint" "apron st: %a\n" D.pretty ctx.local;
      let r = eval_int e in
      if M.tracing then M.traceu "evalint" "apron query %a -> %a\n" d_exp e ID.pretty r;
      r
    | Queries.IterSysVars (vq, vf) ->
      let vf' x = vf (Obj.repr x) in
      Priv.iter_sys_vars ctx.global vq vf'
    | Queries.Invariant context ->
      query_invariant ctx context
    | _ -> Result.top q


  (* Thread transfer functions. *)

  let threadenter ctx lval f args =
    let st = ctx.local in
    match Cilfacade.find_varinfo_fundec f with
    | fd ->
      (* TODO: HACK: Simulate enter_multithreaded for first entering thread to publish global inits before analyzing thread.
        Otherwise thread is analyzed with no global inits, reading globals gives bot, which turns into top, which might get published...
        sync `Thread doesn't help us here, it's not specific to entering multithreaded mode.
        EnterMultithreaded events only execute after threadenter and threadspawn. *)
      if not (ThreadFlag.is_multi (Analyses.ask_of_ctx ctx)) then
        ignore (Priv.enter_multithreaded (Analyses.ask_of_ctx ctx) ctx.global ctx.sideg st);
      let st' = Priv.threadenter (Analyses.ask_of_ctx ctx) ctx.global st in
      let arg_vars =
        fd.sformals
        |> List.filter AD.varinfo_tracked
        |> List.map V.arg
      in
      let new_apr = AD.add_vars st'.apr arg_vars in
      [{st' with apr = new_apr}]
    | exception Not_found ->
      (* Unknown functions *)
      (* TODO: do something like base? *)
      failwith "apron.threadenter: unknown function"

  let threadspawn ctx lval f args fctx =
    ctx.local

  let event ctx e octx =
    let st = ctx.local in
    match e with
    | Events.Lock (addr, _) when ThreadFlag.is_multi (Analyses.ask_of_ctx ctx) -> (* TODO: is this condition sound? *)
      Priv.lock (Analyses.ask_of_ctx ctx) ctx.global st addr
    | Events.Unlock addr when ThreadFlag.is_multi (Analyses.ask_of_ctx ctx) -> (* TODO: is this condition sound? *)
      if addr = UnknownPtr then
        M.info ~category:Unsound "Unknown mutex unlocked, apron privatization unsound"; (* TODO: something more sound *)
      Priv.unlock (Analyses.ask_of_ctx ctx) ctx.global ctx.sideg st addr
    | Events.EnterMultiThreaded ->
      Priv.enter_multithreaded (Analyses.ask_of_ctx ctx) ctx.global ctx.sideg st
    | Events.Escape escaped ->
      Priv.escape ctx.node (Analyses.ask_of_ctx ctx) ctx.global ctx.sideg st escaped
    | _ ->
      st

  let sync ctx reason =
    (* After the solver is finished, store the results (for later comparison) *)
    if !GU.postsolving then begin
      let keep_local = GobConfig.get_bool "ana.apron.invariant.local" in
      let keep_global = GobConfig.get_bool "ana.apron.invariant.global" in

      (* filter variables *)
      let var_filter v = match V.find_metadata v with
        | Some (Global _) -> keep_global
        | Some Local -> keep_local
        | _ -> false
      in
      let st = keep_filter ctx.local.apr var_filter in

      let old_value = RH.find_default results ctx.node (AD.bot ()) in
      let new_value = AD.join old_value st in
      RH.replace results ctx.node new_value;
    end;
    Priv.sync (Analyses.ask_of_ctx ctx) ctx.global ctx.sideg ctx.local (reason :> [`Normal | `Join | `Return | `Init | `Thread])
>>>>>>> master

  let init marshal =
    Priv.init ()

  let name () = "apron"

  let store_data file =
    let convert (m: AD.t PCU.RH.t): OctApron.t PCU.RH.t =
      let convert_single (a: AD.t): OctApron.t =
        if Oct.manager_is_oct AD.Man.mgr then
          Oct.Abstract1.to_oct a
        else
          let generator = AD.to_lincons_array a in
          OctApron.of_lincons_array generator
      in
      PCU.RH.map (fun _ -> convert_single) m
    in
    let post_process m =
      let m = Stats.time "convert" convert m in
      PCU.RH.map (fun _ v -> OctApron.marshal v) m
    in
    let results = post_process results in
    let name = name () ^ "(domain: " ^ (AD.name ()) ^ ", privatization: " ^ (Priv.name ()) ^ (if GobConfig.get_bool "ana.apron.threshold_widening" then ", th" else "" ) ^ ")" in
    let results: ApronPrecCompareUtil.dump = {marshalled = results; name } in
    Serialize.marshal results file

    let finalize () =
      let file = GobConfig.get_string "exp.apron.prec-dump" in
      if file <> "" then begin
        Printf.printf "exp.apron.prec-dump is potentially costly (for domains other than octagons), do not use for performance data!\n";
        Stats.time "apron.prec-dump" store_data (Fpath.v file)
      end;
      Priv.finalize ()
end

let spec_module: (module MCPSpec) Lazy.t =
  lazy (
    let module Man = (val ApronDomain.get_manager ()) in
    let module AD = ApronDomain.D2 (Man) in
    let module RD: RelationDomain.RD =
    struct
      module Var = ApronDomain.Var
      module V = ApronDomain.V
      include AD
    end in
    let module Priv = (val RelationPriv.get_priv ()) in
    let module Spec = ExtendedSpecFunctor (RD) (Priv) in
    (module Spec)
  )


let get_spec (): (module MCPSpec) =
  Lazy.force spec_module

let after_config () =
  let module Spec = (val get_spec ()) in
  MCP.register_analysis (module Spec : MCPSpec);
  GobConfig.set_string "ana.path_sens[+]"  (Spec.name ())

let _ =
  AfterConfig.register after_config
