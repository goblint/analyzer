(** Analysis using Apron for integer variables. *)
open Prelude.Ana
open Analyses
open RelationDomain

module M = Messages

module SpecFunctor (CPriv: ApronPriv.S) (RD: RelationDomain.RD) (PCU: RelPrecCompareUtil.Util) =
struct
  include Analyses.DefaultSpec

  module AD = RD.D2

  let name () = AD.name ()

  module Priv = CPriv (RD)
  module D = RelationDomain.RelComponent (AD) (Priv.D)
  module G = Priv.G
  module C = D
  module V = Priv.V
  module RV = RD.V
  module PCU = PCU(AD)

  open AD

  let results = PCU.RH.create 103

  let should_join = Priv.should_join

  let context fd x =
    if ContextUtil.should_keep ~isAttr:GobContext ~keepOption:"ana.apron.context" ~removeAttr:"apron.no-context" ~keepAttr:"apron.context" fd then
      x
    else
      D.bot () (* just like startstate, heterogeneous AD.bot () means top over empty set of variables *)

  let exitstate  _ = { RelationDomain.apr = AD.bot (); RelationDomain.priv = Priv.startstate () }
  let startstate _ = { RelationDomain.apr = AD.bot (); RelationDomain.priv = Priv.startstate () }

  (* Functions for manipulating globals as temporary locals. *)

  let read_global ask getg st g x =
    if ThreadFlag.is_multi ask then
      Priv.read_global ask getg st g x
    else (
      let apr = st.RelationDomain.apr in
      let g_var = RV.global g in
      let x_var = RV.local x in
      let apr' = AD.add_vars apr [g_var] in
      let apr' = AD.assign_var apr' x_var g_var in
      apr'
    )

  module VH = BatHashtbl.Make (Basetype.Variables)

  let read_globals_to_locals ask getg st e =
    let v_ins = VH.create 10 in
    let visitor = object
      inherit nopCilVisitor
      method! vvrbl (v: varinfo) =
        if v.vglob then (
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
    let apr = AD.add_vars st.RelationDomain.apr (List.map RV.local (VH.values v_ins |> List.of_enum)) in (* add temporary g#in-s *)
    let apr' = VH.fold (fun v v_in apr ->
        if M.tracing then M.trace "apron" "read_global %a %a\n" d_varinfo v d_varinfo v_in;
        read_global ask getg {st with RelationDomain.apr = apr} v v_in (* g#in = g; *)
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
    let apr'' = AD.remove_vars apr' (List.map RV.local (VH.values v_ins |> List.of_enum)) in (* remove temporary g#in-s *)
    apr''

  let write_global ask getg sideg st g x =
    if ThreadFlag.is_multi ask then
      Priv.write_global ask getg sideg st g x
    else (
      let apr = st.RelationDomain.apr in
      let g_var = RV.global g in
      let x_var = RV.local x in
      let apr' = AD.add_vars apr [g_var] in
      let apr' = AD.assign_var apr' g_var x_var in
      {st with RelationDomain.apr = apr'}
    )

  let assign_to_global_wrapper ask getg sideg st lv f =
    match lv with
    (* Lvals which are numbers, have no offset and their address wasn't taken *)
    (* This means that variables of which multiple copies may be reachable via pointers are also also excluded (they have their address taken) *)
    (* and no special handling for them is required (https://github.com/goblint/analyzer/pull/310) *)
    | (Var v, NoOffset) when AD.varinfo_tracked v ->
      if not v.vglob then
        {st with apr = f st v}
      else (
        let v_out = Goblintutil.create_var @@ makeVarinfo false (v.vname ^ "#out") v.vtype in (* temporary local g#out for global g *)
        let st = {st with apr = AD.add_vars st.apr [RV.local v_out]} in (* add temporary g#out *)
        let st' = {st with apr = f st v_out} in (* g#out = e; *)
        if M.tracing then M.trace "apron" "write_global %a %a\n" d_varinfo v d_varinfo v_out;
        let st' = write_global ask getg sideg st' v v_out in (* g = g#out; *)
        let apr'' = AD.remove_vars st'.apr [RV.local v_out] in (* remove temporary g#out *)
        {st' with apr = apr''}
      )
    (* Ignoring all other assigns *)
    | _ ->
      st

  let no_overflow ctx exp =
    let ik = Cilfacade.get_ikind_exp exp in
    if not (Cil.isSigned ik) then false else
    if GobConfig.get_string "sem.int.signed_overflow" = "assume_none" then true else
      let eval_int ctx exp =
        match ctx.ask (Queries.EvalInt exp) with
        | x when Queries.ID.is_int x -> Queries.ID.to_int x
        | _ -> None
      in
      let int_min, int_max = IntDomain.Size.range ik in
      match IntOps.BigIntOps.to_int64 int_min, IntOps.BigIntOps.to_int64 int_max with
      | exception Failure _ -> false
      | int_min, int_max ->
        let min = eval_int ctx (BinOp (Lt, kinteger64 ik int_min, exp , intType)) in
        let max = eval_int ctx (BinOp (Lt,  exp, kinteger64 ik int_max, intType)) in
        match min, max with
        | Some (x), _ when x = (IntOps.BigIntOps.of_int 1) -> true
        | _, Some (x) when x = (IntOps.BigIntOps.of_int 1) -> true
        | _, _ -> false


  let assert_type_bounds apr x ctx =
    assert (AD.varinfo_tracked x);
    let ik = Cilfacade.get_ikind x.vtype in
    if not (IntDomain.should_ignore_overflow ik) then ( (* don't add type bounds for signed when assume_none *)
      let (type_min, type_max) = IntDomain.Size.range ik in
      (* TODO: don't go through CIL exp? *)
      let e1 = (BinOp (Le, Lval (Cil.var x), (Cil.kintegerCilint ik (Cilint.cilint_of_big_int type_max)), intType)) in
      let e2 = (BinOp (Ge, Lval (Cil.var x), (Cil.kintegerCilint ik (Cilint.cilint_of_big_int type_min)), intType)) in
      let apr = AD.assert_inv apr e1 false (no_overflow ctx e1) in
      let apr = AD.assert_inv apr e2 false (no_overflow ctx e2) in
      apr
    )
    else
      apr


  (* Basic transfer functions. *)

  let assign ctx (lv:lval) e =
    let st = ctx.local in
    if !GU.global_initialization && e = MyCFG.unknown_exp then
      st (* ignore extern inits because there's no body before assign, so the apron env is empty... *)
    else (
      if M.tracing then M.traceli "apron" "assign %a = %a\n" d_lval lv d_exp e;
      let ask = Analyses.ask_of_ctx ctx in
      let r = assign_to_global_wrapper ask ctx.global ctx.sideg st lv (fun st v ->
          assign_from_globals_wrapper ask ctx.global st e (fun apr' e' ->
              AD.assign_exp apr' (RV.local v) e' (no_overflow ctx e')
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
        AD.assert_inv apr' e' (not b) (no_overflow ctx e')
      )
    in
    if AD.is_bot_env res then raise Deadcode;
    {st with apr = res}


  (* Function call transfer functions. *)

  let enter ctx r f args =
    let st = ctx.local in
    if M.tracing then M.tracel "combine" "apron enter f: %a\n" d_varinfo f.svar;
    if M.tracing then M.tracel "combine" "apron enter formals: %a\n" (d_list "," d_varinfo) f.sformals;
    if M.tracing then M.tracel "combine" "apron enter local: %a\n" D.pretty ctx.local;
    let arg_assigns =
      GobList.combine_short f.sformals args (* TODO: is it right to ignore missing formals/args? *)
      |> List.filter (fun (x, _) -> AD.varinfo_tracked x)
      |> List.map (Tuple2.map1 RV.arg)
    in
    let arg_vars = List.map fst arg_assigns in
    let new_apr = AD.add_vars st.apr arg_vars in
    (* AD.assign_exp_parallel_with new_apr arg_assigns; (* doesn't need to be parallel since exps aren't arg vars directly *) *)
    (* TODO: parallel version of assign_from_globals_wrapper? *)
    let ask = Analyses.ask_of_ctx ctx in
    let new_apr = List.fold_left (fun new_apr (var, e) ->
        assign_from_globals_wrapper ask ctx.global {st with apr = new_apr} e (fun apr' e' ->
            AD.assign_exp apr' var e' (no_overflow ctx e')
          )
      ) new_apr arg_assigns
    in
    let filtered_new_apr = AD.remove_filter new_apr (fun var ->
        match RV.find_metadata var with
        | Some Local -> true (* remove caller locals *)
        | Some Arg when not (List.mem_cmp RD.Var.compare var arg_vars) -> true (* remove caller args, but keep just added args *)
        | _ -> false (* keep everything else (just added args, globals, global privs) *)
      )
    in
    if M.tracing then M.tracel "combine" "apron enter newd: %a\n" AD.pretty filtered_new_apr;
    [st, {st with apr = filtered_new_apr}]

  let body ctx f =
    let st = ctx.local in
    let formals = List.filter AD.varinfo_tracked f.sformals in
    let locals = List.filter AD.varinfo_tracked f.slocals in
    let new_apr = AD.add_vars st.apr (List.map RV.local (formals @ locals)) in
    (* TODO: do this after local_assigns? *)
    let new_apr = List.fold_left (fun new_apr x ->
        assert_type_bounds new_apr x ctx
      ) new_apr (formals @ locals)
    in
    let local_assigns = List.map (fun x -> (RV.local x, RV.arg x)) formals in
    let assigned_new_apr = AD.assign_var_parallel new_apr local_assigns in (* doesn't need to be parallel since arg vars aren't local vars *)
    {st with apr = assigned_new_apr}

  let return ctx e f =
    let st = ctx.local in
    let ask = Analyses.ask_of_ctx ctx in
    let new_apr =
      if AD.type_tracked (Cilfacade.fundec_return_type f) then (
        let apr' = AD.add_vars st.apr [RV.return] in
        match e with
        | Some e ->
          assign_from_globals_wrapper (Analyses.ask_of_ctx ctx) ctx.global {st with apr = apr'} e (fun apr' e' ->
              AD.assign_exp apr' RV.return e' (no_overflow ctx e')
            )
        | None ->
          apr' (* leaves V.return unconstrained *)
      )
      else
        st.apr
    in
    let local_vars =
      f.sformals @ f.slocals
      |> List.filter AD.varinfo_tracked
      |> List.map RV.local
    in
    let rem_new_apr = AD.remove_vars new_apr local_vars in
    let st' = {st with apr = rem_new_apr} in
    begin match ThreadId.get_current ask with
      | `Lifted tid when ThreadReturn.is_current ask ->
        Priv.thread_return ask ctx.global ctx.sideg tid st'
      | _ ->
        st'
    end

  let combine ctx r fe f args fc fun_st =
    let st = ctx.local in
    if M.tracing then M.tracel "combine" "apron f: %a\n" d_varinfo f.svar;
    if M.tracing then M.tracel "combine" "apron formals: %a\n" (d_list "," d_varinfo) f.sformals;
    if M.tracing then M.tracel "combine" "apron args: %a\n" (d_list "," d_exp) args;
    let new_fun_apr = AD.add_vars fun_st.apr (AD.vars st.apr) in
    let arg_substitutes =
      GobList.combine_short f.sformals args (* TODO: is it right to ignore missing formals/args? *)
      |> List.filter (fun (x, _) -> AD.varinfo_tracked x)
      |> List.map (Tuple2.map1 RV.arg)
    in
    (* AD.substitute_exp_parallel_with new_fun_apr arg_substitutes; (* doesn't need to be parallel since exps aren't arg vars directly *) *)
    (* TODO: parallel version of assign_from_globals_wrapper? *)
    let ask = Analyses.ask_of_ctx ctx in
    let new_fun_apr = List.fold_left (fun new_fun_apr (var, e) ->
        assign_from_globals_wrapper ask ctx.global {st with apr = new_fun_apr} e (fun apr' e' ->
            (* not an assign, but still works? *)
            AD.substitute_exp apr' var e' (no_overflow ctx e')
          )
      ) new_fun_apr arg_substitutes
    in
    let arg_vars = List.map fst arg_substitutes in
    if M.tracing then M.tracel "combine" "apron remove vars: %a\n" (docList (fun v -> Pretty.text (RD.Var.to_string v))) arg_vars;
    let new_fun_apr = AD.remove_vars new_fun_apr arg_vars in (* fine to remove arg vars that also exist in caller because unify from new_apr adds them back with proper constraints *)
    let new_apr = AD.keep_filter st.apr (fun var ->
        match RV.find_metadata var with
        | Some Local -> true (* keep caller locals *)
        | Some Arg -> true (* keep caller args *)
        | _ -> false (* remove everything else (globals, global privs) *)
      )
    in
    let unify_apr = AD.unify new_apr new_fun_apr in
    if M.tracing then M.tracel "combine" "apron unifying %a %a = %a\n" AD.pretty new_apr AD.pretty new_fun_apr AD.pretty unify_apr;
    let unify_st = {fun_st with apr = unify_apr} in
    if AD.type_tracked (Cilfacade.fundec_return_type f) then (
      let unify_st' = match r with
        | Some lv ->
          assign_to_global_wrapper (Analyses.ask_of_ctx ctx) ctx.global ctx.sideg unify_st lv (fun st v ->
              AD.assign_var st.apr (RV.local v) RV.return
            )
        | None ->
          unify_st
      in
      let new_unify_st_apr = AD.remove_vars unify_st'.apr [RV.return] in
      {RelationDomain.apr = new_unify_st_apr; RelationDomain.priv = unify_st'.priv}
    )
    else
      unify_st

  let special ctx r f args =
    let ask = Analyses.ask_of_ctx ctx in
    let invalidate_one st lv =
      assign_to_global_wrapper ask ctx.global ctx.sideg st lv (fun st v ->
          let apr' = AD.forget_vars st.apr [RV.local v] in
          assert_type_bounds apr' v ctx (* re-establish type bounds after forget *)
        )
    in
    let st = ctx.local in
    match LibraryFunctions.classify f.vname args with
    (* TODO: assert handling from https://github.com/goblint/analyzer/pull/278 *)
    | `Assert expression -> st
    | `Unknown "__goblint_check" -> st
    | `Unknown "__goblint_commit" -> st
    | `Unknown "__goblint_assert" -> st
    | `ThreadJoin (id,retvar) ->
      (* nothing to invalidate as only arguments that have their AddrOf taken may be invalidated *)
      (
        let st' = Priv.thread_join ask ctx.global id st in
        match r with
        | Some lv -> invalidate_one st' lv
        | None -> st'
      )
    | _ ->
      let ask = Analyses.ask_of_ctx ctx in
      let invalidate_one st lv =
        assign_to_global_wrapper ask ctx.global ctx.sideg st lv (fun st v ->
            let apr' = AD.forget_vars st.apr [RV.local v] in
            assert_type_bounds apr' v ctx (* re-establish type bounds after forget *)
          )
      in
      let st' = match LibraryFunctions.get_invalidate_action f.vname with
        | Some fnc -> st (* nothing to do because only AddrOf arguments may be invalidated *)
        | None ->
          if GobConfig.get_bool "sem.unknown_function.invalidate.globals" then (
            let globals = foldGlobals !Cilfacade.current_file (fun acc global ->
                match global with
                | GVar (vi, _, _) when not (BaseUtil.is_static vi) ->
                  (Var vi, NoOffset) :: acc
                (* TODO: what about GVarDecl? *)
                | _ -> acc
              ) []
            in
            List.fold_left invalidate_one st globals
          )
          else
            st
      in
      (* invalidate lval if present *)
      match r with
      | Some lv -> invalidate_one st' lv
      | None -> st'


 let query ctx (type a) (q: a Queries.t): a Queries.result =
      let no_overflow ctx' exp' = if not (GobConfig.get_string "sem.int.signed_overflow" = "assume_none") then false else no_overflow ctx' exp' in (*ToDo Overflow handling...*)
      let open Queries in
      let st = ctx.local in
      let eval_int e no_ov =
        read_from_globals_wrapper
          (Analyses.ask_of_ctx ctx)
          ctx.global st e
          (fun apr' e' -> AD.eval_int apr' e' no_ov)
      in
      match q with
      | EvalInt e ->
        if M.tracing then M.traceli "evalint" "apron query %a\n" d_exp e;
        let r = eval_int e false in
        if M.tracing then M.traceu "evalint" "apron query %a -> %a\n" d_exp e ID.pretty r;
        r
      | Queries.MustBeEqual (exp1,exp2) ->
        let exp = (BinOp (Cil.Eq, exp1, exp2, TInt (IInt, []))) in
        let is_eq = eval_int exp (no_overflow ctx exp) in
        Option.default false (ID.to_bool is_eq)
      | Queries.MayBeEqual (exp1,exp2) ->
        let exp = (BinOp (Cil.Eq, exp1, exp2, TInt (IInt, []))) in
        let is_neq = eval_int exp (no_overflow ctx exp) in
        Option.default true (ID.to_bool is_neq)
      | Queries.MayBeLess (exp1, exp2) ->
        let exp = (BinOp (Cil.Lt, exp1, exp2, TInt (IInt, []))) in
        let is_lt = eval_int exp (no_overflow ctx exp) in
        Option.default true (ID.to_bool is_lt)
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
        |> List.map RV.arg
      in
      let new_apr = AD.add_vars st'.apr arg_vars in
      [{st' with apr = new_apr}]
    | exception Not_found ->
      (* Unknown functions *)
      (* TODO: do something like base? *)
      failwith "apron.threadenter: unknown function"

  let threadspawn ctx lval f args fctx =
    ctx.local

  let event ctx e aprx =
    let st = ctx.local in
    match e with
    | Events.Lock addr when ThreadFlag.is_multi (Analyses.ask_of_ctx ctx) -> (* TODO: is this condition sound? *)
      Priv.lock (Analyses.ask_of_ctx aprx) aprx.global st addr
    | Events.Unlock addr when ThreadFlag.is_multi (Analyses.ask_of_ctx ctx) -> (* TODO: is this condition sound? *)
      Priv.unlock (Analyses.ask_of_ctx aprx) aprx.global aprx.sideg st addr
    (* No need to handle escape because escaped variables are always referenced but this analysis only considers unreferenced variables. *)
    | Events.EnterMultiThreaded ->
      Priv.enter_multithreaded (Analyses.ask_of_ctx aprx) aprx.global aprx.sideg st
    | _ ->
      st

  let sync ctx reason =
    (* After the solver is finished, store the results (for later comparison) *)
    if !GU.postsolving then begin
      let old_value = PCU.RH.find_default results ctx.node (AD.bot ()) in
      let new_value = AD.join old_value ctx.local.apr in
      PCU.RH.replace results ctx.node new_value;
    end;
    Priv.sync (Analyses.ask_of_ctx ctx) ctx.global ctx.sideg ctx.local (reason :> [`Normal | `Join | `Return | `Init | `Thread])
end