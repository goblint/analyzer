open Prelude.Ana
open Analyses
open RelationDomain

module M = Messages

module SpecFunctor (CPriv: RelationPriv.S) (RD: RelationDomain.RD) (PCU: RelationPrecCompareUtil.Util) =
struct
  include Analyses.DefaultSpec

  module Priv = CPriv (RD)
  module D = RelationDomain.RelComponents (RD) (Priv.D)
  module G = Priv.G
  module C = D
  module V =
  struct
    include Priv.V
    include StdV
  end

  module RV = RD.V

  module PCU = PCU(RD)

  (* Result map used for comparison of results *)
  let results = PCU.RH.create 103 (*ToDO Remove this*)

  let should_join = Priv.should_join

  let context fd x =
    if ContextUtil.should_keep ~isAttr:GobContext ~keepOption:"ana.relation.context" ~removeAttr:"relation.no-context" ~keepAttr:"relation.context" fd then
      x
    else
      D.bot () (* just like startstate, heterogeneous RD.bot () means top over empty set of variables *)

  let exitstate  _ = { RelationDomain.rel = RD.bot (); RelationDomain.priv = Priv.startstate () }
  let startstate _ = { RelationDomain.rel = RD.bot (); RelationDomain.priv = Priv.startstate () }

  (* Functions for manipulating globals as temporary locals. *)

  let read_global ask getg st g x =
    if ThreadFlag.is_multi ask then
      Priv.read_global ask getg st g x
    else (
      let rel = st.RelationDomain.rel in
      let g_var = RV.global g in
      let x_var = RV.local x in
      let rel' = RD.add_vars rel [g_var] in
      let rel' = RD.assign_var rel' x_var g_var in
      rel'
    )

  module VH = BatHashtbl.Make (Basetype.Variables)

  let read_globals_to_locals (ask:Queries.ask) getg st e =
    let v_ins = VH.create 10 in
    let visitor = object
      inherit nopCilVisitor
      method! vlval = function
        | (Var v, NoOffset) when v.vglob || ThreadEscape.has_escaped ask v ->
          let v_in =
            if VH.mem v_ins v then
              VH.find v_ins v
            else
              let v_in = Goblintutil.create_var @@ makeVarinfo false (v.vname ^ "#in") v.vtype in (* temporary local g#in for global g *)
              VH.replace v_ins v v_in;
              v_in
          in
          ChangeTo (Var v_in, NoOffset)
        | _ ->
          SkipChildren
    end
    in
    let e' = visitCilExpr visitor e in
    let rel = RD.add_vars st.RelationDomain.rel (List.map RV.local (VH.values v_ins |> List.of_enum)) in (* add temporary g#in-s *)
    let rel' = VH.fold (fun v v_in rel ->
        if M.tracing then M.trace "relation" "read_global %a %a\n" d_varinfo v d_varinfo v_in;
        read_global ask getg {st with RelationDomain.rel = rel} v v_in (* g#in = g; *)
      ) v_ins rel
    in
    (rel', e', v_ins)

  let read_globals_to_locals_inv (ask: Queries.ask) getg st vs =
    let v_ins_inv = VH.create (List.length vs) in
    List.iter (fun v ->
        let v_in = Goblintutil.create_var @@ makeVarinfo false (v.vname ^ "#in") v.vtype in (* temporary local g#in for global g *)
        VH.replace v_ins_inv v_in v;
      ) vs;
    let rel = RD.add_vars st.rel (List.map RV.local (VH.keys v_ins_inv |> List.of_enum)) in (* add temporary g#in-s *)
    let rel' = VH.fold (fun v_in v rel ->
        read_global ask getg {st with rel} v v_in (* g#in = g; *)
      ) v_ins_inv rel
    in
    let visitor_inv = object
      inherit nopCilVisitor
      method! vvrbl (v: varinfo) =
        match VH.find_option v_ins_inv v with
        | Some v' -> ChangeTo v'
        | None -> SkipChildren
    end
    in
    let e_inv e = visitCilExpr visitor_inv e in
    (rel', e_inv, v_ins_inv)

  let read_from_globals_wrapper ask getg st e f =
    let (rel', e', _) = read_globals_to_locals ask getg st e in
    f rel' e' (* no need to remove g#in-s *)

  let assign_from_globals_wrapper ask getg st e f =
    let (rel', e', v_ins) = read_globals_to_locals ask getg st e in
    if M.tracing then M.trace "relation" "assign_from_globals_wrapper %a\n" d_exp e';
    let rel' = f rel' e' in (* x = e; *)
    let rel'' = RD.remove_vars rel' (List.map RV.local (VH.values v_ins |> List.of_enum)) in (* remove temporary g#in-s *)
    rel''

  let write_global ask getg sideg st g x =
    if ThreadFlag.is_multi ask then
      Priv.write_global ask getg sideg st g x
    else (
      let rel = st.RelationDomain.rel in
      let g_var = RV.global g in
      let x_var = RV.local x in
      let rel' = RD.add_vars rel [g_var] in
      let rel' = RD.assign_var rel' g_var x_var in
      {st with RelationDomain.rel = rel'}
    )

  let rec assign_to_global_wrapper (ask:Queries.ask) getg sideg st lv f =
    match lv with
    | (Var v, NoOffset) when RD.Tracked.varinfo_tracked v ->
      if not v.vglob && not (ThreadEscape.has_escaped ask v) then (
        if ask.f (Queries.IsMultiple v) then
          {st with rel = RD.join (f st v) st.rel}
        else
          {st with rel = f st v}
      )
      else (
        let v_out = Goblintutil.create_var @@ makeVarinfo false (v.vname ^ "#out") v.vtype in (* temporary local g#out for global g *)
        v_out.vattr <- v.vattr; (*copy the attributes because the tracking may depend on them. Otherwise an assertion fails *)
        let st = {st with rel = RD.add_vars st.rel [RV.local v_out]} in (* add temporary g#out *)
        let st' = {st with rel = f st v_out} in (* g#out = e; *)
        if M.tracing then M.trace "relation" "write_global %a %a\n" d_varinfo v d_varinfo v_out;
        let st' = write_global ask getg sideg st' v v_out in (* g = g#out; *)
        let rel'' = RD.remove_vars st'.rel [RV.local v_out] in (* remove temporary g#out *)
        {st' with rel = rel''}
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

  let no_overflow ctx exp =
    match Cilfacade.get_ikind_exp exp with
     | exception Invalid_argument _ -> false
     | exception Cilfacade.TypeOfError _ -> false
     | ik ->
    if not (Cil.isSigned ik) || GobConfig.get_string "sem.int.signed_overflow" = "assume_wraparound" then false else
    if GobConfig.get_string "sem.int.signed_overflow" = "assume_none" then true else
      let eval_int ctx exp =
        match ctx.ask (Queries.EvalInt exp) with
        | `Lifted x when IntDomain.IntDomTuple.to_int x <> None -> IntDomain.IntDomTuple.to_int x
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

  let no_overflow ctx exp =
    let res = no_overflow ctx exp in
    if M.tracing then M.tracel "no_ov" "no_ov %b exp: %s\n" res
        (Pretty.sprint ~width:1 (Cil.printExp Cil.defaultCilPrinter () exp)); res


  let assert_type_bounds rel x ctx =
    assert (RD.Tracked.varinfo_tracked x);
    match Cilfacade.get_ikind x.vtype with
    | ik when not (IntDomain.should_ignore_overflow ik) -> (* don't add type bounds for signed when assume_none *)
      let (type_min, type_max) = IntDomain.Size.range ik in
      (* TODO: don't go through CIL exp? *)
      let e1 = (BinOp (Le, Lval (Cil.var x), (Cil.kintegerCilint ik (Cilint.cilint_of_big_int type_max)), intType)) in
      let e2 = (BinOp (Ge, Lval (Cil.var x), (Cil.kintegerCilint ik (Cilint.cilint_of_big_int type_min)), intType)) in
      let rel = RD.assert_inv rel e1 false (lazy(no_overflow ctx e1)) in
      let rel = RD.assert_inv rel e2 false (lazy(no_overflow ctx e2)) in
      rel
    | _
    | exception Invalid_argument _ ->
      rel

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
      st (* ignore extern inits because there's no body before assign, so env is empty... *)
    else (
      let simplified_e = replace_deref_exps ctx.ask e in
      if M.tracing then M.traceli "relation" "assign %a = %a (simplified to %a)\n" d_lval lv  d_exp e d_exp simplified_e;
      let ask = Analyses.ask_of_ctx ctx in
      let r = assign_to_global_wrapper ask ctx.global ctx.sideg st lv (fun st v ->
          assign_from_globals_wrapper ask ctx.global st simplified_e (fun apr' e' ->
              if M.tracing then M.traceli "relation" "assign inner %a = %a (%a)\n" d_varinfo v d_exp e' d_plainexp e';
              if M.tracing then M.trace "relation" "st: %a\n" RD.pretty apr';
              let r = RD.assign_exp apr' (RV.local v) e' (lazy(no_overflow ctx e')) in
              if M.tracing then M.traceu "relation" "-> %a\n" RD.pretty r;
              r
            )
        )
      in
      if M.tracing then M.traceu "relation" "-> %a\n" D.pretty r;
      r
    )

  let branch ctx e b =
    let st = ctx.local in
    let res = assign_from_globals_wrapper (Analyses.ask_of_ctx ctx) ctx.global st e (fun rel' e' ->
        (* not an assign, but must remove g#in-s still *)
        RD.assert_inv rel' e' (not b) (lazy(no_overflow ctx e'))
      )
    in
    if RD.is_bot_env res then raise Deadcode;
    {st with rel = res}


  (* Function call transfer functions. *)
  let any_local_reachable fundec reachable_from_args =
    let locals = fundec.sformals @ fundec.slocals in
    let locals_id = List.map (fun v -> v.vid) locals in
    Queries.LS.exists (fun (v',_) -> List.mem v'.vid locals_id && RD.Tracked.varinfo_tracked v') reachable_from_args

  let pass_to_callee fundec any_local_reachable var =
    (* TODO: currently, we pass all locals of the caller to the callee, provided one of them is reachbale to preserve relationality *)
    (* there should be smarter ways to do this, e.g. by keeping track of which values are written etc. ... *)
    (* Also, a local *)
    let vname = RD.Var.to_string var in
    let locals = fundec.sformals @ fundec.slocals in
    match List.find_opt (fun v -> RelVM.var_name (Local v) = vname) locals with (* TODO: optimize *)
    (* match List.find_opt (fun v -> RV.local_name v = vname) locals with *)
    | None -> true
    | Some v -> any_local_reachable

  let enter ctx r f args =
    let fundec = Node.find_fundec ctx.node in
    let st = ctx.local in
    if M.tracing then M.tracel "combine" "relation enter f: %a\n" d_varinfo f.svar;
    if M.tracing then M.tracel "combine" "relation enter formals: %a\n" (d_list "," d_varinfo) f.sformals;
    if M.tracing then M.tracel "combine" "relation enter local: %a\n" D.pretty ctx.local;
    let arg_assigns =
      GobList.combine_short f.sformals args (* TODO: is it right to ignore missing formals/args? *)
      |> List.filter (fun (x, _) -> RD.Tracked.varinfo_tracked x)
      |> List.map (Tuple2.map1 RV.arg)
    in
    let reachable_from_args = List.fold (fun ls e -> Queries.LS.join ls (ctx.ask (ReachableFrom e))) (Queries.LS.empty ()) args in
    let arg_vars = List.map fst arg_assigns in
    let new_rel = RD.add_vars st.rel arg_vars in
    (* RD.assign_exp_parallel_with new_rel arg_assigns; (* doesn't need to be parallel since exps aren't arg vars directly *) *)
    (* TODO: parallel version of assign_from_globals_wrapper? *)
    let ask = Analyses.ask_of_ctx ctx in
    let new_rel = List.fold_left (fun new_rel (var, e) ->
        assign_from_globals_wrapper ask ctx.global {st with rel = new_rel} e (fun rel' e' ->
            RD.assign_exp rel' var e' (lazy(no_overflow ctx e'))
          )
      ) new_rel arg_assigns
    in
    let any_local_reachable = any_local_reachable fundec reachable_from_args in
    let filtered_new_rel = RD.remove_filter_pt_with new_rel (fun var ->
        match RV.find_metadata var with
        | Some (Local _) when not (pass_to_callee fundec any_local_reachable var) -> true (* remove caller locals provided they are unreachable *)
        | Some (Arg _) when not (List.mem_cmp RD.Var.compare var arg_vars) -> true (* remove caller args, but keep just added args *)
        | _ -> false (* keep everything else (just added args, globals, global privs) *)
      )
    in
    if M.tracing then M.tracel "combine" "relation enter newd: %a\n" RD.pretty filtered_new_rel;
    [st, {st with rel = filtered_new_rel}]

  let body ctx f =
    let st = ctx.local in
    let formals = List.filter RD.Tracked.varinfo_tracked f.sformals in
    let locals = List.filter RD.Tracked.varinfo_tracked f.slocals in
    let new_rel = RD.add_vars st.rel (List.map RV.local (formals @ locals)) in
    (* TODO: do this after local_assigns? *)
    let new_rel = List.fold_left (fun new_rel x ->
        assert_type_bounds new_rel x ctx
      ) new_rel (formals @ locals)
    in
    let local_assigns = List.map (fun x -> (RV.local x, RV.arg x)) formals in
    let assigned_new_rel = RD.assign_var_parallel_pt_with new_rel local_assigns in (* doesn't need to be parallel since arg vars aren't local vars *)
    {st with rel = assigned_new_rel}

  let return ctx e f =
    let st = ctx.local in
    let ask = Analyses.ask_of_ctx ctx in
    let new_rel =
      if RD.Tracked.type_tracked (Cilfacade.fundec_return_type f) then (
        let rel' = RD.add_vars st.rel [RV.return] in
        match e with
        | Some e ->
          assign_from_globals_wrapper (Analyses.ask_of_ctx ctx) ctx.global {st with rel = rel'} e (fun rel' e' ->
              RD.assign_exp rel' RV.return e' (lazy(no_overflow ctx e'))
            )
        | None ->
          rel' (* leaves V.return unconstrained *)
      )
      else
        RD.copy_pt st.rel
    in
    let local_vars =
      f.sformals @ f.slocals
      |> List.filter RD.Tracked.varinfo_tracked
      |> List.map RV.local
    in
    let rem_new_rel = RD.remove_vars_pt_with new_rel local_vars in
    let st' = {st with rel = rem_new_rel} in
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
    if M.tracing then M.tracel "combine" "relation f: %a\n" d_varinfo f.svar;
    if M.tracing then M.tracel "combine" "relation formals: %a\n" (d_list "," d_varinfo) f.sformals;
    if M.tracing then M.tracel "combine" "relation args: %a\n" (d_list "," d_exp) args;
    let new_fun_rel = RD.add_vars fun_st.rel (RD.vars st.rel) in
    let arg_substitutes =
      GobList.combine_short f.sformals args (* TODO: is it right to ignore missing formals/args? *)
      (* Do not do replacement for actuals whose value may be modified after the call *)
      |> List.filter (fun (x, e) -> RD.Tracked.varinfo_tracked x && List.for_all (fun v -> not (Queries.LS.exists (fun (v',_) -> v'.vid = v.vid) reachable_from_args)) (Basetype.CilExp.get_vars e))
      |> List.map (Tuple2.map1 RV.arg)
    in
    (* RD.substitute_exp_parallel_with new_fun_rel arg_substitutes; (* doesn't need to be parallel since exps aren't arg vars directly *) *)
    (* TODO: parallel version of assign_from_globals_wrapper? *)
    let ask = Analyses.ask_of_ctx ctx in
    let new_fun_rel = List.fold_left (fun new_fun_rel (var, e) ->
        assign_from_globals_wrapper ask ctx.global {st with rel = new_fun_rel} e (fun rel' e' ->
            (* not an assign, but still works? *)
            RD.substitute_exp rel' var e' (lazy(no_overflow ctx e'))
          )
      ) new_fun_rel arg_substitutes
    in
    let any_local_reachable = any_local_reachable fundec reachable_from_args in
    let arg_vars = f.sformals |> List.filter (RD.Tracked.varinfo_tracked) |> List.map RV.arg in
    if M.tracing then M.tracel "combine" "relation remove vars: %a\n" (docList (fun v -> Pretty.text (RD.Var.to_string v))) arg_vars;
    let new_fun_rel = RD.remove_vars_pt_with new_fun_rel arg_vars in (* fine to remove arg vars that also exist in caller because unify from new_rel adds them back with proper constraints *)
    let new_rel = RD.keep_filter st.rel (fun var ->
        match RV.find_metadata var with
        | Some (Local _) when not (pass_to_callee fundec any_local_reachable var) -> true (* keep caller locals, provided they were not passed to the function *)
        | Some (Arg _) -> true (* keep caller args *)
        | _ -> false (* remove everything else (globals, global privs, reachable things from the caller) *)
      )
    in
    let unify_rel = RD.unify new_rel new_fun_rel in (* TODO: unify_with *)
    if M.tracing then M.tracel "combine" "relation unifying %a %a = %a\n" RD.pretty new_rel RD.pretty new_fun_rel RD.pretty unify_rel;
    let unify_st = {fun_st with rel = unify_rel} in
    if RD.Tracked.type_tracked (Cilfacade.fundec_return_type f) then (
      let unify_st' = match r with
        | Some lv ->
          assign_to_global_wrapper (Analyses.ask_of_ctx ctx) ctx.global ctx.sideg unify_st lv (fun st v ->
              RD.assign_var st.rel (RV.local v) RV.return
            )
        | None ->
          unify_st
      in
      let new_unify_st_rel = RD.remove_vars_pt_with unify_st'.rel [RV.return] in
      {RelationDomain.rel = new_unify_st_rel; RelationDomain.priv = unify_st'.priv}
    )
    else
      unify_st

  let invalidate_one ask ctx st lv =
    assign_to_global_wrapper ask ctx.global ctx.sideg st lv (fun st v ->
        let rel' = RD.forget_vars st.rel [RV.local v] in
        assert_type_bounds rel' v ctx (* re-establish type bounds after forget *)
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
        RD.vars st.rel
        |> List.filter_map RV.to_cil_varinfo
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
          RD.assert_inv apr' e' false (lazy(no_overflow ctx e))
        )
      in
      if RD.is_bot_env res then raise Deadcode;
      {st with rel = res}

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

  let two_or_more_vars expr =
    (*Checks if a CIL expression contains more than two distinct variables*)
    let exception Two_Distinct_Vars in
    let rec find_vars ex =
      match ex with
      | Lval (Var v, NoOffset) when RD.Tracked.varinfo_tracked v -> Some v
      | Const _ -> None
      | exp -> begin match exp with
          | UnOp (_, e, _) -> find_vars e
          | BinOp (_, e1, e2, _) -> begin match find_vars e1, find_vars e2 with
              | Some v1, Some v2 ->  if v1 = v2 then Some v1 else raise Two_Distinct_Vars
              | Some x, None | None, Some x-> Some x
              | _, _ -> None end
          | CastE (_, e) -> find_vars e
          | _ -> None end
    in
    match find_vars expr with
    | exception Two_Distinct_Vars -> true
    | _ -> false


  let query_invariant ctx context =
    let keep_local = GobConfig.get_bool "ana.relation.invariant.local" in
    let keep_global = GobConfig.get_bool "ana.relation.invariant.global" in
    let one_var = GobConfig.get_bool "ana.relation.invariant.one-var" in

    let ask = Analyses.ask_of_ctx ctx in
    let scope = Node.find_fundec ctx.node in

    let (apr, e_inv) =
      if ThreadFlag.is_multi ask then (
        let priv_vars =
          if keep_global then
            Priv.invariant_vars ask ctx.global ctx.local
          else
            [] (* avoid pointless queries *)
        in
        let (rel, e_inv, v_ins_inv) = read_globals_to_locals_inv ask ctx.global ctx.local priv_vars in
        (* filter variables *)
        let var_filter v = match RV.find_metadata v with
          | Some (Local v) ->
            if VH.mem v_ins_inv v then
              keep_global
            else
              keep_local
          | _ -> false
        in
        let rel = RD.keep_filter rel var_filter in
        (rel, e_inv)
      )
      else (
        (* filter variables *)
        let var_filter v = match RV.find_metadata v with
          | Some (Global _) -> keep_global
          | Some (Local _) -> keep_local
          | _ -> false
        in
        let apr = RD.keep_filter ctx.local.rel var_filter in
        (apr, Fun.id)
      )
    in
    RD.invariant ~scope apr
    |> List.enum
    |> Enum.filter_map (fun lincons1 ->
        (* filter one-vars *)
        (* if one_var || Apron.Linexpr0.get_size lincons1.lincons0.linexpr0 >= 2 then
          CilOfApron.cil_exp_of_lincons1 lincons1
          |> Option.map e_inv
          |> Option.filter (fun exp -> not (InvariantCil.exp_contains_tmp exp) && InvariantCil.exp_is_in_scope scope exp)
        else
          None *)
        (* TODO: restore method from master *)
        let expr = RD.cons_to_cil_exp ~scope lincons1 in
        if Option.is_some expr && (one_var || two_or_more_vars @@ Option.get expr)  then
          Option.filter (fun exp -> not (InvariantCil.exp_contains_tmp exp) && InvariantCil.exp_is_in_scope scope exp) expr
        else
          None
      )
    |> Enum.fold (fun acc x -> Invariant.(acc && of_exp x)) Invariant.none

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let no_overflow ctx' exp' =
      IntDomain.should_ignore_overflow (Cilfacade.get_ikind_exp exp') in
    let open Queries in
    let st = ctx.local in
    let eval_int e no_ov =
      let esimple = replace_deref_exps ctx.ask e in
      read_from_globals_wrapper
        (Analyses.ask_of_ctx ctx)
        ctx.global st esimple
        (fun rel' e' -> RD.eval_int rel' e' no_ov)
    in
    match q with
    | EvalInt e ->
      if M.tracing then M.traceli "evalint" "relation query %a (%a)\n" d_exp e d_plainexp e;
      if M.tracing then M.trace "evalint" "relation st: %a\n" D.pretty ctx.local;
      let r = eval_int e (lazy(no_overflow ctx e)) in
      if M.tracing then M.traceu "evalint" "relation query %a -> %a\n" d_exp e ID.pretty r;
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
        |> List.filter RD.Tracked.varinfo_tracked
        |> List.map RV.arg
      in
      let new_rel = RD.add_vars st'.rel arg_vars in
      [{st' with rel = new_rel}]
    | exception Not_found ->
      (* Unknown functions *)
      (* TODO: do something like base? *)
      failwith "relation.threadenter: unknown function"

  let threadspawn ctx lval f args fctx =
    ctx.local

  let event ctx e relx =
    let st = ctx.local in
    match e with
    | Events.Lock (addr, _) when ThreadFlag.is_multi (Analyses.ask_of_ctx ctx) -> (* TODO: is this condition sound? *)
      Priv.lock (Analyses.ask_of_ctx relx) relx.global st addr
    | Events.Unlock addr when ThreadFlag.is_multi (Analyses.ask_of_ctx ctx) -> (* TODO: is this condition sound? *)
      if addr = UnknownPtr then
        M.info ~category:Unsound "Unknown mutex unlocked, relation privatization unsound"; (* TODO: something more sound *)
      Priv.unlock (Analyses.ask_of_ctx ctx) ctx.global ctx.sideg st addr
    | Events.EnterMultiThreaded ->
      Priv.enter_multithreaded (Analyses.ask_of_ctx relx) relx.global relx.sideg st
    | Events.Escape escaped ->
      Priv.escape ctx.node (Analyses.ask_of_ctx ctx) ctx.global ctx.sideg st escaped
    | Assert exp ->
      assert_fn ctx exp true
    | _ ->
      st

  let sync ctx reason =
    (* After the solver is finished, store the results (for later comparison) *)
    if !GU.postsolving then begin
      let keep_local = GobConfig.get_bool "ana.relation.invariant.local" in
      let keep_global = GobConfig.get_bool "ana.relation.invariant.global" in

      (* filter variables *)
      let var_filter v = match RV.find_metadata v with
        | Some (Global _) -> keep_global
        | Some (Local _) -> keep_local
        | _ -> false
      in
      let st = RD.keep_filter ctx.local.rel var_filter in
      let old_value = PCU.RH.find_default results ctx.node (RD.bot ()) in
      let new_value = RD.join old_value st in
      PCU.RH.replace results ctx.node new_value;
    end;
    Priv.sync (Analyses.ask_of_ctx ctx) ctx.global ctx.sideg ctx.local (reason :> [`Normal | `Join | `Return | `Init | `Thread])

  let init marshal =
    Priv.init ()

  let store_data file =
    let results = PCU.RH.map (fun _ v -> RD.marshal v) results in
    let name = RD.name () ^ "(domain: " ^ (RD.name ()) ^ ", privatization: " ^ (Priv.name ()) ^ (if GobConfig.get_bool "ana.relation.threshold_widening" then ", th" else "" ) ^ ")" in
    let results: PCU.dump = {marshalled = results; name } in
    Serialize.marshal results file

  let finalize () =
    let file = GobConfig.get_string "exp.relation.prec-dump" in
    if file <> "" then begin
      Printf.printf "exp.relation.prec-dump is potentially costly (for domains other than octagons), do not use for performance data!\n";
      Timing.wrap "relation.prec-dump" store_data (Fpath.v file)
    end;
    Priv.finalize ()

end
