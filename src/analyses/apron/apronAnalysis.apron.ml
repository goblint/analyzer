(** Analysis using Apron for integer variables. *)

open Prelude.Ana
open Analyses
open ApronDomain

module M = Messages

module SpecFunctor (AD: ApronDomain.S2) (Priv: ApronPriv.S) : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let name () = "apron"

  module Priv = Priv(AD)
  module D = ApronComponents (AD) (Priv.D)
  module G = Priv.G
  module C = D
  module V = Priv.V

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
        let st = {st with apr = AD.add_vars st.apr [V.local v_out]} in (* add temporary g#out *)
        let st' = {st with apr = f st v_out} in (* g#out = e; *)
        if M.tracing then M.trace "apron" "write_global %a %a\n" d_varinfo v d_varinfo v_out;
        let st' = write_global ask getg sideg st' v v_out in (* g = g#out; *)
        let apr'' = AD.remove_vars st'.apr [V.local v_out] in (* remove temporary g#out *)
        {st' with apr = apr''}
      )
    (* Ignoring all other assigns *)
    | _ ->
      st

  let assert_type_bounds apr x =
    assert (AD.varinfo_tracked x);
    let ik = Cilfacade.get_ikind x.vtype in
    if not (IntDomain.should_ignore_overflow ik) then ( (* don't add type bounds for signed when assume_none *)
      let (type_min, type_max) = IntDomain.Size.range ik in
      (* TODO: don't go through CIL exp? *)
      let apr = AD.assert_inv apr (BinOp (Le, Lval (Cil.var x), (Cil.kintegerCilint ik (Cilint.cilint_of_big_int type_max)), intType)) false in
      let apr = AD.assert_inv apr (BinOp (Ge, Lval (Cil.var x), (Cil.kintegerCilint ik (Cilint.cilint_of_big_int type_min)), intType)) false in
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

  let enter ctx r f args =
    let st = ctx.local in
    if M.tracing then M.tracel "combine" "apron enter f: %a\n" d_varinfo f.svar;
    if M.tracing then M.tracel "combine" "apron enter formals: %a\n" (d_list "," d_varinfo) f.sformals;
    if M.tracing then M.tracel "combine" "apron enter local: %a\n" D.pretty ctx.local;
    let arg_assigns =
      GobList.combine_short f.sformals args (* TODO: is it right to ignore missing formals/args? *)
      |> List.filter (fun (x, _) -> AD.varinfo_tracked x)
      |> List.map (Tuple2.map1 V.arg)
    in
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
    AD.remove_filter_with new_apr (fun var ->
        match V.find_metadata var with
        | Some Local -> true (* remove caller locals *)
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
    if M.tracing then M.tracel "combine" "apron f: %a\n" d_varinfo f.svar;
    if M.tracing then M.tracel "combine" "apron formals: %a\n" (d_list "," d_varinfo) f.sformals;
    if M.tracing then M.tracel "combine" "apron args: %a\n" (d_list "," d_exp) args;
    let new_fun_apr = AD.add_vars fun_st.apr (AD.vars st.apr) in
    let arg_substitutes =
      GobList.combine_short f.sformals args (* TODO: is it right to ignore missing formals/args? *)
      |> List.filter (fun (x, _) -> AD.varinfo_tracked x)
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
    let arg_vars = List.map fst arg_substitutes in
    if M.tracing then M.tracel "combine" "apron remove vars: %a\n" (docList (fun v -> Pretty.text (Var.to_string v))) arg_vars;
    AD.remove_vars_with new_fun_apr arg_vars; (* fine to remove arg vars that also exist in caller because unify from new_apr adds them back with proper constraints *)
    let new_apr = AD.keep_filter st.apr (fun var ->
        match V.find_metadata var with
        | Some Local -> true (* keep caller locals *)
        | Some Arg -> true (* keep caller args *)
        | _ -> false (* remove everything else (globals, global privs) *)
      )
    in
    let unify_apr = ApronDomain.A.unify Man.mgr new_apr new_fun_apr in (* TODO: unify_with *)
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

  let special ctx r f args =
    let ask = Analyses.ask_of_ctx ctx in
    let invalidate_one st lv =
      assign_to_global_wrapper ask ctx.global ctx.sideg st lv (fun st v ->
          let apr' = AD.forget_vars st.apr [V.local v] in
          assert_type_bounds apr' v (* re-establish type bounds after forget *)
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
            let apr' = AD.forget_vars st.apr [V.local v] in
            assert_type_bounds apr' v (* re-establish type bounds after forget *)
          )
      in
      let st' = match LibraryFunctions.get_invalidate_action f.vname with
        | Some fnc -> st (* nothing to do because only AddrOf arguments may be invalidated *)
        | None ->
          (* nothing to do for args because only AddrOf arguments may be invalidated *)
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
    let open Queries in
    let st = ctx.local in
    let eval_int e =
      read_from_globals_wrapper
        (Analyses.ask_of_ctx ctx)
        ctx.global st e
        (fun apr' e' -> AD.eval_int apr' e')
    in
    match q with
    | EvalInt e ->
      if M.tracing then M.traceli "evalint" "apron query %a (%a)\n" d_exp e d_plainexp e;
      if M.tracing then M.trace "evalint" "apron st: %a\n" D.pretty ctx.local;
      let r = eval_int e in
      if M.tracing then M.traceu "evalint" "apron query %a -> %a\n" d_exp e ID.pretty r;
      r
    | Queries.MustBeEqual (exp1,exp2) ->
      let exp = (BinOp (Cil.Eq, exp1, exp2, TInt (IInt, []))) in
      let is_eq = eval_int exp in
      Option.default false (ID.to_bool is_eq)
    | Queries.MayBeEqual (exp1,exp2) ->
      let exp = (BinOp (Cil.Eq, exp1, exp2, TInt (IInt, []))) in
      let is_neq = eval_int exp in
      Option.default true (ID.to_bool is_neq)
    | Queries.MayBeLess (exp1, exp2) ->
      let exp = (BinOp (Cil.Lt, exp1, exp2, TInt (IInt, []))) in
      let is_lt = eval_int exp in
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
    (* No need to handle escape because escaped variables are always referenced but this analysis only considers unreferenced variables. *)
    | Events.EnterMultiThreaded ->
      Priv.enter_multithreaded (Analyses.ask_of_ctx ctx) ctx.global ctx.sideg st
    | _ ->
      st

  let sync ctx reason =
    (* After the solver is finished, store the results (for later comparison) *)
    if !GU.postsolving then begin
      let old_value = RH.find_default results ctx.node (AD.bot ()) in
      let st = AD.keep_filter ctx.local.apr (fun v ->
          match V.find_metadata v with
          | Some (Global _) -> true
          | _ -> false
        )
      in
      let new_value = AD.join old_value st in
      RH.replace results ctx.node new_value;
    end;
    Priv.sync (Analyses.ask_of_ctx ctx) ctx.global ctx.sideg ctx.local (reason :> [`Normal | `Join | `Return | `Init | `Thread])

  let init marshal =
    Priv.init ()

  module OctApron = ApronPrecCompareUtil.OctagonD
  let store_data file =
    let convert (m: AD.t RH.t): OctApron.t RH.t =
      let convert_single (a: AD.t): OctApron.t =
        if Oct.manager_is_oct AD.Man.mgr then
          Oct.Abstract1.to_oct a
        else
          let generator = AD.to_lincons_array a in
          OctApron.of_lincons_array generator
      in
      RH.map (fun _ -> convert_single) m
    in
    let post_process m =
      let m = Stats.time "convert" convert m in
      RH.map (fun _ v -> OctApron.marshal v) m
    in
    let results = post_process results in
    let name = name () ^ "(domain: " ^ (AD.Man.name ()) ^ ", privatization: " ^ (Priv.name ()) ^ (if GobConfig.get_bool "ana.apron.threshold_widening" then ", th" else "" ) ^ ")" in
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
    let module Priv = (val ApronPriv.get_priv ()) in
    let module Spec = SpecFunctor (AD) (Priv) in
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


let () =
  Printexc.register_printer
    (function
      | Apron.Manager.Error e ->
        let () = Apron.Manager.print_exclog Format.str_formatter e in
        Some(Printf.sprintf "Apron.Manager.Error\n %s" (Format.flush_str_formatter ()))
      | _ -> None (* for other exceptions *)
    )
