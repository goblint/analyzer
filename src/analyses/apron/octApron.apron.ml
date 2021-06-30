(** Analysis using Apron for integer variables. *)

open Prelude.Ana
open Analyses
open OctApronDomain

module M = Messages

module SpecFunctor (Priv: OctApronPriv.S) : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let name () = "octApron"

  module D = OctApronComponents (Priv.D)
  module G = Priv.G
  module C = D

  module AD = OctApronDomain.D2

  let should_join = Priv.should_join

  let val_of x = x
  let context x = if GobConfig.get_bool "exp.full-context" then x else D.bot ()

  let rec get_vnames_list exp = match exp with
    | Lval lval ->
      let lhost, offset = lval in
      begin match lhost with
        | Var vinfo -> [vinfo.vname]
        | _ -> []
      end
    | UnOp (unop, e, typ) -> get_vnames_list e
    | BinOp (binop, e1, e2, typ) -> (get_vnames_list e1) @ (get_vnames_list e2)
    | AddrOf lval -> get_vnames_list (Lval(lval))
    | CastE(_, e) -> get_vnames_list e
    | _ -> []

  let invalidate oct (exps: exp list) =
    if M.tracing && exps <> [] then M.tracel "invalidate" "Will invalidate expressions [%a]\n" (d_list ", " d_plainexp) exps;
    let l = List.flatten (List.map get_vnames_list exps) in
    AD.forget_vars oct (List.map Var.of_string l)

  let threadenter ctx lval f args =
    let st = ctx.local in
    (* TODO: HACK: Simulate enter_multithreaded for first entering thread to publish global inits before analyzing thread.
       Otherwise thread is analyzed with no global inits, reading globals gives bot, which turns into top, which might get published...
       sync `Thread doesn't help us here, it's not specific to entering multithreaded mode.
       EnterMultithreaded events only execute after threadenter and threadspawn. *)
    if not (ThreadFlag.is_multi (Analyses.ask_of_ctx ctx)) then
      ignore (Priv.enter_multithreaded (Analyses.ask_of_ctx ctx) ctx.global ctx.sideg st);
    [Priv.threadenter (Analyses.ask_of_ctx ctx) ctx.global st]

  let threadspawn ctx lval f args fctx =
    let st = ctx.local in
    {st with oct = invalidate st.oct args}
  let exitstate  _ = { oct = AD.bot (); priv = Priv.startstate () }
  let startstate _ = { oct = AD.bot (); priv = Priv.startstate () }


  let enter ctx r f args =
    let st = ctx.local in
    let ctx_f_locals =
      if MyCFG.Node.equal ctx.prev_node MyCFG.dummy_node then
        [] (* enter_with in Control *)
      else
        (MyCFG.getFun ctx.prev_node).slocals
    in
    let f = Cilfacade.getdec f in
    if M.tracing then M.tracel "combine" "apron enter f: %a\n" d_varinfo f.svar;
    if M.tracing then M.tracel "combine" "apron enter formals: %a\n" (d_list "," d_varinfo) f.sformals;
    if M.tracing then M.tracel "combine" "apron enter local: %a\n" D.pretty ctx.local;
    let is = AD.typesort f.sformals in
    let is = is @ List.map (fun x -> Var.of_string ((Var.to_string x)^"'")) is in (* TODO: use arg_var *)
    let newd = AD.add_vars st.oct is in
    let formargs = Goblintutil.zip f.sformals args in
    let arith_formals = List.filter (fun (x,_) -> isIntegralType x.vtype) formargs in
    List.iter (fun (v, e) -> AD.assign_exp_with newd (V.arg v) e) arith_formals;
    List.iter  (fun (v,_)   -> AD.assign_var_with newd (V.local v) (V.arg v)) arith_formals;
    AD.remove_vars_with newd (List.map V.local ctx_f_locals); (* remove caller locals, keep everything else (globals, global invariant)*)
    (* TODO: also remove arg_vars/primed *)
    if M.tracing then M.tracel "combine" "apron enter newd: %a\n" AD.pretty newd;
    [st, {st with oct = newd}]


  let combine ctx r fe f args fc fun_st =
    let st = ctx.local in
    let f = Cilfacade.getdec f in
    if M.tracing then M.tracel "combine" "apron f: %a\n" d_varinfo f.svar;
    if M.tracing then M.tracel "combine" "apron formals: %a\n" (d_list "," d_varinfo) f.sformals;
    let nd = AD.copy st.oct in
    let fis = AD.vars st.oct in
    let nd' = AD.add_vars fun_st.oct fis in
    if M.tracing then M.tracel "combine" "apron args: %a\n" (d_list "," d_exp) args;
    let formargs = Goblintutil.zip f.sformals args in
    let arith_formals = List.filter (fun (x,_) -> isIntegralType x.vtype) formargs in
    List.iter (fun (v, e) -> AD.substitute_exp_with nd' (V.arg v) e) arith_formals;
    let vars = List.map (fun (x,_) -> V.arg x) arith_formals in
    if M.tracing then M.tracel "combine" "apron remove vars: %a\n" (docList (fun v -> Pretty.text (Var.to_string v))) vars;
    AD.remove_vars_with nd' vars;
    begin match r with
      (* TODO: match conditions with assign *)
      (* TODO: support assign to global *)
      | Some (Var v, NoOffset) when isIntegralType v.vtype && (not v.vglob) ->
        let v_var = V.local v in
        AD.forget_vars_with nd [v_var];
        AD.forget_vars_with nd' [v_var];
        AD.substitute_var_with nd' V.return v_var;
      | _ ->
        ()
    end;
    AD.remove_vars_with nd' [V.return];
    (* remove globals from local, use invariants from function *)
    (* TODO: keep locals+formals instead to handle priv vars *)
    AD.remove_filter_with nd (fun var ->
        match V.find_metadata var with
        | Some (Global _) -> true
        | _ -> false
      );
    let r = A.unify Man.mgr nd nd' in
    if M.tracing then M.tracel "combine" "apron unifying %a %a = %a\n" AD.pretty nd AD.pretty nd' AD.pretty r;
    {fun_st with oct = r}

  let special ctx r f args =
    let st = ctx.local in
    begin
      match LibraryFunctions.classify f.vname args with
      | `Assert expression -> st
      | `Unknown "printf" -> st
      | `Unknown "__goblint_check" -> st
      | `Unknown "__goblint_commit" -> st
      | `Unknown "__goblint_assert" -> st
      | `Malloc size ->
        begin match r with
          | Some lv ->
            {st with oct = AD.forget_vars st.oct [V.local f]}
          | _ -> st
        end
      | `Calloc (n, size) ->
        begin match r with
          | Some lv ->
            {st with oct = AD.forget_vars st.oct [V.local f]}
          | _ -> st
        end
      | `ThreadJoin (id,ret_var) ->
        {st with oct = invalidate st.oct [ret_var]}
      | `ThreadCreate _ -> st
      | _ ->
        begin
          let st =
            match LibraryFunctions.get_invalidate_action f.vname with
            | Some fnc -> {st with oct = invalidate st.oct (fnc `Write  args)}
            | None -> {st with oct = AD.top_env (A.env st.oct)}
          in
          st
        end
    end

  let branch ctx e b =
    let st = ctx.local in
    let res = AD.assert_inv st.oct e (not b) in
    if AD.is_bot_env res then raise Deadcode;
    {st with oct = res}

  let return ctx e f =
    let st = ctx.local in
    let return_type = match f.svar.vtype with
      | TFun (return_type, _, _, _) -> return_type
      | _ -> assert false
    in
    let nd =
      if isIntegralType return_type then (
        let nd = AD.add_vars st.oct [V.return] in
        begin match e with
          | Some e ->
            AD.assign_exp_with nd V.return e
          | None ->
            ()
        end;
        nd
      )
      else
        AD.copy st.oct
    in
    let vars = List.filter (fun x -> isIntegralType x.vtype) (f.slocals @ f.sformals) in
    let vars = List.map V.local vars in
    AD.remove_vars_with nd vars;
    {st with oct = nd}

  let body ctx f =
    let st = ctx.local in
    let vars = AD.typesort f.slocals in
    {st with oct = AD.add_vars st.oct vars}

  let read_global ask getg st g x =
    if ThreadFlag.is_multi ask then
      Priv.read_global ask getg st g x
    else (
      let oct = st.oct in
      let g_var = V.global g in
      let x_var = V.local x in
      let oct' = AD.add_vars oct [g_var] in
      let oct' = AD.assign_var oct' x_var g_var in
      oct'
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
    let oct = AD.add_vars st.oct (List.map V.local (VH.values v_ins |> List.of_enum)) in (* add temporary g#in-s *)
    let oct' = VH.fold (fun v v_in oct ->
        if M.tracing then M.trace "apron" "read_global %a %a\n" d_varinfo v d_varinfo v_in;
        read_global ask getg {st with oct = oct} v v_in (* g#in = g; *)
      ) v_ins oct
    in
    (oct', e', v_ins)

  let assign_with_globals ask getg st v e =
    let (oct', e', v_ins) = read_globals_to_locals ask getg st e in
    if M.tracing then M.trace "apron" "AD.assign %a %a\n" d_varinfo v d_exp e';
    let oct' = AD.assign_var_handling_underflow_overflow oct' v e' in (* x = e; *)
    let oct'' = AD.remove_vars oct' (List.map V.local (VH.values v_ins |> List.of_enum)) in (* remove temporary g#in-s *)
    {st with oct = oct''}

  let write_global ask getg sideg st g x =
    if ThreadFlag.is_multi ask then
      Priv.write_global ask getg sideg st g x
    else (
      let oct = st.oct in
      let g_var = V.global g in
      let x_var = V.local x in
      let oct' = AD.add_vars oct [g_var] in
      let oct' = AD.assign_var oct' g_var x_var in
      {st with oct = oct'}
    )

  let assign ctx (lv:lval) e =
    let st = ctx.local in
    match lv with
    (* Lvals which are numbers, have no offset and their address wasn't taken *)
    | Var v, NoOffset when isIntegralType v.vtype && not v.vaddrof && not (!GU.global_initialization && e = MyCFG.unknown_exp) -> (* ignore extern inits because there's no body before assign, so octagon env is empty... *)
      if M.tracing then M.traceli "apron" "assign %a = %a\n" d_lval lv d_exp e;
      let ask = Analyses.ask_of_ctx ctx in
      let r =
        if not v.vglob then
          assign_with_globals ask ctx.global st v e
        else (
          let v_out = Goblintutil.create_var @@ makeVarinfo false (v.vname ^ "#out") v.vtype in (* temporary local g#out for global g *)
          let st = {st with oct = AD.add_vars st.oct [V.local v_out]} in (* add temporary g#out *)
          let st' = assign_with_globals ask ctx.global st v_out e in (* g#out = e; *)
          if M.tracing then M.trace "apron" "write_global %a %a\n" d_varinfo v d_varinfo v_out;
          let st' = write_global ask ctx.global ctx.sideg st' v v_out in (* g = g#out; *)
          let oct'' = AD.remove_vars st'.oct [V.local v_out] in (* remove temporary g#out *)
          {st' with oct = oct''}
        )
      in
      if M.tracing then M.traceu "apron" "-> %a\n" D.pretty r;
      r
    (* Ignoring all other assigns *)
    | _ -> st

  let check_assert_with_globals ctx e =
    let st = ctx.local in
    let (oct', e', _) = read_globals_to_locals (Analyses.ask_of_ctx ctx) ctx.global st e in
    AD.check_assert e' oct'
    (* no need to remove g#in-s *)

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    let st = ctx.local in
    match q with
    | Assert e ->
      begin match check_assert_with_globals ctx e with
        | `Top -> `Top
        | `True -> `Lifted true
        | `False -> `Lifted false
        | _ -> `Bot
      end
    | EvalInt e ->
      begin
        match AD.get_int_val_for_cil_exp st.oct e with
        | Some i -> ID.of_int i
        | _ -> `Top
      end
    | _ -> Result.top q

  let event ctx e octx =
    let st = ctx.local in
    match e with
    | Events.Lock addr when ThreadFlag.is_multi (Analyses.ask_of_ctx ctx) -> (* TODO: is this condition sound? *)
      Priv.lock (Analyses.ask_of_ctx octx) octx.global st addr
    | Events.Unlock addr when ThreadFlag.is_multi (Analyses.ask_of_ctx ctx) -> (* TODO: is this condition sound? *)
      Priv.unlock (Analyses.ask_of_ctx octx) octx.global octx.sideg st addr
    (* No need to handle escape because escaped variables are always referenced but this analysis only considers unreferenced variables. *)
    | Events.EnterMultiThreaded ->
      Priv.enter_multithreaded (Analyses.ask_of_ctx octx) octx.global octx.sideg st
    | _ ->
      st

  let sync ctx reason =
    Priv.sync (Analyses.ask_of_ctx ctx) ctx.global ctx.sideg ctx.local (reason :> [`Normal | `Join | `Return | `Init | `Thread])

  let init () =
    Priv.init ()

  let finalize () =
    Priv.finalize ()
end


let spec_module: (module MCPSpec) Lazy.t =
  lazy (
    let module Priv = (val OctApronPriv.get_priv ()) in
    let module Spec = SpecFunctor (Priv) in
    (module Spec)
  )

let get_spec (): (module MCPSpec) =
  Lazy.force spec_module

let after_config () =
  let module Spec = (val get_spec ()) in
  MCP.register_analysis (module Spec : MCPSpec)

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
