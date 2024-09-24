open GoblintCil
open Analyses
open GobConfig


module LongjmpLifter (S: Spec): Spec =
struct
  include S

  let name () = "Longjmp (" ^ S.name () ^ ")"

  module V =
  struct
    include Printable.Either3Conf (struct let expand1 = false let expand2 = true let expand3 = true end) (S.V) (Printable.Prod (Node) (C)) (Printable.Prod (CilType.Fundec) (C))
    let name () = "longjmp"
    let s x = `Left x
    let longjmpto x = `Middle x
    let longjmpret x = `Right x
    let is_write_only = function
      | `Left x -> S.V.is_write_only x
      | _ -> false
  end

  module G =
  struct
    include Lattice.Lift2 (S.G) (S.D)

    let s = function
      | `Bot -> S.G.bot ()
      | `Lifted1 x -> x
      | _ -> failwith "LongjmpLifter.s"
    let local = function
      | `Bot -> S.D.bot ()
      | `Lifted2 x -> x
      | _ -> failwith "LongjmpLifter.local"
    let create_s s = `Lifted1 s
    let create_local local = `Lifted2 local

    let printXml f = function
      | `Lifted1 x -> S.G.printXml f x
      | `Lifted2 x -> BatPrintf.fprintf f "<analysis name=\"longjmp\"><value>%a</value></analysis>" S.D.printXml x
      | x -> BatPrintf.fprintf f "<analysis name=\"longjmp-lifter\">%a</analysis>" printXml x
  end

  let conv (ctx: (_, G.t, _, V.t) ctx): (_, S.G.t, _, S.V.t) ctx =
    { ctx with
      global = (fun v -> G.s (ctx.global (V.s v)));
      sideg = (fun v g -> ctx.sideg (V.s v) (G.create_s g));
    }

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | WarnGlobal g ->
      let g: V.t = Obj.obj g in
      begin match g with
        | `Left g ->
          S.query (conv ctx) (WarnGlobal (Obj.repr g))
        | _ ->
          Queries.Result.top q
      end
    | InvariantGlobal g ->
      let g: V.t = Obj.obj g in
      begin match g with
        | `Left g ->
          S.query (conv ctx) (InvariantGlobal (Obj.repr g))
        | _ ->
          Queries.Result.top q
      end
    | IterSysVars (vq, vf) ->
      (* vars for S *)
      let vf' x = vf (Obj.repr (V.s (Obj.obj x))) in
      S.query (conv ctx) (IterSysVars (vq, vf'));
      (* TODO: vars? *)
    | _ ->
      S.query (conv ctx) q


  let branch ctx = S.branch (conv ctx)
  let assign ctx = S.assign (conv ctx)
  let vdecl ctx = S.vdecl (conv ctx)
  let enter ctx = S.enter (conv ctx)
  let paths_as_set ctx = S.paths_as_set (conv ctx)
  let body ctx = S.body (conv ctx)
  let return ctx = S.return (conv ctx)
  let context ctx = S.context (conv ctx)

  let combine_env ctx lv e f args fc fd f_ask =
    let conv_ctx = conv ctx in
    let current_fundec = Node.find_fundec ctx.node in
    let handle_longjmp (cd, fc, longfd) =
      (* This is called per-path. *)
      let rec cd_ctx =
        { conv_ctx with
          ask = (fun (type a) (q: a Queries.t) -> S.query cd_ctx q);
          local = cd;
        }
      in
      let longfd_ctx =
        (* Inner scope to prevent unsynced longfd_ctx from being used. *)
        (* Extra sync like with normal combine. *)
        let rec sync_ctx =
          { conv_ctx with
            ask = (fun (type a) (q: a Queries.t) -> S.query sync_ctx q);
            local = longfd;
            prev_node = Function f;
          }
        in
        let synced = S.sync sync_ctx `Join in
        let rec longfd_ctx =
          { sync_ctx with
            ask = (fun (type a) (q: a Queries.t) -> S.query longfd_ctx q);
            local = synced;
          }
        in
        longfd_ctx
      in
      let combined = lazy ( (* does not depend on target, do at most once *)
        (* Globals are non-problematic here, as they are always carried around without any issues! *)
        (* A combine call is mostly needed to ensure locals have appropriate values. *)
        (* Using f from called function on purpose here! Needed? *)
        S.combine_env cd_ctx None e f args fc longfd_ctx.local (Analyses.ask_of_ctx longfd_ctx) (* no lval because longjmp return skips return value assignment *)
      )
      in
      let returned = lazy ( (* does not depend on target, do at most once *)
        let rec combined_ctx =
          { cd_ctx with
            ask = (fun (type a) (q: a Queries.t) -> S.query combined_ctx q);
            local = Lazy.force combined;
          }
        in
        S.return combined_ctx None current_fundec
      )
      in
      let (active_targets, _) = longfd_ctx.ask ActiveJumpBuf in
      let valid_targets = cd_ctx.ask ValidLongJmp in
      let handle_target target = match target with
        | JmpBufDomain.BufferEntryOrTop.AllTargets -> () (* The warning is already emitted at the point where the longjmp happens *)
        | Target (target_node, target_context) ->
          let target_fundec = Node.find_fundec target_node in
          if CilType.Fundec.equal target_fundec current_fundec && ControlSpecC.equal target_context (ctx.control_context ()) then (
            if M.tracing then Messages.tracel "longjmp" "Fun: Potentially from same context, side-effect to %a" Node.pretty target_node;
            ctx.sideg (V.longjmpto (target_node, ctx.context ())) (G.create_local (Lazy.force combined))
            (* No need to propagate this outwards here, the set of valid longjumps is part of the context, we can never have the same context setting the longjmp multiple times *)
          )
          (* Appropriate setjmp is not in current function & current context *)
          else if JmpBufDomain.JmpBufSet.mem target valid_targets then
            ctx.sideg (V.longjmpret (current_fundec, ctx.context ())) (G.create_local (Lazy.force returned))
          else
            (* It actually is not handled here but was propagated here spuriously, we already warned at the location where this issue is caused *)
            (* As the validlongjumps inside the callee is a a superset of the ones inside the caller *)
            ()
      in
      JmpBufDomain.JmpBufSet.iter handle_target active_targets
    in
    if M.tracing then M.tracel "longjmp" "longfd getg %a" CilType.Fundec.pretty f;
    let longfd = G.local (ctx.global (V.longjmpret (f, Option.get fc))) in
    if M.tracing then M.tracel "longjmp" "longfd %a" D.pretty longfd;
    if not (D.is_bot longfd) then
      handle_longjmp (ctx.local, fc, longfd);
    S.combine_env (conv_ctx) lv e f args fc fd f_ask

  let combine_assign ctx lv e f args fc fd f_ask =
    S.combine_assign (conv ctx) lv e f args fc fd f_ask

  let special ctx lv f args =
    let conv_ctx = conv ctx in
    match (LibraryFunctions.find f).special args with
    | Setjmp {env} ->
      (* Handling of returning for the first time *)
      let normal_return = S.special conv_ctx lv f args in
      let jmp_return = G.local (ctx.global (V.longjmpto (ctx.prev_node, ctx.context ()))) in
      if S.D.is_bot jmp_return then
        normal_return
      else (
        let rec jmp_ctx =
          { conv_ctx with
            ask = (fun (type a) (q: a Queries.t) -> S.query jmp_ctx q);
            local = jmp_return;
          }
        in
        let longjmped = S.event jmp_ctx (Events.Longjmped {lval=lv}) jmp_ctx in
        S.D.join normal_return longjmped
      )
    | Longjmp {env; value} ->
      let current_fundec = Node.find_fundec ctx.node in
      let handle_path path = (
        let rec path_ctx =
          { conv_ctx with
            ask = (fun (type a) (q: a Queries.t) -> S.query path_ctx q);
            local = path;
          }
        in
        let specialed = lazy ( (* does not depend on target, do at most once *)
          S.special path_ctx lv f args
        )
        in
        let returned = lazy ( (* does not depend on target, do at most once *)
          let rec specialed_ctx =
            { path_ctx with
              ask = (fun (type a) (q: a Queries.t) -> S.query specialed_ctx q);
              local = Lazy.force specialed;
            }
          in
          S.return specialed_ctx None current_fundec
        )
        in
        (* Eval `env` again to avoid having to construct bespoke ctx to ask *)
        let targets = path_ctx.ask (EvalJumpBuf env) in
        let valid_targets = path_ctx.ask ValidLongJmp in
        if M.tracing then Messages.tracel "longjmp" "Jumping to %a" JmpBufDomain.JmpBufSet.pretty targets;
        let handle_target target = match target with
          | JmpBufDomain.BufferEntryOrTop.AllTargets ->
            M.warn ~category:Imprecise "Longjmp to potentially invalid target, as contents of buffer %a may be unknown! (imprecision due to heap?)" d_exp env;
            M.msg_final Error ~category:Unsound ~tags:[Category Imprecise; Category Call] "Longjmp to unknown target ignored"
          | Target (target_node, target_context) ->
            let target_fundec = Node.find_fundec target_node in
            if CilType.Fundec.equal target_fundec current_fundec && ControlSpecC.equal target_context (ctx.control_context ()) then (
              if M.tracing then Messages.tracel "longjmp" "Potentially from same context, side-effect to %a" Node.pretty target_node;
              ctx.sideg (V.longjmpto (target_node, ctx.context ())) (G.create_local (Lazy.force specialed))
            )
            else if JmpBufDomain.JmpBufSet.mem target valid_targets then (
              if M.tracing then Messages.tracel "longjmp" "Longjmp to somewhere else, side-effect to %i" (S.C.hash (ctx.context ()));
              ctx.sideg (V.longjmpret (current_fundec, ctx.context ())) (G.create_local (Lazy.force returned))
            )
            else
              M.warn ~category:(Behavior (Undefined Other)) "Longjmp to potentially invalid target! (Target %a in Function %a which may have already returned or is in a different thread)" Node.pretty target_node CilType.Fundec.pretty target_fundec
        in
        if JmpBufDomain.JmpBufSet.is_empty targets then
          M.warn ~category:(Behavior (Undefined Other)) "Longjmp to potentially invalid target (%a is bot?!)" d_exp env
        else
          JmpBufDomain.JmpBufSet.iter handle_target targets
      )
      in
      List.iter handle_path (S.paths_as_set conv_ctx);
      if !AnalysisState.should_warn && List.mem "termination" @@ get_string_list "ana.activated" then (
        AnalysisState.svcomp_may_not_terminate := true;
        M.warn ~category:Termination "The program might not terminate! (Longjmp)"
      );
      S.D.bot ()
    | _ -> S.special conv_ctx lv f args
  let threadenter ctx = S.threadenter (conv ctx)
  let threadspawn ctx ~multiple lv f args fctx = S.threadspawn (conv ctx) ~multiple lv f args (conv fctx)
  let sync ctx = S.sync (conv ctx)
  let skip ctx = S.skip (conv ctx)
  let asm ctx = S.asm (conv ctx)
  let event ctx e octx = S.event (conv ctx) e (conv octx)
end
