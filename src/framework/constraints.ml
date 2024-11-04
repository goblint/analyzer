(** Construction of a {{!Analyses.MonSystem} constraint system} from an {{!Analyses.Spec} analysis specification} and {{!MyCFG.CfgBackward} CFGs}.
    Transformatons of analysis specifications as functors. *)

open Batteries
open GoblintCil
open MyCFG
open Analyses
open ConstrSys
open GobConfig


module type Increment =
sig
  val increment: increment_data option
end


(** The main point of this file---generating a [GlobConstrSys] from a [Spec]. *)
module FromSpec (S:Spec) (Cfg:CfgBidirSkip) (I: Increment)
  : sig
    include GlobConstrSys with module LVar = UnrollVarF (S.C)
                           and module GVar = GVarF (S.V)
                           and module D = S.D
                           and module G = GVarG (S.G) (S.C)
  end
=
struct
  type lv = MyCFG.node * (S.C.t * LoopCounts.t)
  (* type gv = varinfo *)
  type ld = S.D.t
  (* type gd = S.G.t *)
  module LVar = UnrollVarF (S.C)
  module GVar = GVarF (S.V)
  module D = S.D
  module G = GVarG (S.G) (S.C)

  (* Two global invariants:
     1. S.V -> S.G  --  used for Spec
     2. fundec -> set of S.C  --  used for IterSysVars Node *)

  let sync ctx =
    match ctx.prev_node, Cfg.prev ctx.prev_node with
    | _, _ :: _ :: _ -> (* Join in CFG. *)
      S.sync ctx `Join
    | FunctionEntry f, _ -> (* Function entry, also needs sync because partial contexts joined by solver, see 00-sanity/35-join-contexts. *)
      S.sync ctx (`JoinCall f)
    | _, _ -> S.sync ctx `Normal

  let side_context sideg f c =
    if !AnalysisState.postsolving then
      sideg (GVar.contexts f) (G.create_contexts (G.CSet.singleton c))

  let common_ctx (n,(c,l)) edge prev_node pval (getl:lv -> ld) sidel getg sideg : (D.t, S.G.t, S.C.t, S.V.t) ctx * D.t list ref * (lval option * varinfo * exp list * D.t * bool) list ref =
    let r = ref [] in
    let spawns = ref [] in
    (* now watch this ... *)
    let rec ctx =
      { ask     = (fun (type a) (q: a Queries.t) -> S.query ctx q)
      ; emit    = (fun _ -> failwith "emit outside MCP")
      ; node    = n
      ; prev_node = prev_node
      ; control_context = c |> Obj.obj
      ; context = c |> Obj.obj
      ; edge    = edge
      ; local   = pval
      ; global  = (fun g -> G.spec (getg (GVar.spec g)))
      ; spawn   = spawn
      ; split   = (fun (d:D.t) es -> assert (List.is_empty es); r := d::!r)
      ; sideg   = (fun g d -> sideg (GVar.spec g) (G.create_spec d))
      }
    and spawn ?(multiple=false) lval f args =
      (* TODO: adjust ctx node/edge? *)
      (* TODO: don't repeat for all paths that spawn same *)
      let ds = S.threadenter ~multiple ctx lval f args in
      List.iter (fun d ->
          spawns := (lval, f, args, d, multiple) :: !spawns;
          match Cilfacade.find_varinfo_fundec f with
          | fd ->
            let c = S.context ctx fd d in
            sidel (FunctionEntry fd, (c, l)) d;
            ignore (getl (Function fd, (c, l)))
          | exception Not_found ->
            (* unknown function *)
            M.error ~category:Imprecise ~tags:[Category Unsound] "Created a thread from unknown function %s" f.vname
            (* actual implementation (e.g. invalidation) is done by threadenter *)
        ) ds
    in
    (* ... nice, right! *)
    let pval = sync ctx in
    { ctx with local = pval }, r, spawns

  let rec bigsqcup = function
    | []    -> D.bot ()
    | [x]   -> x
    | x::xs -> D.join x (bigsqcup xs)

  let thread_spawns ctx d spawns =
    if List.is_empty spawns then
      d
    else
      let rec ctx' =
        { ctx with
          ask = (fun (type a) (q: a Queries.t) -> S.query ctx' q)
        ; local = d
        }
      in
      (* TODO: don't forget path dependencies *)
      let one_spawn (lval, f, args, fd, multiple) =
        let rec fctx =
          { ctx with
            ask = (fun (type a) (q: a Queries.t) -> S.query fctx q)
          ; local = fd
          }
        in
        S.threadspawn ctx' ~multiple lval f args fctx
      in
      bigsqcup (List.map one_spawn spawns)

  let common_join ctx d splits spawns =
    thread_spawns ctx (bigsqcup (d :: splits)) spawns

  let common_joins ctx ds splits spawns = common_join ctx (bigsqcup ds) splits spawns

  let tf_assign var edge prev_node lv e getl sidel getg sideg d =
    let ctx, r, spawns = common_ctx var edge prev_node d getl sidel getg sideg in
    let d = S.assign ctx lv e in (* Force transfer function to be evaluated before dereferencing in common_join argument. *)
    common_join ctx d !r !spawns

  let tf_vdecl var edge prev_node v getl sidel getg sideg d =
    let ctx, r, spawns = common_ctx var edge prev_node d getl sidel getg sideg in
    let d = S.vdecl ctx v in (* Force transfer function to be evaluated before dereferencing in common_join argument. *)
    common_join ctx d !r !spawns

  let normal_return r fd ctx sideg =
    let spawning_return = S.return ctx r fd in
    let nval = S.sync { ctx with local = spawning_return } `Return in
    nval

  let toplevel_kernel_return r fd ctx sideg =
    let st = if fd.svar.vname = MyCFG.dummy_func.svar.vname then ctx.local else S.return ctx r fd in
    let spawning_return = S.return {ctx with local = st} None MyCFG.dummy_func in
    let nval = S.sync { ctx with local = spawning_return } `Return in
    nval

  let tf_ret var edge prev_node ret fd getl sidel getg sideg d =
    let ctx, r, spawns = common_ctx var edge prev_node d getl sidel getg sideg in
    let d = (* Force transfer function to be evaluated before dereferencing in common_join argument. *)
      if (CilType.Fundec.equal fd MyCFG.dummy_func ||
          List.mem fd.svar.vname (get_string_list "mainfun")) &&
         get_bool "kernel"
      then toplevel_kernel_return ret fd ctx sideg
      else normal_return ret fd ctx sideg
    in
    common_join ctx d !r !spawns

  let tf_entry var edge prev_node fd getl sidel getg sideg d =
    (* Side effect function context here instead of at sidel to FunctionEntry,
       because otherwise context for main functions (entrystates) will be missing or pruned during postsolving. *)
    let c: unit -> S.C.t = fst (snd var) |> Obj.obj in
    side_context sideg fd (c ());
    let ctx, r, spawns = common_ctx var edge prev_node d getl sidel getg sideg in
    let d = S.body ctx fd in (* Force transfer function to be evaluated before dereferencing in common_join argument. *)
    common_join ctx d !r !spawns

  let tf_test var edge prev_node e tv getl sidel getg sideg d =
    let ctx, r, spawns = common_ctx var edge prev_node d getl sidel getg sideg in
    let d = S.branch ctx e tv in (* Force transfer function to be evaluated before dereferencing in common_join argument. *)
    common_join ctx d !r !spawns

  let tf_normal_call var ctx lv e (f:fundec) args getl sidel getg sideg =
    let combine (cd, fc, fd) =
      if M.tracing then M.traceli "combine" "local: %a" S.D.pretty cd;
      if M.tracing then M.trace "combine" "function: %a" S.D.pretty fd;
      let rec cd_ctx =
        { ctx with
          ask = (fun (type a) (q: a Queries.t) -> S.query cd_ctx q);
          local = cd;
        }
      in
      let fd_ctx =
        (* Inner scope to prevent unsynced fd_ctx from being used. *)
        (* Extra sync in case function has multiple returns.
           Each `Return sync is done before joining, so joined value may be unsound.
           Since sync is normally done before tf (in common_ctx), simulate it here for fd. *)
        (* TODO: don't do this extra sync here *)
        let rec sync_ctx =
          { ctx with
            ask = (fun (type a) (q: a Queries.t) -> S.query sync_ctx q);
            local = fd;
            prev_node = Function f;
          }
        in
        (* TODO: more accurate ctx? *)
        let synced = sync sync_ctx in
        let rec fd_ctx =
          { sync_ctx with
            ask = (fun (type a) (q: a Queries.t) -> S.query fd_ctx q);
            local = synced;
          }
        in
        fd_ctx
      in
      let r = List.fold_left (fun acc fd1 ->
          let rec fd1_ctx =
            { fd_ctx with
              ask = (fun (type a) (q: a Queries.t) -> S.query fd1_ctx q);
              local = fd1;
            }
          in
          let combine_enved = S.combine_env cd_ctx lv e f args fc fd1_ctx.local (Analyses.ask_of_ctx fd1_ctx) in
          let rec combine_assign_ctx =
            { cd_ctx with
              ask = (fun (type a) (q: a Queries.t) -> S.query combine_assign_ctx q);
              local = combine_enved;
            }
          in
          S.D.join acc (S.combine_assign combine_assign_ctx lv e f args fc fd1_ctx.local (Analyses.ask_of_ctx fd1_ctx))
        ) (S.D.bot ()) (S.paths_as_set fd_ctx)
      in
      if M.tracing then M.traceu "combine" "combined local: %a" S.D.pretty r;
      r
    in
    let paths = S.enter ctx lv f args in
    let paths = List.map (fun (c,v) -> (c, S.context ctx f v, v)) paths in
    List.iter (fun (c,fc,v) -> if not (S.D.is_bot v) then sidel (FunctionEntry f, (fc, snd (snd var))) v) paths;
    let paths = List.map (fun (c,fc,v) -> (c, fc, if S.D.is_bot v then v else getl (Function f, (fc, snd (snd var))))) paths in
    (* Don't filter bot paths, otherwise LongjmpLifter is not called. *)
    (* let paths = List.filter (fun (c,fc,v) -> not (D.is_bot v)) paths in *)
    let paths = List.map (Tuple3.map2 Option.some) paths in
    if M.tracing then M.traceli "combine" "combining";
    let paths = List.map combine paths in
    let r = List.fold_left D.join (D.bot ()) paths in
    if M.tracing then M.traceu "combine" "combined: %a" S.D.pretty r;
    r

  let tf_special_call ctx lv f args = S.special ctx lv f args

  let tf_proc var edge prev_node lv e args getl sidel getg sideg d =
    let ctx, r, spawns = common_ctx var edge prev_node d getl sidel getg sideg in
    let functions =
      match e with
      | Lval (Var v, NoOffset) ->
        (* Handle statically known function call directly.
           Allows deactivating base. *)
        [v]
      | _ ->
        (* Depends on base for query. *)
        let ad = ctx.ask (Queries.EvalFunvar e) in
        Queries.AD.to_var_may ad (* TODO: don't convert, handle UnknownPtr below *)
    in
    let one_function f =
      match f.vtype with
      | TFun (_, params, var_arg, _)  ->
        let arg_length = List.length args in
        let p_length = Option.map_default List.length 0 params in
        (* Check whether number of arguments fits. *)
        (* If params is None, the function or its parameters are not declared, so we still analyze the unknown function call. *)
        if Option.is_none params || p_length = arg_length || (var_arg && arg_length >= p_length) then
          begin Some (match Cilfacade.find_varinfo_fundec f with
              | fd when LibraryFunctions.use_special f.vname ->
                M.info ~category:Analyzer "Using special for defined function %s" f.vname;
                tf_special_call ctx lv f args
              | fd ->
                tf_normal_call var ctx lv e fd args getl sidel getg sideg
              | exception Not_found ->
                tf_special_call ctx lv f args)
          end
        else begin
          let geq = if var_arg then ">=" else "" in
          M.warn ~category:Unsound ~tags:[Category Call; CWE 685] "Potential call to function %a with wrong number of arguments (expected: %s%d, actual: %d). This call will be ignored." CilType.Varinfo.pretty f geq p_length arg_length;
          None
        end
      | _ ->
        M.warn ~category:Call "Something that is not a function (%a) is called." CilType.Varinfo.pretty f;
        None
    in
    let funs = List.filter_map one_function functions in
    if [] = funs && not (S.D.is_bot ctx.local) then begin
      M.msg_final Warning ~category:Unsound ~tags:[Category Call] "No suitable function to call";
      M.warn ~category:Unsound ~tags:[Category Call] "No suitable function to be called at call site. Continuing with state before call.";
      d (* because LevelSliceLifter *)
    end else
      common_joins ctx funs !r !spawns

  let tf_asm var edge prev_node getl sidel getg sideg d =
    let ctx, r, spawns = common_ctx var edge prev_node d getl sidel getg sideg in
    let d = S.asm ctx in (* Force transfer function to be evaluated before dereferencing in common_join argument. *)
    common_join ctx d !r !spawns

  let tf_skip var edge prev_node getl sidel getg sideg d =
    let ctx, r, spawns = common_ctx var edge prev_node d getl sidel getg sideg in
    let d = S.skip ctx in (* Force transfer function to be evaluated before dereferencing in common_join argument. *)
    common_join ctx d !r !spawns

  let find_loop_head = function
    | Statement s ->
      let n' = Statement (LoopUnrolling.find_original s) in
      let prevs = Cfg.prev n' in
      Stdlib.List.find_map (fun (edges, prev) ->
          let stmts = Cfg.skippedByEdge prev edges n' in
          Stdlib.List.find_map (fun s ->
              match s.GoblintCil.skind with
              | Loop (block, loc, _, cont_opt, break_opt) -> Some (block, loc, cont_opt, break_opt)
              | _ -> None
            ) stmts
        ) prevs
    | FunctionEntry _ | Function _ -> None

  exception Found
  class loop_end_visitor v = object
    inherit nopCilVisitor

    method! vstmt = function
      | s when Node.equal (Statement s) v -> raise Found
      | _ -> DoChildren
  end

  let from_loop_head (v,(c,l)) (edges, u) (block, loc, cont_opt, break_opt) max_iter =
    (* When exiting the loop, we calculate the state based on the loop head of the last unrolled iteration *)
    let cont_node = Option.map (fun x -> Statement (fst (CfgTools.find_real_stmt x))) cont_opt in
    let break_node = Option.map (fun x -> Statement (fst (CfgTools.find_real_stmt x))) break_opt in
    match break_node with
    | Some b when Node.equal b v ->
      Logs.info "Out of loop \n current node: %a\n loopcount before: %a\n prev node: %a\n continue: %a\n break node: %a" Node.pretty v LoopCounts.pretty l Node.pretty u (Pretty.docOpt (Node.pretty ())) cont_node (Pretty.docOpt (Node.pretty ())) break_node;
      List.init (max_iter + 1) (fun i -> (u, (c, LoopCounts.add u i l)))
    | _ ->
      Logs.info "Into loop\n current node: %a\n loopcount: %a\n prev node: %a\n continue: %a\n break node: %a" Node.pretty v LoopCounts.pretty l Node.pretty u (Pretty.docOpt (Node.pretty ())) cont_node (Pretty.docOpt (Node.pretty ())) break_node;
      [(u, (c, l))]

  exception WrongCase
  let to_loop_head (v,(c,l)) (edges, u) (block, loc, cont_opt, break_opt) max_iter =
    (* When calculating the state in the loop head *)
    try
      (* We either enter the loop for the first time *)
      ignore @@ visitCilBlock (new loop_end_visitor u) block;
      if LoopCounts.find v l = 0 then (
        let l = LoopCounts.remove v l in
        Logs.info "Loop entry edge\n current node: %a\n loopcount: %a\n prev node: %a" Node.pretty v LoopCounts.pretty l Node.pretty u;
        [(u, (c, l))]
      )
      else
        raise WrongCase
    with Found ->
      (* Or come from within the loop using a back edge *)
      if LoopCounts.find v l = 0 then
        raise WrongCase
      else (
        let l' = LoopCounts.add v (LoopCounts.find v l - 1) l in
        Logs.info "Back edge\n current node: %a\n loopcount: %a\n prev node: %a" Node.pretty v LoopCounts.pretty l Node.pretty u;
        if LoopCounts.find v l = max_iter then
          [(u, (c, l')); (u, (c, l))]
        else
          [(u, (c, l'))]
      )

  let unroll (v,(c,l)) (edges, u) max_iter : lv list =
    match find_loop_head u, find_loop_head v with
    | Some head_u, Some head_v ->
      (* TODO: is this correct? *)
      let open GobList.Syntax in
      let* (u',(c',l')) = from_loop_head (v,(c,l)) (edges, u) head_u max_iter in
      to_loop_head (v,(c',l')) (edges, u') head_v max_iter
    | Some head_u, _ ->
      from_loop_head (v,(c,l)) (edges, u) head_u max_iter
    | _, Some head_v ->
      to_loop_head (v,(c,l)) (edges, u) head_v max_iter
    | _, _ -> [(u,(c,l))]

  let tf var getl sidel getg sideg prev_node edge d =
    begin match edge with
      | Assign (lv,rv) -> tf_assign var edge prev_node lv rv
      | VDecl (v)      -> tf_vdecl var edge prev_node v
      | Proc (r,f,ars) -> tf_proc var edge prev_node r f ars
      | Entry f        -> tf_entry var edge prev_node f
      | Ret (r,fd)     -> tf_ret var edge prev_node r fd
      | Test (p,b)     -> tf_test var edge prev_node p b
      | ASM (_, _, _)  -> tf_asm var edge prev_node (* TODO: use ASM fields for something? *)
      | Skip           -> tf_skip var edge prev_node
    end getl sidel getg sideg d

  type Goblint_backtrace.mark += TfLocation of location

  let () = Goblint_backtrace.register_mark_printer (function
      | TfLocation loc ->
        Some ("transfer function at " ^ CilType.Location.show loc)
      | _ -> None (* for other marks *)
    )

  let tf var getl sidel getg sideg prev_node (_,edge) d (f,t) =
    let old_loc  = !Goblint_tracing.current_loc in
    let old_loc2 = !Goblint_tracing.next_loc in
    Goblint_tracing.current_loc := f;
    Goblint_tracing.next_loc := t;
    Goblint_backtrace.protect ~mark:(fun () -> TfLocation f) ~finally:(fun () ->
        Goblint_tracing.current_loc := old_loc;
        Goblint_tracing.next_loc := old_loc2
      ) (fun () ->
        let d       = tf var getl sidel getg sideg prev_node edge d in
        d
      )

  let tf (v,(c,l)) (edges, u) getl sidel getg sideg =
    match unroll (v,(c,l)) (edges, u) 10 (* TODO: value hardcoded *) with
    | vars ->
      let res = List.fold_left (fun acc var ->
          let pval = getl var in
          let _, locs = List.fold_right (fun (f,e) (t,xs) -> f, (f,t)::xs) edges (Node.location v,[]) in
          let res = List.fold_left2 (|>) pval (List.map (tf (v,(Obj.repr (fun () -> c),l)) getl sidel getg sideg u) edges) locs in
          S.D.join acc res
        ) (S.D.bot () ) vars
      in
      res
    | exception WrongCase ->
      S.D.bot ()

  let tf (v,(c,l)) (e,u) getl sidel getg sideg =
    let old_node = !current_node in
    let old_fd = Option.map Node.find_fundec old_node |? Cil.dummyFunDec in
    let new_fd = Node.find_fundec v in
    if not (CilType.Fundec.equal old_fd new_fd) then
      Timing.Program.enter new_fd.svar.vname;
    let old_context = !M.current_context in
    current_node := Some u;
    M.current_context := Some (Obj.magic c); (* magic is fine because Spec is top-level Control Spec *)
    Fun.protect ~finally:(fun () ->
        current_node := old_node;
        M.current_context := old_context;
        if not (CilType.Fundec.equal old_fd new_fd) then
          Timing.Program.exit new_fd.svar.vname
      ) (fun () ->
        let d       = tf (v,(c,l)) (e,u) getl sidel getg sideg in
        d
      )

  let system (v,(c,l)) =
    match v with
    | FunctionEntry _ ->
      None
    | _ ->
      let tf getl sidel getg sideg =
        let tf' eu = tf (v,(c,l)) eu getl sidel getg sideg in

        match NodeH.find_option CfgTools.node_scc_global v with
        | Some scc when false && NodeH.mem scc.prev v && NodeH.length scc.prev = 1 ->
          (* Limited to loops with only one entry node. Otherwise unsound as is. *)
          (* TODO: Is it possible to do soundly for multi-entry loops? *)
          let stricts = NodeH.find_default scc.prev v [] in
          let xs_stricts = List.map tf' stricts in
          (* Evaluate non-strict for dead code warnings. See 00-sanity/36-strict-loop-dead. *)
          let equal = [%eq: (CilType.Location.t * Edge.t) list * Node.t] in
          let is_strict eu = List.exists (equal eu) stricts in
          let non_stricts = List.filter (neg is_strict) (Cfg.prev v) in
          let xs_non_stricts = List.map tf' non_stricts in
          if List.for_all S.D.is_bot xs_stricts then
            S.D.bot ()
          else (
            let xs_strict = List.fold_left S.D.join (S.D.bot ()) xs_stricts in
            List.fold_left S.D.join xs_strict xs_non_stricts
          )
        | _ ->
          let xs = List.map tf' (Cfg.prev v) in
          List.fold_left S.D.join (S.D.bot ()) xs
      in
      Some tf

  let iter_vars getl getg vq fl fg =
    (* vars for Spec *)
    let rec ctx =
      { ask    = (fun (type a) (q: a Queries.t) -> S.query ctx q)
      ; emit   = (fun _ -> failwith "Cannot \"emit\" in query context.")
      ; node   = MyCFG.dummy_node (* TODO maybe ask should take a node (which could be used here) instead of a location *)
      ; prev_node = MyCFG.dummy_node
      ; control_context = (fun () -> ctx_failwith "No context in query context.")
      ; context = (fun () -> ctx_failwith "No context in query context.")
      ; edge    = MyCFG.Skip
      ; local  = S.startstate Cil.dummyFunDec.svar (* bot and top both silently raise and catch Deadcode in DeadcodeLifter *)
      ; global = (fun g -> G.spec (getg (GVar.spec g)))
      ; spawn  = (fun ?(multiple=false) v d    -> failwith "Cannot \"spawn\" in query context.")
      ; split  = (fun d es   -> failwith "Cannot \"split\" in query context.")
      ; sideg  = (fun v g    -> failwith "Cannot \"split\" in query context.")
      }
    in
    let f v = fg (GVar.spec (Obj.obj v)) in
    S.query ctx (IterSysVars (vq, f));

    (* node vars for locals *)
    match vq with
    | Node {node; fundec} ->
      let fd = Option.default_delayed (fun () -> Node.find_fundec node) fundec in
      let cs = G.contexts (getg (GVar.contexts fd)) in
      G.CSet.iter (fun c ->
          fl (node, (c, LoopCounts.empty ())) (* TODO: empty map *)
        ) cs
    | _ ->
      ()

  let sys_change getl getg =
    let open CompareCIL in

    let c = match I.increment with
      | Some {changes; _} -> changes
      | None -> empty_change_info ()
    in
    List.(Logs.info "change_info = { unchanged = %d; changed = %d (with unchangedHeader = %d); added = %d; removed = %d }" (length c.unchanged) (length c.changed) (BatList.count_matching (fun c -> c.unchangedHeader) c.changed) (length c.added) (length c.removed));

    let changed_funs = List.filter_map (function
        | {old = {def = Some (Fun f); _}; diff = None; _} ->
          Logs.info "Completely changed function: %s" f.svar.vname;
          Some f
        | _ -> None
      ) c.changed
    in
    let part_changed_funs = List.filter_map (function
        | {old = {def = Some (Fun f); _}; diff = Some nd; _} ->
          Logs.info "Partially changed function: %s" f.svar.vname;
          Some (f, nd.primObsoleteNodes, nd.unchangedNodes)
        | _ -> None
      ) c.changed
    in
    let removed_funs = List.filter_map (function
        | {def = Some (Fun f); _} ->
          Logs.info "Removed function: %s" f.svar.vname;
          Some f
        | _ -> None
      ) c.removed
    in

    let module HM = Hashtbl.Make (Var2 (LVar) (GVar)) in

    let mark_node hm f node =
      iter_vars getl getg (Node {node; fundec = Some f}) (fun v ->
          HM.replace hm (`L v) ()
        ) (fun v ->
          HM.replace hm (`G v) ()
        )
    in

    let reluctant = GobConfig.get_bool "incremental.reluctant.enabled" in
    let reanalyze_entry f =
      (* destabilize the entry points of a changed function when reluctant is off,
         or the function is to be force-reanalyzed  *)
      (not reluctant) || CompareCIL.VarinfoSet.mem f.svar c.exclude_from_rel_destab
    in
    let obsolete_ret = HM.create 103 in
    let obsolete_entry = HM.create 103 in
    let obsolete_prim = HM.create 103 in

    (* When reluctant is on:
       Only add function entry nodes to obsolete_entry if they are in force-reanalyze *)
    List.iter (fun f ->
        if reanalyze_entry f then
          (* collect function entry for eager destabilization *)
          mark_node obsolete_entry f (FunctionEntry f)
        else
          (* collect function return for reluctant analysis *)
          mark_node obsolete_ret f (Function f)
      ) changed_funs;
    (* Primary changed unknowns from partially changed functions need only to be collected for eager destabilization when reluctant is off *)
    (* The return nodes of partially changed functions are collected in obsolete_ret for reluctant analysis *)
    (* We utilize that force-reanalyzed functions are always considered as completely changed (and not partially changed) *)
    List.iter (fun (f, pn, _) ->
        if not reluctant then (
          List.iter (fun n ->
              mark_node obsolete_prim f n
            ) pn
        )
        else
          mark_node obsolete_ret f (Function f)
      ) part_changed_funs;

    let obsolete = Enum.append (HM.keys obsolete_entry) (HM.keys obsolete_prim) |> List.of_enum in
    let reluctant = HM.keys obsolete_ret |> List.of_enum in

    let marked_for_deletion = HM.create 103 in

    let dummy_pseudo_return_node f =
      (* not the same as in CFG, but compares equal because of sid *)
      Node.Statement ({Cil.dummyStmt with sid = Cilfacade.get_pseudo_return_id f})
    in
    let add_nodes_of_fun (functions: fundec list) (withEntry: fundec -> bool) =
      let add_stmts (f: fundec) =
        List.iter (fun s ->
            mark_node marked_for_deletion f (Statement s)
          ) f.sallstmts
      in
      List.iter (fun f ->
          if withEntry f then
            mark_node marked_for_deletion f (FunctionEntry f);
          mark_node marked_for_deletion f (Function f);
          add_stmts f;
          mark_node marked_for_deletion f (dummy_pseudo_return_node f)
        ) functions;
    in

    add_nodes_of_fun changed_funs reanalyze_entry;
    add_nodes_of_fun removed_funs (fun _ -> true);
    (* it is necessary to remove all unknowns for changed pseudo-returns because they have static ids *)
    let add_pseudo_return f un =
      let pseudo = dummy_pseudo_return_node f in
      if not (List.exists (Node.equal pseudo % fst) un) then
        mark_node marked_for_deletion f (dummy_pseudo_return_node f)
    in
    List.iter (fun (f,_,un) ->
        mark_node marked_for_deletion f (Function f);
        add_pseudo_return f un
      ) part_changed_funs;

    let delete = HM.keys marked_for_deletion |> List.of_enum in

    let restart = match I.increment with
      | Some data ->
        let restart = ref [] in
        List.iter (fun g ->
            iter_vars getl getg g (fun v ->
                restart := `L v :: !restart
              ) (fun v ->
                restart := `G v :: !restart
              )
          ) data.restarting;
        !restart
      | None -> []
    in

    {obsolete; delete; reluctant; restart}
end
