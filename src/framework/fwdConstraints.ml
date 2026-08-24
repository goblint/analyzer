(** Construction of a {{!Goblint_constraint} constraint system} from an {{!Analyses.Spec} analysis specification} and
    {{!MyCFG.CfgForward} CFGs}.
    Transformations of analysis specifications as functors. *)

open Batteries
open GoblintCil
open MyCFG
open Analyses
open Goblint_constraint.ConstrSys
open GobConfig


module type Increment =
sig
  val increment: increment_data option
end


(** The main point of this file---generating a [FwdGlobConstrSys] from a [Spec]. *)
module FromSpec (S:Spec') (Cfg:CfgBidir) (I: Increment)
  : sig
    include FwdGlobConstrSys with module LVar = VarDigestF (S.C) (S.P)
                              and module GVar = GVarFCNW (S.V) (S.C) (S.P)
                              and module D = S.D
                              and module G = GVar3 (S.G) (S.D) (S.LVarSet)
  end
=
struct

  module LVar = VarDigestF (S.C) (S.P)

  type lv = LVar.t
  (* type gv = varinfo *)
  type ld = S.D.t
  (* type gd = S.G.t *)
  module GVar = GVarFCNW (S.V) (S.C) (S.P)
  module D = S.D
  module G = GVar3 (S.G) (S.D) (S.LVarSet)

  (* Two global invariants:
     1. S.V -> S.G  --  used for Spec
     2. fundec -> set of S.C  --  used for IterSysVars Node *)

  let sync man =
    match man.prev_node, Cfg.prev man.prev_node with
    | _, _ :: _ :: _ -> (* Join in CFG. *)
      S.sync man `Join
    | FunctionEntry f, _ -> (* Function entry, also needs sync because partial contexts joined by solver, see 00-sanity/35-join-contexts. *)
      S.sync man (`JoinCall f)
    | _, _ -> S.sync man `Normal

  let common_man' (var : LVar.t) edge target_node pval (getl:lv -> ld) (sidel : lv -> ld -> unit) getg sideg : (D.t, S.G.t, S.C.t, S.V.t) man * D.t list ref * (lval option * lval option * varinfo * exp list * D.t * bool) list ref =
    let r = ref [] in
    let spawns = ref [] in
    (* now watch this ... *)
    let rec man =
      { ask     = (fun (type a) (q: a Queries.t) -> S.query man q)
      ; emit    = (fun _ -> failwith "emit outside MCP")
      ; node    = target_node
      ; prev_node = var.node
      ; control_context = (fun () -> Obj.magic var.context)
      ; context = (fun () -> Obj.magic var.context)
      ; edge    = edge
      ; local   = pval
      ; global  = (fun g -> G.spec (getg (GVar.spec g)))
      ; spawn   = spawn
      ; split   = (fun (d:D.t) es -> assert (List.is_empty es); r := d::!r)
      ; sideg   = (fun g d -> sideg (GVar.spec g) (G.create_spec d))
      }
    and spawn ?(multiple=false) ~result_lval lval f args =
      (* TODO: adjust man node/edge? *)
      (* TODO: don't repeat for all paths that spawn same *)
      let ds = S.threadenter ~multiple man lval f args in
      List.iter (fun d ->
          match Cilfacade.find_varinfo_fundec f with
          | fd ->
            spawns := (result_lval, lval, f, args, d, multiple) :: !spawns;
            let c = S.context man fd d in
            (* Derive digest from abstract state *)
            let p = S.P.of_elt d in
            let target_unknown  : lv = {node = FunctionEntry fd; context = c; original_digest = p; current_digest = p} in
            sidel target_unknown d
          | exception Not_found ->
            (* Run the unknown thread function in the created thread state
               before synchronizing its side effects back to the creator. *)
            let rec special_man =
              { man with
                ask = (fun (type a) (q: a Queries.t) -> S.query special_man q);
                local = d;
                prev_node = Function dummyFunDec;
              }
            in
            let d = S.special special_man None f args in
            (* unknown function *)
            M.error ~category:Imprecise ~tags:[Category Unsound] "Created a thread from unknown function %s" f.vname;
            (* Sync the created thread after its unknown-function effects so
               privatization side effects become visible to the creator. *)
            let rec sync_man =
              { special_man with
                ask = (fun (type a) (q: a Queries.t) -> S.query sync_man q);
                local = d;
              }
            in
            let d = sync sync_man in
            spawns := (result_lval, lval, f, args, d, multiple) :: !spawns
        ) ds
    in
    (* ... nice, right! *)
    let pval = sync man in
    { man with local = pval }, r, spawns

  let rec bigsqcup = function
    | []    -> D.bot ()
    | [x]   -> x
    | x::xs -> D.join x (bigsqcup xs)

  let thread_spawns man d spawns =
    if List.is_empty spawns then
      d
    else
      let rec man' =
        { man with
          ask = (fun (type a) (q: a Queries.t) -> S.query man' q)
        ; local = d
        }
      in
      (* TODO: don't forget path dependencies *)
      let one_spawn (result_lval, lval, f, args, fd, multiple) =
        let rec fman =
          { man with
            ask = (fun (type a) (q: a Queries.t) -> S.query fman q)
          ; local = fd
          }
        in
        let d = S.threadspawn man' ~multiple lval f args fman in
        Option.map_default (fun lval ->
            (* [threadspawn] has processed [EnterMultiThreaded], so the thread
               creation result is assigned in multithreaded mode. *)
            let rec event_man =
              { man' with
                ask = (fun (type a) (q: a Queries.t) -> S.query event_man q);
                local = d;
              }
            in
            S.event event_man (Events.Assign {lval; exp = MyCFG.unknown_exp}) event_man
          ) d result_lval
      in
      bigsqcup (List.map one_spawn spawns)

  let man_with_local (man: (D.t, S.G.t, S.C.t, S.V.t) man) (local: D.t) =
    let rec man' =
      { man with
        ask = (fun (type a) (q: a Queries.t) -> S.query man' q);
        local;
      }
    in
    man'

  let paths_with_splits r f =
    let ds =
      try
        let d = f () in
        d :: !r
      with Deadcode -> !r
    in
    r := [];
    ds

  let rec transfer_on_synced_splits man r spawns f =
    let sync_splits = !r in
    r := [];
    spawns := [];
    List.concat_map
      (fun local ->
         let ds = paths_with_splits r (fun () -> f (man_with_local man local)) in
         let path_spawns = !spawns in
         spawns := [];
         finish_transfer man r ds path_spawns)
      (man.local :: sync_splits)

  and transfer_many_on_synced_splits man r spawns f =
    let sync_splits = !r in
    r := [];
    spawns := [];
    List.concat_map
      (fun local ->
         let ds =
           try f (man_with_local man local) local
           with Deadcode -> []
         in
         let splits = !r in
         r := [];
         let path_spawns = !spawns in
         spawns := [];
         finish_transfer man r (ds @ splits) path_spawns)
      (man.local :: sync_splits)

  (** Process thread spawns and the target sync separately for each path. The
      demand-driven framework can join these here because its path-sensitive
      lifter preserves the disjunction. With forward digests there is no such
      outer lifter: joining first would turn distinct phase constants into top
      before they can be routed to distinct digest unknowns. *)
  and finish_transfer man r ds spawns =
    let ds = ds @ !r in
    r := [];
    List.concat_map (fun d ->
        let d = thread_spawns man d spawns in
        paths_with_splits r (fun () -> sync (man_with_local man d))
      ) ds

  let tf_assign var edge target_node lv e getl sidel getg sideg d =
    let man, r, spawns = common_man' var edge target_node d getl sidel getg sideg in
    transfer_on_synced_splits man r spawns (fun man -> S.assign man lv e)

  let tf_vdecl var edge target_node v getl sidel getg sideg d =
    let man, r, spawns = common_man' var edge target_node d getl sidel getg sideg in
    transfer_on_synced_splits man r spawns (fun man -> S.vdecl man v)

  let normal_return r fd man sideg =
    let spawning_return = S.return man r fd in
    let nval = S.sync { man with local = spawning_return } `Return in
    nval

  let toplevel_kernel_return r fd man sideg =
    let st = if fd.svar.vname = MyCFG.dummy_func.svar.vname then man.local else S.return man r fd in
    let spawning_return = S.return {man with local = st} None MyCFG.dummy_func in
    let nval = S.sync { man with local = spawning_return } `Return in
    nval

  let tf_ret var edge target_node ret fd getl sidel getg sideg d =
    let man, r, spawns = common_man' var edge target_node d getl sidel getg sideg in
    let transfer man =
      if (CilType.Fundec.equal fd MyCFG.dummy_func ||
          List.mem fd.svar.vname (get_string_list "mainfun")) &&
         get_bool "kernel"
      then toplevel_kernel_return ret fd man sideg
      else normal_return ret fd man sideg
    in
    transfer_on_synced_splits man r spawns transfer

  let tf_entry var edge target_node fd getl sidel getg sideg d =
    let man, r, spawns = common_man' var edge target_node d getl sidel getg sideg in
    transfer_on_synced_splits man r spawns (fun man -> S.body man fd)

  let tf_test var edge target_node e tv getl sidel getg sideg d =
    let man, r, spawns = common_man' var edge target_node d getl sidel getg sideg in
    transfer_on_synced_splits man r spawns (fun man -> S.branch man e tv)

  let tf_normal_call man lv e (f:fundec) args getl (sidel : lv -> ld -> unit) getg sideg =
    let combine (cd, fc, fd) =
      if M.tracing then M.traceli "combine" "local: %a" S.D.pretty cd;
      if M.tracing then M.trace "combine" "function: %a" S.D.pretty fd;
      let rec cd_man =
        { man with
          ask = (fun (type a) (q: a Queries.t) -> S.query cd_man q);
          local = cd;
        }
      in
      let fd_man =
        (* Inner scope to prevent unsynced fd_man from being used. *)
        (* Extra sync in case function has multiple returns.
           Each `Return sync is done before joining, so joined value may be unsound.
           Since sync is normally done before tf (in common_man), simulate it here for fd. *)
        (* TODO: don't do this extra sync here *)
        let rec sync_man =
          { man with
            ask = (fun (type a) (q: a Queries.t) -> S.query sync_man q);
            local = fd;
            prev_node = Function f;
          }
        in
        (* TODO: more accurate man? *)
        let synced = sync sync_man in
        let rec fd_man =
          { sync_man with
            ask = (fun (type a) (q: a Queries.t) -> S.query fd_man q);
            local = synced;
          }
        in
        fd_man
      in
      let r = List.fold_left (fun acc fd1 ->
          let rec fd1_man =
            { fd_man with
              ask = (fun (type a) (q: a Queries.t) -> S.query fd1_man q);
              local = fd1;
            }
          in
          let combine_enved = S.combine_env cd_man lv e f args fc fd1_man.local (Analyses.ask_of_man fd1_man) in
          let rec combine_assign_man =
            { cd_man with
              ask = (fun (type a) (q: a Queries.t) -> S.query combine_assign_man q);
              local = combine_enved;
            }
          in
          S.D.join acc (S.combine_assign combine_assign_man lv e f args fc fd1_man.local (Analyses.ask_of_man fd1_man))
        ) (S.D.bot ()) (S.paths_as_set fd_man)
      in
      if M.tracing then M.traceu "combine" "combined local: %a" S.D.pretty r;
      r
    in
    let combine_each (cd, fc, fd_list) =
      List.map (fun fd -> combine (cd, fc, fd)) fd_list
    in
    let paths = S.enter man lv f args in
    let paths = List.map (fun (c,v) -> (c, S.context man f v, v)) paths in
    let sidel_entries (c,fc,v) =
      if not (S.D.is_bot v) then begin
        let p = S.P.of_elt v in
        let target_unknown : lv = {node = FunctionEntry f; context = fc; original_digest = p; current_digest = p}  in
        sidel target_unknown v
      end
    in
    List.iter sidel_entries paths;
    let paths = List.map (fun (c,fc,v) ->
        let p = S.P.of_elt v in
        let endvar = (GVar.return (f,fc,p)) in
        let end_paths =
          if S.D.is_bot v then [S.D.bot ()]
          else
            let return_nodes = G.return @@ getg endvar |> S.LVarSet.to_seq in
            let return_value x =
              G.single_return @@ getg (GVar.single_return x)
            in
            let s = Seq.map return_value return_nodes in
            if Seq.is_empty s then
              (* In case the callee does not return, create one bottom return path for LongjmpLifter to do its work *)
              [S.D.bot ()]
            else
              List.of_seq s
        in
        (c, fc, end_paths)) paths
    in
    (* Don't filter bot paths, otherwise LongjmpLifter is not called. *)
    (* let paths = List.filter (fun (c,fc,v) -> not (D.is_bot v)) paths in *)
    let paths = List.map (Tuple3.map2 Option.some) paths in
    let paths = List.map combine_each paths in
    let paths = List.flatten paths in
    paths


  let rec tf_proc var edge target_node lv e args getl sidel getg sideg d =
    let tf_special_call man f =
      let once once_control init_routine =
        (* Executes leave event for new local state d if it is not bottom *)
        let leave_once d =
          if not (S.D.is_bot d) then
            let rec man' =
              { man with
                ask = (fun (type a) (q: a Queries.t) -> S.query man' q);
                local = d;
              }
            in
            S.event man' (Events.LeaveOnce { once_control }) man'
          else
            S.D.bot ()
        in
        let first_call =
          let d' = S.event man (Events.EnterOnce { once_control;  ran = false }) man in
          tf_proc var edge target_node None init_routine [] getl sidel getg sideg d'
        in
        let later_call = S.event man (Events.EnterOnce { once_control;  ran = true }) man in
        let first_call = List.fold D.join (D.bot ()) first_call in
        [D.join (leave_once first_call) (leave_once later_call)]
      in
      let is_once = LibraryFunctions.find ~nowarn:true f in
      (* If the prototpye for a library function is wrong, this will throw an exception. Such exceptions are usually unrelated to pthread_once, it is just that the call to `is_once.special` raises here *)
      match is_once.special args with
      | Once { once_control; init_routine } -> once once_control init_routine
      | _  -> [S.special man lv f args]
    in
    let man, r, spawns = common_man' var edge target_node d getl sidel getg sideg in
    let one_man man fallback =
      let functions =
        match e with
        | Lval (Var v, NoOffset) -> [v]
        | _ ->
          let ad = man.ask (Queries.EvalFunvar e) in
          Queries.AD.to_var_may ad
      in
      let one_function f =
        match Cil.unrollType f.vtype with
        | TFun (_, params, var_arg, attrs) ->
          if Cil.hasAttribute "missingproto" attrs then (
            M.msg_final Warning ~category:Program "Function declaration missing";
            M.warn ~category:Program "Function declaration missing for %s" f.vname
          );
          let arg_length = List.length args in
          let p_length = Option.map_default List.length 0 params in
          if Option.is_none params || p_length = arg_length || (var_arg && arg_length >= p_length) then
            Some (match Cilfacade.find_varinfo_fundec f with
                | fd when LibraryFunctions.use_special f.vname ->
                  M.info ~category:Analyzer "Using special for defined function %s" f.vname;
                  tf_special_call man f
                | fd -> tf_normal_call man lv e fd args getl sidel getg sideg
                | exception Not_found -> tf_special_call man f)
          else begin
            let geq = if var_arg then ">=" else "" in
            M.warn ~category:Unsound ~tags:[Category Call; CWE 685] "Potential call to function %a with wrong number of arguments (expected: %s%d, actual: %d). This call will be ignored." CilType.Varinfo.pretty f geq p_length arg_length;
            None
          end
        | _ ->
          M.warn ~category:Call "Something that is not a function (%a) is called." CilType.Varinfo.pretty f;
          None
      in
      let funs = List.filter_map one_function functions |> List.flatten in
      if List.is_empty funs && not (S.D.is_bot man.local) then begin
        M.msg_final Warning ~category:Unsound ~tags:[Category Call] "No suitable function to call";
        M.warn ~category:Unsound ~tags:[Category Call] "No suitable function to be called at call site. Continuing with state before call.";
        [fallback]
      end else
        funs
    in
    transfer_many_on_synced_splits man r spawns one_man

  let tf_asm var edge target_node getl sidel getg sideg d =
    let man, r, spawns = common_man' var edge target_node d getl sidel getg sideg in
    transfer_on_synced_splits man r spawns (fun man -> S.asm man)

  let tf_skip var edge target_node getl sidel getg sideg d =
    let man, r, spawns = common_man' var edge target_node d getl sidel getg sideg in
    transfer_on_synced_splits man r spawns (fun man -> S.skip man)

  let tf (x : lv) getl sidel getg sideg target_node edge d =
    let target_unknown d : lv =
      let current_digest = S.P.of_elt d in
      {node = target_node; context = x.context; original_digest = x.original_digest; current_digest }
    in
    let sidel_target_unkonwn d =
      let target_unknown = target_unknown d in
      sidel target_unknown d
    in
    (** Takes a list of values and propagates them to the appropriate successor local unknowns . *)
    let propagate ds =
      List.iter sidel_target_unkonwn ds
    in
    begin match edge with
      | Assign (lv,rv) ->
        let r = tf_assign x edge target_node lv rv getl sidel getg sideg d in
        propagate r
      | VDecl (v)      ->
        let r = tf_vdecl x edge target_node v getl sidel getg sideg d in
        propagate r
      | Proc (r,f,ars) ->
        let r = tf_proc x edge target_node r f ars getl sidel getg sideg d in
        propagate r
      | Entry f        ->
        let r = tf_entry x edge target_node f getl sidel getg sideg d in
        propagate r
      | Ret (r,fd)     ->
        let r = tf_ret x edge target_node r fd getl sidel getg sideg d in
        let propagate_to_return_global d =
          let target_unknown = target_unknown d in
          let target_unknown_g = GVar.single_return target_unknown  in
          sideg target_unknown_g (G.create_single_return d)
        in
        List.iter propagate_to_return_global r;

        (* Propagate to locals for returns. Currently only needed for the result view at return nodes.
           The analysis will look up the return state at the global for the return. *)
        (* TODO: Adapt generation of result view so that this can be avoided. *)
        propagate r;

        let set = S.LVarSet.bot () in
        let add_entry set d =
          let return_unknown = target_unknown d in
          S.LVarSet.add return_unknown set
        in

        (* Set of return unkonwns, split by current digest, that this transfer function contributes to. *)
        let return_set = List.fold add_entry set r |> G.create_return in
        (* Unknown, consisting of function, context and original (calling) digest, where the possible unknowns, split by current digest, are collected *)
        let return_collector = GVar.return (fd, x.context, x.original_digest) in
        sideg return_collector return_set
      | Test (e,b)     ->
        let r = tf_test x edge target_node e b getl sidel getg sideg d in
        propagate r
      | ASM (_, _, _)  ->
        let r = tf_asm x edge target_node getl sidel getg sideg d in
        propagate r
      | Skip           ->
        let r = tf_skip x edge target_node getl sidel getg sideg d in
        propagate r
    end

  type Goblint_backtrace.mark += TfLocation of location

  let () = Goblint_backtrace.register_mark_printer (function
      | TfLocation loc ->
        Some ("transfer function at " ^ CilType.Location.show loc)
      | _ -> None (* for other marks *)
    )

  let tf x getl sidel getg sideg target_node (_,edge) d (f,t):unit =
    let old_loc  = !Goblint_tracing.current_loc in
    let old_loc2 = !Goblint_tracing.next_loc in
    Goblint_tracing.current_loc := f;
    Goblint_tracing.next_loc := t;
    Goblint_backtrace.protect ~mark:(fun () -> TfLocation f) ~finally:(fun () ->
        Goblint_tracing.current_loc := old_loc;
        Goblint_tracing.next_loc := old_loc2
      ) (fun () ->
        tf x getl sidel getg sideg target_node edge d
      )

  let tf_fwd value (x : lv) (edges, u) getl sidel getg sideg:unit =
    let pval = value in
    let _, locs = List.fold_right (fun (f,e) (t,xs) -> f, (f,t)::xs) edges (Node.location x.node,[]) in
    let es = List.map (tf x getl sidel getg sideg u) edges in
    List.iter2 (fun e l -> e pval l) es locs

  let tf value (x : lv) (e,u) getl sidel getg sideg =
    let old_node = !current_node in
    let old_fd = Option.map Node.find_fundec old_node |? Cil.dummyFunDec in
    let new_fd = Node.find_fundec x.node in
    if not (CilType.Fundec.equal old_fd new_fd) then
      Timing.Program.enter new_fd.svar.vname;
    let old_context = !M.current_context in
    current_node := Some x.node;
    M.current_context := Some (Obj.magic x.context); (* magic is fine because Spec is top-level Control Spec *)
    Fun.protect ~finally:(fun () ->
        current_node := old_node;
        M.current_context := old_context;
        if not (CilType.Fundec.equal old_fd new_fd) then
          Timing.Program.exit new_fd.svar.vname
      ) (fun () ->
        tf_fwd value x (e,u) getl sidel getg sideg
      )

  let system (x : lv) =
    let tf value getl sidel getg sideg =
      let tf' eu = tf value x eu getl sidel getg sideg in
      let xs = Cfg.next x.node in
      List.iter (fun eu -> tf' eu) xs
    in
    Some tf


end
