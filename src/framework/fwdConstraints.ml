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
module FromSpec (S:Spec) (Cfg:CfgForward) (I: Increment)
  : sig
    include FwdGlobConstrSys with module LVar = VarF (S.C)
                              and module GVar = GVarFCNW (S.V)(S.C)
                              and module D = S.D
                              and module G = GVarL (S.G) (S.D)
  end
=
struct
  type lv = MyCFG.node * S.C.t
  (* type gv = varinfo *)
  type ld = S.D.t
  (* type gd = S.G.t *)
  module LVar = VarF (S.C)
  module GVar = GVarFCNW (S.V)(S.C)
  module D = S.D
  module G = GVarL (S.G) (S.D)

  (* Two global invariants:
     1. S.V -> S.G  --  used for Spec
     2. fundec -> set of S.C  --  used for IterSysVars Node *)

  let sync man =
    match man.prev_node with
    | FunctionEntry f -> (* Function entry, also needs sync because partial contexts joined by solver, see 00-sanity/35-join-contexts. *)
      S.sync man (`JoinCall f)
    | _ -> S.sync man `Join

  let common_man' var edge target_node pval (getl:lv -> ld) sidel getg sideg : (D.t, S.G.t, S.C.t, S.V.t) man * D.t list ref * (lval option * varinfo * exp list * D.t * bool) list ref =
    let r = ref [] in
    let spawns = ref [] in
    (* now watch this ... *)
    let rec man =
      { ask     = (fun (type a) (q: a Queries.t) -> S.query man q)
      ; emit    = (fun _ -> failwith "emit outside MCP")
      ; node    = target_node
      ; prev_node = fst var
      ; control_context = snd var |> Obj.obj
      ; context = snd var |> Obj.obj
      ; edge    = edge
      ; local   = pval
      ; global  = (fun g -> G.spec (getg (GVar.spec g)))
      ; spawn   = spawn
      ; split   = (fun (d:D.t) es -> assert (List.is_empty es); r := d::!r)
      ; sideg   = (fun g d -> sideg (GVar.spec g) (G.create_spec d))
      }
    and spawn ?(multiple=false) lval f args =
      (* TODO: adjust man node/edge? *)
      (* TODO: don't repeat for all paths that spawn same *)
      let ds = S.threadenter ~multiple man lval f args in
      List.iter (fun d ->
          spawns := (lval, f, args, d, multiple) :: !spawns;
          match Cilfacade.find_varinfo_fundec f with
          | fd ->
            let c = S.context man fd d in
            sidel (FunctionEntry fd, c) d
          | exception Not_found ->
            (* unknown function *)
            M.error ~category:Imprecise ~tags:[Category Unsound] "Created a thread from unknown function %s" f.vname;
            (* actual implementation (e.g. invalidation) is done by threadenter *)
            (* must still sync for side effects, e.g., old sync-based none privatization soundness in 02-base/51-spawn-special *)
            let rec sync_man =
              { man with
                ask = (fun (type a) (q: a Queries.t) -> S.query sync_man q);
                local = d;
                prev_node = Function dummyFunDec;
              }
            in
            (* TODO: more accurate man? *)
            ignore (sync sync_man)
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
      let one_spawn (lval, f, args, fd, multiple) =
        let rec fman =
          { man with
            ask = (fun (type a) (q: a Queries.t) -> S.query fman q)
          ; local = fd
          }
        in
        S.threadspawn man' ~multiple lval f args fman
      in
      bigsqcup (List.map one_spawn spawns)

  let common_join man d splits spawns =
    thread_spawns man (bigsqcup (d :: splits)) spawns

  let common_joins man ds splits spawns = common_join man (bigsqcup ds) splits spawns

  let tf_assign var edge target_node lv e getl sidel getg sideg d =
    let man, r, spawns = common_man' var edge target_node d getl sidel getg sideg in
    let d = S.assign man lv e in (* Force transfer function to be evaluated before dereferencing in common_join argument. *)
    common_join man d !r !spawns

  let tf_vdecl var edge target_node v getl sidel getg sideg d =
    let man, r, spawns = common_man' var edge target_node d getl sidel getg sideg in
    let d = S.vdecl man v in (* Force transfer function to be evaluated before dereferencing in common_join argument. *)
    common_join man d !r !spawns

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
    let d = (* Force transfer function to be evaluated before dereferencing in common_join argument. *)
      if (CilType.Fundec.equal fd MyCFG.dummy_func ||
          List.mem fd.svar.vname (get_string_list "mainfun")) &&
         get_bool "kernel"
      then toplevel_kernel_return ret fd man sideg
      else normal_return ret fd man sideg
    in
    common_join man d !r !spawns

  let tf_entry var edge target_node fd getl sidel getg sideg d =
    let man, r, spawns = common_man' var edge target_node d getl sidel getg sideg in
    let d = S.body man fd in (* Force transfer function to be evaluated before dereferencing in common_join argument. *)
    common_join man d !r !spawns

  let tf_test var edge target_node e tv getl sidel getg sideg d =
    let man, r, spawns = common_man' var edge target_node d getl sidel getg sideg in
    let d = S.branch man e tv in (* Force transfer function to be evaluated before dereferencing in common_join argument. *)
    common_join man d !r !spawns

  let tf_normal_call man lv e (f:fundec) args getl sidel getg sideg =
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
    let paths = S.enter man lv f args in
    let paths = List.map (fun (c,v) -> (c, S.context man f v, v)) paths in
    List.iter (fun (c,fc,v) -> if not (S.D.is_bot v) then sidel (FunctionEntry f, fc) v) paths;
    let paths = List.map (fun (c,fc,v) -> 
        let endvar = (GVar.return (f,fc)) in
        (c, fc, if S.D.is_bot v then v else G.return @@ getg endvar)) paths in
    (* Don't filter bot paths, otherwise LongjmpLifter is not called. *)
    (* let paths = List.filter (fun (c,fc,v) -> not (D.is_bot v)) paths in *)
    let paths = List.map (Tuple3.map2 Option.some) paths in
    if M.tracing then M.traceli "combine" "combining";
    let paths = List.map combine paths in
    let r = List.fold_left D.join (D.bot ()) paths in
    if M.tracing then M.traceu "combine" "combined: %a" S.D.pretty r;
    r


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
        D.join (leave_once first_call) (leave_once later_call)
      in
      let is_once = LibraryFunctions.find ~nowarn:true f in
      (* If the prototpye for a library function is wrong, this will throw an exception. Such exceptions are usually unrelated to pthread_once, it is just that the call to `is_once.special` raises here *)
      match is_once.special args with
      | Once { once_control; init_routine } -> once once_control init_routine
      | _  -> S.special man lv f args
    in
    let man, r, spawns = common_man' var edge target_node d getl sidel getg sideg in
    let functions =
      match e with
      | Lval (Var v, NoOffset) ->
        (* Handle statically known function call directly.
           Allows deactivating base. *)
        [v]
      | _ ->
        (* Depends on base for query. *)
        let ad = man.ask (Queries.EvalFunvar e) in
        Queries.AD.to_var_may ad (* TODO: don't convert, handle UnknownPtr below *)
    in
    let one_function f =
      match Cil.unrollType f.vtype with
      | TFun (_, params, var_arg, _)  ->
        let arg_length = List.length args in
        let p_length = Option.map_default List.length 0 params in
        (* Check whether number of arguments fits. *)
        (* If params is None, the function or its parameters are not declared, so we still analyze the unknown function call. *)
        if Option.is_none params || p_length = arg_length || (var_arg && arg_length >= p_length) then
          let d =
            (match Cilfacade.find_varinfo_fundec f with
             | fd when LibraryFunctions.use_special f.vname ->
               M.info ~category:Analyzer "Using special for defined function %s" f.vname;
               tf_special_call man f
             | fd ->
               tf_normal_call man lv e fd args getl sidel getg sideg
             | exception Not_found ->
               tf_special_call man f)
          in
          Some d
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
    if [] = funs && not (S.D.is_bot man.local) then begin
      M.msg_final Warning ~category:Unsound ~tags:[Category Call] "No suitable function to call";
      M.warn ~category:Unsound ~tags:[Category Call] "No suitable function to be called at call site. Continuing with state before call.";
      d (* because LevelSliceLifter *)
    end else
      common_joins man funs !r !spawns

  let tf_asm var edge target_node getl sidel getg sideg d =
    let man, r, spawns = common_man' var edge target_node d getl sidel getg sideg in
    let d = S.asm man in (* Force transfer function to be evaluated before dereferencing in common_join argument. *)
    common_join man d !r !spawns

  let tf_skip var edge target_node getl sidel getg sideg d =
    let man, r, spawns = common_man' var edge target_node d getl sidel getg sideg in
    let d = S.skip man in (* Force transfer function to be evaluated before dereferencing in common_join argument. *)
    common_join man d !r !spawns

  let tf ((n,c) as var) getl sidel getg sideg target_node edge d =
    begin match edge with
      | Assign (lv,rv) ->
        let r = tf_assign var edge target_node lv rv getl sidel getg sideg d in
        sidel (target_node, Obj.obj c) r
      | VDecl (v)      -> 
        let r = tf_vdecl var edge target_node v getl sidel getg sideg d in
        sidel (target_node, Obj.obj c) r
      | Proc (r,f,ars) ->
        let r = tf_proc var edge target_node r f ars getl sidel getg sideg d in
        sidel (target_node, Obj.obj c) r
      | Entry f        -> 
        let r = tf_entry var edge target_node f getl sidel getg sideg d in
        sidel (target_node, Obj.obj c) r
      | Ret (r,fd)     ->
        let r = tf_ret var edge target_node r fd getl sidel getg sideg d in
        sidel (target_node, Obj.obj c) r;
        sideg (GVar.return (fd,Obj.obj c)) (G.create_return r)
      | Test (p,b)     -> 
        let r = tf_test var edge target_node p b getl sidel getg sideg d in
        sidel (target_node, Obj.obj c) r
      | ASM (_, _, _)  ->
        let r = tf_asm var edge target_node getl sidel getg sideg d in
        sidel (target_node, Obj.obj c) r
      | Skip           -> 
        let r = tf_skip var edge target_node getl sidel getg sideg d in
        sidel (target_node, Obj.obj c) r
    end

  type Goblint_backtrace.mark += TfLocation of location

  let () = Goblint_backtrace.register_mark_printer (function
      | TfLocation loc ->
        Some ("transfer function at " ^ CilType.Location.show loc)
      | _ -> None (* for other marks *)
    )

  let tf var getl sidel getg sideg target_node (_,edge) d (f,t):unit =
    let old_loc  = !Goblint_tracing.current_loc in
    let old_loc2 = !Goblint_tracing.next_loc in
    Goblint_tracing.current_loc := f;
    Goblint_tracing.next_loc := t;
    Goblint_backtrace.protect ~mark:(fun () -> TfLocation f) ~finally:(fun () ->
        Goblint_tracing.current_loc := old_loc;
        Goblint_tracing.next_loc := old_loc2
      ) (fun () ->
        tf var getl sidel getg sideg target_node edge d
      )

  let tf_fwd (v,c) (edges, u) getl sidel getg sideg:unit =
    let pval = getl (v,c) in
    let _, locs = List.fold_right (fun (f,e) (t,xs) -> f, (f,t)::xs) edges (Node.location v,[]) in
    let es = List.map (tf (v,Obj.repr (fun () -> c)) getl sidel getg sideg u) edges in
    List.iter2 (fun e l -> e pval l) es locs

  let tf (v,c) (e,u) getl sidel getg sideg =
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
        tf_fwd (v,c) (e,u) getl sidel getg sideg
      )

  let system (v,c) =
    match v with
    | FunctionEntry _ ->
      None
    | _ ->
      let tf getl sidel getg sideg =
        let tf' eu = tf (v,c) eu getl sidel getg sideg in
        let xs = Cfg.next v in
        List.iter (fun eu -> tf' eu) xs
      in
      Some tf


end
