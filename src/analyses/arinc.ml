(** Tracking of arinc processes and their actions. Output to console, graphviz and promela. *)

open Prelude.Ana
open Analyses

let debug_doc doc = M.debug_each (Pretty.sprint 99 doc)

module Functions = struct
  let prefix = "LAP_Se_"
  (* ARINC functions copied from stdapi.c *)
  let with_timeout = List.map ((^) prefix) ["SuspendSelf"; "ReadBlackboard"; "WaitSemaphore"; "WaitEvent"]
  let wout_timeout = List.map ((^) prefix) ["TimedWait";"PeriodicWait";"GetTime";"ReplenishAperiodic";"CreateProcess";"SetPriority";"Suspend";"Resume";"StopSelf";"Stop";"Start";"DelayedStart";"LockPreemption";"UnlockPreemption";"GetMyId";"GetProcessId";"GetProcessStatus";"GetPartitionStatus";"SetPartitionMode";"GetPartitionStartCondition";"CreateLogBook";"ReadLogBook";"WriteLogBook";"ClearLogBook";"GetLogbookId";"GetLogBookStatus";"CreateSamplingPort";"WriteSamplingMessage";"ReadSamplingMessage";"GetSamplingPortId";"GetSamplingPortStatus";"CreateQueuingPort";"SendQueuingMessage";"ReceiveQueuingMessage";"GetQueuingPortId";"GetQueuingPortStatus";"CreateBuffer";"SendBuffer";"ReceiveBuffer";"GetBufferId";"GetBufferStatus";"CreateBlackboard";"DisplayBlackboard";"ClearBlackboard";"GetBlackboardId";"GetBlackboardStatus";"CreateSemaphore";"SignalSemaphore";"GetSemaphoreId";"GetSemaphoreStatus";"CreateEvent";"SetEvent";"ResetEvent";"GetEventId";"GetEventStatus";"CreateErrorHandler";"GetErrorStatus";"RaiseApplicationError"]
  (* functions from string.h which are implemented in the scrambled code *)
  (* this is needed since strcpy is used for some process names, which will be top otherwise *)
  let others = [
    "F59" (* strcpy *); "F60" (* strncpy *); "F63" (* memcpy *); "F1" (* memset *);
    (* "F61"; "F62" (* these are optional. add them to speed up the analysis. *) *)
  ]
  let arinc_special = with_timeout @ wout_timeout
  let special = arinc_special @ others
  open ArincUtil
  (* possible return code classes *)
  let ret_success = [NO_ERROR; NO_ACTION]
  let ret_error   = [NOT_AVAILABLE; INVALID_PARAM; INVALID_CONFIG; INVALID_MODE; TIMED_OUT]
  let ret_any     = ret_success @ ret_error
  let ret_no_timeout = List.remove ret_any TIMED_OUT
  (* abstract value for return codes *)
  (* TODO: Check whether Cil.IInt is correct here *)
  let vd ret = `Int (ValueDomain.ID.(List.map (of_int Cil.IInt % IntOps.BigIntOps.of_int % return_code_to_enum) ret |> List.fold_left join (bot ()))) (* ana.int.enums should be enabled *)
  let effects fname args =
    if not (List.mem fname arinc_special) || List.is_empty args then None
    else
      match List.last args with
      | AddrOf lv ->
        Some (fun set ->
            let ret = if GobConfig.get_bool "ana.arinc.assume_success" then ret_success else if List.mem fname with_timeout then ret_any else ret_no_timeout in
            let v = vd ret in
            debug_doc @@ Pretty.dprintf "effect of %s: set %a to %a" fname d_lval lv ValueDomain.Compound.pretty v;
            set lv v
          )
      | _ -> None
end

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  let name () = "arinc"

  let init () =
    LibraryFunctions.add_lib_funs Functions.special;
    LibraryFunctions.add_effects Functions.effects

  (* ARINC types and Hashtables for collecting CFG *)
  type id = varinfo
  type resource = ArincUtil.resource
  (* lookup/generate id from resource type and name (needed for LAP_Se_GetXId functions; specified by LAP_Se_CreateX functions during init) *)
  (* map from tuple (resource, name) to varinfo (need to be saved b/c makeGlobalVar x t <> makeGlobalVar x t) *)
  let resources = Hashtbl.create 13
  let get_id (resource,name as k:resource*string) : id =
    try Hashtbl.find resources k
    with Not_found ->
      let vname = ArincUtil.show_resource resource^":"^name in
      let v = Goblintutil.create_var (makeGlobalVar vname voidPtrType) in
      Hashtbl.replace resources k v;
      v
  let get_by_id (id:id) : (resource*string) option =
    Hashtbl.filter ((=) id) resources |> Hashtbl.keys |> Enum.get
  let get_name_by_id id = get_by_id id |> Option.get |> snd

  (* map process name to integer used in Pid domain *)
  let pnames = Hashtbl.create 13
  let _ = Hashtbl.add pnames "mainfun" 0L
  let get_by_pid pid =
    Hashtbl.filter ((=) pid) pnames |> Hashtbl.keys |> Enum.get
  let get_pid pname =
    try Hashtbl.find pnames pname
    with Not_found ->
      let ids = Hashtbl.values pnames in
      let id = if Enum.is_empty ids then 1L else Int64.succ (Enum.arg_max identity ids) in
      Hashtbl.replace pnames pname id;
      id
  let get_pid_by_id id = get_by_id id |> Option.get |> snd |> get_pid


  (* Domains *)
  include ArincDomain

  module Tasks = SetDomain.Make (Lattice.Prod (Queries.LS) (ArincDomain.D)) (* set of created tasks to spawn when going multithreaded *)
  module G = Tasks
  module C = D

  let sprint_map f xs = String.concat ", " @@ List.map (sprint f) xs

  let context d = { d with pred = Pred.bot (); ctx = Ctx.bot () }
  (* let val_of d = d *)

  (* function for creating a new intermediate node (will generate a new sid every time!) *)
  let mkDummyNode ?loc line =
    let loc = { (loc |? !Tracing.current_loc) with line = line } in
    MyCFG.Statement { (mkStmtOneInstr @@ Set (var dummyFunDec.svar, zero, loc)) with sid = new_sid () }
  (* table from sum type to negative line number for new intermediate node (-1 to -4 have special meanings) *)
  type tmpNodeUse = Branch of stmt | Combine of lval
  module NodeTbl = ArincUtil.SymTbl (struct type k = tmpNodeUse type v = MyCFG.node let getNew xs = mkDummyNode @@ -5 - (List.length (List.of_enum xs)) end)
  (* context hash to differentiate function calls *)
  module CtxTbl = ArincUtil.SymTbl (struct type k = int type v = int let getNew xs = if Enum.is_empty xs then 0 else (Enum.arg_max identity xs)+1 end) (* generative functor *)
  let fname_ctx ctx f = f.vname ^ "_" ^ (match Ctx.to_int ctx with Some i -> i |> i64_to_int |> CtxTbl.get |> string_of_int | None -> "TOP")

  let is_single ctx =
    not (ThreadFlag.is_multi ctx.ask)
  let tasks_var = Goblintutil.create_var (makeGlobalVar "__GOBLINT_ARINC_TASKS" voidPtrType)
  let is_mainfun name = List.mem name (List.map Json.string (GobConfig.get_list "mainfun"))

  type env = { d: D.t; node: MyCFG.node; fundec: fundec; pname: string; procid: ArincUtil.id; id: ArincUtil.id }
  let get_env ctx =
    let open ArincUtil in let _ = 42 in
    let d = ctx.local in
    let node = Option.get !MyCFG.current_node in
    (* determine if we are at the root of a process or in some called function *)
    let fundec = MyCFG.getFun node in
    let curpid = match Pid.to_int d.pid with Some i -> i | None -> failwith @@ "get_env: Pid.to_int = None inside function "^fundec.svar.vname^". State: " ^ D.short 100 d in
    let pname = match get_by_pid curpid with Some s -> s | None -> failwith @@ "get_env: no processname for pid in Hashtbl!" in
    let procid = Process, pname in
    let pfuns = funs_for_process (Process,pname) in
    let id = if List.exists ((=) fundec.svar) pfuns || is_mainfun fundec.svar.vname then Process, pname else Function, fname_ctx d.ctx fundec.svar in
    { d; node; fundec; pname; procid; id }
  let add_edges ?r ?dst ?d env action =
    Pred.iter (fun node -> ArincUtil.add_edge env.id (node, action, r, MyCFG.getLoc (dst |? env.node))) (d |? env.d).pred
  let add_actions env xs =
    (* add edges for all predecessor nodes (from pred. node to env.node) *)
    List.iter (fun (action,r) -> match r with Some r -> add_edges ~r env action | None -> add_edges env action) xs;
    (* update domain by replacing the set of pred. nodes with the current node *)
    if List.is_empty xs then env.d else D.pred (const @@ Pred.of_node env.node) env.d
  (* is exp of the return code type (pointers are not considered!) *)
  let is_return_code_type exp = typeOf exp |> unrollTypeDeep |> function
    | TEnum(ei, _) when ei.ename = "T13" -> true
    | _ -> false
  let return_code_is_success = function 0L | 1L -> true | _ -> false
  let str_return_code i = if return_code_is_success i then "SUCCESS" else "ERROR"
  let str_return_dlval (v,o as dlval) =
    sprint d_lval (Lval.CilLval.to_lval dlval) ^ "_" ^ string_of_int v.vdecl.line |>
    Str.global_replace (Str.regexp "[^a-zA-Z0-9]") "_"
  let add_return_dlval env kind dlval =
    ArincUtil.add_return_var env.procid kind (str_return_dlval dlval)
  let dummy_global_dlval = { dummyFunDec.svar with vname = "Gret" }, `NoOffset
  let global_dlval dlval fname =
    if Lval.CilLval.class_tag dlval = `Global then (
      M.debug_each @@ "WARN: " ^ fname ^ ": use of global lval: " ^ str_return_dlval dlval;
      if GobConfig.get_bool "ana.arinc.merge_globals" then dummy_global_dlval else dlval
    ) else dlval
  let mayPointTo ctx exp =
    match ctx.ask (Queries.MayPointTo exp) with
    | `LvalSet a when not (Queries.LS.is_top a) && Queries.LS.cardinal a > 0 ->
      let top_elt = (dummyFunDec.svar, `NoOffset) in
      let a' = if Queries.LS.mem top_elt a then (
          M.debug_each @@ "mayPointTo: query result for " ^ sprint d_exp exp ^ " contains TOP!"; (* UNSOUND *)
          Queries.LS.remove top_elt a
        ) else a
      in
      Queries.LS.elements a'
    | `Bot -> []
    | v ->
      M.debug_each @@ "mayPointTo: query result for " ^ sprint d_exp exp ^ " is " ^ sprint Queries.Result.pretty v;
      (*failwith "mayPointTo"*)
      []
  let mustPointTo ctx exp = let xs = mayPointTo ctx exp in if List.length xs = 1 then Some (List.hd xs) else None
  let iterMayPointTo ctx exp f = mayPointTo ctx exp |> List.iter f
  let debugMayPointTo ctx exp = M.debug_each @@ sprint d_exp exp ^ " mayPointTo " ^ (String.concat ", " (List.map (sprint Lval.CilLval.pretty) (mayPointTo ctx exp)))


  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    (* if the lhs is (or points to) a return code variable, then:
        if the rhs is a constant or another return code, output an assignment edge
        else output edge that non-det. sets the lhs to a value from the range *)
    (* things to note:
       1. is_return_code_type must be checked on the result of mayPointTo and not on lval! otherwise we have problems with pointers
       2. Cil.typeOf throws an exception because the base type of a query result from goblint is not an array but is accessed with an index TODO why is this? ignored casts?
       3. mayPointTo also returns pointers if there's a top element in the set (outputs warning), so that we don't miss anything *)
    if GobConfig.get_bool "ana.arinc.assume_success" then ctx.local else
      (* OPT: this matching is just for speed up to avoid querying on every assign *)
      match lval with Var _, _ when not @@ is_return_code_type (Lval lval) -> ctx.local | _ ->
        (* TODO why is it that current_node can be None here, but not in other transfer functions? *)
        if not @@ Option.is_some !MyCFG.current_node then (M.debug_each "assign: MyCFG.current_node not set :("; ctx.local) else
        if D.is_bot1 ctx.local then ctx.local else
          let env = get_env ctx in
          let edges_added = ref false in
          let f dlval =
            (* M.debug_each @@ "assign: MayPointTo " ^ sprint d_plainlval lval ^ ": " ^ sprint d_plainexp (Lval.CilLval.to_exp dlval); *)
            let is_ret_type = try is_return_code_type @@ Lval.CilLval.to_exp dlval with _ -> M.debug_each @@ "assign: Cil.typeOf "^ sprint d_exp (Lval.CilLval.to_exp dlval) ^" threw exception Errormsg.Error \"Bug: typeOffset: Index on a non-array\". Will assume this is a return type to remain sound."; true in
            if (not is_ret_type) || Lval.CilLval.has_index dlval then () else
              let dlval = global_dlval dlval "assign" in
              edges_added := true;
              add_return_dlval env `Write dlval;
              let add_one str_rhs = add_edges env @@ ArincUtil.Assign (str_return_dlval dlval, str_rhs) in
              let add_top () = add_edges ~r:(str_return_dlval dlval) env @@ ArincUtil.Nop in
              match stripCasts rval with
              | Const CInt64(i,_,_) -> add_one @@ str_return_code i
              (*       | Lval rlval ->
                        iterMayPointTo ctx (AddrOf rlval) (fun rdlval -> add_return_dlval env `Read rdlval; add_one @@ str_return_dlval rdlval) *)
              | _ -> add_top ()
          in
          iterMayPointTo ctx (AddrOf lval) f;
          if !edges_added then D.pred (const @@ Pred.of_node env.node) env.d else env.d

  let branch ctx (exp:exp) (tv:bool) : D.t =
    (* ignore(printf "if %a = %B (line %i)\n" d_plainexp exp tv (!Tracing.current_loc).line); *)
    (* only if we don't assume success, we need to consider branching on return codes *)
    if D.is_bot1 ctx.local then ctx.local else (* TODO how can it be that everything is bot here, except pred? *)
      let env = get_env ctx in
      if GobConfig.get_bool "ana.arinc.assume_success" then ctx.local else
        let check a b tv =
          (* we are interested in a comparison between some lval lval (which has the type of the return code enum) and a value of that enum (which gets converted to an Int by CIL) *)
          match a, b with
          | Lval lval, Const CInt64(i,_,_)
          | Const CInt64(i,_,_), Lval lval when is_return_code_type (Lval lval) ->
            (* let success = return_code_is_success i = tv in (* both must be true or false *) *)
            (* ignore(printf "if %s: %a = %B (line %i)\n" (if success then "success" else "error") d_plainexp exp tv (!Tracing.current_loc).line); *)
            (match env.node with
             | MyCFG.Statement({ skind = If(e, bt, bf, loc); _ } as stmt) ->
               (* 1. write out edges to predecessors, 2. set predecessors to current node, 3. write edge to the first node of the taken branch and set it as predecessor *)
               (* the then-block always has some stmts, but the else-block might be empty! in this case we use the successors of the if instead. *)
               let then_node = NodeTbl.get @@ Branch (List.hd bt.bstmts) in
               let else_stmts = if List.is_empty bf.bstmts then stmt.succs else bf.bstmts in
               let else_node = NodeTbl.get @@ Branch (List.hd else_stmts) in
               let dst_node = if tv then then_node else else_node in
               let d_if = if List.length stmt.preds > 1 then ( (* seems like this never happens *)
                   M.debug_each @@ "WARN: branch: If has more than 1 predecessor, will insert Nop edges!";
                   add_edges env ArincUtil.Nop;
                   { ctx.local with pred = Pred.of_node env.node }
                 ) else ctx.local
               in
               (* now we have to add Pos/Neg-edges (depending on tv) for everything v may point to *)
               let f dlval =
                 if Lval.CilLval.has_index dlval then () else
                   let dlval = global_dlval dlval "branch" in
                   let str_dlval = str_return_dlval dlval in
                   let cond = str_dlval ^ " == " ^ str_return_code i in
                   let cond = if tv then cond else "!(" ^ cond ^ ")" in
                   let cond = if dlval = dummy_global_dlval || String.exists str_dlval "int___unknown" then "true" else cond in (* we don't know the index of the array -> assume that branch could always be taken *)
                   add_edges ~dst:dst_node ~d:d_if env (ArincUtil.Cond (str_dlval, cond));
                   add_return_dlval env `Read dlval
               in
               iterMayPointTo ctx (AddrOf lval) f;
               { ctx.local with pred = Pred.of_node dst_node }
             | _ -> failwith "branch: current_node is not an If") (* this should never happen since CIL transforms switch *)
          | _ -> ctx.local
        in
        match exp with
        (* TODO limit to BinOp Eq/Ne? Could also be other type of expression! *)
        | BinOp(Eq, a, b, _) -> check (stripCasts a) (stripCasts b) tv
        | BinOp(Ne, a, b, _) -> check (stripCasts a) (stripCasts b) (not tv)
        | _ -> ctx.local

  let checkPredBot d tf f xs =
    if d.pred = Pred.bot () then M.debug_each @@ tf^": mapping is BOT!!! function: "^f.vname^". "^(String.concat "\n" @@ List.map (fun (n,d) -> n ^ " = " ^ Pretty.sprint 200 (Pred.pretty () d.pred)) xs);
    d

  let body ctx (f:fundec) : D.t = (* enter is not called for spawned processes -> initialize them here *)
    (* M.debug_each @@ "BODY " ^ f.svar.vname ^" @ "^ string_of_int (!Tracing.current_loc).line; *)
    (* if not (is_single ctx || !Goblintutil.global_initialization || fst (ctx.global part_mode_var)) then raise Analyses.Deadcode; *)
    (* checkPredBot ctx.local "body" f.svar [] *)
    let base_context = Base.Main.context_cpa @@ Obj.obj @@ List.assoc "base" ctx.presub in
    let context_hash = Hashtbl.hash (base_context, ctx.local.pid) in
    { ctx.local with ctx = Ctx.of_int (Int64.of_int context_hash) }

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list = (* on function calls (also for main); not called for spawned processes *)
    (* print_endline @@ "ENTER " ^ f.vname ^" @ "^ string_of_int (!Tracing.current_loc).line; (* somehow M.debug_each doesn't print anything here *) *)
    let d_caller = ctx.local in
    let d_callee = if D.is_bot ctx.local then ctx.local else { ctx.local with pred = Pred.of_node (MyCFG.Function f); ctx = Ctx.top () } in (* set predecessor set to start node of function *)
    [d_caller, d_callee]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t =
    if D.is_bot1 ctx.local || D.is_bot1 au then ctx.local else
      let env = get_env ctx in
      let d_caller = ctx.local in
      let d_callee = au in
      (* check if the callee has some relevant edges, i.e. advanced from the entry point. if not, we generate no edge for the call and keep the predecessors from the caller *)
      if Pred.is_bot d_callee.pred then failwith "d_callee.pred is bot!"; (* set should never be empty *)
      if Pred.equal d_callee.pred (Pred.of_node (MyCFG.Function f)) then
        { d_callee with pred = d_caller.pred; ctx = d_caller.ctx }
      else (
        (* write out edges with call to f coming from all predecessor nodes of the caller *)
        (* if Option.is_some !last_ctx_hash && current_ctx_hash () = string_of_int (Option.get !last_ctx_hash) then *)
        if Ctx.is_int d_callee.ctx then (
          (* before doing the call we need to assign call-by-value return code parameters
             we only need to take care of them and not AddrOf args since those are found by query *)
          (* TODO optimally we would track if the caller_exp was used as a return code in an ARINC call before;
             as a first step we should check if its value is top (if it's set to some value and not invalidated by a call, then we are not interested in creating branches for it) *)
          let check (callee_var,caller_exp) = match stripCasts caller_exp with
            | Lval lval when is_return_code_type (Lval (var callee_var)) -> Some (callee_var, lval)
            | _ -> None
          in
          let rargs = List.combine (Cilfacade.getdec f).sformals args |> List.filter_map check in
          let assign pred dst_node callee_var caller_dlval =
            let caller_dlval = global_dlval caller_dlval "combine" in
            let callee_dlval = callee_var, `NoOffset in
            (* add edge to an intermediate node that assigns the caller return code to the one of the function params *)
            add_edges ~dst:dst_node ~d:{ d_caller with pred = pred } env (ArincUtil.Assign (str_return_dlval callee_dlval, str_return_dlval caller_dlval));
            (* we also need to add the callee param as a `Write lval so that we see that it is written to *)
            add_return_dlval env `Write callee_dlval;
            (* also add the caller param because it is read *)
            add_return_dlval env `Read caller_dlval;
          in
          (* we need to assign all lvals each caller arg may point to *)
          let last_pred = if GobConfig.get_bool "ana.arinc.assume_success" then d_caller.pred else List.fold_left (fun pred (callee_var,caller_lval) -> let dst_node = NodeTbl.get (Combine caller_lval) in iterMayPointTo ctx (AddrOf caller_lval) (assign pred dst_node callee_var); Pred.of_node dst_node) d_caller.pred rargs in
          add_edges ~d:{ d_caller with pred = last_pred } env (ArincUtil.Call (fname_ctx d_callee.ctx f))
        );
        (* set current node as new predecessor, since something interesting happend during the call *)
        { d_callee with pred = Pred.of_node env.node; ctx = d_caller.ctx }
      )

  (* ARINC utility functions *)
  let mode_is_init  i = match Pmo.to_int i with Some 1L | Some 2L -> true | _ -> false
  let mode_is_multi i = Pmo.to_int i = Some 3L
  let pname_ErrorHandler = "ErrorHandler"

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let open ArincUtil in let _ = 42 in (* sublime's syntax highlighter gets confused without the second let... *)
    let d : D.t = ctx.local in
    if D.is_bot1 d then d else
      let env = get_env ctx in
      let is_arinc_fun = startsWith Functions.prefix f.vname in
      let is_creating_fun = startsWith (Functions.prefix^"Create") f.vname in
      if M.tracing && is_arinc_fun then (
        let args_str = String.concat ", " (List.map (sprint d_exp) arglist) in
        (* M.tracel "arinc" "found %s(%s)\n" f.vname args_str *)
        M.debug_each @@ "found "^f.vname^"("^args_str^") in "^env.fundec.svar.vname
      );
      let is_error_handler = env.pname = pname_ErrorHandler in
      let eval_int exp =
        match ctx.ask (Queries.EvalInt exp) with
        | `Int i -> i
        | _ -> failwith @@ "Could not evaluate int-argument "^sprint d_plainexp exp^" in "^f.vname
      in
      let eval_str exp =
        match ctx.ask (Queries.EvalStr exp) with
        | `Str s -> s
        | _ -> failwith @@ "Could not evaluate string-argument "^sprint d_plainexp exp^" in "^f.vname
      in
      let eval_id exp = mayPointTo ctx exp |> List.map (Option.get % get_by_id % fst) in
      let assign_id exp id =
        match exp with
        (* call assign for all analyses (we only need base)! *)
        | AddrOf lval -> ctx.assign ~name:"base" lval (mkAddrOf @@ var id)
        (* TODO not needed for the given code, but we could use Queries.MayPointTo exp in this case *)
        | _ -> failwith @@ "Could not assign id. Expected &id. Found "^sprint d_exp exp
      in
      let assign_id_by_name resource_type name id =
        assign_id id (get_id (resource_type, eval_str name))
      in
      let arglist = List.map (stripCasts%(constFold false)) arglist in
      (* if assume_success we set the return code and don't need to do anything else
         otherwise we need to invalidate the return code (TODO maybe not top but sth more precise) and include the return code variable for all arinc calls (so that it can be set non-deterministically in promela) *)
      let add_actions actions =
        if is_arinc_fun && not @@ List.is_empty arglist then
          let r = List.last arglist in
          if GobConfig.get_bool "ana.arinc.assume_success" then (
            add_actions env @@ List.map (fun action -> action, None) actions
          ) else (
            let xs = mayPointTo ctx r in
            (* warn about wrong type (r should always be a return code) and setting globals! *)
            let f dlval =
              let dlval = global_dlval dlval "special" in
              if not @@ is_return_code_type @@ Lval.CilLval.to_exp dlval
              then (M.debug_each @@ "WARN: last argument in arinc function may point to something other than a return code: " ^ str_return_dlval dlval; None)
              else (add_return_dlval env `Write dlval; Some (str_return_dlval dlval))
            in
            (* add actions for all lvals r may point to *)
            let rets = List.map Option.some @@ List.filter_map f xs in
            add_actions env @@ List.cartesian_product actions rets
          )
        else (* no args *)
          add_actions env @@ List.map (fun action -> action,None) actions
      in
      let add_action action = add_actions [action] in
      (*
         let assert_ptr e = match unrollType (typeOf e) with
           | TPtr _ -> ()
           | _ -> failwith @@ f.vname ^ " expects arguments to be some pointer, but got "^sprint d_exp e^" which is "^sprint d_plainexp e
         in
      *)
      let todo () = if false then failwith @@ f.vname^": Not implemented yet!" else add_action Nop in
      match f.vname, arglist with
      | _ when is_arinc_fun && is_creating_fun && not(mode_is_init d.pmo) ->
        failwith @@ f.vname^" is only allowed in partition mode COLD_START or WARM_START"
      (* Preemption *)
      | "LAP_Se_LockPreemption", [lock_level; r] when not is_error_handler ->
        add_action LockPreemption
        |> D.pre (PrE.add (PrE.of_int 1L))
      | "LAP_Se_UnlockPreemption", [lock_level; r] when not is_error_handler ->
        add_action UnlockPreemption
        |> D.pre (PrE.sub (PrE.of_int 1L))
      (* Partition *)
      | "LAP_Se_SetPartitionMode", [mode; r] -> begin
          match ctx.ask (Queries.EvalInt mode) with
          | `Int i ->
            let pm = partition_mode_of_enum @@ Int64.to_int i in
            if M.tracing then M.tracel "arinc" "setting partition mode to %Ld (%s)\n" i (show_partition_mode_opt pm);
            if mode_is_multi (Pmo.of_int i) then (
              let tasks = ctx.global tasks_var in
              ignore @@ printf "arinc: SetPartitionMode NORMAL: spawning %i processes!\n" (Tasks.cardinal tasks);
              Tasks.iter (fun (fs,f_d) -> Queries.LS.iter (fun f -> ctx.spawn None (fst f) []) fs) tasks;
            );
            add_action (SetPartitionMode pm)
            |> D.pmo (const @@ Pmo.of_int i)
          | `Bot -> failwith "DEAD"
          | _ -> D.top ()
        end
      | "LAP_Se_GetPartitionStatus", [status; r] -> todo () (* != mode *)
      | "LAP_Se_GetPartitionStartCondition", [start_condition; r] -> todo ()
      (* treat functions from string.h as extern if they are added at the end of libraryFunctions.ml *)
      (*
      | "F59", [dst; src] (* strcpy: stops at \000 in src *)
      | "F60", [dst; src; _] (* strncpy: stops at \000 in src or if len has been copied. if src ends before len, dst is padded with zeros. TODO len *)
        (* | "F61", [str1; str2] (* strcmp: compares chars until they differ or \000 is reached. returns c1 - c2  *) *)
        (* | "F62", [dst; src; len] (* strncmp *) *)
        (* | "F63", [dst; src; len] (* memcpy *) *)
        ->
        M.debug_each @@ "strcpy/"^f.vname^"("^sprint d_plainexp dst^", "^sprint d_plainexp src^")";
        (*debugMayPointTo ctx dst;*)
        assert_ptr dst; assert_ptr src;
        (* let dst_lval = mkMem ~addr:dst ~off:NoOffset in *)
        (* let src_expr = Lval (mkMem ~addr:src ~off:NoOffset) in *)
        begin match ctx.ask (Queries.MayPointTo dst) with
        | `LvalSet ls ->
            ignore @@ Pretty.printf "strcpy %a points to %a\n" d_exp dst Queries.LS.pretty ls;
            Queries.LS.iter (fun (v,o) -> ctx.assign ~name:"base" (Var v, Lval.CilLval.to_ciloffs o) src) ls
        | _ -> M.debug_each @@ "strcpy/"^f.vname^"("^sprint d_plainexp dst^", "^sprint d_plainexp src^"): dst may point to anything!";
        end;
        d
      | "F63" , [dst; src; len] (* memcpy *)
        ->
        M.debug_each @@ "memcpy/"^f.vname^"("^sprint d_plainexp dst^", "^sprint d_plainexp src^")";
        (match ctx.ask (Queries.EvalInt len) with
         | `Int i ->
           (*
             let len = i64_to_int @@ eval_int len in
             for i = 0 to len-1 do
               let dst_lval = mkMem ~addr:dst ~off:(Index (integer i, NoOffset)) in
               let src_lval = mkMem ~addr:src ~off:(Index (integer i, NoOffset)) in
               ctx.assign ~name:"base" dst_lval (Lval src_lval);
             done;
           *)
           assert_ptr dst; assert_ptr src;
           let dst_lval = mkMem ~addr:dst ~off:NoOffset in
           let src_lval = mkMem ~addr:src ~off:NoOffset in
           ctx.assign ~name:"base" dst_lval (Lval src_lval); (* this is only ok because we use ArrayDomain.Trivial per default, i.e., there's no difference between the first element or the whole array *)
         | v -> M.debug_each @@ "F63/memcpy: don't know length: " ^ sprint Queries.Result.pretty v;
           let lval = mkMem ~addr:dst ~off:NoOffset in
           ctx.assign ~name:"base" lval MyCFG.unknown_exp
        );
        M.debug_each @@ "done with memcpy/"^f.vname;
        d
      | "F1" , [dst; data; len] (* memset: write char to dst len times *)
        ->
        (match ctx.ask (Queries.EvalInt len) with
         | `Int i ->
           (*
             let len = i64_to_int @@ eval_int len in
             for i = 0 to len-1 do
               let lval = mkMem ~addr:dst ~off:(Index (integer i, NoOffset)) in
               ctx.assign ~name:"base" lval data;
             done;
           *)
           let dst_lval = mkMem ~addr:dst ~off:NoOffset in
           ctx.assign ~name:"base" dst_lval data; (* this is only ok because we use ArrayDomain.Trivial per default, i.e., there's no difference between the first element or the whole array *)
         | v -> M.debug_each @@ "F1/memset: don't know length: " ^ sprint Queries.Result.pretty v;
           let lval = mkMem ~addr:dst ~off:NoOffset in
           ctx.assign ~name:"base" lval MyCFG.unknown_exp
        );
        d
        *)
      (* Processes *)
      | "LAP_Se_CreateProcess", [AddrOf attr; pid; r] ->
        let cm = match unrollType (typeOfLval attr) with
          | TComp (c,_) -> c
          | _ -> failwith "type-error: first argument of LAP_Se_CreateProcess not a struct."
        in
        let struct_fail f x =
          f @@ "LAP_Se_CreateProcess: problem with first argument: " ^
               begin match x with
                 | `Field ofs -> "cannot access field " ^ ofs
                 | `Result (name, entry_point, pri, per, cap) ->
                   "struct PROCESS_ATTRIBUTE_TYPE needs all of the following fields (with result): NAME ("^name^"), ENTRY_POINT ("^entry_point^"), BASE_PRIORITY ("^pri^"), PERIOD ("^per^"), TIME_CAPACITY ("^cap^")"
               end ^ ". Running scrambled: "^string_of_bool Goblintutil.scrambled
        in
        let field ofs =
          try Lval (addOffsetLval (Field (getCompField cm ofs, NoOffset)) attr)
          with Not_found -> struct_fail failwith (`Field ofs)
        in
        let name = ctx.ask (Queries.EvalStr (field Goblintutil.arinc_name)) in
        let entry_point = ctx.ask (Queries.ReachableFrom (AddrOf attr)) in
        let pri  = ctx.ask (Queries.EvalInt (field Goblintutil.arinc_base_priority)) in
        let per  = ctx.ask (Queries.EvalInt (field Goblintutil.arinc_period)) in
        let cap  = ctx.ask (Queries.EvalInt (field Goblintutil.arinc_time_capacity)) in
        begin match name, entry_point, pri, per, cap with
          | `Str name, `LvalSet ls, `Int pri, `Int per, `Int cap when not (Queries.LS.is_top ls)
                                                                   && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) ls) ->
            let funs_ls = Queries.LS.filter (fun (v,o) -> let lval = Var v, Lval.CilLval.to_ciloffs o in isFunctionType (typeOfLval lval)) ls in (* do we need this? what happens if we spawn a variable that's not a function? shouldn't this check be in spawn? *)
            if M.tracing then M.tracel "arinc" "starting a thread %a with priority '%Ld' \n" Queries.LS.pretty funs_ls pri;
            let funs = funs_ls |> Queries.LS.elements |> List.map fst |> List.unique in
            let f_d = { pid = Pid.of_int (get_pid name); pri = Pri.of_int pri; per = Per.of_int per; cap = Cap.of_int cap; pmo = Pmo.of_int 3L; pre = PrE.top (); pred = Pred.of_node (MyCFG.Function f); ctx = Ctx.top () } in
            let tasks = Tasks.add (funs_ls, f_d) (ctx.global tasks_var) in
            ctx.sideg tasks_var tasks;
            let pid' = Process, name in
            assign_id pid (get_id pid');
            add_actions (List.map (fun f -> CreateProcess Action.({ pid = pid'; f; pri; per; cap })) funs)
          | _ -> let f = Queries.Result.short 30 in struct_fail M.debug_each (`Result (f name, f entry_point, f pri, f per, f cap)); d
        end
      | "LAP_Se_GetProcessId", [name; pid; r] ->
        assign_id_by_name Process name pid; d
      | "LAP_Se_GetProcessStatus", [pid; status; r] -> todo ()
      | "LAP_Se_GetMyId", [pid; r] ->
        assign_id pid (get_id env.procid); d
      | "LAP_Se_Start", [pid; r] ->
        (* at least one process should be started in main *)
        add_actions @@ List.map (fun pid -> Start pid) (eval_id pid)
      | "LAP_Se_DelayedStart", [pid; delay; r] -> todo ()
      | "LAP_Se_Stop", [pid; r] ->
        add_actions @@ List.map (fun pid -> Stop pid) (eval_id pid)
      | "LAP_Se_StopSelf", [] ->
        if mode_is_init d.pmo then failwith @@ "The behavior of " ^ f.vname ^ " is not defined in WARM_START/COLD_START!";
        add_action (Stop env.procid)
      | "LAP_Se_Suspend", [pid; r] ->
        add_actions @@ List.map (fun pid -> Suspend pid) (eval_id pid)
      | "LAP_Se_SuspendSelf", [timeout; r] ->
        let t = eval_int timeout in
        add_action (SuspendSelf (env.procid, t))
      | "LAP_Se_Resume", [pid; r] ->
        add_actions @@ List.map (fun pid -> Resume pid) (eval_id pid)
      (* Logbook - not used *)
      | "LAP_Se_CreateLogBook", [name; max_size; max_logged; max_in_progress; lbid; r] -> todo ()
      | "LAP_Se_ReadLogBook", _ -> todo ()
      | "LAP_Se_WriteLogBook", _ -> todo ()
      | "LAP_Se_ClearLogBook", _ -> todo ()
      | "LAP_Se_GetLogBookId", _ -> todo ()
      | "LAP_Se_GetLogBookStatus", _ -> todo ()
      (* SamplingPort - inter-partition *)
      | "LAP_Se_CreateSamplingPort", [name; max_size; dir; period; spid; r] -> todo ()
      | "LAP_Se_WriteSamplingMessage", _ -> todo ()
      | "LAP_Se_ReadSamplingMessage", _ -> todo ()
      | "LAP_Se_GetSamplingPortId", _ -> todo ()
      | "LAP_Se_GetSamplingPortStatus", _ -> todo ()
      (* QueuingPort - inter-partition *)
      | "LAP_Se_CreateQueuingPort", [name; max_size; max_range; dir; queuing; qpid; r] -> todo ()
      | "LAP_Se_SendQueuingMessage", _ -> todo ()
      | "LAP_Se_ReceiveQueuingMessage", _ -> todo ()
      | "LAP_Se_GetQueuingPortId", _ -> todo ()
      | "LAP_Se_GetQueuingPortStatus", _ -> todo ()
      (* Buffer - not used *)
      | "LAP_Se_CreateBuffer", [name; max_size; max_range; queuing; buid; r] -> todo ()
      | "LAP_Se_SendBuffer", _ -> todo ()
      | "LAP_Se_ReceiveBuffer", _ -> todo ()
      | "LAP_Se_GetBufferId", _ -> todo ()
      | "LAP_Se_GetBufferStatus", _ -> todo ()
      (* Blackboard *)
      | "LAP_Se_CreateBlackboard", [name; max_size; bbid; r] ->
        let bbid' = Blackboard, eval_str name in
        assign_id bbid (get_id bbid');
        add_action (CreateBlackboard bbid')
      | "LAP_Se_DisplayBlackboard", [bbid; msg_addr; len; r] ->
        add_actions @@ List.map (fun id -> DisplayBlackboard id) (eval_id bbid)
      | "LAP_Se_ReadBlackboard", [bbid; timeout; msg_addr; len; r] ->
        let t = eval_int timeout in
        add_actions @@ List.map (fun id -> ReadBlackboard (id, t)) (eval_id bbid)
      | "LAP_Se_ClearBlackboard", [bbid; r] ->
        add_actions @@ List.map (fun id -> ClearBlackboard id) (eval_id bbid)
      | "LAP_Se_GetBlackboardId", [name; bbid; r] ->
        assign_id_by_name Blackboard name bbid; d
      | "LAP_Se_GetBlackboardStatus", _ -> todo ()
      (* Semaphores *)
      | "LAP_Se_CreateSemaphore", [name; cur; max; queuing; sid; r] ->
        (* create resource for name *)
        let sid' = Semaphore, eval_str name in
        assign_id sid (get_id sid');
        add_action (CreateSemaphore Action.({ sid = sid'; cur = eval_int cur; max = eval_int max; queuing = eval_int queuing }))
      | "LAP_Se_WaitSemaphore", [sid; timeout; r] -> (* TODO timeout *)
        let t = eval_int timeout in
        add_actions @@ List.map (fun id -> WaitSemaphore (id, t)) (eval_id sid)
      | "LAP_Se_SignalSemaphore", [sid; r] ->
        add_actions @@ List.map (fun id -> SignalSemaphore id) (eval_id sid)
      | "LAP_Se_GetSemaphoreId", [name; sid; r] ->
        assign_id_by_name Semaphore name sid; d
      | "LAP_Se_GetSemaphoreStatus", [sid; status; r] -> todo ()
      (* Events (down after create/reset, up after set) *)
      | "LAP_Se_CreateEvent", [name; eid; r] ->
        let eid' = Event, eval_str name in
        assign_id eid (get_id eid');
        add_action (CreateEvent eid')
      | "LAP_Se_SetEvent", [eid; r] ->
        add_actions @@ List.map (fun id -> SetEvent id) (eval_id eid)
      | "LAP_Se_ResetEvent", [eid; r] ->
        add_actions @@ List.map (fun id -> ResetEvent id) (eval_id eid)
      | "LAP_Se_WaitEvent", [eid; timeout; r] -> (* TODO timeout *)
        let t = eval_int timeout in
        add_actions @@ List.map (fun id -> WaitEvent (id, t)) (eval_id eid)
      | "LAP_Se_GetEventId", [name; eid; r] ->
        assign_id_by_name Event name eid; d
      | "LAP_Se_GetEventStatus", [eid; status; r] -> todo ()
      (* Time *)
      | "LAP_Se_GetTime", [time; r] -> todo ()
      | "LAP_Se_TimedWait", [delay; r] ->
        add_action (TimedWait (eval_int delay))
      | "LAP_Se_PeriodicWait", [r] ->
        add_action PeriodicWait
      (* Errors *)
      | "LAP_Se_CreateErrorHandler", [entry_point; stack_size; r] ->
        begin match ctx.ask (Queries.ReachableFrom (entry_point)) with
          | `LvalSet ls when not (Queries.LS.is_top ls) && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) ls) ->
            let pid = get_pid pname_ErrorHandler in
            let funs_ls = Queries.LS.filter (fun (v,o) -> let lval = Var v, Lval.CilLval.to_ciloffs o in isFunctionType (typeOfLval lval)) ls in
            let funs = funs_ls |> Queries.LS.elements |> List.map fst |> List.unique in
            let f_d = { pid = Pid.of_int pid; pri = Pri.of_int infinity; per = Per.of_int infinity; cap = Cap.of_int infinity; pmo = Pmo.of_int 3L; pre = PrE.top (); pred = Pred.of_node (MyCFG.Function f); ctx = Ctx.top () } in
            let tasks = Tasks.add (funs_ls, f_d) (ctx.global tasks_var) in
            ctx.sideg tasks_var tasks;
            add_actions (List.map (fun f -> CreateErrorHandler ((Process, pname_ErrorHandler), f)) funs)
          | _ -> failwith @@ "CreateErrorHandler: could not find out which functions are reachable from first argument!"
        end
      | "LAP_Se_GetErrorStatus", [status; r] -> todo ()
      | "LAP_Se_RaiseApplicationError", [error_code; message_addr; length; r] -> todo ()
      (* Not allowed: change configured schedule *)
      | "LAP_Se_SetPriority", [pid; prio; r] -> todo ()
      | "LAP_Se_Replenish", [budget; r] -> todo () (* name used in docs *)
      | "LAP_Se_ReplenishAperiodic", [budget; r] -> todo () (* name used in stdapi.c *)
      | _ when is_arinc_fun -> failwith @@ "Function "^f.vname^" not handled!"
      | _ -> d

  let query ctx (q:Queries.t) : Queries.Result.t =
    let d = ctx.local in
    match q with
    | Queries.Priority _ ->
      if Pri.is_int d.pri then
        `Int (Option.get @@ Pri.to_int d.pri)
      else if Pri.is_top d.pri then `Top else `Bot
    (* | Queries.MayBePublic _ -> *)
    (*   `Bool ((PrE.to_int d.pre = Some 0L || PrE.to_int d.pre = None) && (not (mode_is_init d.pmo))) *)
    | _ -> Queries.Result.top ()

  let finalize () =
    ArincUtil.print_actions ();
    if Sys.file_exists "result" then ArincUtil.marshal @@ open_out_bin @@ "result/arinc.out";
    if GobConfig.get_bool "ana.arinc.simplify" then ArincUtil.simplify ();
    if GobConfig.get_bool "ana.arinc.export" then (
      ArincUtil.save_dot_graph ();
      ArincUtil.save_promela_model ()
    );
    if GobConfig.get_bool "ana.arinc.validate" then ArincUtil.validate ()

  let startstate v = { pid = Pid.of_int 0L; pri = Pri.top (); per = Per.top (); cap = Cap.top (); pmo = Pmo.of_int 1L; pre = PrE.of_int 0L; pred = Pred.of_node (MyCFG.Function (emptyFunction "main").svar); ctx = Ctx.top () }
  let exitstate  v = D.bot ()

  let threadenter ctx lval f args =
    let d : D.t = ctx.local in
    let tasks = ctx.global tasks_var in
    (* TODO: optimize finding *)
    let tasks_f = Tasks.filter (fun (fs,f_d) ->
        Queries.LS.exists (fun (ls_f, _) -> ls_f = f) fs
      ) tasks
    in
    let f_d = snd (Tasks.choose tasks_f) in
    { f_d with pre = d.pre }

  let threadspawn ctx lval f args fctx = D.bot ()
end

let _ =
  MCP.register_analysis ~dep:["base"] (module Spec : Spec)
