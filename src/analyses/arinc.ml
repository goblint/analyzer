(** Tracking of priorities an partition modes. *)

open Batteries
open Cil
open Pretty
open Analyses

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  let name = "arinc"

  (* ARINC types and Hashtables for collecting CFG *)
  type id = varinfo
  type ids = id list
  type time = int64 (* Maybe use Nativeint which is the same as C long. OCaml int is just 31 or 63 bits wide! *)
  type action =
    | Epsilon (* e.g. used for loops *)
    | LockPreemption | UnlockPreemption | SetPartitionMode of int64
    | CreateProcess of id * varinfo list * int64 * time * time | CreateErrorHandler of id * varinfo list | Start of ids | Stop of ids | Suspend of ids | Resume of ids
    | CreateBlackboard of id | DisplayBlackboard of ids | ReadBlackboard of ids * time | ClearBlackboard of ids
    | CreateSemaphore of id * int64 * int64 * int64 | WaitSemaphore of ids | SignalSemaphore of ids
    | CreateEvent of id | WaitEvent of ids * time | SetEvent of ids | ResetEvent of ids
    | TimedWait of time | PeriodicWait
  (* callstack for locations *)
  type callstack = location list
  type node = MyCFG.node
  type edge = node * action * callstack * node
  let action_of_edge (_, action, _, _) = action
  let edges = Hashtbl.create 123
  let get_edges (pid:id) : edge Set.t =
    Hashtbl.find_default edges pid Set.empty
  let add_edge (pid:id) edge =
    Hashtbl.modify_def Set.empty pid (Set.add edge) edges

  (* lookup/generate id from resource type and name (needed for LAP_Se_GetXId functions, specified by LAP_Se_CreateX functions during init) *)
  type resource = Process | Semaphore | Event | Logbook | SamplingPort | QueuingPort | Buffer | Blackboard
  let str_resource_type = function
    | Process -> "Process"
    | Semaphore -> "Semaphore"
    | Event -> "Event"
    | Logbook -> "Logbook"
    | SamplingPort -> "SamplingPort"
    | QueuingPort -> "QueuingPort"
    | Buffer -> "Buffer"
    | Blackboard -> "Blackboard"

  (* map from tuple (resource, name) to varinfo (need to be saved b/c/ makeGlobalVar x t <> makeGlobalVar x t) *)
  let resources = Hashtbl.create 123
  let get_id (resource,name as k:resource*string) : id =
    try Hashtbl.find resources k
    with Not_found ->
      let vname = str_resource_type resource^":"^name in
      let v = makeGlobalVar vname voidPtrType in
      Hashtbl.replace resources k v;
      v
  let get_by_id (id:id) : (resource*string) option =
    Hashtbl.filter ((=) id) resources |> Hashtbl.keys |> Enum.get

  let funs_for_process name : varinfo list =
    let id = get_id (Process, name) in
    let get_funs = function
      | CreateProcess (id', funs, _, _, _) when id'=id -> funs
      | CreateErrorHandler (id', funs) when id'=id -> funs
      | _ -> []
    in
    let all_edges = Hashtbl.values edges |> List.of_enum |> List.map Set.elements |> List.concat in
    List.map (get_funs%action_of_edge) all_edges |> List.concat

  (* map process name to integer used in Pid domain *)
  let pnames = Hashtbl.create 123
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


  (* Domains *)

  (* Information for one task *)
  (* Process ID *)
  module Pid = IntDomain.Flattened
  (* Priority *)
  module Pri = IntDomain.Reverse (IntDomain.Lifted) (* TODO reverse? *)
  (* Period *)
  module Per = IntDomain.Flattened
  (* Capacity *)
  module Cap = IntDomain.Flattened

  (* Information for all tasks *)
  (* Partition mode *)
  module Pmo = IntDomain.Flattened
  (* Preemption lock *)
  module PrE = IntDomain.Flattened
  module Node =
  struct
    module Base =
    struct
      type t = MyCFG.node
      include Printable.Std
      include Lattice.StdCousot
      let short w x = string_of_int (MyCFG.getLoc x).line
      include Printable.PrintSimple (struct
        type t' = t
        let name () = "flat nodes"
        let short = short
      end)
      let equal = Util.equals
      let hash = Hashtbl.hash
    end
    include Lattice.Flat (Base) (struct let top_name = "Unknown node" let bot_name = "Error node" end)
    let of_node node = `Lifted node
    let is_node = function `Lifted _ -> true | _ -> false
    let to_node = function `Lifted node -> node | _ -> failwith "Unknown/Error node"
    let string_of node = Base.short 0 node
  end

  (* define record type here so that fields are accessable outside of D *)
  type process = { pid: Pid.t; pri: Pri.t; per: Per.t; cap: Cap.t; pmo: Pmo.t; pre: PrE.t; node: Node.t; callstack: callstack }
  module D =
  struct
    type t = process
    include Printable.Std
    include Lattice.StdCousot

    (* printing *)
    let string_of_callstack xs = "["^String.concat ", " (List.map (fun loc -> string_of_int loc.line) xs)^"]"
    let short w x = Printf.sprintf "{ pid=%s; pri=%s; per=%s; cap=%s; pmo=%s; pre=%s; node=%s; callstack=%s)" (Pid.short 3 x.pid) (Pri.short 3 x.pri) (Per.short 3 x.per) (Cap.short 3 x.cap) (Pmo.short 3 x.pmo) (PrE.short 3 x.pre) (Node.short 6 x.node) (string_of_callstack x.callstack)
    include Printable.PrintSimple (struct
      type t' = t
      let name () = "ARINC state"
      let short = short
    end)
    let toXML_f sf d =
      let replace_top name = function
          | Xml.Element (node, [text, n], elems) -> Xml.Element (node, [text, name ^ n], elems)
          | x -> x
      in
      let elems = [ replace_top "PID: "   @@ Pid.toXML  d.pid
                  ; replace_top "Priority: "  @@ Pri.toXML d.pri
                  ; replace_top "Period: "  @@ Per.toXML d.per
                  ; replace_top "Capacity: "  @@ Cap.toXML d.cap
                  ; replace_top "Partition mode: "  @@ Pmo.toXML d.pmo
                  ; replace_top "Preemption lock: " @@ PrE.toXML  d.pre ] in
      Xml.Element ("Node", ["text", "ARINC state"], elems)
    let toXML s  = toXML_f short s
    (* Printable.S *)
    let equal = Util.equals
    (* let equal x y = let f z = { z with callstack = List.sort_unique compare z.callstack } in Util.equals (f x) (f y) *)
    let hash = Hashtbl.hash

    (* modify fields *)
    let pid f d = { d with pid = f d.pid }
    let pri f d = { d with pri = f d.pri }
    let per f d = { d with per = f d.per }
    let cap f d = { d with cap = f d.cap }
    let pmo f d = { d with pmo = f d.pmo }
    let pre f d = { d with pre = f d.pre }
    let node f d = { d with node = f d.node }
    let callstack f d = { d with callstack = f d.callstack }
    (* if x is already in the callstack we move it to the front, this way we can do tail on combine *)
    let callstack_length = 1
    let callstack_push x d = if List.length d.callstack < callstack_length then callstack (fun xs -> x :: List.remove xs x) d else d

    let bot () = { pid = Pid.bot (); pri = Pri.bot (); per = Per.bot (); cap = Cap.bot (); pmo = Pmo.bot (); pre = PrE.bot (); node = Node.bot (); callstack = [] }
    let is_bot x = { x with callstack = [] } = bot ()
    let top () = { pid = Pid.top (); pri = Pri.top (); per = Per.top (); cap = Cap.top (); pmo = Pmo.top (); pre = PrE.top (); node = Node.top (); callstack = [] }
    let is_top x = { x with callstack = [] } = top ()

    let rec is_prefix = function [],_ -> true | x::xs,y::ys when x=y -> is_prefix (xs,ys) | _ -> false
    (* let leq x y = Pid.leq x.pid y.pid && Pri.leq x.pri y.pri && Per.leq x.per y.per && Cap.leq x.cap y.cap && List.subset compare x.callstack y.callstack && Pmo.leq x.pmo y.pmo && PrE.leq x.pre y.pre *)
    let leq x y = Pid.leq x.pid y.pid && Pri.leq x.pri y.pri && Per.leq x.per y.per && Cap.leq x.cap y.cap && Pmo.leq x.pmo y.pmo && PrE.leq x.pre y.pre (* && Node.leq x.node y.node *) && is_prefix (x.callstack, y.callstack)
    let join_callstack xs ys = if xs<>ys then M.debug_each @@ "JOIN callstacks " ^ string_of_callstack xs ^ " and " ^ string_of_callstack ys; xs
    let op_scheme op1 op2 op3 op4 op5 op6 op7 x y: t = { pid = op1 x.pid y.pid; pri = op2 x.pri y.pri; per = op3 x.per y.per; cap = op4 x.cap y.cap; pmo = op5 x.pmo y.pmo; pre = op6 x.pre y.pre; node = op7 x.node y.node; callstack = join_callstack x.callstack y.callstack }
    let join x y = let r = op_scheme Pid.join Pri.join Per.join Cap.join Pmo.join PrE.join Node.join x y
      in let s x = if is_top x then "TOP" else if is_bot x then "BOT" else short 0 x in M.debug_each @@ "JOIN\t" ^ if equal x y then "EQUAL" else s x ^ "\n\t" ^ s y ^ "\n->\t" ^ s r;
      if x.pid = y.pid && Pid.is_int x.pid && Node.is_node x.node && Node.is_node y.node then begin
        let a = Node.to_node x.node in let b = Node.to_node y.node in
        let pname = get_by_pid @@ Option.get @@ Pid.to_int x.pid in
        let pid = get_id (Process, Option.get pname) in
        add_edge pid (a, Epsilon, x.callstack, b)
      end;
      { r with node = if Node.is_top r.node then (if Node.leq x.node y.node then x.node else y.node) else r.node }
    let meet = op_scheme Pid.meet Pri.meet Per.meet Cap.meet Pmo.meet PrE.meet Node.meet
  end
  module G = IntDomain.Booleans
  module C = D

  let is_single ctx =
    let fl : BaseDomain.Flag.t = snd (Obj.obj (List.assoc "base" ctx.presub)) in
    not (BaseDomain.Flag.is_multi fl)

  let part_mode_var = makeGlobalVar "__GOBLINT_ARINC_MUTLI_THREADED" voidPtrType

  let is_main_fun name = List.mem name (List.map Json.string (GobConfig.get_list "mainfun"))

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t = (* on entering function body -> called for spawned processes *)
    (* M.debug_each @@ "BODY " ^ f.svar.vname ^" @ "^ string_of_int (!Tracing.current_loc).line; *)
    if not (is_single ctx || !Goblintutil.global_initialization || ctx.global part_mode_var) then raise Analyses.Deadcode;
    let d = ctx.local in
    if Node.is_bot d.node && Option.is_some !MyCFG.current_node
    then { d with node = Node.of_node @@ Option.get !MyCFG.current_node }
    else d

  let return ctx (exp:exp option) (f:fundec) : D.t =
    (* D.callstack List.tl ctx.local *)
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list = (* on function calls -> not called for spawned processes *)
    (* print_endline @@ "ENTER " ^ f.vname ^" @ "^ string_of_int (!Tracing.current_loc).line; (* somehow M.debug_each doesn't print anything here *) *)
    let d_caller = ctx.local in
    let d_callee =
      if is_main_fun f.vname
      then ctx.local (* mainfun is the init process -> ignore enter here *)
      else D.callstack_push !Tracing.current_loc ctx.local (* push location onto callee's callstack *)
    in
    [d_caller, d_callee]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    (* M.debug_each @@ "ctx.local: " ^ D.string_of_callstack ctx.local.callstack ^ ", au: " ^ D.string_of_callstack au.callstack; *)
    { au with callstack = ctx.local.callstack } (* just keep the caller's callstack *)

  (* ARINC utility functions *)
  let sprint f x = Pretty.sprint 80 (f () x)
  let string_of_partition_mode = function
    | 0L -> "IDLE"
    | 1L -> "COLD_START"
    | 2L -> "WARM_START"
    | 3L -> "NORMAL"
    | _  -> "UNKNOWN!"
  let string_of_queuing_discipline = function
    | 0L -> "FIFO"
    | 1L -> "PRIO"
    | _  -> "UNKNOWN!"
  let mode_is_init  i = match Pmo.to_int i with Some 1L | Some 2L -> true | _ -> false
  let mode_is_multi i = Pmo.to_int i = Some 3L
  let infinity = 4294967295L (* time value used for infinity *)

  (* set of processes to spawn once partition mode is set to NORMAL *)
  let processes = ref []
  let add_process p = processes := List.append !processes [p]

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let d : D.t = ctx.local in
    let is_arinc_fun = startsWith "LAP_Se_" f.vname in
    let is_creating_fun = startsWith "LAP_Se_Create" f.vname in
    let is_error_handler = false in (* TODO *)
    (* if is_arinc_fun then M.debug_each @@ "d.callstack: " ^ D.string_of_callstack d.callstack; *)
    if M.tracing && is_arinc_fun then (
      let args_str = String.concat ", " (List.map (sprint d_exp) arglist) in
      (* M.tracel "arinc" "found %s(%s)\n" f.vname args_str *)
      M.debug_each @@ "found "^f.vname^"("^args_str^")"
    );
    let todo () = if false then failwith @@ f.vname^": Not implemented yet!" else d in
    let curfun = MyCFG.getFun (Option.get !MyCFG.current_node) in (* current_node should always be set here *)
    (* M.debug_each @@ "Inside function "^curfun.svar.vname; *)
    let curpid = match Pid.to_int d.pid with Some i -> i | None -> failwith @@ "special: Pid.to_int = None inside function "^curfun.svar.vname in
    let pname = match get_by_pid curpid with Some s -> s | None -> failwith @@ "special: no processname for pid in Hashtbl!" in
    let curpid = get_id (Process, pname) in
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
    let eval_id exp =
      match ctx.ask (Queries.MayPointTo exp) with
      | `LvalSet a when not (Queries.LS.is_top a) ->
                     (* && not (Queries.LS.mem (dummyFunDec.svar, `NoOffset) a) -> *)
          Queries.LS.remove (dummyFunDec.svar, `NoOffset) a |> Queries.LS.elements |> List.map fst
      | `LvalSet a -> (* failwith "LvalSet was top" *) []
      | x -> M.debug_each @@ "Could not evaluate id-argument "^sprint d_plainexp exp^" in "^f.vname^". Query returned "^sprint Queries.Result.pretty x; []
    in
    let assign_id exp id =
      match exp with
      (* call assign for all analyses (we only need base)! *)
      | AddrOf lval -> ctx.assign ~name:"base" lval (mkAddrOf @@ var id)
      | _ -> failwith @@ "Could not assign id. Expected &id. Found "^sprint d_exp exp
    in
    let assign_id_by_name resource_type name id =
      assign_id id (get_id (resource_type, eval_str name))
    in
    let current_node = Option.get !MyCFG.current_node in
    (* let current_callstack = !Tracing.current_loc :: d.callstack in *)
    let add_action action d =
      add_edge curpid (Node.to_node d.node, action, d.callstack, current_node);
      { d with node = Node.of_node current_node }
    in
    let arglist = List.map (stripCasts%(constFold false)) arglist in
    match f.vname, arglist with
      | _ when is_arinc_fun && is_creating_fun && not(mode_is_init d.pmo) ->
          failwith @@ f.vname^" is only allowed in partition mode COLD_START or WARM_START"
    (* Preemption *)
      | "LAP_Se_LockPreemption", _ when not is_error_handler ->
          add_action LockPreemption d
          |> D.pre (PrE.add (PrE.of_int 1L))
      | "LAP_Se_UnlockPreemption", _ when not is_error_handler ->
          add_action UnlockPreemption d
          |> D.pre (PrE.sub (PrE.of_int 1L))
    (* Partition *)
      | "LAP_Se_SetPartitionMode", [mode; r] -> begin
          match ctx.ask (Queries.EvalInt mode) with
          | `Int i ->
              if M.tracing then M.tracel "arinc" "setting partition mode to %Ld (%s)\n" i (string_of_partition_mode i);
              if mode_is_multi (Pmo.of_int i) then (
                ctx.sideg part_mode_var true;
                (* spawn processes *)
                ignore @@ printf "arinc: SetPartitionMode NORMAL: spawning %i processes!\n" (List.length !processes);
                List.iter (fun (f,f_d) -> ctx.spawn f (f_d d.pre)) !processes; (* what about duplicates? List.unique fails because d is fun! *)
                (* clear list *)
                processes := []
              );
              add_action (SetPartitionMode i) d
              |> D.pmo (const @@ Pmo.of_int i)
          | `Bot -> D.bot ()
          | _ -> ctx.sideg part_mode_var true; D.top ()
          end
      | "LAP_Se_GetPartitionStatus", [status; r] -> todo () (* != mode *)
      | "LAP_Se_GetPartitionStartCondition", _ -> todo ()
    (* Processes *)
      | "F59", [dst; src] ->
          (* M.debug @@ "strcpy("^sprint d_plainexp dst^", "^sprint d_plainexp src^")"; *)
          (* let exp = mkAddrOrStartOf (mkMem ~addr:dst ~off:NoOffset) in *)
          let lval = match dst with
            | Lval lval
            | StartOf lval -> lval
            | AddrOf lval -> lval
            | _ -> failwith @@ "F59/strcpy expects first argument to be some Lval, but got "^sprint d_plainexp dst
          in
          let exp = mkAddrOf lval in
          begin match ctx.ask (Queries.MayPointTo exp) with
          | `LvalSet ls when not (Queries.LS.is_top ls) && Queries.LS.cardinal ls = 1 ->
              let v, offs = Queries.LS.choose ls in
              let ciloffs = Lval.CilLval.to_ciloffs offs in
              let lval = Var v, ciloffs in
              (* ignore @@ printf "dst: %a, MayPointTo: %a" d_plainexp dst d_plainlval lval; *)
              ctx.assign ~name:"base" lval src;
              d
          | _ -> failwith @@ "F59/strcpy could not query MayPointTo "^sprint d_plainexp exp
          end
      | "LAP_Se_CreateProcess", [AddrOf attr; pid; r] ->
          let cm = match unrollType (typeOfLval attr) with
            | TComp (c,_) -> c
            | _ -> failwith "type-error: first argument of LAP_Se_CreateProcess not a struct."
          in
          let struct_fail () = failwith @@ "LAP_Se_CreateProcess: problem with first argument (struct PROCESS_ATTRIBUTE_TYPE): needs fields NAME, ENTRY_POINT, BASE_PRIORITY, PERIOD, TIME_CAPACITY\nRunning scrambled: "^string_of_bool Goblintutil.scrambled in
          let field ofs =
            try Lval (addOffsetLval (Field (getCompField cm ofs, NoOffset)) attr)
            with Not_found -> struct_fail ()
          in
          let name = ctx.ask (Queries.EvalStr (field Goblintutil.arinc_name)) in
          let entry_point = ctx.ask (Queries.ReachableFrom (AddrOf attr)) in
          let pri  = ctx.ask (Queries.EvalInt (field Goblintutil.arinc_base_priority)) in
          let per  = ctx.ask (Queries.EvalInt (field Goblintutil.arinc_period)) in
          let cap  = ctx.ask (Queries.EvalInt (field Goblintutil.arinc_time_capacity)) in
          begin match name, entry_point, pri, per, cap with
          | `Str name, `LvalSet ls, `Int pri, `Int per, `Int cap when not (Queries.LS.is_top ls)
            && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) ls) ->
              let funs = Queries.LS.filter (fun l -> isFunctionType (fst l).vtype) ls in
              if M.tracing then M.tracel "arinc" "starting a thread %a with priority '%Ld' \n" Queries.LS.pretty funs pri;
              let fun_list = funs |> Queries.LS.elements |> List.map fst in
              let spawn f =
                let f_d pre = { pid = Pid.of_int (get_pid name); pri = Pri.of_int pri; per = Per.of_int per; cap = Cap.of_int cap; callstack = []; pmo = Pmo.of_int 3L; pre = pre; node = Node.bot () } in (* int64 -> D.t *)
                add_process (f,f_d)
              in
              List.iter spawn fun_list;
              let pid' = get_id (Process, name) in
              assign_id pid pid';
              add_action (CreateProcess (pid', fun_list, pri, per, cap)) d
          (* TODO when is `Bot returned? *)
          (* | `Bot, _ | _, `Bot -> D.bot () *)
          | _ -> struct_fail ()
          end
      | "LAP_Se_GetProcessId", [name; pid; r] ->
          assign_id_by_name Process name pid; d
      | "LAP_Se_GetProcessStatus", [pid; status; r] -> todo ()
      | "LAP_Se_GetMyId", [pid; r] ->
          assign_id pid curpid; d
      | "LAP_Se_Start", [pid; r] ->
          (* at least one process should be started in main *)
          let pid = eval_id pid in
          add_action (Start pid) d
      | "LAP_Se_DelayedStart", [pid; delay; r] -> todo ()
      | "LAP_Se_Stop", [pid; r] ->
          let pid = eval_id pid in
          add_action (Stop pid) d
      | "LAP_Se_StopSelf", [] ->
          add_action (Stop [curpid]) d
      | "LAP_Se_Suspend", [pid; r] ->
          let pid = eval_id pid in
          add_action (Suspend pid) d
      | "LAP_Se_SuspendSelf", [timeout; r] -> (* TODO timeout *)
          add_action (Suspend [curpid]) d
      | "LAP_Se_Resume", [pid; r] ->
          let pid = eval_id pid in
          add_action (Resume pid) d
    (* Logbook *)
      | "LAP_Se_CreateLogBook", [name; max_size; max_logged; max_in_progress; lbid; r] -> todo ()
      | "LAP_Se_ReadLogBook", _ -> todo ()
      | "LAP_Se_WriteLogBook", _ -> todo ()
      | "LAP_Se_ClearLogBook", _ -> todo ()
      | "LAP_Se_GetLogBookId", _ -> todo ()
      | "LAP_Se_GetLogBookStatus", _ -> todo ()
    (* SamplingPort *)
      | "LAP_Se_CreateSamplingPort", [name; max_size; dir; period; spid; r] -> todo ()
      | "LAP_Se_WriteSamplingMessage", _ -> todo ()
      | "LAP_Se_ReadSamplingMessage", _ -> todo ()
      | "LAP_Se_GetSamplingPortId", _ -> todo ()
      | "LAP_Se_GetSamplingPortStatus", _ -> todo ()
    (* QueuingPort *)
      | "LAP_Se_CreateQueuingPort", [name; max_size; max_range; dir; queuing; qpid; r] -> todo ()
      | "LAP_Se_SendQueuingMessage", _ -> todo ()
      | "LAP_Se_ReceiveQueuingMessage", _ -> todo ()
      | "LAP_Se_GetQueuingPortId", _ -> todo ()
      | "LAP_Se_GetQueuingPortStatus", _ -> todo ()
    (* Buffer *)
      | "LAP_Se_CreateBuffer", [name; max_size; max_range; queuing; buid; r] -> todo ()
      | "LAP_Se_SendBuffer", _ -> todo ()
      | "LAP_Se_ReceiveBuffer", _ -> todo ()
      | "LAP_Se_GetBufferId", _ -> todo ()
      | "LAP_Se_GetBufferStatus", _ -> todo ()
    (* Blackboard *)
      | "LAP_Se_CreateBlackboard", [name; max_size; bbid; r] ->
          let bbid' = get_id (Blackboard, eval_str name) in
          assign_id bbid bbid';
          add_action (CreateBlackboard bbid') d
      | "LAP_Se_DisplayBlackboard", [bbid; msg_addr; len; r] ->
          add_action (DisplayBlackboard (eval_id bbid)) d
      | "LAP_Se_ReadBlackboard", [bbid; timeout; msg_addr; len; r] ->
          add_action (ReadBlackboard (eval_id bbid, eval_int timeout)) d
      | "LAP_Se_ClearBlackboard", [bbid; r] ->
          add_action (ClearBlackboard (eval_id bbid)) d
      | "LAP_Se_GetBlackboardId", [name; bbid; r] ->
          assign_id_by_name Blackboard name bbid; d
      | "LAP_Se_GetBlackboardStatus", _ -> todo ()
    (* Semaphores *)
      | "LAP_Se_CreateSemaphore", [name; cur; max; queuing; sid; r] ->
          (* create resource for name *)
          let sid' = get_id (Semaphore, eval_str name) in
          assign_id sid sid';
          add_action (CreateSemaphore (sid', eval_int cur, eval_int max, eval_int queuing)) d
      | "LAP_Se_WaitSemaphore", [sid; timeout; r] -> (* TODO timeout *)
          let sid = eval_id sid in
          add_action (WaitSemaphore sid) d
      | "LAP_Se_SignalSemaphore", [sid; r] ->
          let sid = eval_id sid in
          add_action (SignalSemaphore sid) d
      | "LAP_Se_GetSemaphoreId", [name; sid; r] ->
          assign_id_by_name Semaphore name sid; d
      | "LAP_Se_GetSemaphoreStatus", [sid; status; r] -> todo ()
    (* Events (down after create/reset, up after set) *)
      | "LAP_Se_CreateEvent", [name; eid; r] ->
          let eid' = get_id (Event, eval_str name) in
          assign_id eid  eid';
          add_action (CreateEvent eid') d
      | "LAP_Se_SetEvent", [eid; r] ->
          let eid = eval_id eid in
          add_action (SetEvent eid) d
      | "LAP_Se_ResetEvent", [eid; r] ->
          let eid = eval_id eid in
          add_action (ResetEvent eid) d
      | "LAP_Se_WaitEvent", [eid; timeout; r] -> (* TODO timeout *)
          let eid = eval_id eid in
          add_action (WaitEvent (eid, eval_int timeout)) d
      | "LAP_Se_GetEventId", [name; eid; r] ->
          assign_id_by_name Event name eid; d
      | "LAP_Se_GetEventStatus", [eid; status; r] -> todo ()
    (* Time *)
      | "LAP_Se_GetTime", [time; r] -> todo ()
      | "LAP_Se_TimedWait", [delay; r] ->
          add_action (TimedWait (eval_int delay)) d
      | "LAP_Se_PeriodicWait", [r] ->
          add_action PeriodicWait d
    (* Errors *)
      | "LAP_Se_CreateErrorHandler", [entry_point; stack_size; r] ->
          let name = "ErrorHandler" in
          let pid = get_id (Process, name) in
          begin match ctx.ask (Queries.ReachableFrom (entry_point)) with
          | `LvalSet ls when not (Queries.LS.is_top ls) && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) ls) ->
              let funs = Queries.LS.filter (fun l -> isFunctionType (fst l).vtype) ls |> Queries.LS.elements |> List.map fst in
              let spawn f =
                let f_d pre = { pid = Pid.of_int (get_pid name); pri = Pri.of_int infinity; per = Per.of_int infinity; cap = Cap.of_int infinity; callstack = []; pmo = Pmo.of_int 3L; pre = pre; node = Node.bot () } in (* int64 -> D.t *)
                add_process (f,f_d)
              in
              List.iter spawn funs;
              add_action (CreateErrorHandler (pid, funs)) d
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
      | Queries.IsPrivate _ ->
          `Bool ((PrE.to_int d.pre <> Some 0L && PrE.to_int d.pre <> None) || mode_is_init d.pmo)
      | _ -> Queries.Result.top ()

  (* ARINC output *)
  let str_i64 id = string_of_int (i64_to_int id)
  let str_funs funs = "["^(List.map (fun v -> v.vname) funs |> String.concat ", ")^"]"
  let str_resource id =
    match get_by_id id with
    | Some (Process, "mainfun") ->
        "mainfun/["^String.concat ", " (List.map Json.string (GobConfig.get_list "mainfun"))^"]"
    | Some (Process, name) ->
        name^"/"^str_funs @@ funs_for_process name
    | Some (resource_type, name) ->
        name
    | None -> "Unknown resource"
  let str_resources ids = "["^(String.concat ", " @@ List.map str_resource ids)^"]"
  let str_time t = if t = infinity then "∞" else str_i64 t^"ns"
  let str_action pid = function
    | Epsilon -> "ε"
    | LockPreemption -> "LockPreemption"
    | UnlockPreemption -> "UnlockPreemption"
    | SetPartitionMode i -> "SetPartitionMode "^string_of_partition_mode i
    | CreateProcess (id, funs, prio, period, capacity) ->
        "CreateProcess "^str_resource id^" (funs "^str_funs funs^", prio "^str_i64 prio^", period "^str_time period^", capacity "^str_time capacity^")"
    | CreateErrorHandler (id, funs) -> "CreateErrorHandler "^str_resource id
    | Start ids -> "Start "^str_resources ids
    | Stop ids when ids=[pid] -> "StopSelf"
    | Stop ids -> "Stop "^str_resources ids
    | Suspend ids when ids=[pid] -> "SuspendSelf"
    | Suspend ids -> "Suspend "^str_resources ids
    | Resume ids -> "Resume "^str_resources ids
    | CreateBlackboard id -> "CreateBlackboard "^str_resource id
    | DisplayBlackboard ids -> "DisplayBlackboard "^str_resources ids
    | ReadBlackboard (ids, timeout) -> "ReadBlackboard "^str_resources ids^" (timeout "^str_time timeout^")"
    | ClearBlackboard ids -> "ClearBlackboard "^str_resources ids
    | CreateSemaphore (id, cur, max, queuing) ->
        "CreateSemaphore "^str_resource id^" ("^str_i64 cur^"/"^str_i64 max^", "^string_of_queuing_discipline queuing^")"
    | WaitSemaphore ids -> "WaitSemaphore "^str_resources ids
    | SignalSemaphore ids -> "SignalSemaphore "^str_resources ids
    | CreateEvent id -> "CreateEvent "^str_resource id
    | WaitEvent (ids, timeout) -> "WaitEvent "^str_resources ids^" (timeout "^str_time timeout^")"
    | SetEvent ids -> "SetEvent "^str_resources ids
    | ResetEvent ids -> "ResetEvent "^str_resources ids
    | TimedWait t -> "TimedWait "^str_time t
    | PeriodicWait -> "PeriodicWait"
  let str_node node = Node.string_of node
  let str_callstack cs = if List.is_empty cs then "" else " cs=" ^ D.string_of_callstack cs
  let print_actions () =
    let print_process pid =
      let str_edge (a, action, cs, b) = str_node a ^ " -> " ^ str_action pid action ^ str_callstack cs ^ " -> " ^ str_node b in
      let xs = Set.map str_edge (get_edges pid) in
      M.debug @@ str_resource pid^" ->\n\t"^String.concat "\n\t" (Set.elements xs)
    in
    Hashtbl.keys edges |> Enum.iter print_process
  let save_dot_graph () =
    let dot_process pid =
      (* 1 -> w1 [label="fopen(_)"]; *)
      let str_node x = "\"" ^ str_node x ^ "\"" in (* quote node names for dot b/c of callstack *)
      let str_edge (a, action, cs, b) = str_node a ^ "\t->\t" ^ str_node b ^ "\t[label=\"" ^ str_action pid action ^ str_callstack cs ^ "\"]" in
      let xs = Set.map str_edge (get_edges pid) |> Set.elements in
      ("// "^str_resource pid) :: xs
    in
    let lines = Hashtbl.keys edges |> List.of_enum |> List.map dot_process |> List.concat in
    let dot_graph = String.concat "\n  " ("digraph file {"::lines) ^ "\n}" in
    let path = "result/arinc.dot" in
    output_file path dot_graph;
    print_endline ("saved graph as "^Sys.getcwd ()^"/"^path)
  let finalize () =
    print_actions ();
    if GobConfig.get_bool "ana.arinc.dot" then save_dot_graph ()

  let startstate v = { (D.bot ()) with  pid = Pid.of_int 0L; pmo = Pmo.of_int 1L; pre = PrE.of_int 0L; node = Node.bot () }
  let otherstate v = D.bot ()
  let exitstate  v = D.bot ()
end

let _ =
  MCP.register_analysis ~dep:["base"] (module Spec : Spec)
