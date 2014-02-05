(** Tracking of priorities an partition modes. *)

open Batteries
open Cil
open Pretty
open Analyses

(* Information for one task *)
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

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  let name = "arinc"
  module D =
  struct
    module Intra = Lattice.Prod3 (Pri) (Per) (Cap)
    module Inter = Lattice.Prod (Pmo) (PrE)
    include Lattice.Prod (Intra) (Inter)
    let toXML_f sf ((pri,per,cap), (pmo,pre)) =
      let replace_top name = function
          | Xml.Element (node, [text, n], elems) -> Xml.Element (node, [text, name ^ n], elems)
          | x -> x
      in
      let elems = [ replace_top "Priority: "   @@ Pri.toXML  pri
                  ; replace_top "Period: "  @@ Per.toXML per
                  ; replace_top "Capacity: "  @@ Cap.toXML cap
                  ; replace_top "Partition mode: "  @@ Pmo.toXML pmo
                  ; replace_top "Preemption lock: " @@ PrE.toXML  pre ] in
      Xml.Element ("Node", ["text", "ARINC state"], elems)

    let toXML s  = toXML_f short s

    let pri f ((pri,per,cap), (pmo,pre)) = ((f pri,per,cap), (pmo,pre))
    let per f ((pri,per,cap), (pmo,pre)) = ((pri,f per,cap), (pmo,pre))
    let cap f ((pri,per,cap), (pmo,pre)) = ((pri,per,f cap), (pmo,pre))
    let pmo f ((pri,per,cap), (pmo,pre)) = ((pri,per,cap), (f pmo,pre))
    let pre f ((pri,per,cap), (pmo,pre)) = ((pri,per,cap), (pmo,f pre))
  end
  module G = IntDomain.Booleans
  module C = D

  let is_single ctx =
    let fl : BaseDomain.Flag.t = snd (Obj.obj (List.assoc "base" ctx.presub)) in
    not (BaseDomain.Flag.is_multi fl)

  let part_mode_var = makeGlobalVar "__GOBLINT_ARINC_MUTLI_THREADED" voidPtrType

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    if not (is_single ctx || !Goblintutil.global_initialization || ctx.global part_mode_var) then raise Analyses.Deadcode;
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local, ctx.local]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    au

  let sprint f x = Pretty.sprint 80 (f () x)
  let string_of_partition_mode = function
    | 0L -> "IDLE"
    | 1L -> "COLD_START"
    | 2L -> "WARM_START"
    | 3L -> "NORMAL"
    | _  -> "UNKNOWN!"
  let mode_is_init  i = match Pmo.to_int i with Some 1L | Some 2L -> true | _ -> false
  let mode_is_multi i = Pmo.to_int i = Some 3L
  let infinity = 4294967295 (* time value used for infinity *)

  type pid = int64 (* process id. PROCESS_ID_TYPE and/or ENTRY_POINT? *)
  type sid = int64 (* semaphore id. SEMAPHORE_ID_TYPE and/or SEMAPHORE_NAME_TYPE? *)
  type eid = int64 (* semaphore id. EVENT_ID_TYPE and/or EVENT_NAME_TYPE? *)
  type time = int64 (* Maybe use Nativeint which is the same as C long. OCaml int is just 31 or 63 bits wide! *)
  type action = Start of pid | Stop of pid | Suspend of pid | Resume of pid
    | WaitSemaphore of sid | SignalSemaphore of sid
    | WaitEvent of eid | SetEvent of eid | ResetEvent of eid
    | TimedWait of time | PeriodicWait
  let actions = Hashtbl.create 123 (* use BatMultiPMap later? *)
  let get_actions pid : action list =
    (* Hashtbl.find_default actions pid [] |> List.unique *)
    Hashtbl.find_all actions pid |> List.unique (* current binding first, then previous bindings *)
  let add_action pid action =
    (* Hashtbl.modify_def [] pid (List.cons action) actions *)
    Hashtbl.add actions pid action (* old binding is just hidden *)

  (* lookup/generate id from resource type and name (needed for LAP_Se_GetXId functions, specified by LAP_Se_CreateX functions during init) *)
  type resource = Process | Semaphore | Event (* TODO Logbook, SamplingPort, QueuingPort, Buffer, Blackboard *)
  let resources = Hashtbl.create 123
  let get_id (resource:resource*string) : int64 =
    try Hashtbl.find resources resource
    with Not_found ->
      let ids = Hashtbl.values resources in
      let id = if Enum.is_empty ids then 0L else Int64.succ (Enum.arg_max identity ids) in
      Hashtbl.add resources resource id;
      id
  let get_by_id (id:int64) : (resource*string) option =
    Hashtbl.filter (fun x -> x=id) resources |> Hashtbl.keys |> Enum.get
  (* map function to process name in order to get pid (1:1 relation?) *)
  (* TODO maybe there can be multiple processes for the same function -> save pid in D! *)
  let funs = Hashtbl.create 123
  let add_fun (f:varinfo) (processname:string) =
    Hashtbl.add funs f processname
  let pid_from_fun (f:varinfo) : pid option =
    try let name = Hashtbl.find funs f in Some (get_id (Process, name))
    with Not_found -> None


  let print_actions () =
    let string_of_id id = string_of_int (i64_to_int id) in
    let string_of_resource_type = function
      | Process -> "Process"
      | Semaphore -> "Semaphore"
      | Event -> "Event"
    in
    let string_of_resource id =
      let name = match get_by_id id with
        | Some (resource_type, name) -> string_of_resource_type resource_type^" "^name
        | None when id = -1L -> "init/mainfun"
        | None -> "Unknown resource"
      in name^" (id "^string_of_id id^")"
    in
    let string_of_action = function
      | Start id -> "Start "^string_of_resource id
      | Stop id -> "Stop "^string_of_resource id
      | Suspend id -> "Suspend "^string_of_resource id
      | Resume id -> "Resume "^string_of_resource id
      | WaitSemaphore id -> "WaitSemaphore "^string_of_resource id
      | SignalSemaphore id -> "SignalSemaphore "^string_of_resource id
      | WaitEvent id -> "WaitEvent "^string_of_resource id
      | SetEvent id -> "SetEvent "^string_of_resource id
      | ResetEvent id -> "ResetEvent "^string_of_resource id
      | TimedWait id -> "TimedWait "^string_of_resource id
      | PeriodicWait -> "PeriodicWait"
    in
    let print_process pid =
      let actions = get_actions pid in
      M.debug @@ string_of_resource pid^" -> "^String.concat ", " (List.map string_of_action actions)
    in
    Hashtbl.keys actions |> Enum.iter print_process
  let finalize () = print_actions ()


  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let is_arinc_fun = startsWith "LAP_Se_" f.vname in
    let is_creating_fun = startsWith "LAP_Se_Create" f.vname in
    if M.tracing && is_arinc_fun then (
      let args_str = String.concat ", " (List.map (sprint d_exp) arglist) in
      (* M.tracel "arinc" "found %s(%s)\n" f.vname args_str *)
      M.debug_each @@ "found "^f.vname^"("^args_str^")"
    );
    let todo () = if false then failwith @@ f.vname^": Not implemented yet!" else ctx.local in
    let curfun = MyCFG.getFun (Option.get !MyCFG.current_node) in (* current_node should always be set here *)
    (* M.debug_each @@ "Inside function "^curfun.svar.vname; *)
    let curpid = pid_from_fun curfun.svar |? -1L in (* -1 is init/mainfun *)
    let ((pri,per,cap), (pmo,pre)) = ctx.local in
    let arglist = List.map stripCasts arglist in
    let assign_id exp id =
      match exp with
      (* call assign for all analyses (we only need base)! *)
      | AddrOf lval -> ctx.assign ~name:"base" lval (kinteger64 ILong id); ctx.local
      | _ -> failwith @@ "Could not assign id. Expected &id. Found "^sprint d_exp exp
    in
    let assign_id_by_name resource_type name id =
      match ctx.ask (Queries.EvalStr name) with
      | `Str name -> assign_id id (get_id (resource_type, name))
      | _ -> failwith @@ "Problem with arguments for "^f.vname
    in
    let eval_int exp =
      match ctx.ask (Queries.EvalInt exp) with
      | `Int i -> i
      | _ -> failwith @@ "Could not evaluate int-argument "^sprint d_exp exp^" in "^f.vname
    in
    let eval_str exp =
      match ctx.ask (Queries.EvalStr exp) with
      | `Str s -> s
      | _ -> failwith @@ "Could not evaluate string-argument "^sprint d_exp exp^" in "^f.vname
    in
    match f.vname, arglist with
    (* Preemption *)
      | "LAP_Se_LockPreemption", _ -> D.pre (PrE.add (PrE.of_int 1L)) ctx.local
      | "LAP_Se_UnlockPreemption", _ -> D.pre (PrE.sub (PrE.of_int 1L)) ctx.local
    (* Partition *)
      | "LAP_Se_SetPartitionMode", [mode; r] -> begin
          match ctx.ask (Queries.EvalInt mode) with
          | `Int i ->
              if M.tracing then M.tracel "arinc" "setting partition mode to %Ld (%s)\n" i (string_of_partition_mode i);
              if mode_is_multi (Pmo.of_int i) then ctx.sideg part_mode_var true;
              D.pmo (const @@ Pmo.of_int i) ctx.local
          | `Bot -> D.bot ()
          | _ -> ctx.sideg part_mode_var true; D.top ()
          end
      | "LAP_Se_GetPartitionStatus", [status; r] -> todo () (* != mode *)
      | "LAP_Se_GetPartitionStartCondition", _ -> todo ()
    (* Processes *)
      | "LAP_Se_CreateProcess", [AddrOf attr; pid; r] when mode_is_init pmo ->
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
              let pid' = get_id (Process, name) in (* better to keep that in D (would also supersede mapping from f to name) *)
              let spawn f =
                add_fun f name;
                ctx.spawn f ((Pri.of_int pri, Per.of_int per, Cap.of_int cap), (Pmo.of_int 3L, PrE.of_int 0L))  (* spawn should happen only when scheduled (after Start() and set mode NORMAL) *)
              in
              Queries.LS.iter (spawn%fst) funs;
              assign_id pid pid'
          (* TODO when is `Bot returned? *)
          (* | `Bot, _ | _, `Bot -> D.bot () *)
          | _ -> struct_fail ()
          end
      | "LAP_Se_GetProcessId", [name; pid; r] ->
          assign_id_by_name Process name pid
      | "LAP_Se_GetMyId", [pid; r] ->
          assign_id pid curpid
      | "LAP_Se_Start", [pid; r] ->
          (* at least one process should be started in main *)
          let pid = eval_int pid in
          add_action curpid (Start pid);
          ctx.local
      | "LAP_Se_DelayedStart", [pid; delay; r] -> todo ()
      | "LAP_Se_Stop", [pid; r] ->
          let pid = eval_int pid in
          add_action curpid (Stop pid);
          ctx.local
      | "LAP_Se_StopSelf", [] ->
          add_action curpid (Stop curpid);
          ctx.local
      | "LAP_Se_Suspend", [pid; r] ->
          let pid = eval_int pid in
          add_action curpid (Suspend pid);
          ctx.local
      | "LAP_Se_SuspendSelf", [timeout; r] -> (* TODO timeout *)
          add_action curpid (Suspend curpid);
          ctx.local
      | "LAP_Se_Resume", [pid; r] ->
          let pid = eval_int pid in
          add_action curpid (Resume pid);
          ctx.local
    (* Semaphores *)
      | "LAP_Se_CreateSemaphore", [name; cur; max; queuing; sid; r] when mode_is_init pmo ->
          (* create resource for name *)
          let sid' = get_id (Semaphore, eval_str name) in
          assign_id sid sid'
      | "LAP_Se_GetSemaphoreId", [name; sid; r] ->
          assign_id_by_name Semaphore name sid
      | "LAP_Se_WaitSemaphore", [sid; timeout; r] -> (* TODO timeout *)
          let sid = eval_int sid in
          add_action curpid (WaitSemaphore sid);
          ctx.local
      | "LAP_Se_SignalSemaphore", [sid; r] ->
          let sid = eval_int sid in
          add_action curpid (SignalSemaphore sid);
          ctx.local
    (* Events (down after create/reset, up after set) *)
      | "LAP_Se_CreateEvent", [name; eid; r] when mode_is_init pmo ->
          let eid' = get_id (Event, eval_str name) in
          assign_id eid  eid'
      | "LAP_Se_GetEventId", [name; eid; r] ->
          assign_id_by_name Event name eid
      | "LAP_Se_WaitEvent", [eid; timeout; r] -> (* TODO timeout *)
          let eid = eval_int eid in
          add_action curpid (WaitEvent eid);
          ctx.local
      | "LAP_Se_SetEvent", [eid; r] ->
          let eid = eval_int eid in
          add_action curpid (SetEvent eid);
          ctx.local
      | "LAP_Se_ResetEvent", [eid; r] ->
          let eid = eval_int eid in
          add_action curpid (ResetEvent eid);
          ctx.local
    (* Time *)
      | "LAP_Se_GetTime", [time; r] -> todo ()
      | "LAP_Se_TimedWait", [delay; r] -> todo ()
      | "LAP_Se_PeriodicWait", [] -> todo ()
    (* Not allowed: change configured schedule *)
      | "LAP_Se_SetPriority", [pid; prio; r] -> todo ()
      | "LAP_Se_Replenish", [budget; r] -> todo () (* name used in docs *)
      | "LAP_Se_ReplenishAperiodic", [budget; r] -> todo () (* name used in stdapi.c *)
      | _ when is_arinc_fun && is_creating_fun && not(mode_is_init pmo) ->
          failwith @@ f.vname^" is only allowed in partition mode COLD_START or WARM_START"
      | _ when is_arinc_fun -> failwith @@ "Function "^f.vname^" not handled!"
      | _ -> ctx.local

  let query ctx (q:Queries.t) : Queries.Result.t =
    let ((pri,per,cap), (pmo,pre)) = ctx.local in
    match q with
      | Queries.Priority _ ->
          if Pri.is_int pri then
            `Int (Option.get @@ Pri.to_int pri)
          else if Pri.is_top pri then `Top else `Bot
      | Queries.IsPrivate _ ->
          `Bool ((PrE.to_int pre <> Some 0L && PrE.to_int pre <> None) || Pmo.to_int pmo = Some 1L || Pmo.to_int pmo = Some 2L)
      | _ -> Queries.Result.top ()

  let startstate v = ((Pri.top (), Per.top (), Cap.top ()), (Pmo.of_int 1L, PrE.of_int 0L))
  let otherstate v = D.top ()
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis ~dep:["base"] (module Spec : Spec)
