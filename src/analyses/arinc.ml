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

  let create_process ctx arglist =
    (* if M.tracing then M.tracel "arinc" "found LAP_Se_CreateProcess\n"; *)
    match List.hd arglist with
      | AddrOf lv -> begin
        let cm  =
          match unrollType (typeOfLval lv) with
            | TComp (c,_) -> c
            | _ -> failwith "type-error: first arg. of LAP_Se_CreateProcess not a struct."
        in
        let field ofs = Lval (addOffsetLval (Field (getCompField cm ofs, NoOffset)) lv) in
        let pri = ctx.ask (Queries.EvalInt (field Goblintutil.arinc_base_priority)) in
        let per = ctx.ask (Queries.EvalInt (field Goblintutil.arinc_period)) in
        let cap = ctx.ask (Queries.EvalInt (field Goblintutil.arinc_time_capacity)) in
        let entry_point = ctx.ask (Queries.ReachableFrom (AddrOf lv)) in
        match pri, per, cap, entry_point with
          | `Int pri, `Int per, `Int cap, `LvalSet ls when not (Queries.LS.is_top ls)
            && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) ls) ->
              let funs = Queries.LS.filter (fun l -> isFunctionType (fst l).vtype) ls in
              if M.tracing then M.tracel "arinc" "starting a thread %a with priority '%Ld' \n" Queries.LS.pretty funs pri;
              Queries.LS.iter (fun f -> ctx.spawn (fst f) ((Pri.of_int pri, Per.of_int per, Cap.of_int cap), (Pmo.of_int 3L, PrE.of_int 0L))) funs; (* spawn should happen only when scheduled (after Start() and set mode NORMAL) *)
              ctx.local
          (* TODO when is `Bot returned? *)
          (* | `Bot, _ | _, `Bot -> D.bot () *)
          (* | _ -> ctx.local *)
          | _ -> failwith "LAP_Se_CreateProcess: problem with struct PROCESS_ATTRIBUTE_TYPE"
        end
      | _ -> ctx.local

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
    | TimedWait of time | PerdiodicWait
  let actions = Hashtbl.create 123 (* use BatMultiPMap later? *)
  let get_actions pid : action list =
    (* Hashtbl.find_default actions pid [] |> List.unique *)
    Hashtbl.find_all actions pid |> List.unique (* current binding first, then previous bindings *)
  let add_action action pid =
    (* Hashtbl.modify_def [] pid (List.cons action) actions *)
    Hashtbl.add actions pid action (* old binding is just hidden *)
  (* map function to pid (1:1 relation? -> unique pid generated) *)
  (* TODO maybe there can be multiple processes for the same function -> save pid in D *)
  let funs = Hashtbl.create 123
  let pid_from_fun (f:varinfo) : pid =
    try Hashtbl.find funs f
    with Not_found ->
      let pids = Hashtbl.values funs in
      let pid = if Enum.is_empty pids then 0L else Int64.succ (Enum.arg_max identity pids) in (* 0 is mainfun *)
      Hashtbl.add funs f pid;
      pid

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    if M.tracing && startsWith "LAP_Se_" f.vname then (
      let args_str = String.concat ", " (List.map (sprint d_exp) arglist) in
      (* M.tracel "arinc" "found %s(%s)\n" f.vname args_str *)
      M.debug_each @@ "found "^f.vname^"("^args_str^")"
    );
    let todo () = if false then failwith @@ f.vname^": Not implemented yet!" else ctx.local in
    let only_init () = failwith @@ f.vname^" is only allowed in partition mode COLD_START or WARM_START" in
    let curfun = MyCFG.getFun (Option.get !MyCFG.current_node) in (* current_node should always be set here *)
    (* M.debug_each @@ "Inside function "^curfun.svar.vname; *)
    let curpid = pid_from_fun curfun.svar in
    let ((pri,per,cap), (pmo,pre)) = ctx.local in
    let arglist = List.map stripCasts arglist in
    match f.vname, arglist with
    (* Preemption *)
      | "LAP_Se_LockPreemption", _ -> D.pre (PrE.add (PrE.of_int 1L)) ctx.local
      | "LAP_Se_UnlockPreemption", _ -> D.pre (PrE.sub (PrE.of_int 1L)) ctx.local
    (* Partition *)
      | "LAP_Se_SetPartitionMode", _ -> begin
          match ctx.ask (Queries.EvalInt (List.hd arglist)) with
            | `Int i ->
              if M.tracing then M.tracel "arinc" "setting partition mode to %Ld (%s)\n" i (string_of_partition_mode i);
              if mode_is_multi (Pmo.of_int i) then ctx.sideg part_mode_var true;
              D.pmo (const @@ Pmo.of_int i) ctx.local
            | `Bot -> D.bot ()
            | _ -> ctx.sideg part_mode_var true; D.top ()
          end
      | "LAP_Se_GetPartitionStatus", _ -> todo ()
      | "LAP_Se_GetPartitionStartCondition", _ -> todo ()
    (* Processes *)
      | "LAP_Se_CreateProcess", _ ->
          if mode_is_init pmo then create_process ctx arglist
          else only_init ()
      | "LAP_Se_GetProcessId", _ -> todo ()
      | "LAP_Se_GetMyId", _ -> todo ()
      | "LAP_Se_Start", [pid; r] -> begin
          (* at least one process should be started in main (use special pid?) *)
          match ctx.ask (Queries.EvalInt pid) with
          | `Int pid -> add_action (Start pid) curpid; ctx.local
          | _ -> failwith "Could not evaluate pid in LAP_Se_Start"
          end
      | "LAP_Se_DelayedStart", _ -> todo ()
      | "LAP_Se_Stop", _ -> todo ()
      | "LAP_Se_StopSelf", _ -> todo ()
      | "LAP_Se_Suspend", _ -> todo ()
      | "LAP_Se_SuspendSelf", _ -> todo ()
      | "LAP_Se_Resume", _ -> todo ()
    (* Semaphores *)
      | "LAP_Se_CreateSemaphore", _ ->
          if mode_is_init pmo then todo ()
          else only_init ()
      | "LAP_Se_GetSemaphoreId", _ -> todo ()
      | "LAP_Se_WaitSemaphore", _ -> todo ()
      | "LAP_Se_SignalSemaphore", _ -> todo ()
    (* Events *)
      | "LAP_Se_CreateEvent", _ ->
          if mode_is_init pmo then todo ()
          else only_init ()
      | "LAP_Se_GetEventId", _ -> todo ()
      | "LAP_Se_WaitEvent", _ -> todo ()
      | "LAP_Se_SetEvent", _ -> todo ()
      | "LAP_Se_ResetEvent", _ -> todo ()
    (* Time *)
      | "LAP_Se_GetTime", _ -> todo ()
      | "LAP_Se_TimedWait", _ -> todo ()
      | "LAP_Se_PeriodicWait", _ -> todo ()
    (* Not allowed: change configured schedule *)
      | "LAP_Se_SetPriority", _ -> todo ()
      | "LAP_Se_Replenish", _ -> todo ()
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
