open BatteriesExceptionless

(* enums *)
type state = SUSPENDED | WAITING | READY
and waiting_for = NONE | EVENT | RESOURCE
[@@deriving show { with_path = false }, enum]

let init oil =
  (*let has_resources = Hashtbl.length OilUtil.resources > 0 in  *)
  let open Pml in let open Chan in

  let nproc     = fst @@ var (Byte (Hashtbl.length OilUtil.tasks)) "nproc" in
  let nresource = fst @@ var (Byte (Hashtbl.length OilUtil.resources)) "nresource" in
  (*let nevent    = fst @@ var (Byte (Hashtbl.length OilUtil.events)) "nevent" in*)

  Pml.do_; (* from now on ; is bind *)
  (* type delcarations, TODO generate this? *)
  (* TODO might need adjustment if there are enums with gaps or enums not starting at 0 *)
  enum state_of_enum show_state "state";
  enum waiting_for_of_enum show_waiting_for "waiting_for";
  (* variable declarations *)
  state       <-- arr !nproc (Enum (SUSPENDED, show_state)) "state";
  waiting_for <-- arr !nproc (Enum (NONE, show_waiting_for)) "waiting_for";
  waiting_id  <-- arr !nproc (Byte 0) "waiting_id";
  resources   <-- arr !nresource (Byte 0) "resources";
  resources_max <-- arr !nresource (Byte 0) "resources_max";
  resources_chan  <-- arr !nresource (Chan.create !nproc (Byte 0)) "resources_chan";
  events      <-- arr !nproc (Byte 0) "events";
  events_chan <-- arr !nproc (Chan.create !nproc (Byte 0)) "events_chan";
  events_max  <-- arr !nproc (Byte 0) "events_max";

  (* task and argument variables *)
  let tid,tid_decl = var (Byte 0) "tid" in (* used inside a task to refer to its id *)
  let id,_     = var (Byte 0) "id" in
  (*let name,_   = var (String "") "name" in*)

  (* macros - used in extracted pml *)
  define "can_run" @@ A1 (id, fun id -> (!state !id == e READY show_state));

  (* helpers - these get inlined *)
  let task_info id = s "state["^i2s id^s "] = "^e2s (!state id)^s ", waiting_for[] = "^e2s (!waiting_for id)^s ", waiting_id[] = "^i2s (!waiting_id id) in
  let resource_info id = s "resources["^i2s id^s "] = "^i2s (!resources id) in
  let event_info id = s "events["^i2s id^s "] = "^i2s (!events id) in
  let set_waiting id wfor wid = Pml.do_;
    println (s "set_waiting: process "^i2s id^s " will wait for "^i2s wid);
    waiting_for := id, (e wfor show_waiting_for);
    waiting_id  := id, wid;
    state      := id, (e WAITING show_state)
  in
  let set_ready id = Pml.do_;
    println (s "set_ready: process "^i2s id^s " set to ready. "^task_info id);
    waiting_for := id, (e NONE show_waiting_for);
    waiting_id  := id, i 0;
    state      := id, (e READY show_state)
  in
  let is_waiting id wfor wid = !state id == e WAITING show_state && !waiting_for id == e wfor show_waiting_for && !waiting_id id == wid in  
  (*let remove_waiting id = Pml.do_;
    if has_resources then
      _foreach resources (fun j _ ->
          _ift (poll `Any (!resources_chan j) id) (recv `Any (!resources_chan j) id)
        )
    else nop;
    waiting_for := id, e NONE show_waiting_for;
  in*)

  (* Specification of operating system services *)
  (* Task management *)
  (*extract "DeclareTask" @@ A1 (id, fun id -> Pml.do_;
    nop
  );*)
  extract "ActivateTask" @@ A1 (id, fun id -> Pml.do_;
    state := !id, (e READY show_state)
    (* TODO When an extended task is transferred from suspended state into ready state all its events are cleared. *)
  );
  extract "TerminateTask" @@ A0 (Pml.do_;
    state := !tid, (e SUSPENDED show_state)
    (* TODO NON release internal resource *)
  );
  extract "ChainTask" @@ A1 (id, fun id -> Pml.do_;
    state := !tid, (e SUSPENDED show_state);
    state := !id, (e READY show_state)
    (* TODO NON ensures that the succeeding task starts to run at the earliest?, release internal resource *)
  );
  extract "Schedule" @@ A0 (Pml.do_;
    nop
    (* TODO NON release internal resource *)
  );
  extract "GetTaskID" @@ A1 (id, fun id -> Pml.do_;
    (* TODO ANA assign tid to id *)
    nop
  );
  extract "GetTaskState" @@ A1 (id,(* state, *) fun id -> Pml.do_;
    (* TODO ANA Returns the state of a task (running, ready, waiting, suspended) at the time of calling GetTaskState. *)
    nop
  );

  (* Interrupt handling *)
  extract "EnableAllInterrupts" @@ A0 (Pml.do_;
    nop
  );
  extract "DisableAllInterrupts" @@ A0 (Pml.do_;
    nop
  );
  extract "ResumeAllInterrupts" @@ A0 (Pml.do_;
    nop
  );
  extract "SuspendAllInterrupts" @@ A0 (Pml.do_;
    nop
  );
  extract "ResumeOSInterrupts" @@ A0 (Pml.do_;
    nop
  );
  extract "SuspendOSInterrupts" @@ A0 (Pml.do_;
    nop
  );

  (* Resource management *)
  (*extract "DeclareResource" @@ A1 (id, fun id -> Pml.do_;
    nop
  );*)
  extract "GetResource" @@ A1 (id, fun id -> Pml.do_;
    let id = !id in
    let resource = !resources id in
    let chan = !resources_chan id in
    _if [
      resource == i 0,
        println (s "GetResource will block: "^resource_info id) >>
        _if [
          full  chan, fail (s "GetResource: queue is full: "^resource_info id);
          nfull chan, println (s "GetResource: Process "^i2s !tid^s " put into queue for resource "^i2s id)
        ] >>
        set_waiting !tid RESOURCE id;
      resource > i 0,
        println (s "GetResource will go through: "^resource_info id) >>
        incr resources id;
      resource < i 0,
        fail (s "GetResource: count<0: "^resource_info id)
    ]
  );
  extract "ReleaseResource" @@ A1 (id, fun id -> Pml.do_;
    let id = !id in
    let resource = !resources id in
    let chan = !resources_chan id in
    _if [
      (* no processes waiting on this resourcephore -> increase count until max *)
      empty chan,
        println (s "ReleaseResource: empty queue") >>
        _ift (resource < !resources_max id) (incr resources id);
      nempty chan,
        println (s "ReleaseResource: "^i2s (len chan)^s " processes in queue for "^resource_info id) >>
        _foreach state (fun j _ ->
          println (s "ReleaseResource: check if process "^i2s j^s " is waiting. "^task_info j) >>
          _ift (is_waiting j RESOURCE id && poll `First chan j) (* process is waiting for this resource and is at the front of its queue *) (
              println (s "ReleaseResource: process "^i2s !tid^s " is waking up process "^i2s j) >>
              recv `First chan j >> (* consume msg from queue *)
              set_ready j >>
              break
          )
        )
    ]
  );

  (* Event control *)
  let mask,_ = var (Byte 0) "mask" in  
  (*extract "DeclareEvent" @@ A1 (id, fun id -> Pml.do_;
    nop
  );*)
  extract "SetEvent" @@ A2 (id, mask, fun id mask -> Pml.do_;
    events := !id, !mask
  );
  extract "ClearEvent" @@ A1 (mask, fun mask -> Pml.do_;
    (*events := !id, mask*)
    let id = !tid in
    let event = !events id in
    let chan = !events_chan id in
    _if [
      (* no processes waiting on this event -> increase count until max *)
      empty chan,
        println (s "ClearEvent: empty queue") >>
        _ift (event < !events_max id) (incr events id);
      nempty chan,
        println (s "ClearEvent: "^i2s (len chan)^s " processes in queue for "^event_info id) >>
        _foreach state (fun j _ ->
          println (s "ClearEvent: check if process "^i2s j^s " is waiting. "^task_info j) >>
          _ift (is_waiting j EVENT id && poll `First chan j) (* process is waiting for this event and is at the front of its queue *) (
              println (s "ClearEvent: process "^i2s !tid^s " is waking up process "^i2s j) >>
              recv `First chan j >> (* consume msg from queue *)
              set_ready j >>
              break
          )
        )
    ]
  );
  extract "GetEvent" @@ A2 (id, mask, fun id mask -> Pml.do_;
    (* TODO ANA? *)
    !events !id
  );
  extract "WaitEvent" @@ A1 (mask, fun mask -> Pml.do_;
    let id = !id in
    let resource = !resources id in
    let chan = !resources_chan id in
    _if [
      resource == i 0,
        println (s "WaitEvent will block: "^event_info id) >>
        _if [
          full  chan, fail (s "WaitEvent: queue is full: "^event_info id);
          nfull chan, println (s "WaitEvent: Process "^i2s !tid^s " put into queue for resource "^i2s id)
        ] >>
        set_waiting !tid EVENT id;
      resource > i 0,
        println (s "WaitEvent will go through: "^event_info id) >>
        incr resources id;
      resource < i 0,
        fail (s "WaitEvent: count<0: "^event_info id)
    ]
  );

  (* Alarms *)
  (*extract "DeclareAlarm" @@ A1 (id, fun id -> Pml.do_;
    nop
  );*)
  extract "GetAlarmBase" @@ A1 (id,(* info, *) fun id -> Pml.do_;
    (* TODO ANA *)
    nop
  );
  extract "GetAlarm" @@ A1 (id,(* tick, *) fun id -> Pml.do_;
    (* TODO ANA *)
    nop
  );
  extract "SetRelAlarm" @@ A1 (id,(* increment, cycle, *) fun id -> Pml.do_;
    (* TODO *)
    nop
  );
  extract "SetAbsAlarm" @@ A1 (id,(* start, cycle, *) fun id -> Pml.do_;
    (* TODO *)
    nop
  );
  extract "CancelAlarm" @@ A1 (id, fun id -> Pml.do_;
    (* TODO *)
    nop
  );