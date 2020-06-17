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
  (* Pml.do_; (* ppx_monadic: from now on ; is bind *) *)
  (* switched to ocaml-monadic because ppx_monadic was constraining us to ocaml <4.08, now have to use ;%bind instead of just ; and `let%bind x = e in` instead of `x <-- e;` *)

  (* type delcarations, TODO generate this? *)
  (* TODO might need adjustment if there are enums with gaps or enums not starting at 0 *)
  enum state_of_enum show_state "state";%bind
  enum waiting_for_of_enum show_waiting_for "waiting_for";%bind
  (* variable declarations *)
  let%bind state       = arr !nproc (Enum (SUSPENDED, show_state)) "state" in
  let%bind waiting_for = arr !nproc (Enum (NONE, show_waiting_for)) "waiting_for" in
  let%bind waiting_id  = arr !nproc (Byte 0) "waiting_id" in
  let%bind resources   = arr !nresource (Byte 0) "resources" in
  let%bind resources_max = arr !nresource (Byte 0) "resources_max" in
  let%bind resources_chan  = arr !nresource (Chan.create !nproc (Byte 0)) "resources_chan" in
  let%bind events      = arr !nproc (Byte 0) "events" in
  let%bind events_chan = arr !nproc (Chan.create !nproc (Byte 0)) "events_chan" in
  let%bind events_max  = arr !nproc (Byte 0) "events_max" in

  (* task and argument variables *)
  let tid,tid_decl = var (Byte 0) "tid" in (* used inside a task to refer to its id *)
  let id,_     = var (Byte 0) "id" in
  (*let name,_   = var (String "") "name" in*)

  (* macros - used in extracted pml *)
  Macro.define "can_run" @@ A1 (id, fun id -> (!state !id == e READY show_state));%bind

  (* helpers - these get inlined *)
  let task_info id = s "state["^i2s id^s "] = "^e2s (!state id)^s ", waiting_for[] = "^e2s (!waiting_for id)^s ", waiting_id[] = "^i2s (!waiting_id id) in
  let resource_info id = s "resources["^i2s id^s "] = "^i2s (!resources id) in
  let event_info id = s "events["^i2s id^s "] = "^i2s (!events id) in
  let set_waiting id wfor wid =
    println (s "set_waiting: process "^i2s id^s " will wait for "^i2s wid);%bind
    waiting_for := id, (e wfor show_waiting_for);%bind
    waiting_id  := id, wid;%bind
    state      := id, (e WAITING show_state)
  in
  let set_ready id =
    println (s "set_ready: process "^i2s id^s " set to ready. "^task_info id);%bind
    waiting_for := id, (e NONE show_waiting_for);%bind
    waiting_id  := id, i 0;%bind
    state      := id, (e READY show_state)
  in
  let is_waiting id wfor wid = !state id == e WAITING show_state && !waiting_for id == e wfor show_waiting_for && !waiting_id id == wid in
  (*let remove_waiting id =
    if has_resources then
      _foreach resources (fun j _ ->
          _ift (poll `Any (!resources_chan j) id) (recv `Any (!resources_chan j) id)
        )
    else nop;%bind
    waiting_for := id, e NONE show_waiting_for
  in*)

  (* Specification of operating system services *)
  (* Task management *)
  (*extract "DeclareTask" @@ A1 (id, fun id ->
    nop
    );*)
  extract "ActivateTask" @@ A1 (id, fun id ->
      state := !id, (e READY show_state)
      (* TODO When an extended task is transferred from suspended state into ready state all its events are cleared. *)
    );%bind
  extract "TerminateTask" @@ A0 (
    state := !tid, (e SUSPENDED show_state)
    (* TODO NON release internal resource *)
  );%bind
  extract "ChainTask" @@ A1 (id, fun id ->
      state := !tid, (e SUSPENDED show_state);%bind
      state := !id, (e READY show_state)
      (* TODO NON ensures that the succeeding task starts to run at the earliest?, release internal resource *)
    );%bind
  extract "Schedule" @@ A0 (
    nop
    (* TODO NON release internal resource *)
  );%bind
  extract "GetTaskID" @@ A1 (id, fun id ->
      (* TODO ANA assign tid to id *)
      nop
    );%bind
  extract "GetTaskState" @@ A1 (id,(* state, *) fun id ->
      (* TODO ANA Returns the state of a task (running, ready, waiting, suspended) at the time of calling GetTaskState. *)
      nop
    );%bind

  (* Interrupt handling *)
  extract "EnableAllInterrupts" @@ A0 (
    nop
  );%bind
  extract "DisableAllInterrupts" @@ A0 (
    nop
  );%bind
  extract "ResumeAllInterrupts" @@ A0 (
    nop
  );%bind
  extract "SuspendAllInterrupts" @@ A0 (
    nop
  );%bind
  extract "ResumeOSInterrupts" @@ A0 (
    nop
  );%bind
  extract "SuspendOSInterrupts" @@ A0 (
    nop
  );%bind

  (* Resource management *)
  (*extract "DeclareResource" @@ A1 (id, fun id ->
    nop
    );%bind*)
  extract "GetResource" @@ A1 (id, fun id ->
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
    );%bind
  extract "ReleaseResource" @@ A1 (id, fun id ->
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
    );%bind

  (* Event control *)
  let mask,_ = var (Byte 0) "mask" in
  (*extract "DeclareEvent" @@ A1 (id, fun id ->
    nop
    );%bind*)
  extract "SetEvent" @@ A2 (id, mask, fun id mask ->
      events := !id, !mask
    );%bind
  extract "ClearEvent" @@ A1 (mask, fun mask ->
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
    );%bind
  extract "GetEvent" @@ A2 (id, mask, fun id mask ->
      (* TODO ANA? *)
      !events !id
    );%bind
  extract "WaitEvent" @@ A1 (mask, fun mask ->
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
    );%bind

  (* Alarms *)
  (*extract "DeclareAlarm" @@ A1 (id, fun id ->
    nop
    );%bind*)
  extract "GetAlarmBase" @@ A1 (id,(* info, *) fun id ->
      (* TODO ANA *)
      nop
    );%bind
  extract "GetAlarm" @@ A1 (id,(* tick, *) fun id ->
      (* TODO ANA *)
      nop
    );%bind
  extract "SetRelAlarm" @@ A1 (id,(* increment, cycle, *) fun id ->
      (* TODO *)
      nop
    );%bind
  extract "SetAbsAlarm" @@ A1 (id,(* start, cycle, *) fun id ->
      (* TODO *)
      nop
    );%bind
  extract "CancelAlarm" @@ A1 (id, fun id ->
      (* TODO *)
      nop
    )
