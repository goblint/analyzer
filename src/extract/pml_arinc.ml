open BatteriesExceptionless

(* enums *)
type return_code = SUCCESS | ERROR
and partition_mode = IDLE | COLD_START | WARM_START | NORMAL
and status = NOTCREATED | STOPPED | SUSPENDED | WAITING | READY | DONE
and waiting_for = NONE | BLACKBOARD | SEMA | EVENT | TIME
and queuing_discipline = FIFO | PRIO
[@@deriving show { with_path = false }, enum]

let extract_types = ["t123"] (* TODO extract variables of a certain type *)

let init ?(nproc=99) ?(nsema=99) ?(nevent=99) ?(nbboard=99) () = (* TODO better solution for the number of resources? the numbers are only for checking out-of-bounds accesses... *)
  let has_semas = nsema > 0 in
  let open Pml in let open Chan in
  let nproc   = fst @@ var (Byte nproc) "nproc" in
  let nsema   = fst @@ var (Byte nsema) "nsema" in
  let nevent  = fst @@ var (Byte nevent) "nevent" in
  let nbboard = fst @@ var (Byte nbboard) "nbboard" in

  (* Pml.do_; (* ppx_monadic: from now on ; is bind *) *)
  (* switched to ocaml-monadic because ppx_monadic was constraining us to ocaml <4.08, now have to use ;%bind instead of just ; and `let%bind x = e in` instead of `x <-- e;` *)
  (* Dropped ocaml-monadic and used let* syntax introduced in OCaml 4.08. Use `let* () = e in` instead of `e;%bind` *)

  (* type delcarations, TODO generate this? *)
  (* TODO might need adjustment if there are enums with gaps or enums not starting at 0 *)
  let* () = enum return_code_of_enum show_return_code "return_code" in
  let* () = enum partition_mode_of_enum show_partition_mode "partition_mode" in
  let* () = enum status_of_enum show_status "status" in
  let* () = enum waiting_for_of_enum show_waiting_for "waiting_for" in
  let* () = enum queuing_discipline_of_enum show_queuing_discipline "queuing_discipline" in
  (* variable declarations *)
  (* TODO inject: let%s status = arr nprocNOTCREATED in *)
  let* partition_mode = var (Enum (COLD_START, show_partition_mode)) "partition_mode" in
  let* lock_level  = var (Byte 0) "lock_level" in (* scheduling only takes place if this is 0 *)
  let* exclusive   = var (Byte 0) "exclusive" in (* id of process that has exclusive privilige toecute if lockLevel > 0 *)
  let* status      = arr !nproc (Enum (NOTCREATED, show_status)) "status" in
  (* TODO type for structured data types *)
  let* waiting_for = arr !nproc (Enum (NONE, show_waiting_for)) "waiting_for" in
  let* waiting_id  = arr !nproc (Byte 0) "waiting_id" in
  let* () = Macro._if !nsema in
  let* semas       = arr !nsema (Byte 0) "semas" in
  let* semas_max   = arr !nsema (Byte 0) "semas_max" in
  let* semas_chan  = arr !nsema (Chan.create !nproc (Byte 0)) "semas_chan" in
  let* () = Macro._endif in
  let* () = Macro._if !nevent in
  let* events      = arr !nevent (Bool false) "events" in
  let* () = Macro._endif in
  let* () = Macro._if !nbboard in
  let* bboards     = arr !nbboard (Bool false) "bboards" in
  let* () = Macro._endif in

  (* just for asserts *)
  let* tasks_created = var (Byte 0) "tasks_created" in
  let* semas_created = var (Byte 0) "semas_created" in

  (* dummy variables for use in arguments *)
  let tid,tid_decl = var (Byte 0) "tid" in (* this is the id we give out for every new task *)
  (* general arguments *)
  let id,_     = var (Byte 0) "id" in
  let name,_   = var (String "") "name" in
  (*let r,_    = var (Enum (SUCCESS, show_return_code)) "r" in*)

  (* macros - used in extracted pml *)
  let* () = Macro.define "can_run" @@ A1 (id, fun id -> (!status !id == e READY show_status) && (!lock_level == i 0 || !exclusive == !id) && (!partition_mode == e NORMAL show_partition_mode || !id == i 0)) in

  (* helpers - these get inlined *)
  let task_info id = s "status["^i2s id^s "] = "^e2s (!status id)^s ", waiting_for[] = "^e2s (!waiting_for id)^s ", waiting_id[] = "^i2s (!waiting_id id) in
  let sema_info id = s "semas["^i2s id^s "] = "^i2s (!semas id) in
  let set_waiting id wfor wid =
    let* () = println (s "set_waiting: process "^i2s id^s " will wait for "^i2s wid) in
    let* () = waiting_for := id, (e wfor show_waiting_for) in
    let* () = waiting_id  := id, wid in
    status      := id, (e WAITING show_status)
  in
  let set_ready id =
    let* () = println (s "set_ready: process "^i2s id^s " set to ready. "^task_info id) in
    let* () = waiting_for := id, (e NONE show_waiting_for) in
    let* () = waiting_id  := id, i 0 in
    status      := id, (e READY show_status)
  in
  let is_waiting id wfor wid = !status id == e WAITING show_status && !waiting_for id == e wfor show_waiting_for && !waiting_id id == wid in
  let remove_waiting id =
    if has_semas then
      _foreach semas (fun j _ ->
          _ift (poll `Any (!semas_chan j) id) (recv `Any (!semas_chan j) id)
        )
    else let* () = nop in
      waiting_for := id, e NONE show_waiting_for
  in

  (* preemption *)
  let mode,_ = var (Enum (COLD_START, show_partition_mode)) "mode" in
  let* () = extract "LockPreemption" @@ A0 (
      let* () = incr lock_level in
      exclusive := !tid (* TODO is this really changed if lock_level > 0? if yes, it is probably also restored... *)
    ) in
  let* () = extract "UnlockPreemption" @@ A0 (
      _ift (!lock_level > i 0) (decr lock_level)
    ) in
  let* () = extract "SetPartitionMode" @@ A1 (mode, fun mode ->
      partition_mode := !mode
    ) in

  (* processes *)
  let* () = extract "CreateProcess" @@ A1 (id(*; pri; per; cap]*), fun id ->
      let* () = _assert (!status !id == e NOTCREATED show_status) in
      let* () = status := !id, e STOPPED show_status in
      let* () = waiting_for := !id, e NONE show_waiting_for in
      incr tasks_created
    ) in
  (* CreateErrorHandler *)
  let* () = extract "Start" @@ A1 (id, fun id ->
      let* () = _assert (!status !id != e NOTCREATED show_status) in
      let* () = remove_waiting !id in
      status := !id, e READY show_status
    ) in
  let* () = extract "Stop" @@ A1 (id, fun id ->
      let* () = _assert (!status !id != e NOTCREATED show_status) in
      let* () = remove_waiting !id in
      status := !id, e STOPPED show_status
    ) in
  let* () = extract "Suspend" @@ A1 (id, fun id ->
      let* () = _assert (!status !id != e NOTCREATED show_status) in
      status := !id, e SUSPENDED show_status
    ) in
  let* () = extract "Resume" @@ A1 (id, fun id ->
      let* () = _assert (!status !id != e NOTCREATED show_status) in
      let* () = _ift (!status !id == e SUSPENDED show_status) (
          _ifte (!waiting_for !id == e NONE show_waiting_for)
            (status := !id, e READY show_status)
            (status := !id, e WAITING show_status)
        ) in
      status := !id, e SUSPENDED show_status
    ) in

  (* semaphores *)
  let cur,_   = var (Byte 0) "cur" in
  let max,_   = var (Byte 0) "max" in
  let queuing,_ = var (Enum (FIFO, show_queuing_discipline)) "queuing" in
  let* () = extract "CreateSemaphore" ~id:(4,0,"sema") @@ A5 (name,cur,max,queuing,id, fun name cur max queuing id ->
      let* () = println (s "CreateSemaphore: " ^ !name ^s ", "^ i2s !cur ^s ", "^ i2s !max ^s ", "^ e2s !queuing) in
      let* () = _assert (!queuing == e FIFO show_queuing_discipline) in
      let* () = semas := !id, !cur in
      let* () = semas_max := !id, !max in
      incr semas_created
    ) in
  let* () = extract "GetSemaphoreId" ~id:(1,0,"sema") @@ A2 (name, id, fun name id -> skip) in
  let* () = extract "WaitSemaphore" @@ A1 (id, fun id ->
      let id = !id in
      let sema = !semas id in
      let chan = !semas_chan id in
      _if [
        sema == i 0,
        println (s "WaitSema will block: "^sema_info id) >>
        _if [
          full  chan, fail (s "WaitSema: queue is full: "^sema_info id);
          nfull chan, println (s "WaitSema: Process "^i2s !tid^s " put into queue for sema "^i2s id)
        ] >>
        set_waiting !tid SEMA id;
        sema > i 0,
        println (s "WaitSema will go through: "^sema_info id) >>
        incr semas id;
        sema < i 0,
        fail (s "WaitSema: count<0: "^sema_info id)
      ]
    ) in
  extract "SignalSemaphore" @@ A1 (id, fun id ->
      let id = !id in
      let sema = !semas id in
      let chan = !semas_chan id in
      _if [
        (* no processes waiting on this semaphore -> increase count until max *)
        empty chan,
        println (s "SignalSema: empty queue") >>
        _ift (sema < !semas_max id) (incr semas id);
        nempty chan,
        println (s "SignalSema: "^i2s (len chan)^s " processes in queue for "^sema_info id) >>
        _foreach status (fun j _ ->
            println (s "SignalSema: check if process "^i2s j^s " is waiting. "^task_info j) >>
            _ift (is_waiting j SEMA id && poll `First chan j) (* process is waiting for this semaphore and is at the front of its queue *) (
              println (s "SignalSema: process "^i2s !tid^s " is waking up process "^i2s j) >>
              recv `First chan j >> (* consume msg from queue *)
              set_ready j >>
              break
            )
          )
      ]
    )

(* events *)
