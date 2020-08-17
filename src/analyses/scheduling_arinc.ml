(** An analysis specification for didactic purposes. *)
open Prelude.Ana
open Analyses_arinc
open Arinc_schedulabilty_domain
open Arinc_cfg

module Spec : Analyses_arinc.ArincSpec =
struct
  include Analyses_arinc.DefaultSpec

  let name () = "scheduling_arinc"
  module SD = Arinc_schedulabilty_domain.D
  module D = Lattice.Lift(SD)(Printable.DefaultNames)
  module G = Lattice.Unit
  module C = Lattice.Unit
  module TInterval = IntDomain.Interval32

  let numtasks = 2

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    failwith "lol, wut?!!!"

  let branch ctx (exp:exp) (tv:bool) : D.t =
    failwith "lol, wut?!!!"

  let body ctx (f:fundec) : D.t =
    failwith "lol, wut?!!!"

  let return ctx (exp:exp option) (f:fundec) : D.t =
    failwith "lol, wut?!!!"

  let get_info_for t s =
    List.at t s

  let update_info_for t fn s =
    List.modify_at t fn s

  let get_info_for_leg t (a,b) =
    if t=0 then a else b

  let update_info_for_leg t v (a,b) =
    if t = 0 then (v,b) else (a,v)

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    let t = Times.start_state 2 in
    let state1 = {
      pid = Pid.of_int (Int64.of_int 1);
      priority = Priority.of_int (Int64.of_int 15);
      period = Period.of_int (Int64.of_int 600);
      capacity = Capacity.of_int (Int64.of_int 600);
      processState = PState.ready;
      waitingFor = WaitingForEvent.bot ()
      } in
    let state2 = {
      pid = Pid.of_int (Int64.of_int 1);
      priority = Priority.of_int (Int64.of_int 10);
      period = Period.top ();
      capacity = Capacity.top ();
      processState = PState.ready;
      waitingFor = WaitingForEvent.bot ()
      }
    in
    [ctx.local, `Lifted([state1; state2], t)]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    failwith "lol, wut?!!!"

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    failwith "lol, wut?!!!"

  (* What if none can run unless time passes? *)
  (* Can ours run relative to other? *)
  let can_run_relative ours other =
    if ours.processState <> PState.ready then
      (* We can not take any actions while not ready *)
      false
    else
      let ours_prio = BatOption.get @@ Priority.to_int ours.priority in
      let other_prio = BatOption.get @@ Priority.to_int other.priority in
      let cmp = Int64.compare ours_prio other_prio in
      if cmp = 0 then
        (* two processes with the same priority *)
        failwith "processes with same prio"
      else if cmp > 0 then
        (* ours > other *)
        true
      else
        (* ours < other *)
        (* we can run if the other is blocked *)
        if other.processState = PState.ready then
          false
        else
          true

  let can_run tid taskstates =
    (* TODO: What if there is only one task *)
    let ours = List.at taskstates tid in
    List.fold_lefti (fun acc i other -> acc && if i=tid then true else can_run_relative ours other) true taskstates

  (* Check if any other task can run relative to t (when assuming that t is not ready) *)
  let any_other_can_run tid taskstates =
    let ours = List.at taskstates tid in
    List.fold_lefti  (fun acc i other -> acc || if i=tid then false else can_run_relative other ours) false taskstates

  (* Get a list of ids of tasks that are: *)
  (* - waiting on something that can happen without any further computation occuring, i.e. *)
  (*      - waiting for period                                                             *)
  (*      - end of a timed wait                                                            *)
  (*   NOT waiting for a signal, for the signal to be set, computation needs to be done    *)
  let tasks_waiting_timed_id tid taskstates =
    let t_with_id = List.mapi (fun i x -> (i,x)) taskstates in
    let fn (i,task) = tid <> i && (task.processState = PState.waiting_for_period || (task.processState = PState.wait && task.waitingFor = WaitingForEvent.bot ())) in
    let res = List.filter fn t_with_id in
    List.map (fun (i,_) -> i) res

  (* Get the join of all waiting times of tasks in tasks *)
  let get_wait_interval tasks times =
    List.fold_left (fun acc i -> TInterval.join  acc (Times.get_remaining_wait i times)) (TInterval.bot ()) tasks

  (* Subtract y from x and ensure result is not negative *)
  let subtract_if_not_zero x y  =
    if TInterval.to_int x = Some Int64.zero then
      x
    else
      let wait_time = TInterval.sub x y in
      TInterval.meet wait_time (TInterval.starting Int64.zero) (* These numbers may not become negative *)

  let wait_for_period (taskstates,times) tid =
    let do_restart_period (taskstates, times) tid =
      let times = Times.set_since_period tid Times.zeroInterval times in
      let times = Times.set_remaining_wait tid Times.zeroInterval times in
      let s = update_info_for tid (fun x -> {x with processState = PState.ready}) taskstates in
      s, times
    in
    let t_since_period = Times.get_since_period tid times in
    let our_waiting_time = Times.get_remaining_wait tid times in
    let period = BatOption.get @@ Period.to_int ((get_info_for taskstates tid).period) in
    if TInterval.leq (TInterval.of_int period) t_since_period then
      do_restart_period (taskstates, times) tid
    else
      (* if no other task can take any action, and this is longest wait time, we can simply increase the time by how long we are waiting for the start of the period *)
      (* TODO: We need to check that this is indeed the longest wait time! *)
      if (not (any_other_can_run tid taskstates)) then
        (* other can not run here, we are in waiting for period *)
        (* TODO: What if both are waiting for a period *)
        (* if other.processState = PState.waiting_for_period then
          failwith "Both waiting for period - Didn't think about this yet" *)
          (* both are waiting for period *)
          (* if t = 0 then
            let min_waiting_time_t0 = Option.get @@ TInterval.minimal waiting_time_t0 in
            let min_waiting_time_t1 = Option.get @@ TInterval.minimal waiting_time_t1 in

            if min_waiting_time_t0 <= min_waiting_time_t1 then
              let times = Times.update_all ["overall"; "since_period_t0"; "since_period_t1"] (TInterval.add  (TInterval.of_int min_waiting_time_t0)) x in
              (* If our waiting time is the smaller one, we can increase all other times by this value *)
              raise Deadcode
            else
              raise Deadcode
          else
            raise Deadcode *)
        (* else *)
          (* Other is blocked for some other reason (not waiting for a period) => We can move ahead*)
          let times = Times.advance_all_times_by our_waiting_time times in
          do_restart_period (taskstates, times) tid
      else
        (* another task can execute some sort of task here, we should let it do its business and then come to what we are doing *)
        raise Deadcode

  let wait_for_endwait (taskstates,times) tid =
    let do_end_wait (s,x) t =
      let times = Times.set_remaining_wait t Times.zeroInterval x in
      let s = update_info_for t (fun x -> {x with processState = PState.ready}) s in
      s, times
    in
    let waiting_time = Times.get_remaining_wait tid times in
    if TInterval.leq Times.zeroInterval waiting_time then
      do_end_wait (taskstates, times) tid
    else
      (* if no other task can take any action, and this is longest wait time, we can simply increase the time by how long we are waiting for the start of the period *)
      (* TODO: We need to check that this is indeed the longest wait time! *)
      if (not (any_other_can_run tid taskstates)) then
        (* other can not run here, we are in waiting for period *)
        (* TODO: What if both are waiting for a period *)
        (* if other.processState = PState.waiting_for_period then
          failwith "Both waiting for period - Didn't think about this yet"
          (* both are waiting for period *)
          (* if t = 0 then
            let min_waiting_time_t0 = Option.get @@ TInterval.minimal waiting_time_t0 in
            let min_waiting_time_t1 = Option.get @@ TInterval.minimal waiting_time_t1 in

            if min_waiting_time_t0 <= min_waiting_time_t1 then
              let times = Times.update_all ["overall"; "since_period_t0"; "since_period_t1"] (TInterval.add  (TInterval.of_int min_waiting_time_t0)) x in
              (* If our waiting time is the smaller one, we can increase all other times by this value *)
              raise Deadcode
            else
              raise Deadcode
          else
            raise Deadcode *)
        else *)
          (* Other is blocked for some other reason (not waiting for a period) => We can move ahead*)
          let times = Times.advance_all_times_by waiting_time times in
          do_end_wait (taskstates, times) tid
      else
        (* the other task can execute some sort of task here, we should let it do its business and then come to what we are doing *)
        raise Deadcode


  let finish_computation (taskstates, times) tid =
    (* We can always take this edge, because we never have a lower bound on how long a computation can take      *)
    (* If I am here, the only thing that could interrupt me is a (higher priority) task finishing waiting on sth *)
    (* If the other task wanted to do computation here, we would raise Deadcode even before calling this         *)
    let wcetInterval = Times.get_remaining_processing tid times in
    let possibleInterrupters = tasks_waiting_timed_id tid taskstates in
    if List.length possibleInterrupters == 0 then
      (* This will be not interrupted *)
      let times = Times.set_remaining_processing tid Times.zeroInterval times in
      let times = Times.advance_all_times_by wcetInterval times in
      taskstates, times
    else
      (* As an improvement, we could model here that a "wait" (N/B not a waiting for period) from a lower priority thread will not have any influence here *)
      (* as even when it's wait ends, it will not interrupt this process *)
      let waiting_time_other = get_wait_interval possibleInterrupters times in
      let less_than_waiting_time = TInterval.lt waiting_time_other wcetInterval in
      if not (TInterval.is_bot (TInterval.meet (TInterval.of_int (Int64.of_int 1)) less_than_waiting_time)) then
        (* This is the minimum interval we can compute for sure, so we can subtract this everywhere. If we take less than this time, we can move on *)
        let minimum_definite_compute_interval = TInterval.of_interval (Int64.zero, BatOption.get @@ TInterval.minimal waiting_time_other) in
        let times = Times.set_remaining_processing tid Times.zeroInterval times in
        let times = Times.advance_all_times_by minimum_definite_compute_interval times in
        taskstates, times
      else
      (* We can finish the entire thing, even if it takes WCET *)
      let times = Times.set_remaining_processing tid Times.zeroInterval times in
      let times = Times.advance_all_times_by wcetInterval times in
      taskstates, times

  let continue_computation (taskstates, times) tid =
    let wcetInterval = Times.get_remaining_processing tid times in
    let possibleInterrupters = tasks_waiting_timed_id tid taskstates in
    if List.length possibleInterrupters == 0 then
      (* this can definitely finish -> we should only take the FinishComputation edge*)
      raise Deadcode
    else
      (* As an improvement, we could model here that a "wait" (N/B not a waiting for period) from a lower priority thread will not have any influence here *)
      (* as even when it's wait ends, it will not interrupt this process *)
      let waiting_time_other = get_wait_interval possibleInterrupters times in
      let less_than_waiting_time = TInterval.lt waiting_time_other wcetInterval in
      if not (TInterval.is_bot (TInterval.meet (TInterval.of_int (Int64.of_int 1)) less_than_waiting_time)) then
        (* This is the minimum interval we can compute for sure, so we can subtract this everywhere *)
        let minimum_definite_compute_interval = TInterval.of_int (Option.get @@ TInterval.minimal waiting_time_other) in
        let _ = Printf.printf "minimum waiting time is %s\n" (string_of_int (Int64.to_int @@ Option.get @@ TInterval.minimal waiting_time_other)) in
        let times = Times.update_remaining_processing tid (fun x -> TInterval.meet wcetInterval (subtract_if_not_zero x minimum_definite_compute_interval)) times in
        let times = Times.advance_all_times_by minimum_definite_compute_interval times in
        taskstates, times
      else
        (* this can definitely finish -> we should only take the FinishComputation edge*)
        raise Deadcode

  let start_computation (taskstates, times) tid wcet =
    let wcetInterval = TInterval.of_interval (Int64.zero, Int64.of_int wcet) in
    let times = Times.set_remaining_processing tid wcetInterval times in
    taskstates, times

  let periodic_wait (taskstates, times) tid node =
    (* Check that the deadline is not violated *)
    let time_since_period = Times.get_since_period tid times in
    let deadline  =  BatOption.get @@ Capacity.to_int ((get_info_for taskstates tid).capacity) in
    let deadline_interval = TInterval.of_int deadline in
    let miss = TInterval.meet (TInterval.of_int (Int64.of_int 1)) (TInterval.gt time_since_period deadline_interval) in
    (if not (TInterval.is_bot miss) then Printf.printf "%s deadline of t%i was missed: %s > %s (!!!!)\n%s \n\n"
      (Arinc_Node.to_string node)
      tid (TInterval.short 80 time_since_period) (TInterval.short 80 deadline_interval)
      (SD.short 800 (taskstates,times))
      else
      ());
    let period = BatOption.get @@ Period.to_int ((get_info_for taskstates tid).period) in
    let remaining_wait = TInterval.sub (TInterval.of_int period) time_since_period in
    let times = Times.set_remaining_wait tid remaining_wait times in (* set remaining wait time *)
    let s = SD.periodic_wait tid taskstates in
    s, times

  let timed_wait (taskstates, times) tid waittime =
    let remaining_wait = TInterval.of_int (Int64.of_int waittime) in
    let times = Times.set_remaining_wait tid remaining_wait times in (* set remaining wait time *)
    let s = SD.timed_wait tid taskstates in
    s, times

  let arinc_edge_not_bot ((taskstates, times) as state) (tid,e) node =
    if tid = -1 then
      (* This is some special edge e.g. during init *)
      state
    else if e = WaitingForPeriod then
      (* The restart of a period can happen at any time even if the task does not have priority *)
      wait_for_period state tid
    else if e = WaitingForEndWait then
      (* The end of waiting can happen at any time if the task does not have priority *)
      wait_for_endwait state tid
    else if not (can_run tid taskstates) then
      (* all other actions can only happen if this task is in running state, and has the highest priority *)
      raise Deadcode
    else
      match e with
      | SuspendTask i -> SD.suspend i taskstates, times
      | ResumeTask i -> SD.resume i taskstates, times
      | WaitEvent i -> SD.wait_event tid i taskstates, times
      | SetEvent i -> SD.set_event i taskstates, times
      | StartComputation wcet -> start_computation state tid wcet
      | FinishComputation -> finish_computation state tid
      | ContinueComputation -> continue_computation state tid
      | PeriodicWait -> periodic_wait state tid node
      | TimedWait tw -> timed_wait state tid tw
      | _ -> state

  let arinc_edge ctx (t,e) =
    match ctx.local with
    | `Lifted x -> `Lifted (arinc_edge_not_bot x (t,e) ctx.node)
    | `Top -> `Top
    | `Bot -> `Bot


  let should_join_one a b =
    a.processState = b.processState && a.waitingFor = b.waitingFor

  let should_join a b =
    match a,b with
    | `Lifted (a,x), `Lifted(b, x') ->
      List.fold_left2 (fun x e f -> x && should_join_one e f) true a b
    | _ -> true

  let val_of () = D.bot ()
  let context _ = ()

  let startstate v = D.bot ()
  let otherstate v = D.bot ()
  let exitstate  v = D.bot ()
end

let _ =
  MCP_arinc.register_analysis (module Spec : ArincSpec)
