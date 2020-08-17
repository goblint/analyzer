(** An analysis specification for didactic purposes. *)
open Prelude.Ana
open Analyses_arinc
open Arinc_schedulabilty_domain
open Arinc_cfg

module Spec : Analyses_arinc.ArincSpec =
struct
  include Analyses_arinc.DefaultSpec

  let name () = "scheduling_arinc"
  module DInner = Arinc_schedulabilty_domain.D
  module D = Lattice.Lift(DInner)(Printable.DefaultNames)
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

  let can_run t s =
    (* TODO: What if there is only one task *)
    let ours = List.at s t in
    List.fold_lefti (fun acc i other -> acc && if i=t then true else can_run_relative ours other) true s

  (* Check if any other task can run relative to t (when assuming that t is not ready) *)
  let one_other_can_run t s =
    let ours = List.at s t in
    List.fold_lefti  (fun acc i other -> acc || if i=t then false else can_run_relative other ours) false s

  (* Get a list of ids of tasks that are: *)
  (* - waiting on something that can happen without any further computation occuring, i.e. *)
  (*      - waiting for period                                                             *)
  (*      - end of a timed wait                                                            *)
  (*   NOT waiting for a signal, for the signal to be set, computation needs to be done    *)
  let tasks_waiting_timed_id t s =
    let t_with_id = List.mapi (fun i x -> (i,x)) s in
    let fn (i,task) = t <> i && (task.processState = PState.waiting_for_period || (task.processState = PState.wait && task.waitingFor = WaitingForEvent.bot ())) in
    let res = List.filter fn t_with_id in
    List.map (fun (i,_) -> i) res

  let get_wait_interval tasks times =
    List.fold_left (fun acc i -> TInterval.join  acc (Times.get_remaining_wait i times)) (TInterval.bot ()) tasks

  (* Subtract y from x and ensure result is not negative *)
  let subtract_if_not_zero x y  =
    if TInterval.to_int x = Some Int64.zero then
      x
    else
      let wait_time = TInterval.sub x y in
      TInterval.meet wait_time (TInterval.starting Int64.zero) (* These numbers may not become negative *)

  let wait_for_period (s,x) t =
    let do_restart_period (s, x) t =
      let times = Times.set_since_period t Times.zeroInterval x in
      let times = Times.set_remaining_wait t Times.zeroInterval times in
      let s = update_info_for t (fun x -> {x with processState = PState.ready}) s in
      s, times
    in
    let t_since_period = Times.get_since_period t x in
    let our_waiting_time = Times.get_remaining_wait t x in
    let period = BatOption.get @@ Period.to_int ((get_info_for s t).period) in
    if TInterval.leq (TInterval.of_int period) t_since_period then
      do_restart_period (s, x) t
    else
      (* if no other task can take any action, and this is longest wait time, we can simply increase the time by how long we are waiting for the start of the period *)
      (* TODO: We need to check that this is indeed the longest wait time! *)
      if (not (one_other_can_run t s)) then
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
          let times = Times.advance_all_times_by our_waiting_time x in
          do_restart_period (s, times) t
      else
        (* another task can execute some sort of task here, we should let it do its business and then come to what we are doing *)
        raise Deadcode

  let wait_for_endwait (s,x) t =
    let do_end_wait (s,x) t =
      let times = Times.set_remaining_wait t Times.zeroInterval x in
      let s = update_info_for t (fun x -> {x with processState = PState.ready}) s in
      s, times
    in
    let waiting_time = Times.get_remaining_wait t x in
    if TInterval.leq Times.zeroInterval waiting_time then
      do_end_wait (s, x) t
    else
      (* if no other task can take any action, and this is longest wait time, we can simply increase the time by how long we are waiting for the start of the period *)
      (* TODO: We need to check that this is indeed the longest wait time! *)
      if (not (one_other_can_run t s)) then
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
          let times = Times.advance_all_times_by waiting_time x in
          do_end_wait (s, times) t
      else
        (* the other task can execute some sort of task here, we should let it do its business and then come to what we are doing *)
        raise Deadcode

  let arinc_edge xin (t,e) node =
    let s, x = xin in
    if t = -1 then
      (* This is some special edge e.g. during init *)
      xin
    else if e = WaitingForPeriod then
      (* The restart of a period can happen at any time even if the task does not have priority *)
      wait_for_period (s,x) t
    else if e = WaitingForEndWait then
      (* The end of waiting can happen at any time if the task does not have priority *)
      wait_for_endwait (s,x) t
    else
      if not (can_run t s) then
        (* all other actions can only happen if this task is in running state, and has the highest priority *)
        raise Deadcode
      else
        match e with
        | SuspendTask i -> DInner.suspend i xin
        | ResumeTask i -> DInner.resume i xin
        | WaitEvent i -> DInner.wait_event t i xin
        | SetEvent i -> DInner.set_event i xin
        | StartComputation i ->
          let wcetInterval = TInterval.of_interval (Int64.zero, Int64.of_int i) in
          let times = Times.set_remaining_processing t wcetInterval x in
          s, times
        | FinishComputation ->
          (* We can always take this edge, because we never have a lower bound on how long a computation can take *)
          begin
            (* If I am here, the only thing that could interrupt me is a higher priority task finishing waiting on sth *)
            (* If the other task wanted to do computation here, we would raise Deadcode even before *)
            let wcetInterval = Times.get_remaining_processing t x in
            let possibleInterrupters = tasks_waiting_timed_id t s in
            if List.length possibleInterrupters == 0 then
              (* This will be not interrupted *)
              let times = Times.set_remaining_processing t Times.zeroInterval x in
              let times = Times.advance_all_times_by wcetInterval times in
              s, times
            else
              (* As an improvement, we could model here that a "wait" (N/B not a waiting for period) from a lower priority thread will not have any influence here *)
              (* as even when it's wait ends, it will not interrupt this process *)
              let waiting_time_other = get_wait_interval possibleInterrupters x in
              let less_than_waiting_time = TInterval.lt waiting_time_other wcetInterval in
              if not (TInterval.is_bot (TInterval.meet (TInterval.of_int (Int64.of_int 1)) less_than_waiting_time)) then
                (* This is the minimum interval we can compute for sure, so we can subtract this everywhere. If we take less than this time, we can move on *)
                let minimum_definite_compute_interval = TInterval.of_interval (Int64.zero, BatOption.get @@ TInterval.minimal waiting_time_other) in
                let times = Times.set_remaining_processing t Times.zeroInterval x in
                let times = Times.advance_all_times_by minimum_definite_compute_interval times in
                s, times
              else
              (* We can finish the entire thing, even if it takes WCET *)
              let times = Times.set_remaining_processing t Times.zeroInterval x in
              let times = Times.advance_all_times_by wcetInterval times in
              s, times
          end
        | ContinueComputation ->
          begin
          let wcetInterval = Times.get_remaining_processing t x in
          let possibleInterrupters = tasks_waiting_timed_id t s in
          if List.length possibleInterrupters == 0 then
            (* this can definitely finish -> we should only take the FinishComputation edge*)
            raise Deadcode
          else
            (* As an improvement, we could model here that a "wait" (N/B not a waiting for period) from a lower priority thread will not have any influence here *)
            (* as even when it's wait ends, it will not interrupt this process *)
            let waiting_time_other = get_wait_interval possibleInterrupters x in
            let less_than_waiting_time = TInterval.lt waiting_time_other wcetInterval in
            if not (TInterval.is_bot (TInterval.meet (TInterval.of_int (Int64.of_int 1)) less_than_waiting_time)) then
              (* This is the minimum interval we can compute for sure, so we can subtract this everywhere *)
              let minimum_definite_compute_interval = TInterval.of_int (Option.get @@ TInterval.minimal waiting_time_other) in
              let _ = Printf.printf "minimum waiting time is %s\n" (string_of_int (Int64.to_int @@ Option.get @@ TInterval.minimal waiting_time_other)) in
              let times = Times.update_remaining_processing t (fun x -> TInterval.meet wcetInterval (subtract_if_not_zero x minimum_definite_compute_interval)) x in
              let times = Times.advance_all_times_by minimum_definite_compute_interval times in
              s, times
            else
              (* this can definitely finish -> we should only take the FinishComputation edge*)
              raise Deadcode
          end
        | PeriodicWait ->
          (* Check that the deadline is not violated *)
          let time_since_period = Times.get_since_period t x in
          let deadline  =  BatOption.get @@ Capacity.to_int ((get_info_for s t).capacity) in
          let deadline_interval = TInterval.of_int deadline in
          let miss = TInterval.meet (TInterval.of_int (Int64.of_int 1)) (TInterval.gt time_since_period deadline_interval) in
          let PC [u; v] = node in
          (if not (TInterval.is_bot miss) then Printf.printf "(%s,%s) deadline of t%i was missed: %s > %s (!!!!)\n%s \n\n"
            (string_of_int u) (string_of_int v)
            t (TInterval.short 80 time_since_period) (TInterval.short 80 deadline_interval)
           (DInner.short 800 xin)
           else
           ());
          let period = BatOption.get @@ Period.to_int ((get_info_for s t).period) in
          let remaining_wait = TInterval.sub (TInterval.of_int period) time_since_period in
          let times = Times.set_remaining_wait t remaining_wait x in (* set remaining wait time *)
          let s, _ = DInner.periodic_wait t xin in
          s, times
        | TimedWait tw ->
          let remaining_wait = TInterval.of_int (Int64.of_int tw) in
          let times = Times.set_remaining_wait t remaining_wait x in (* set remaining wait time *)
          let s, _ = DInner.timed_wait t xin in
          s, times
        | _ -> xin

  let arinc_edge ctx (t,e) =
    match ctx.local with
    | `Lifted x -> `Lifted (arinc_edge x (t,e) ctx.node)
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
