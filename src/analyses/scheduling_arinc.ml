(** An analysis specification for didactic purposes. *)
open Prelude.Ana
open Analyses_arinc
open Arinc_schedulabilty_domain
open Arinc_cfg

module Spec : Analyses_arinc.ArincSpec =
struct
  include Analyses_arinc.DefaultSpec

  let name () = "scheduling_arinc"
  module D = Arinc_schedulabilty_domain.D
  module G = Arinc_schedulabilty_domain.D
  module C = Lattice.Unit
  module TInterval = IntDomain.Interval32

  let numtasks = 2
  let zeroInterval = TInterval.of_int Int64.zero

  (* time since period for each task *)
  let update_since_period tid fn times =
    Times.update_val ("since_period_t" ^ string_of_int(tid)) fn times

  let set_since_period tid v times =
    update_since_period tid (fun _ -> v) times

  let get_since_period tid times =
    Times.find ("since_period_t" ^ string_of_int(tid)) times


  (* remaining wait time for each task *)
  let update_remaining_wait tid fn times =
    Times.update_val ("remaining_wait_t" ^ string_of_int(tid)) fn times

  let set_remaining_wait tid v times =
    update_remaining_wait tid (fun _ -> v) times

  let get_remaining_wait tid times =
    Times.find ("remaining_wait_t" ^ string_of_int(tid)) times

  (* remaining compute time for each task *)
  let update_remaining_processing tid fn times =
    Times.update_val ("remaining_processing_t" ^ string_of_int(tid)) fn times

  let set_remaining_processing tid v times =
    update_remaining_processing tid (fun _ -> v) times

  let get_remaining_processing tid times =
    Times.find ("remaining_processing_t" ^ string_of_int(tid)) times

  (* overall times *)
  let update_overall fn times =
    Times.update_val "overall" fn times


  (** Advance overall and all since_period and remaining_wait by interval  *)
  let advance_all_times_by interval times =
    (* Subtract interval from x and ensure result is not negative *)
    let decrement x  = (* TODO: Do we need this here, even if times does the work to ensure it does not get negative *)
      if TInterval.to_int x = Some Int64.zero then
        x
      else
        let wait_time = TInterval.sub x interval in
        TInterval.meet wait_time (TInterval.starting Int64.zero) (* These numbers may not become negative *)
    in
    let increment = TInterval.add interval in
    let all_tids = List.range 0 `To (numtasks-1) in
    (* update overall time *)
    let times = update_overall increment times in
    (* update since_period for all tasks *)
    let times = List.fold_left (fun times tid -> update_since_period tid increment times) times all_tids in
    (* update remaining_wait for all tasks *)
    let times = List.fold_left (fun times tid -> update_remaining_wait tid decrement times) times all_tids in
    times


  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    failwith "lol, wut?!!!"

  let branch ctx (exp:exp) (tv:bool) : D.t =
    failwith "lol, wut?!!!"

  let body ctx (f:fundec) : D.t =
    failwith "lol, wut?!!!"

  let return ctx (exp:exp option) (f:fundec) : D.t =
    failwith "lol, wut?!!!"

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    let t = Times.add "overall" zeroInterval (Times.bot ()) in
    let t = Times.add_list_set ["since_period_t0"; "since_period_t1"] zeroInterval t in
    let t = Times.add_list_set ["remaining_wait_t0"; "remaining_wait_t1"] zeroInterval t in
    let t = Times.add_list_set ["remaining_processing_t0"; "remaining_processing_t1"] zeroInterval t in
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
    [ctx.local, ((state1, state2), t)]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    failwith "lol, wut?!!!"

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    failwith "lol, wut?!!!"

  (* What if none can run unless time passes? *)
  let can_run ours other =
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


  (* Subtract y from x and ensure result is not negative *)
  let subtract_if_not_zero x y  =
    if TInterval.to_int x = Some Int64.zero then
      x
    else
      let wait_time = TInterval.sub x y in
      TInterval.meet wait_time (TInterval.starting Int64.zero) (* These numbers may not become negative *)

  let wait_for_period ((a,b),x) t =
    let do_restart_period ((a,b), x) t =
      let times = set_since_period t zeroInterval x in
      let times = set_remaining_wait t zeroInterval times in
      if t = 0 then
        ({a with processState = PState.ready},b), times
      else
        (a, {b with processState = PState.ready}), times
    in
    let t_since_period = get_since_period t x in
    let period = BatOption.get @@ Period.to_int (if t = 0 then a.period else b.period) in
    if TInterval.leq (TInterval.of_int period) t_since_period then
      do_restart_period ((a,b), x) t
    else
      (* if no other task can take any action, and this is longest wait time, we can simply increase the time by how long we are waiting for the start of the period *)
      let waiting_time_t0 = get_remaining_wait 0 x in
      let waiting_time_t1 = get_remaining_wait 1 x in
      let our_waiting_time = if t=0 then waiting_time_t0 else waiting_time_t1 in
      let ours, other = if t = 0 then a, b else b, a in
      if (not (can_run other ours)) then
        (* other can not run here, we are in waiting for period *)
        if other.processState = PState.waiting_for_period then
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
        else
          (* Other is blocked for some other reason (not waiting for a period) => We can move ahead*)
          let times = advance_all_times_by our_waiting_time x in
          do_restart_period ((a,b), times) t
      else
        (* the other task can execute some sort of task here, we should let it do its business and then come to what we are doing *)
        raise Deadcode

  let wait_for_endwait ((a,b),x) t =
    let do_end_wait ((a,b), x) t =
      let times = set_remaining_wait t zeroInterval x in
      if t = 0 then
        ({a with processState = PState.ready},b), times
      else
        (a, {b with processState = PState.ready}), times
    in
    let waiting_time = get_remaining_wait t x in
    if TInterval.leq zeroInterval waiting_time then
      do_end_wait ((a,b), x) t
    else
      (* if no other task can take any action, and this is longest wait time, we can simply increase the time by how long we are waiting for the start of the period *)
      let waiting_time_t0 = get_remaining_wait 0 x in
      let waiting_time_t1 = get_remaining_wait 1 x in
      let our_waiting_time = if t=0 then waiting_time_t0 else waiting_time_t1 in
      let ours, other = if t = 0 then a, b else b, a in
      if (not (can_run other ours)) then
        (* other can not run here, we are in waiting for period *)
        if other.processState = PState.waiting_for_period then
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
        else
          (* Other is blocked for some other reason (not waiting for a period) => We can move ahead*)
          let times = advance_all_times_by our_waiting_time x in
          do_end_wait ((a,b), times) t
      else
        (* the other task can execute some sort of task here, we should let it do its business and then come to what we are doing *)
        raise Deadcode

  let arinc_edge ctx (t,e) =
    let (a, b), x = ctx.local in
    if t = -1 then
      (* This is some special edge e.g. during init *)
      ctx.local
    else if e = WaitingForPeriod then
      (* The restart of a period can happen at any time even if the task does not have priority *)
      wait_for_period ((a,b),x) t
    else if e = WaitingForEndWait then
      (* The end of waiting can happen at any time if the task does not have priority *)
      wait_for_endwait ((a,b),x) t
    else
      let ours, other = if t = 0 then a, b else b, a in
      if not (can_run ours other) then
          if not (can_run other ours) then
            begin
              (* Printf.printf "No task can run ?!\n"; *)
              raise Deadcode
            end
          else
            raise Deadcode
      else
        match e with
        | SuspendTask i -> D.suspend i ctx.local
        | ResumeTask i -> D.resume i ctx.local
        | WaitEvent i -> D.wait_event t i ctx.local
        | SetEvent i -> D.set_event i ctx.local
        | StartComputation i ->
          let wcetInterval = TInterval.of_interval (Int64.zero, Int64.of_int i) in
          let times = set_remaining_processing t wcetInterval x in
          (a,b), times
        | FinishComputation ->
          begin
            let wcetInterval = Times.find ("remaining_processing_t" ^ string_of_int(t)) x in
            (* If I am here, the only thing that could interrupt me is a higher priority task finishing waiting on sth *)
            (* If the other task wanted to do computation here, we would raise Deadcode even before *)
            if other.processState = PState.waiting_for_period || (other.processState = PState.wait && other.waitingFor = WaitingForEvent.bot () ) then
              (* As an improvement, we could model here that a "wait" (N/B not a waiting for period) from a lower priority thread will not have any influence here *)
              (* as even when it's wait ends, it will not interrupt this process *)
              let waiting_time_other = get_remaining_wait (if t = 1 then 0 else 1) x in
              let less_than_waiting_time = TInterval.lt waiting_time_other wcetInterval in
              if not (TInterval.is_bot (TInterval.meet (TInterval.of_int (Int64.of_int 1)) less_than_waiting_time)) then
              (* This is the minimum interval we can compute for sure, so we can subtract this everywhere. If we take less than this time, we can move on *)
                let minimum_definite_compute_interval = TInterval.of_interval (Int64.zero, BatOption.get @@ TInterval.minimal waiting_time_other) in
                let times = set_remaining_processing t zeroInterval x in
                let times = advance_all_times_by minimum_definite_compute_interval times in
                (a,b), times
              else
              (* We can finish the entire thing, even if it takes WCET *)
              let times = set_remaining_processing t zeroInterval x in
              let times = advance_all_times_by wcetInterval times in
              (a,b), times
            else
              let times = set_remaining_processing t zeroInterval x in
              let times = advance_all_times_by wcetInterval times in
              (a,b), times
          end
        | ContinueComputation ->
          let wcetInterval = get_remaining_processing t x in
          if other.processState = PState.waiting_for_period || (other.processState = PState.wait && other.waitingFor = WaitingForEvent.bot () ) then
              (* As an improvement, we could model here that a "wait" (N/B not a waiting for period) from a lower priority thread will not have any influence here *)
              (* as even when it's wait ends, it will not interrupt this process *)
              let waiting_time_other = get_remaining_wait (if t = 1 then 0 else 1) x in
              let less_than_waiting_time = TInterval.lt waiting_time_other wcetInterval in
              if not (TInterval.is_bot (TInterval.meet (TInterval.of_int (Int64.of_int 1)) less_than_waiting_time)) then
                (* This is the minimum interval we can compute for sure, so we can subtract this everywhere *)
                let minimum_definite_compute_interval = TInterval.of_int (Option.get @@ TInterval.minimal waiting_time_other) in
                let _ = Printf.printf "minimum waiting time is %s\n" (string_of_int (Int64.to_int @@ Option.get @@ TInterval.minimal waiting_time_other)) in
                let times = update_remaining_processing t (fun x -> TInterval.meet wcetInterval (subtract_if_not_zero x minimum_definite_compute_interval)) x in
                let times = advance_all_times_by minimum_definite_compute_interval times in
                (a,b), times
              else
                (* this can definitely finish -> we should only take the FinishComputation edge*)
                raise Deadcode
          else
            (* this can definitely finish -> we should only take the FinishComputation edge*)
            raise Deadcode
        | PeriodicWait ->
          (* Check that the deadline is not violated *)
          let time_since_period = get_since_period t x in
          let deadline  = BatOption.get @@ Capacity.to_int (if t = 0 then a.capacity else b.capacity) in
          let deadline_interval = TInterval.of_int deadline in
          let miss = TInterval.meet (TInterval.of_int (Int64.of_int 1)) (TInterval.gt time_since_period deadline_interval) in
          let PC [u; v] = ctx.node in
          (if not (TInterval.is_bot miss) then Printf.printf "(%s,%s) deadline of t%i was missed: %s > %s (!!!!)\n%s \n\n"
            (string_of_int u) (string_of_int v)
            t (TInterval.short 80 time_since_period) (TInterval.short 80 deadline_interval)
           (D.short 800 ctx.local)
           else
           ());
          let period = BatOption.get @@ Period.to_int (if t = 0 then a.period else b.period) in
          let remaining_wait = TInterval.sub (TInterval.of_int period) time_since_period in
          let times = set_remaining_wait t remaining_wait x in (* set remaining wait time *)
          let (a,b), _ = D.periodic_wait t ctx.local in
          (a,b), times
        | TimedWait tw ->
          let remaining_wait = TInterval.of_int (Int64.of_int tw) in
          let times = set_remaining_wait t remaining_wait x in (* set remaining wait time *)
          let (a,b), _ = D.timed_wait t ctx.local in
          (a,b), times
        | _ -> ctx.local


  let should_join ((a, b), x) ((a', b'), x') = a.processState = a'.processState && b.processState = b'.processState && a.waitingFor = a'.waitingFor && b.waitingFor = b'.waitingFor

  let val_of () = D.bot ()
  let context _ = ()

  let startstate v = D.bot ()
  let otherstate v = D.bot ()
  let exitstate  v = D.bot ()
end

let _ =
  MCP_arinc.register_analysis (module Spec : ArincSpec)
