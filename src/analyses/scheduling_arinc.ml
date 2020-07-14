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
    (* let s = match ctx.node with
      | PC [a;b] -> "["^ string_of_int a ^ ","^ string_of_int b ^ "]"
      | _ -> ""
    in
    Printf.printf "enter for %s : %s\n %s \n\n------------------------------------------\n" s (D.short 80 ctx.local) (Printexc.raw_backtrace_to_string (Printexc.get_callstack 20)); *)
    let zero = (TInterval.of_int (Int64.zero)) in
    let t = Times.add "overall" zero (Times.bot ()) in
    let t = Times.add "since_period_t0" zero t in
    let t = Times.add "since_period_t1" zero t in
    let t = Times.add "remaining_wait_t0" zero t in
    let t = Times.add "remaining_wait_t1" zero t in
    let t = Times.add "remaining_processing_t0" zero t in
    let t = Times.add "remaining_processing_t1" zero t in
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
        other.processState <> PState.ready

  (* Subtract y from x and ensure result is not negative *)
  let subtract_if_not_zero x y  =
    if TInterval.to_int x = Some Int64.zero then
      x
    else
      let wait_time = TInterval.sub x y in
      TInterval.meet wait_time (TInterval.starting Int64.zero) (* These numbers may not become negative *)

  let wait_for_period ((a,b),x) t =
    let do_restart_period ((a,b), x) t =
      let times = Times.update_all ["since_period_t" ^ string_of_int(t); "remaining_wait_t" ^ string_of_int(t);] (fun _ -> TInterval.of_int Int64.zero) x in (* set time since period to 0 *)
      if t = 0 then
        ({a with processState = PState.ready},b), times
      else
        (a, {b with processState = PState.ready}), times
    in
    let t_since_period = Times.find ("since_period_t" ^ string_of_int(t)) x in
    let period = BatOption.get @@ Period.to_int (if t = 0 then a.period else b.period) in
    if TInterval.leq (TInterval.of_int period) t_since_period then
      do_restart_period ((a,b), x) t
    else
      (* if no other task can take any action, and this is longest wait time, we can simply increase the time by how long we are waiting for the start of the period *)
      let waiting_time_t0 = Times.find "remaining_wait_t0" x in
      let waiting_time_t1 = Times.find "remaining_wait_t1" x in
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
          let times = Times.update_all ["overall"; "since_period_t0"; "since_period_t1"] (TInterval.add our_waiting_time) x in
          let times = Times.update_all ["remaining_wait_t0"; "remaining_wait_t1"] (fun x -> subtract_if_not_zero x our_waiting_time) times in
          do_restart_period ((a,b), times) t
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
        | Computation i ->
          begin
            let wcetInterval = TInterval.of_interval (Int64.zero, Int64.of_int i) in
            (* If I am here, the only thing that could interrupt me is a higher priority task finishing waiting on sth *)
            (* If the other task wanted to do computation here, we would raise Deadcode even before *)
            if (other.processState = PState.waiting_for_period || other.processState = PState.wait) then
              (* As an improvement, we could model here that a "wait" (N/B not a waiting for period) from a lower priority thread will not have any influence here *)
              (* as even when it's wait ends, it will not interrupt this process *)
              let waiting_time_other = Times.find ("remaining_wait_t" ^ if t = 1 then "0" else "1") x in
              let less_than_waiting_time = TInterval.lt waiting_time_other wcetInterval in
              if not (TInterval.is_bot (TInterval.meet (TInterval.of_int (Int64.of_int 1)) less_than_waiting_time)) then
                failwith ("Block might be interrupted for t" ^ string_of_int(t) ^ " waiting time " ^ (TInterval.short 80 waiting_time_other));
              ()
            else
              ()
            ;
            (* Check how much time is still remaining here  *)
            (* Check how long this one can do things uninterrupted for by looking at the remaining time of the other process *)
            let times = Times.update_all ["overall"; "since_period_t0"; "since_period_t1"] (TInterval.add wcetInterval) x in
            let times = Times.update_all ["remaining_wait_t0"; "remaining_wait_t1"] (fun x -> subtract_if_not_zero x wcetInterval) times in
            (a,b), times
          end
        | PeriodicWait ->
          (* Check that the deadline is not violated *)
          let time_since_period = Times.find ("since_period_t" ^ string_of_int(t)) x in
          let deadline  = BatOption.get @@ Capacity.to_int (if t = 0 then a.capacity else b.capacity) in
          let deadline_interval = TInterval.of_int deadline in
          let miss = TInterval.meet (TInterval.of_int (Int64.of_int 1)) (TInterval.gt time_since_period deadline_interval) in
          if not (TInterval.is_bot miss) then Printf.printf "deadline of t%i was missed: %s > %s (!!!!)\n" t (TInterval.short 80 time_since_period) (TInterval.short 80 deadline_interval) else ();
          let period = BatOption.get @@ Period.to_int (if t = 0 then a.period else b.period) in
          let remaining_wait = TInterval.sub (TInterval.of_int period) time_since_period in
          let times = Times.add ("remaining_wait_t" ^ string_of_int(t)) remaining_wait x in (* set remaining wait time *)
          let (a,b), _ = D.periodic_wait t ctx.local in
          (a,b), times
        | _ -> ctx.local


  let should_join ((a, b), x) ((a', b'), x') = a.processState = a'.processState && b.processState = b'.processState

  let val_of () = D.bot ()
  let context _ = ()

  let startstate v = D.bot ()
  let otherstate v = D.bot ()
  let exitstate  v = D.bot ()
end

let _ =
  MCP_arinc.register_analysis (module Spec : ArincSpec)
