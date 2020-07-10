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
    let zero = (IntDomain.Interval32.of_int (Int64.zero)) in
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



  let arinc_edge ctx (t,e) =
    let (a, b), x = ctx.local in
    if t = -1 then
      (* This is some special edge e.g. during init *)
      ctx.local
    else if e = WaitingForPeriod then
      (* The restart of a period can happen at any time even if the task does not have priority *)
      let t_since_period = Times.find ("since_period_t" ^ string_of_int(t)) x in
      let period = BatOption.get @@ Period.to_int (if t = 0 then a.period else b.period) in
      if IntDomain.Interval32.leq (IntDomain.Interval32.of_int period) t_since_period then
        let times = Times.update_val ("since_period_t" ^ string_of_int(t)) (fun _ -> IntDomain.Interval32.of_int Int64.zero) x in (* set time since period to 0 *)
        if t = 0 then
          ({a with processState = PState.ready},b), times
        else
          (a, {b with processState = PState.ready}), times
      else
        (* if no other task can take any action, and this is longest wait time, we can simply increase the time by how long we are waiting for the start of the period  *)
        raise Deadcode
    else
      let ours, other = if t = 0 then a, b else b, a in
      if not (can_run ours other) then
          if not (can_run other ours) then
            (Printf.printf "None can run ?!\n";
            raise Deadcode)
          else
            raise Deadcode
      else
        match e with
        | SuspendTask i -> D.suspend i ctx.local
        | ResumeTask i -> D.resume i ctx.local
        | WaitEvent i -> D.wait_event t i ctx.local
        | SetEvent i -> D.set_event i ctx.local
        | Computation i ->
          (* Check how much time is still remaining here  *)
          (* Check how long this one can do things uninterrupted for by looking at the remaining time of the other process *)
          let times = Times.update_all ["overall"; "since_period_t0"; "since_period_t1"] (IntDomain.Interval32.add  (IntDomain.Interval32.of_interval (Int64.zero, Int64.of_int i))) x in
          (a,b), times
        | PeriodicWait ->
          (M.warn "periodic_wait";
          (* Check that the deadline is not violated *)
          let times = Times.update_val ("since_period_t" ^ string_of_int(t)) (fun _ -> IntDomain.Interval32.of_int Int64.zero) x in (* set time since period to 0 *)
          D.periodic_wait t ctx.local)
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
