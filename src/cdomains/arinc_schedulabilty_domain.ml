open Prelude

(* Information for one task *)
(* Process ID *)
module Pid = IntDomain.Flattened
(* Priority *)
module Priority = IntDomain.Reverse (IntDomain.Lifted) (* TODO reverse? *)
(* Period *)
module Period = IntDomain.Flattened
(* Capacity *)
module Capacity = IntDomain.Flattened

(* Information for all tasks *)
(* Partition mode *)
module PartitionMode = IntDomain.Flattened
(* Preemption lock *)
module PreemptionLock = IntDomain.Flattened
(* Current state *)
module ProcessState = IntDomain.Flattened
(* Ready -> 0, Running -> 1, Suspended -> 2, Done -> 3, Wait -> 4, Susp_Wait -> 5*)
module WaitingForEvent = IntDomain.Flattened

module PState = struct
  let ready = ProcessState.of_int (Int64.of_int 0)
  (* let running = ProcessState.of_int (Int64.of_int 1) (* needed? *) *)
  let suspended = ProcessState.of_int (Int64.of_int 2)
  let tdone = ProcessState.of_int (Int64.of_int 3)
  let wait = ProcessState.of_int (Int64.of_int 4)
  let susp_wait = ProcessState.of_int (Int64.of_int 5)
  let waiting_for_period = ProcessState.of_int (Int64.of_int 6)
end

(* define record type here so that fields are accessable outside of D *)
type process = { pid: Pid.t; priority: Priority.t; period: Period.t; capacity: Capacity.t; processState: ProcessState.t; waitingFor:WaitingForEvent.t } [@@deriving to_yojson]
type overall_state = { partitionMode:PartitionMode.t; preemptionLock:PreemptionLock.t } [@@deriving to_yojson]

module OneTask =
struct
  type t = process [@@deriving to_yojson]
  include Printable.Std
  include Lattice.StdCousot

  (* printing *)
  let short w x = Printf.sprintf "{ pid=%s; priority=%s; period=%s; capacity=%s; proState=%s; waitingFor=%s }"
    (Pid.short 3 x.pid) (Priority.short 3 x.priority) (Period.short 3 x.period) (Capacity.short 3 x.capacity) (ProcessState.short 3 x.processState) (WaitingForEvent.short 3 x.waitingFor)

  include Printable.PrintSimple (struct
      type t' = t
      let name () = "ARINC state"
      let short = short
    end)

  let toXML_f sf d =
    let replace_top name = function
      | Xml.Element (node, [text, n], elems) -> Xml.Element (node, [text, name ^ n], elems)
      | x -> x
    in
    let elems = [ replace_top "PID: "   @@ Pid.toXML  d.pid
                ; replace_top "Priority: "  @@ Priority.toXML d.priority
                ; replace_top "Period: "  @@ Period.toXML d.period
                ; replace_top "Capacity: "  @@ Capacity.toXML d.capacity
                ; replace_top "Process_state: " @@ ProcessState.toXML d.processState
                ; replace_top "Waiting for:" @@ WaitingForEvent.toXML d.waitingFor]
                 in
    Xml.Element ("Node", ["text", "ARINC state"], elems)
  let toXML s  = toXML_f short s
  (* Printable.S *)
  (* let equal = Util.equals *)
  let equal x y = Pid.equal x.pid y.pid && Priority.equal x.priority y.priority && Period.equal x.period y.period && Capacity.equal x.capacity y.capacity && ProcessState.equal x.processState y.processState
  (* Compare all fields with correspoding compare operators. TODO: make a "lazy" comparision *)
  let compare x y = List.fold_left (fun acc v -> if acc = 0 && v <> 0 then v else acc) 0 [Pid.compare x.pid y.pid; Priority.compare x.priority y.priority; Period.compare x.period y.period; ProcessState.compare x.processState y.processState]
  (* let hash = Hashtbl.hash *)
  let hash x = Hashtbl.hash (Pid.hash x.pid, Priority.hash x.priority, Period.hash x.period, Capacity.hash x.capacity, ProcessState.hash x.processState)

  let bot () = { pid = Pid.bot (); priority = Priority.bot (); period = Period.bot (); capacity = Capacity.bot (); processState = ProcessState.bot (); waitingFor = WaitingForEvent.bot () }
  let is_bot x = x = bot ()

  let top () = { pid = Pid.top (); priority = Priority.top (); period = Period.top (); capacity = Capacity.top (); processState = ProcessState.top (); waitingFor = WaitingForEvent.bot () }
  let is_top x = Pid.is_top x.pid && Priority.is_top x.priority && Period.is_top x.period && Capacity.is_top x.capacity && ProcessState.is_top x.processState && WaitingForEvent.is_top x.waitingFor

  let leq x y = Pid.leq x.pid y.pid && Priority.leq x.priority y.priority && Period.leq x.period y.period && Capacity.leq x.capacity y.capacity && ProcessState.leq x.processState y.processState && WaitingForEvent.leq x.waitingFor y.waitingFor

  let op_scheme op1 op2 op3 op4 op5 op6  x y: t = { pid = op1 x.pid y.pid; priority = op2 x.priority y.priority; period = op3 x.period y.period;
     capacity = op4 x.capacity y.capacity; processState = op5 x.processState y.processState; waitingFor = op6 x.waitingFor y.waitingFor}

  let join = op_scheme Pid.join Priority.join Period.join Capacity.join ProcessState.join WaitingForEvent.join
  let meet = op_scheme Pid.meet Priority.meet Period.meet Capacity.meet ProcessState.meet WaitingForEvent.meet

  let suspend p = {p with processState = (if p.processState = PState.ready then PState.suspended else PState.susp_wait)}
  let resume p = {p with processState = (if p.processState = PState.suspended then PState.ready else PState.wait)}

  let wait_event i p =
    (* A task may only execute a wait if it currently has the highest priority *)
    {p with processState = PState.wait; waitingFor = WaitingForEvent.of_int (Int64.of_int i)}

  let set_event i p =
    if p.waitingFor = WaitingForEvent.of_int (Int64.of_int i) then
      {p with processState = (if p.processState = PState.wait then PState.ready else PState.suspended); waitingFor = WaitingForEvent.of_int (Int64.of_int i)}
    else
      p

  let periodic_wait p =
    {p with processState = PState.waiting_for_period}
end

module GroupableStrings:(MapDomain.Groupable with type t = string) =
  struct
    include Printable.Strings
  end
module Times = struct
  include MapDomain.MapBot_LiftTop(GroupableStrings)(IntDomain.Interval32)
  let update_val (k:key) (f:value -> value) t =
    let old = find k t in
    add k (f old) t

  let update_all (ks:key list) (f:value -> value) t =
    List.fold_left (fun t k -> update_val k f t) t ks


end

module D =
struct
  include Lattice.Prod(Lattice.Prod(OneTask)(OneTask))(Times)

  type t = (process * process) * Times.t

  let suspend t ((a, b), x) =
    if t = 0 then
      (OneTask.suspend a, b), x
    else if t = 1 then
      (a, OneTask.suspend b), x
    else
      failwith "lol, wut?!"

  let resume t ((a, b), x) =
    if t = 0 then
      (OneTask.resume a, b), x
    else if t = 1 then
      (a, OneTask.resume b), x
    else
      failwith "lol, wut?!"

  let wait_event t i ((a, b), x) =
    if t = 0 then
      (OneTask.wait_event i a, b), x
    else if t = 1 then
      (a, OneTask.wait_event i b), x
    else
      failwith "lol, wut?!"

  let periodic_wait t ((a, b), x) =
    if t = 0 then
      (OneTask.periodic_wait a, b), x
    else if t = 1 then
      (a, OneTask.periodic_wait b), x
    else
      failwith "lol, wut?!"

  let set_event i ((a, b), x) =
    (OneTask.set_event i a, OneTask.set_event i b), x

end
