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
(* Current state *)
module ProcessState = IntDomain.Flattened
(* Ready -> 0, Running -> 1, Suspended -> 2, Done -> 3, Wait -> 4, Susp_Wait -> 5*)
module WaitingForEvent = IntDomain.Flattened

(* Information for all tasks *)
(* Partition mode *)
module PartitionMode = IntDomain.Flattened
(* Preemption lock *)
module PreemptionLock = IntDomain.Flattened

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


module GroupableStrings:(MapDomain.Groupable with type t = string) =
struct
  include Printable.Strings
end

module TInterval = struct
  include IntDomain.Interval32

  let ensure_pos x =
    let r = meet (starting 0L) x in
    (if is_bot r then Printf.printf "ensure pos returned bot \n\n");
    r

  let of_int = ensure_pos % of_int
  let of_interval = ensure_pos % of_interval
  let starting ?ikind = ensure_pos % starting ?ikind
  let ending ?ikind = ensure_pos % ending ?ikind
  let maximal x = Option.map (fun x -> if x < 0L then failwith "Negative max" else x) (maximal x)
  let minmal x =  Option.map (fun x -> if x < 0L then 0L else x) (maximal x)

  let sub_zero_if_neg x y =
    let r = meet (starting 0L) (sub x y) in
    if is_bot r then of_int 0L else r

  let neg x = failwith "Negating times?"
  let bitnot _  = failwith "Bitwise operation on times"
  let bitand _ _ = failwith "Bitwise operation on times"
  let bitor _ _ = failwith "Bitwise operation on times"
  let bitxor _ _ = failwith "Bitwise operation on times"
  let shift_left _ _ = failwith "Shifting on times"
  let shift_right _ _ = failwith "Shifting on times"

  let op_helper op x y =
    let r = op x y in
    ensure_pos r

  let add = op_helper add
  let sub = op_helper sub
  let mul = op_helper mul
  let div = op_helper div
  let rem = op_helper rem

  let join = op_helper join
  let meet = op_helper meet
  let widen = op_helper widen
  let narrow = op_helper narrow
end

module Times:(sig
  include Lattice.S
  type v = TInterval.t
  type tid = int

  val start_state: int -> t
  val zeroInterval: v

  val update_since_period: tid -> (v -> v) -> t -> t
  val set_since_period: tid -> v -> t -> t
  val get_since_period: tid -> t -> v

  val update_remaining_wait: tid -> (v -> v) -> t -> t
  val set_remaining_wait: tid -> v -> t -> t
  val get_remaining_wait: tid -> t -> v

  val update_remaining_processing: tid -> (v -> v) -> t -> t
  val set_remaining_processing: tid -> v -> t -> t
  val get_remaining_processing : tid -> t -> v

  val update_overall: (v -> v) -> t -> t
  val advance_all_times_by: v -> t -> t
end) =
struct
  include MapDomain.MapBot_LiftTop(GroupableStrings)(TInterval)
  type v = TInterval.t
  type tid = int

  let zeroInterval = TInterval.of_int Int64.zero
  let numtasks = 2 (* TODO: Fix *)

  let update_val (k:key) (f:value -> value) t =
    let old = find k t in
    add k (f old) t

  let update_all (ks:key list) (f:value -> value) t =
    List.fold_left (fun t k -> update_val k f t) t ks

  let start_state n =
    let t = add "overall" zeroInterval (bot ()) in
    let t = add_list_set ["since_period_t0"; "since_period_t1"] zeroInterval t in
    let t = add_list_set ["remaining_wait_t0"; "remaining_wait_t1"] zeroInterval t in
    let t = add_list_set ["remaining_processing_t0"; "remaining_processing_t1"] zeroInterval t in
    t

  (* time since period for each task *)
  let update_since_period tid fn times =
    update_val ("since_period_t" ^ string_of_int(tid)) fn times

  let set_since_period tid v times =
    update_since_period tid (fun _ -> v) times

  let get_since_period tid times =
    find ("since_period_t" ^ string_of_int(tid)) times


  (* remaining wait time for each task *)
  let update_remaining_wait tid fn times =
    update_val ("remaining_wait_t" ^ string_of_int(tid)) fn times

  let set_remaining_wait tid v times =
    update_remaining_wait tid (fun _ -> v) times

  let get_remaining_wait tid times =
    find ("remaining_wait_t" ^ string_of_int(tid)) times

  (* remaining compute time for each task *)
  let update_remaining_processing tid fn times =
    update_val ("remaining_processing_t" ^ string_of_int(tid)) fn times

  let set_remaining_processing tid v times =
    update_remaining_processing tid (fun _ -> v) times

  let get_remaining_processing tid times =
    find ("remaining_processing_t" ^ string_of_int(tid)) times

  (* overall times *)
  let update_overall fn times =
    update_val "overall" fn times


  (** Advance overall and all since_period and remaining_wait by interval  *)
  let advance_all_times_by interval times =
    (* Subtract interval from x and ensure result is not negative *)
    let decrement x = TInterval.sub_zero_if_neg x interval in
    let increment = TInterval.add interval in
    let all_tids = List.range 0 `To (numtasks-1) in
    (* update overall time *)
    let times = update_overall increment times in
    (* update since_period for all tasks *)
    let times = List.fold_left (fun times tid -> update_since_period tid increment times) times all_tids in
    (* update remaining_wait for all tasks *)
    let times = List.fold_left (fun times tid -> update_remaining_wait tid decrement times) times all_tids in
    times

end


module OneTask =
struct
  type t = process [@@deriving to_yojson]
  include Printable.Std

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
  let widen = join
  let narrow = meet

  let suspend p = {p with processState = (if p.processState = PState.ready then PState.suspended else PState.susp_wait)}
  let resume p = {p with processState = (if p.processState = PState.suspended then PState.ready else PState.wait)}

  let wait_event i p =
    (* A task may only execute a wait if it currently has the highest priority *)
    (* TODO: We should only suspend and wait for the event if it is down at the moment *)
    {p with processState = PState.wait; waitingFor = WaitingForEvent.of_int (Int64.of_int i)}

  let set_event i p =
    if p.waitingFor = WaitingForEvent.of_int (Int64.of_int i) then
      {p with processState = (if p.processState = PState.wait then PState.ready else PState.suspended); waitingFor = WaitingForEvent.bot ()}
    else
      p

  let periodic_wait p =
    {p with processState = PState.waiting_for_period}

  let timed_wait p =
    {p with processState = PState.wait}
end

module D =
struct
  include Lattice.Prod(Lattice.Liszt(OneTask))(Times)

  type t = (process list) * Times.t

  let apply_to_t t fn s = List.mapi (fun i e -> if i = t then fn e else e) s
  let suspend t x = apply_to_t t OneTask.suspend x
  let resume t x = apply_to_t t OneTask.resume x
  let periodic_wait t x = apply_to_t t OneTask.periodic_wait x
  let timed_wait t x = apply_to_t t OneTask.timed_wait x
  let wait_event t i x = apply_to_t t (OneTask.wait_event i) x

  let set_event i s = List.map (OneTask.set_event i) s
end

module LiftedD = Lattice.LiftBot(D)
