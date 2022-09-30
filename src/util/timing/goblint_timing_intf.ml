module type Name =
sig
  val name: string
end

type options = {
  cputime: bool;
  walltime: bool;
  (* TODO: allocated_bytes *)
  count: bool;
}

(** A timing entry *)
type tree = {
  name: string;        (** Name of the task *)
  mutable cputime: float; (** In seconds *)
  mutable walltime: float; (** In seconds *)
  mutable count: int; (** Number of repetitions. Only set if {!Timing.countCalls} is true. *)
  mutable children: tree list; (** Subtasks *)
}

module type S =
sig
  val start: options -> unit
  val stop: unit -> unit
  val reset: unit -> unit

  val enter: string -> unit
  val exit: string -> unit

  val wrap: string -> ('a -> 'b) -> 'a -> 'b
  (** Time a function and associate the time with the given string. If some
      timing information is already associated with that string, then accumulate
      the times. If this function is invoked within another timed function then
      you can have a hierarchy of timings *)

  (** Print the current stats *)
  val print: Format.formatter -> unit

  (** The top-level timing entry *)
  val root: tree
end

module type Goblint_timing =
sig
  module type Name = Name
  module type S = S

  module Make (_: Name): S
end
