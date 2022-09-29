(** Utilities for maintaining timing statistics *)

(** A timing entry *)
type t = {
  name : string;        (** Name of the task *)
  mutable time : float; (** In seconds *)
  mutable ncalls : int; (** Number of repetitions. Only set if {!Timing.countCalls} is true. *)
  mutable sub : t list; (** Subtasks *)
}

(** The top-level timing entry *)
val top : t

(** Resets all the timings and specifies whether to measure future timings.
    Call this before doing any timing. *)
val reset: bool -> unit

(** Flag to indicate whether or not to count the number of calls of
    to {!Timing.val-time} for each label.
    (default: false) *)
val countCalls: bool ref

(** Time a function and associate the time with the given string. If some
    timing information is already associated with that string, then accumulate
    the times. If this function is invoked within another timed function then
    you can have a hierarchy of timings *)
val time : string -> ('a -> 'b) -> 'a -> 'b

(** Print the current stats preceeded by a message *)
val print : out_channel -> string -> unit
