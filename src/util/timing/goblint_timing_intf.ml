module type Name =
sig
  val name: string
  (** Name of timing hierarchy.
      Used in {!S.print} and TEF track. *)
end

(** Timing options. *)
type options = {
  cputime: bool; (** Measure CPU time, both userspace and kernel, including child processes. *)
  walltime: bool; (** Measure wall time. *)
  allocated: bool; (** Measure allocated memory. *)
  count: bool; (** Count calls. *)
  tef: bool; (** Output a TEF track. *)
}

(** Timing tree node. *)
type tree = {
  name: string; (** Name of node. *)
  mutable cputime: float; (** Accumulated CPU time in seconds. *)
  mutable walltime: float; (** Accumulated wall time in seconds. *)
  mutable allocated: float; (** Accumulated allocated memory in bytes. *)
  mutable count: int; (** Number of repetitions. Only set if {!Timing.countCalls} is true. *)
  mutable children: tree list; (** Child nodes. *)
}

(** Timing hierarchy. *)
module type S =
sig
  (** {2 Lifecycle} *)

  val start: options -> unit
  (** Start timing with options. *)

  val stop: unit -> unit
  (** Stop timing, but don't reset timing information. *)

  val reset: unit -> unit
  (** Reset timing information. *)

  (** {2 Measurement} *)

  val enter: string -> unit
  (** [enter name] enters a new nested timed section called [name]. *)

  val exit: string -> unit
  (** [exit name] exits the current timed section called [name].
      Sections must be exited in valid nested fashion. *)

  val wrap: string -> ('a -> 'b) -> 'a -> 'b
  (** [wrap name f x] runs [f x] and measures it as a timed section called [name]. *)

  (** {2 Output} *)

  val print: Format.formatter -> unit
  (** Pretty-print current timing hierarchy. *)

  val root: tree
  (** Root node of timing tree.
      Must not be mutated! *)
end

module type Goblint_timing =
sig
  module type Name = Name

  type nonrec options = options
  type nonrec tree = tree

  module type S = S

  module Make (Name: Name): S
  (** Make a new timing hierarchy. *)

  val setup_tef: string -> unit
  (** [setup_tef filename] sets up Trace Event Format (TEF) output to [filename].

      @see <https://ui.perfetto.dev> for Perfetto UI for opening TEF files.
      @see <https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/edit> for TEF specification. *)

  val teardown_tef: unit -> unit
  (** Tear down TEF output. Must be called to correctly terminate the file. *)
end
