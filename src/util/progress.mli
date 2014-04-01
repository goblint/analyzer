(** Functions for tracking progress and stuckness of the analyzer. *)

val tracking: bool
(** Turn this of to optimize away progress tracking *)

val track: unit -> unit
(** Notifies, if [track] is called more than n times when analyzing the same
  * location, also increasing n itself *)

val track_with: (int -> unit) -> unit
(** Like {!track} but takes a callback as parameter, so [track_with notify] will
  * call notify with the number of visits as argument, use
  * {!Messages.current_loc} to find the line. *)

val track_with_profile: unit -> unit
(** Print out profiling info collected by {!track_with}. *)

val track_call : Cil.varinfo -> int -> unit
(** *)

val track_call_profile : unit -> unit
(** *)

val show_subtask: string -> int -> unit
(** change subtask *)

val show_worked: int -> unit
(** show done work *)

val show_add_work: int -> unit
(** add work to subtask *)

val show_worked_buf: int -> unit
(** show done work (buffered)*)

val show_add_work_buf: int -> unit
(** add work to subtask (buffered) *)
