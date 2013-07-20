open Cil

exception StopTheWorld
val worldStopped : bool ref
val waitWhat : string -> unit 
  
exception Bailure of string
(** Raise this when you want to bail out of doing any analysis. This will
  * continue the analysis with a warning and leaving the state untouched. *)

val bailwith: string -> 'a
(** Behaves like [failwith], but with bailures that will not terminate the
  * analysis. *)

val xml_warn : (location, (string*string) list) Hashtbl.t
val warn_out : out_channel ref
val get_out: string -> out_channel -> out_channel

val print_msg: string -> location -> unit
(** Prints a message and adds the given location information. *)

val print_group: string -> (string * location) list -> unit
(** Prints a group of warning with a title and individual messages and given
  * location information. *)

val soundness: bool ref
(** The soundness of the current analysis. We begin with sound analyses, but if
  * we can't keep soundness, we try to continue and maybe find some bugs. *)

val warnings: bool ref
(** Turns on printing of soundness warnings. *)

val write: string -> unit
(** Print out a message, does not affect soundness. *)

val report: ?loc:Cil.location -> string -> unit
(** Print out a message, does not affect soundness. One message is 
  * printed once per line of code. *)

val report_error: string -> unit
(** Print out a message, does not affect soundness. One message is 
  * printed once per line of code. Used in case analysis has failed. *)

val warn: string -> unit
(** Prints a warning and adds the source code location where the warning
  * occured. This should also be called from within transfer functions. 
  * Same message is printed only once. *)

val warn_each: string -> unit
(** Prints a warning and adds the source code location where the warning
  * occured. This should also be called from within transfer functions. 
  * One message is printed per line of code. *)

val warn_all: string -> unit
(** Prints a warning and adds the source code location where the warning
  * occured. This should also be called from within transfer functions. *)

val warn_urgent: string -> unit
(** Prints a warning no matter what. And sets soundness to false. *)

val debug: string -> unit
(** Prints a debugging warning with location. *)

val debug_each: string -> unit
(** Prints a debugging warning with location. Uses warn_each. *)

val tracing: bool
(** Static flag to turn off tracing (significantly improves performance) *)

val tracevars: string list ref
(** List of identifier names that the below functions may care about. *)

val tracelocs: int list ref
(** List of source locations that the below functions may care about. *)

val addsystem: string -> unit
(** Activates one of the tracing subsystems. *)

val trace: string -> ?var:string -> ('a, unit, Pretty.doc, unit) format4 -> 'a
(** Similar to {!Trace.trace}. *)

val tracei: string -> ?var:string -> ?subsys: string list -> ('a, unit, Pretty.doc, unit) format4-> 'a
(** Similar to {!Trace.tracei}.  *)

val tracec: string -> ('a, unit, Pretty.doc, unit) format4 -> 'a
(** Similar to {!Trace.trace}, but to be used within indent/outdent. *)

val traceu: string -> ('a, unit, Pretty.doc, unit) format4 -> 'a
(** Similar to {!Trace.traceu}. *)

val tracel: string -> ?var:string -> ('a, unit, Pretty.doc, unit) format4 -> 'a
(** Like {!Analyses.trace}, but adds the location information to the message. *)

val traceli: string -> ?var:string -> ?subsys: string list -> ('a, unit, Pretty.doc, unit) format4 -> 'a
(** Like {!Analyses.tracei}, but adds the location information to the message. *)
