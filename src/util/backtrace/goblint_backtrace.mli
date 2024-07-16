(** Backtraces with custom marks. *)

(** {2 Mark definition} *)

type mark = ..
(** Extensible type of marks. *)

val register_mark_printer: (mark -> string option) -> unit
(** Register printing function for custom mark.
    The function should return [None] for other marks. *)

val mark_to_string: mark -> string
(** Convert mark to string using registered printers, or {!mark_to_string_default} if unhandled. *)

val mark_to_string_default: mark -> string
(** Convert mark to default string. *)


(** {2 Mark usage} *)

val add_mark: exn -> mark -> unit
(** Add mark to exception. *)

val protect: mark:(unit -> mark) -> finally:(unit -> unit) -> (unit -> 'a) -> 'a
(** {!Fun.protect} with additional [~mark] addition to all exceptions. *)

val print_marktrace: out_channel -> exn -> unit
(** Print trace of marks of an exception.

    Used by default for uncaught exceptions. *)

val find_marks: exn -> mark list
(** Find all marks of an exception. *)
