(** This allows us to query information about library functions. *)

type action = [ `Write  (** argument may be read or written to *)
              | `Read   (** argument may be read *)
              ]
(** Specifies what is known about an argument. *)

val get_invalidate_action : string -> (action -> Cil.exp list -> Cil.exp list) option
(** Returns None if nothing is known about given function.
  * Otherwise will return function that filters out arguments
  * that may be read or also written to.
  *)
