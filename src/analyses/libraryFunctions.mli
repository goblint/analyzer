
val get_invalidate_action : string -> (Cil.exp list -> Cil.exp list) option
(** Returns None if nothing is known about given function.
  * Otherwise will return function that filters out arguments
  * that have to be invalidated.
  *)
