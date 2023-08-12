(** Lazy type which can be reset to a closure. *)

type 'a t

val from_fun: (unit -> 'a) -> 'a t
val force: 'a t -> 'a
val reset: 'a t -> unit
