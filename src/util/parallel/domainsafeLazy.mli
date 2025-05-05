(** Lazy type which protects against concurrent calls of {!force}. *)

type 'a t

val from_fun: (unit -> 'a) -> 'a t
val force: 'a t -> 'a
