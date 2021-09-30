(** Minimal signature for hashtables. *)

module type H =
sig
  type key
  type 'a t
  val create: int -> 'a t
  val clear: 'a t -> unit
  val copy: 'a t -> 'a t
  val add: 'a t -> key -> 'a -> unit
  val remove: 'a t -> key -> unit
  val find: 'a t -> key -> 'a
  val find_default: 'a t -> key -> 'a -> 'a
  val find_all: 'a t -> key -> 'a list
  val replace : 'a t -> key -> 'a -> unit
  val mem : 'a t -> key -> bool
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val length: 'a t -> int
end
