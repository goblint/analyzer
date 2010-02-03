(** A wrapper for hashtables that adds default value *)

module type S =
  sig
    type key
    type 'a t
    val create: int -> 'a -> 'a t
    val clear: 'a t -> unit
    val copy: 'a t -> 'a t
    val add: 'a t -> key -> 'a -> unit
    val remove: 'a t -> key -> unit
    val find: 'a t -> key -> 'a
    val find_all: 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length: 'a t -> int
  end
(** The output signature of the functor {!Hash.Make}, the only difference is the
  * signature for creating hashtables, where the default value is given. *)

module Make (H: Hashtbl.HashedType) : S with type key = H.t
(** Functor building the implementation. *)

module type SP =
sig
  include Printable.S 
  type key
  type value
  val create: int -> t
  val clear: t -> unit
  val copy: t -> t
  val add: t -> key -> value -> unit
  val remove: t -> key -> unit
  val find: t -> key -> value
  val find_all: t -> key -> value list
  val replace : t -> key -> value -> unit
  val mem : t -> key -> bool
  val iter: (key -> value -> unit) -> t -> unit
  val fold: (key -> value -> 'b -> 'b) -> t -> 'b -> 'b
  val length: t -> int
end
(** The output signature of the functor {!Hash.Printable}. *)

module Printable (Domain: Printable.S) (Range: Printable.S): SP with 
  type t = Range.t Hashtbl.Make(Domain).t and
  type key = Domain.t and 
  type value = Range.t
