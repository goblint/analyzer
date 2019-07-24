(** Abstract domains representing arrays. *)

open Pretty
module type S =
sig
  include Lattice.S
  type idx (** The abstract domain used to index on arrays. *)
  type value (** The abstract domain of values stored in the array. *)

  val get: t -> idx -> value
  (** Returns the element residing at the given index. *)
  val set: t -> idx -> value -> t
  (** Returns a new abstract value, where the given index is replaced with the
    * given element. *)
  val make: int -> value -> t
  (** [make l e] creates an abstract representation of an array of length [l]
    * containing the element [e]. *)
  val length: t -> int option
  (** returns length of array if known *)
end

module Trivial (Val: Lattice.S) (Idx: Lattice.S): S with type value = Val.t and type idx = Idx.t
(** This functor creates a trivial single cell representation of an array. The
  * indexing type is taken as a parameter to satisfy the type system, it is not
  * used in the implementation. *)

module TrivialWithLength (Val: Lattice.S) (Idx: IntDomain.S): S with type value = Val.t and type idx = Idx.t
(** This functor creates a trivial single cell representation of an array. The
  * indexing type is also used to manage the length. *)