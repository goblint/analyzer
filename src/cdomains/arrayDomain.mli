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
  val get_e: t -> idx option
  (** returns the expression that is used to partition the array (if any) **)
  val is_affected_by: t -> Cil.varinfo -> bool
  (** returns whether assigning a new value to the var will change the way the
  array is partitioned **)
  val move: t -> int -> t
  (** moves the way in which the array is partitioned **)
end


module Trivial (Val: Lattice.S) (Idx: Lattice.S): S with type value = Val.t and type idx = Idx.t
(** This functor creates a trivial single cell representation of an array. The
  * indexing type is taken as a parameter to satisfy the type system, it is not
  * used in the implementation. *)

module TrivialWithLength (Val: Lattice.S) (Idx: IntDomain.S): S with type value = Val.t and type idx = Idx.t
(** This functor creates a trivial single cell representation of an array. The
  * indexing type is also used to manage the length. *)

module TrivialFragmented (Val: Lattice.S): S with type value = Val.t and type idx = ExpDomain.t
(* Comments are TODO *)

(*
module TrivialFragmentedWithLength (Val: Lattice.S) (Idx: IntDomain.S): S with type value = Val.t and type idx = Idx.t
(* Comments are TODO *)
*)
