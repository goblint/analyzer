(** Abstract domains representing arrays. *)
module type S =
sig
  include Lattice.S
  type idx (** The abstract domain used to index on arrays. *)
  type value (** The abstract domain of values stored in the array. *)

  val get: Queries.ask -> t -> ExpDomain.t * idx -> value
  (** Returns the element residing at the given index. *)
  val set: Queries.ask -> t -> ExpDomain.t * idx -> value -> t
  (** Returns a new abstract value, where the given index is replaced with the
    * given element. *)
  val make: idx -> value -> t
  (** [make l e] creates an abstract representation of an array of length [l]
    * containing the element [e]. *)
  val length: t -> idx option
  (** returns length of array if known *)
  val move_if_affected: ?replace_with_const:bool -> Queries.ask -> t -> Cil.varinfo -> (Cil.exp -> int option) -> t
  (** changes the way in which the array is partitioned if this is necessitated by a change
    * to the variable **)
  val get_vars_in_e: t -> Cil.varinfo list
  (** returns the variables occuring in the expression according to which the
    * array was partitioned (if any) *)
  val map: (value -> value) -> t -> t
  (** Apply a function to all elements of the array. *)
  val fold_left: ('a -> value -> 'a) -> 'a -> t -> 'a
  (** Left fold (like List.fold_left) over the arrays elements *)
  val fold_left2: ('a -> value -> value -> 'a) -> 'a -> t -> t -> 'a
  (** Left fold over the elements of two arrays (like List.fold_left2 *)
  val smart_join: (Cil.exp -> int64 option) -> (Cil.exp -> int64 option) -> t -> t  -> t
  val smart_widen: (Cil.exp -> int64 option) -> (Cil.exp -> int64 option) -> t -> t -> t
  val smart_leq: (Cil.exp -> int64 option) -> (Cil.exp -> int64 option) -> t -> t  -> bool
  val update_length: idx -> t -> t
end

module type LatticeWithSmartOps =
sig
  include Lattice.S
  val smart_join: (Cil.exp -> int64 option) -> (Cil.exp -> int64 option) -> t -> t ->  t
  val smart_widen: (Cil.exp -> int64 option) -> (Cil.exp -> int64 option) -> t -> t -> t
  val smart_leq: (Cil.exp -> int64 option) -> (Cil.exp -> int64 option) -> t -> t -> bool
end

module Trivial (Val: Lattice.S) (Idx: IntDomain.S): S with type value = Val.t and type idx = Idx.t
(** This functor creates a trivial single cell representation of an array. The
  * indexing type is taken as a parameter to satisfy the type system, it is not
  * used in the implementation. *)

module TrivialWithLength (Val: Lattice.S) (Idx: IntDomain.S): S with type value = Val.t and type idx = Idx.t
(** This functor creates a trivial single cell representation of an array. The
  * indexing type is also used to manage the length. *)

module Partitioned (Val: LatticeWithSmartOps) (Idx: IntDomain.S): S with type value = Val.t and type idx = Idx.t
(** This functor creates an array representation that allows for partitioned arrays
  * Such an array can be partitioned according to an expression in which case it
  * uses three values from Val to represent the elements of the array to the left,
  * at, and to the right of the expression. The Idx domain is required only so to
  * have a signature that allows for choosing an array representation at runtime.
   *)

module PartitionedWithLength (Val: LatticeWithSmartOps) (Idx:IntDomain.S): S with type value = Val.t and type idx = Idx.t
(** Like partitioned but additionally manages the length of the array. *)

module FlagConfiguredArrayDomain(Val: LatticeWithSmartOps) (Idx:IntDomain.S):S with type value = Val.t and type idx = Idx.t
(** Switches between PartitionedWithLength and TrivialWithLength based on the value of exp.partition-arrays. *)
