open IntOps
open GoblintCil

type domain = TrivialDomain | PartitionedDomain | UnrolledDomain

val get_domain: varAttr:Cil.attributes -> typAttr:Cil.attributes -> domain
(** gets the underlying domain: chosen by the attributes in AttributeConfiguredArrayDomain *)

val can_recover_from_top: domain -> bool
(** Some domains such as Trivial cannot recover from their value ever being top. {!ValueDomain} handles intialization differently for these *)

(** Abstract domains representing arrays. *)
module type S =
sig
  include Lattice.S
  type idx
  (** The abstract domain used to index on arrays. *)

  type value
  (** The abstract domain of values stored in the array. *)

  val get: ?checkBounds:bool -> Queries.ask -> t -> Basetype.CilExp.t option * idx -> value
  (** Returns the element residing at the given index. *)

  val set: Queries.ask -> t -> Basetype.CilExp.t option * idx -> value -> t
  (** Returns a new abstract value, where the given index is replaced with the
    * given element. *)

  val make: ?varAttr:Cil.attributes -> ?typAttr:Cil.attributes -> idx -> value -> t
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

  val smart_join: (Cil.exp -> BigIntOps.t option) -> (Cil.exp -> BigIntOps.t option) -> t -> t  -> t
  val smart_widen: (Cil.exp -> BigIntOps.t option) -> (Cil.exp -> BigIntOps.t option) -> t -> t -> t
  val smart_leq: (Cil.exp -> BigIntOps.t option) -> (Cil.exp -> BigIntOps.t option) -> t -> t  -> bool
  val update_length: idx -> t -> t

  val project: ?varAttr:Cil.attributes -> ?typAttr:Cil.attributes -> Queries.ask -> t -> t
end

module type LatticeWithSmartOps =
sig
  include Lattice.S
  val smart_join: (Cil.exp -> BigIntOps.t option) -> (Cil.exp -> BigIntOps.t option) -> t -> t ->  t
  val smart_widen: (Cil.exp -> BigIntOps.t option) -> (Cil.exp -> BigIntOps.t option) -> t -> t -> t
  val smart_leq: (Cil.exp -> BigIntOps.t option) -> (Cil.exp -> BigIntOps.t option) -> t -> t -> bool
end

module Idx = PreValueDomain.IndexDomain

module Trivial (Val: Lattice.S): S with type value = Val.t
(** This functor creates a trivial single cell representation of an array. The
  * indexing type is taken as a parameter to satisfy the type system, it is not
  * used in the implementation. *)

module TrivialWithLength (Val: Lattice.S): S with type value = Val.t
(** This functor creates a trivial single cell representation of an array. The
  * indexing type is also used to manage the length. *)

module Partitioned (Val: LatticeWithSmartOps): S with type value = Val.t
(** This functor creates an array representation that allows for partitioned arrays
  * Such an array can be partitioned according to an expression in which case it
  * uses three values from Val to represent the elements of the array to the left,
  * at, and to the right of the expression. The Idx domain is required only so to
  * have a signature that allows for choosing an array representation at runtime.
*)

module PartitionedWithLength (Val: LatticeWithSmartOps): S with type value = Val.t
(** Like partitioned but additionally manages the length of the array. *)

module AttributeConfiguredArrayDomain(Val: LatticeWithSmartOps):S with type value = Val.t and type idx = Idx.t
(** Switches between PartitionedWithLength, TrivialWithLength and Unroll based on variable, type, and flag. *)
