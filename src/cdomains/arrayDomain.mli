(** Abstract domains for C arrays. *)

open IntOps
open GoblintCil
module VDQ = ValueDomainQueries

type domain = TrivialDomain | PartitionedDomain | UnrolledDomain

val get_domain: varAttr:Cil.attributes -> typAttr:Cil.attributes -> domain
(** gets the underlying domain: chosen by the attributes in AttributeConfiguredArrayDomain *)

val can_recover_from_top: domain -> bool
(** Some domains such as Trivial cannot recover from their value ever being top. {!ValueDomain} handles intialization differently for these *)

module type S0 =
sig
  include Lattice.S
  type idx
  (** The abstract domain used to index on arrays. *)

  type value
  (** The abstract domain of values stored in the array. *)

  val set: VDQ.t -> t -> Basetype.CilExp.t option * idx -> value -> t
  (** Returns a new abstract value, where the given index is replaced with the
    * given element. *)

  val make: ?varAttr:Cil.attributes -> ?typAttr:Cil.attributes -> idx -> value -> t
  (** [make l e] creates an abstract representation of an array of length [l]
    * containing the element [e]. *)

  val length: t -> idx option
  (** returns length of array if known *)

  val move_if_affected: ?replace_with_const:bool -> VDQ.t -> t -> Cil.varinfo -> (Cil.exp -> int option) -> t
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
  val project: ?varAttr:Cil.attributes -> ?typAttr:Cil.attributes -> VDQ.t -> t -> t
  val invariant: value_invariant:(offset:Cil.offset -> lval:Cil.lval -> value -> Invariant.t) -> offset:Cil.offset -> lval:Cil.lval -> t -> Invariant.t
end

(** Abstract domains representing arrays. *)
module type S =
sig
  include S0

  val domain_of_t: t -> domain
  (* Returns the domain used for the array*)

  val get: ?checkBounds:bool -> VDQ.t -> t -> Basetype.CilExp.t option * idx -> value
  (** Returns the element residing at the given index. *)
end

(** Abstract domains representing strings a.k.a. null-terminated char arrays. *)
module type Str =
sig
  include S0

  type ret = Null | NotNull | Maybe
  type substr = IsNotSubstr | IsSubstrAtIndex0 | IsMaybeSubstr

  val get: VDQ.t -> t -> Basetype.CilExp.t option * idx -> ret
  (* overwrites get of module S *)

  val to_null_byte_domain: string -> t
  (* Converts a string to its abstract value in the NullByte domain *)

  val to_string_length: t -> idx
  (** Returns length of string represented by input abstract value *)

  val string_copy: t -> t -> int option -> t
  (** [string_copy dest src n] returns an abstract value representing the copy of string [src]
    * into array [dest], taking at most [n] bytes of [src] if present *)

  val string_concat: t -> t -> int option -> t
  (** [string_concat s1 s2 n] returns a new abstract value representing the string
    * concatenation of the input abstract values [s1] and [s2], taking at most [n] bytes of
    * [s2] if present *)

  val substring_extraction: t -> t -> substr
  (** [substring_extraction haystack needle] returns {!IsNotSubstr} if the string represented by
    * the abstract value [needle] surely isn't a substring of [haystack], {!IsSubstrAtIndex0} if
    * [needle] is the empty string, else {!IsMaybeSubstr} *)

  val string_comparison: t -> t -> int option -> idx
  (** [string_comparison s1 s2 n] returns a negative / positive idx element if the string
    * represented by [s1] is less / greater than the one by [s2] or zero if they are equal;
    * only compares the first [n] bytes if present *)
end

module type StrWithDomain =
sig
  include Str
  include S with type t := t and type idx := idx
end

module type LatticeWithInvalidate =
sig
  include Lattice.S
  val invalidate_abstract_value: t -> t
end

module type LatticeWithSmartOps =
sig
  include LatticeWithInvalidate
  val smart_join: (Cil.exp -> BigIntOps.t option) -> (Cil.exp -> BigIntOps.t option) -> t -> t ->  t
  val smart_widen: (Cil.exp -> BigIntOps.t option) -> (Cil.exp -> BigIntOps.t option) -> t -> t -> t
  val smart_leq: (Cil.exp -> BigIntOps.t option) -> (Cil.exp -> BigIntOps.t option) -> t -> t -> bool
end

module type Null =
sig
  type t
  type retnull = Null | NotNull | Maybe

  val null: unit -> t
  val is_null: t -> retnull

  val get_ikind: t -> Cil.ikind option
  val zero_of_ikind: Cil.ikind -> t
  val not_zero_of_ikind: Cil.ikind -> t
end

module type LatticeWithNull =
sig
  include LatticeWithSmartOps
  include Null with type t := t
end

module Trivial (Val: LatticeWithInvalidate) (Idx: Lattice.S): S with type value = Val.t and type idx = Idx.t
(** This functor creates a trivial single cell representation of an array. The
  * indexing type is taken as a parameter to satisfy the type system, it is not
  * used in the implementation. *)

module TrivialWithLength (Val: LatticeWithInvalidate) (Idx: IntDomain.Z): S with type value = Val.t and type idx = Idx.t
(** This functor creates a trivial single cell representation of an array. The
  * indexing type is also used to manage the length. *)

module Partitioned (Val: LatticeWithSmartOps) (Idx: IntDomain.Z): S with type value = Val.t and type idx = Idx.t
(** This functor creates an array representation that allows for partitioned arrays
  * Such an array can be partitioned according to an expression in which case it
  * uses three values from Val to represent the elements of the array to the left,
  * at, and to the right of the expression. The Idx domain is required only so to
  * have a signature that allows for choosing an array representation at runtime.
*)

module PartitionedWithLength (Val: LatticeWithSmartOps) (Idx:IntDomain.Z): S with type value = Val.t and type idx = Idx.t
(** Like partitioned but additionally manages the length of the array. *)

module NullByte (Val: LatticeWithNull) (Idx: IntDomain.Z): Str with type value = Val.t and type idx = Idx.t
(** This functor creates an array representation by the indexes of all null bytes
  * the array must and may contain. This is useful to analyze strings, i.e. null-
  * terminated char arrays, and particularly to determine if operations on strings
  * could lead to a buffer overflow. Concrete values from Val are not interesting
  * for this domain. It additionally tracks the array size.
*)

module AttributeConfiguredArrayDomain (Val: LatticeWithSmartOps) (Idx: IntDomain.Z): S with type value = Val.t and type idx = Idx.t
(** Switches between PartitionedWithLength, TrivialWithLength and Unroll based on variable, type, and flag. *)

module AttributeConfiguredAndNullByteArrayDomain (Val: LatticeWithNull) (Idx: IntDomain.Z): StrWithDomain with type value = Val.t and type idx = Idx.t
(** Like FlagHelperAttributeConfiguredArrayDomain but additionally runs NullByte
  * in parallel if flag "ana.base.arrays.nullbytes" is set.
*)
