module type AddressDomain =
sig

  type unknownKind =
    | Cast
    | Invalidate
    | Join
    | PointerArithmetic
    | String
    | TypeMismatch
    | Uninitialized
    | Union
    | Unknown
  [@@deriving eq, ord, hash]

  type unknownOrigin = {
    node : Node.t option;
    kind : unknownKind;
  } [@@deriving eq, ord, hash]

  module AddressBase (Mval: Printable.S):
  sig
    type t =
      | Addr of Mval.t (** Pointer to mvalue. *)
      | NullPtr (** NULL pointer. *)
      | UnknownPtr of unknownOrigin (** Unknown pointer. Could point to globals, heap and escaped variables. *)
      | StrPtr of string option (** String literal pointer. [StrPtr None] abstracts any string pointer *)
    include Printable.S with type t := t (** @closed *)

    val of_string: string -> t
    (** Convert string to {!StrPtr}. *)

    val to_string: t -> string option
    (** Convert {!StrPtr} to string if possible. *)

    (** C strings are different from OCaml strings as they are not processed after the first [NUL] byte, even though the OCaml string (and a C string literal) may be longer. *)

    val to_c_string: t -> string option
    (** Convert {!StrPtr} to C string if possible. *)

    val to_n_c_string: int -> t -> string option
    (** Convert {!StrPtr} to C string of given maximum length if possible. *)

    val to_string_length: t -> int option
    (** Find length of C string if possible. *)
  end

  module AddressPrintable (Mval: Mval.Printable):
  sig
    include module type of AddressBase (Mval)

    val is_definite: t -> bool
    (** Whether address is a [NULL] pointer or an mvalue that has only definite integer indexing (and fields). *)

    val add_offset: t -> Mval.idx Offset.t -> t
    (** [add_offset a o] appends [o] to an mvalue address [a]. *)

    val of_var: GoblintCil.varinfo -> t
    (** Convert from variable (without offset). *)

    val of_mval: Mval.t -> t
    (** Convert from mvalue. *)

    val to_var: t -> GoblintCil.varinfo option
    (** Convert to variable if possible. *)

    val to_var_may: t -> GoblintCil.varinfo option
    (** Convert to variable with any offset if possible. *)

    val to_var_must: t -> GoblintCil.varinfo option
    (** Convert to variable without offset if possible. *)

    val to_mval: t -> Mval.t option
    (** Convert to mvalue if possible. *)

    val to_exp: t -> GoblintCil.exp
    (** Convert to CIL expression. *)

    val type_of: t -> GoblintCil.typ
    (** Type of address. *)
  end

  (** Address lattice.

      Actually a disjoint union of lattices without top or bottom.
      Addresses are grouped as follows:

      - Each {!Addr}, modulo precise index expressions in the offset, is a sublattice with ordering induced by {!Mval}.
      - {!NullPtr} is a singleton sublattice.
      - {!UnknownPtr} is a singleton sublattice.
      - If [ana.base.limit-string-addresses] is enabled, then all {!StrPtr} are together in one sublattice with flat ordering. If [ana.base.limit-string-addresses] is disabled, then each {!StrPtr} is a singleton sublattice. *)
  module AddressLattice (Mval: Mval.Lattice):
  sig
    include module type of AddressPrintable (Mval)
    include Lattice.S with type t := t (** @closed *)

    val top_indices: t -> t
    (** Change all indices to top indices. *)

    val semantic_equal: t -> t -> bool option
    (** Check semantic equality of two addresses.

        @return [Some true] if definitely equal, [Some false] if definitely not equal, [None] if unknown. *)
  end

  (** Address lattice with sublattice representatives for {!DisjointDomain}. *)
  module AddressLatticeRepr (Mval: Mval.Lattice):
  sig
    include module type of AddressLattice (Mval) (** @closed *)

    module VariableRepr: DisjointDomain.Representative with type elt = t
    (** Representative without mvalue offsets. *)

    module UnitOffsetRepr: DisjointDomain.Representative with type elt = t
    (** Representative without mvalue offset indices. *)
  end

  (** Address set lattice.

      @param Mval mvalue used in addresses.
      @param ID integers used for conversions. *)
  module AddressSet (Mval: Mval.Lattice) (ID: IntDomain.Z):
  sig
    module Addr: module type of AddressLattice (Mval)
    include SetDomain.S with type elt = Addr.t (** @closed *)

    val null_ptr: t
    (** Address set containing only the [NULL] pointer. *)

    val unknown_ptr: unknownKind -> t
    (** Address set containing the unknown pointer, which is non-[NULL]. *)

    val not_null: unknownKind -> t
    (** Address set containing the unknown pointer, which is non-[NULL]. *)

    val top_ptr: unknownKind -> t
    (** Address set containing any pointer, [NULL] or not. *)

    val is_null: t -> bool
    (** Whether address set contains only the [NULL] pointer. *)

    val is_not_null: t -> bool
    (** Whether address set does not contain the [NULL] pointer. *)

    val may_be_null: t -> bool
    (** Whether address set contains the [NULL] pointer. *)

    val may_be_unknown: t -> bool
    (** Whether address set contains the unknown pointer. *)

    val is_definite: t -> bool
    (** Whether address set is a single [NULL] pointer or mvalue that has only definite integer indexing (and fields). *)

    val is_element: Addr.t -> t -> bool
    (** Whether address set contains only the given address. *)

    val of_var: GoblintCil.varinfo -> t
    (** Convert from variable (without offset). *)

    val of_mval: Mval.t -> t
    (** Convert from mvalue. *)

    val of_int: ID.t -> t
    (** Convert from integer. *)

    val to_var_may: t -> GoblintCil.varinfo list
    (** Convert to variables with any offset. *)

    val to_var_must: t -> GoblintCil.varinfo list
    (** Convert to variables without offset. *)

    val to_mval: t -> Mval.t list
    (** Convert to mvalues. *)

    val to_int: t -> ID.t
    (** Convert to integer. *)

    val to_bool: t -> bool option
    (** Convert to boolean if possible. *)

    val type_of: t -> GoblintCil.typ
    (** Type of address set. *)

    val of_string: string -> t
    (** Convert from string literal. *)

    val to_string: t -> string list
    (** Convert to string literals. *)

    val to_string_length: t -> ID.t
    (** Find length of C string. *)

    val substring_extraction: t -> t -> t
    val string_comparison: t -> t -> int option -> ID.t
    val string_writing_defined: t -> bool
    val remove_unknownptrs: t -> t
    val unknownptrs_origins: GoblintCil.Pretty.doc -> t -> (GoblintCil.Pretty.doc * Messages.Location.t option) list
  end
end
