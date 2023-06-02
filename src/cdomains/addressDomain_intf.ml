module type AddressDomain =
sig
  module AddressBase (Mval: Printable.S):
  sig
    type t =
      | Addr of Mval.t (** Pointer to offset of a variable. *)
      | NullPtr (** NULL pointer. *)
      | UnknownPtr (** Unknown pointer. Could point to globals, heap and escaped variables. *)
      | StrPtr of string option (** String literal pointer. [StrPtr None] abstracts any string pointer *)
    include Printable.S with type t := t (** @closed *)
    include MapDomain.Groupable with type t := t (** @closed *)

    val of_string: string -> t
    val to_string: t -> string option
    val to_c_string: t -> string option
    val to_n_c_string: int -> t -> string option
    val to_string_length: t -> int option
  end

  module AddressPrintable (Mval: Mval.Printable):
  sig
    include module type of AddressBase (Mval)
    include MapDomain.Groupable with type t := t and type group = Basetype.Variables.group (** @closed *)

    val is_definite: t -> bool
    val add_offset: t -> Mval.idx Offset.t -> t

    val of_var: GoblintCil.varinfo -> t
    (** Creates an address from variable. *)

    val of_mval: Mval.t -> t
    (** Creates an address from a variable and offset. *)

    val to_var: t -> GoblintCil.varinfo option
    (** Strips the varinfo out of the address representation. *)

    val to_var_may: t -> GoblintCil.varinfo option
    val to_var_must: t -> GoblintCil.varinfo option
    (** Strips the varinfo out of the address representation. *)

    val to_mval: t -> Mval.t option
    (** Get the offset *)

    val to_exp: t -> GoblintCil.exp

    val type_of: t -> GoblintCil.typ
    (** Finds the type of the address location. *)
  end

  (** Lvalue lattice.

      Actually a disjoint union of lattices without top or bottom.
      Lvalues are grouped as follows:

      - Each {!Addr}, modulo precise index expressions in offset, is a sublattice with ordering induced by {!Offset}.
      - {!NullPtr} is a singleton sublattice.
      - {!UnknownPtr} is a singleton sublattice.
      - If [ana.base.limit-string-addresses] is enabled, then all {!StrPtr} are together in one sublattice with flat ordering. If [ana.base.limit-string-addresses] is disabled, then each {!StrPtr} is a singleton sublattice. *)
  module AddressLattice (Mval: Mval.Lattice):
  sig
    include module type of AddressPrintable (Mval)
    include Lattice.S with type t := t (** @closed *)

    val drop_ints: t -> t

    val semantic_equal: t -> t -> bool option
    (** Semantic equal. [Some true] if definitely equal,  [Some false] if definitely not equal, [None] otherwise *)
  end

  (** Lvalue lattice with sublattice representatives for {!DisjointDomain}. *)
  module AddressLatticeRepr (Mval: Mval.Lattice):
  sig
    include module type of AddressLattice (Mval) (** @closed *)

    module VariableRepr: DisjointDomain.Representative with type elt = t

    module UnitOffsetRepr: DisjointDomain.Representative with type elt = t
    (** Representatives for lvalue sublattices as defined by {!AddressLattice}. *)
  end

  module AddressSet (Mval: Mval.Lattice) (ID: IntDomain.Z):
  sig
    module Addr: module type of AddressLattice (Mval)
    include SetDomain.S with type elt = Addr.t (** @closed *)

    val null_ptr: t
    val unknown_ptr: t
    val not_null: t
    val top_ptr: t

    val is_null: t -> bool
    val is_not_null: t -> bool
    val may_be_null: t -> bool
    val may_be_unknown: t -> bool
    val is_definite: t -> bool
    val is_element: Addr.t -> t -> bool

    val of_var: GoblintCil.varinfo -> t
    val of_mval: Mval.t -> t
    val of_int: ID.t -> t

    val to_var_may: t -> GoblintCil.varinfo list
    val to_var_must: t -> GoblintCil.varinfo list
    val to_mval: t -> Mval.t list
    val to_int: t -> ID.t
    val to_bool: t -> bool option

    val type_of: t -> GoblintCil.typ

    val of_string: string -> t
    val to_string: t -> string list
    val to_string_length: t -> ID.t
    val substring_extraction: t -> t -> t
    val string_comparison: t -> t -> int option -> ID.t
    val string_writing_defined: t -> bool
  end
end
