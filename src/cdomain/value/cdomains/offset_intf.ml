type 'i t = [
  | `NoOffset (** No offset. Marks the end of offset list. *)
  | `Field of CilType.Fieldinfo.t * 'i t (** Offset starting with a struct field. *)
  | `Index of 'i * 'i t (** Offset starting with an array index. *)
] [@@deriving eq, ord, hash]

type 'i offs = 'i t [@@deriving eq, ord, hash]
(** Outer alias to allow referring to {!t} in inner signatures. *)

module Index =
struct

  (** Subinterface of {!IntDomain.Z} which is sufficient for Printable (but not Lattice) Offset. *)
  module type Printable =
  sig
    include Printable.S (** @closed *)

    val equal_to: Z.t -> t -> [`Eq | `Neq | `Top]
    (** Check semantic equality of an integer and the index.

        @return [`Eq] if definitely equal, [`Neq] if definitely not equal, [`Top] if unknown. *)

    val to_int: t -> Z.t option
    (** Convert to definite integer if possible. *)
  end

  module type Lattice = IntDomain.Z
end

exception Type_of_error of GoblintCil.typ * string

module type Printable =
sig
  type idx
  (** Type of indices in offset. *)

  include Printable.S with type t = idx offs (** @closed *)

  val is_definite: t -> bool
  (** Whether offset has only definite integer indexing (and fields). *)

  val contains_index: t -> bool
  (** Whether offset contains any indexing. *)

  val add_offset: t -> t -> t
  (** [add_offset o1 o2] appends [o2] to [o1]. *)

  val remove_offset: t -> t
  (** Remove last indexing or field from offset. *)

  val prefix: t -> t -> t option
  (** [prefix o1 o2] checks if [o1] is a prefix of [o2].

      @return [Some o] if it is (such that [add_offset o1 o = o2]), [None] if it is not. *)

  val map_indices: (idx -> idx) -> t -> t
  (** Apply function to all indexing. *)

  val to_cil: t -> GoblintCil.offset
  (** Convert to CIL offset. *)

  val to_exp: t -> GoblintCil.exp offs
  (** Convert to Goblint offset with {!GoblintCil.exp} indices. *)

  val to_cil_offset: t -> GoblintCil.offset
  (** Version of {!to_cil} which drops indices for {!ArrayDomain}. *)

  val cmp_zero_offset: t -> [`MustZero | `MustNonzero | `MayZero]
  (** Compare offset to zero offset.

      Zero indices and first fields of a struct are in the same physical memory location as the outer object.

      @return [`MustZero] if definitely zero, [`MustNonzero] if definitely not zero, [`MayZero] if unknown.*)

  val type_of: base:GoblintCil.typ -> t -> GoblintCil.typ
  (** Type of the offset on the [base] type.

      @raise Type_of_error if could not follow offset completely. *)
end

module type Lattice =
sig
  include Printable (** @closed *)
  include Lattice.S with type t := t (** @closed *)

  val top_indices: t -> t
  (** Change all indices to top indices. *)

  val of_exp: GoblintCil.exp offs -> t
  (** Convert from Goblint offset with {!GoblintCil.exp} indices. *)

  val to_index: ?typ:GoblintCil.typ -> t -> idx
  (** Physical memory offset in bytes of the entire offset.
      Used for {!semantic_equal}.

      @param typ base type. *)

  val semantic_equal: typ1:GoblintCil.typ -> t -> typ2:GoblintCil.typ -> t -> bool option
  (** Check semantic equality of two offsets.

      @param typ1 base type of first offset.
      @param typ2 base type of second offset.
      @return [Some true] if definitely equal, [Some false] if definitely not equal, [None] if unknown. *)
end

module type Offset =
sig
  type nonrec 'i t = 'i t [@@deriving eq, ord, hash]
  (** List of nested offsets.

      @param 'i Type of indices. *)

  (** Domains for offset indices. *)
  module Index:
  sig
    include module type of Index

    module Unit: Printable with type t = unit
    (** Unit index.
        Usually represents an arbitrary index. *)

    module Exp:
    sig
      include Printable with type t = GoblintCil.exp

      (** Special index expression for some unknown index.
          Weakly updates array in assignment.
          Used for [exp.fast_global_inits]. *)
      val any: GoblintCil.exp Lazy.t

      (** [is_any e] is like [CilType.Exp.equal e any], except it ignores cast kind. *)
      val is_any: GoblintCil.exp -> bool

      (** Special index expression for all indices.
          Strongly updates array in assignment.
          Used for Goblint-specific witness invariants. *)
      val all: GoblintCil.exp Lazy.t

      (** [is_all e] is like [CilType.Exp.equal e all], except it ignores cast kind. *)
      val is_all: GoblintCil.exp -> bool
    end

    module Z: Printable with type t = Z.t
    (** {!Z} index.
        Represents a definite index. *)
  end

  exception Type_of_error of GoblintCil.typ * string
  (** {!Printable.type_of} could not follow offset completely. *)

  (** Polymorphic offset operations. *)
  module Poly:
  sig
    val map_indices: ('a -> 'b) -> 'a t -> 'b t
    (** Apply function to all indexing. *)

    (* TODO: include more operations *)
  end

  module type Printable = Printable
  module type Lattice = Lattice

  module MakePrintable (Idx: Index.Printable): Printable with type idx = Idx.t
  (** Make {!Printable} offset from {{!Index.Printable} printable indices}. *)

  module MakeLattice (Idx: Index.Lattice): Lattice with type idx = Idx.t
  (** Make offset {!Lattice} from {{!Index.Lattice} lattice indices}. *)

  (** Offset with {!Index.Unit} indices. *)
  module Unit:
  sig
    include Printable with type idx = unit
    val of_offs : 'i offs -> t
    (** Convert from Goblint offset with arbitrary indices. *)

    val of_cil : GoblintCil.offset -> t
    (** Convert from CIL offset. *)
  end

  (** Offset with {!Index.Exp} indices. *)
  module Exp:
  sig
    include Printable with type idx = GoblintCil.exp

    val top_indices: t -> t
    (** Change all indices to top indices. *)

    val of_cil : GoblintCil.offset -> t
    (** Convert from CIL offset. *)

    val to_cil : t -> GoblintCil.offset
    (** Convert to CIL offset. *)
  end

  (** Offset with {!Index.Z} indices. *)
  module Z: Printable with type idx = Z.t
end
