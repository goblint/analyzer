type 'i t = [
  | `NoOffset
  | `Field of CilType.Fieldinfo.t * 'i t
  | `Index of 'i * 'i t
] [@@deriving eq, ord, hash]

(* TODO: remove? *)
type 'i offs = 'i t [@@deriving eq, ord, hash]

module Index =
struct

  (** Subinterface of {!IntDomain.Z} which is sufficient for Printable (but not Lattice) Offset. *)
  module type Printable =
  sig
    include Printable.S (** @closed *)
    val top: unit -> t

    val equal_to: Z.t -> t -> [`Eq | `Neq | `Top]
    val to_int: t -> Z.t option
  end

  module type Lattice = IntDomain.Z
end

exception Type_of_error of GoblintCil.typ * string
(** exception if the offset can't be followed completely *)

module type Printable =
sig
  type idx
  include Printable.S with type t = idx offs (** @closed *)

  val is_definite: t -> bool
  val contains_index: t -> bool
  val add_offset: t -> t -> t
  val remove_offset: t -> t
  val prefix: t -> t -> t option
  val map_indices: (idx -> idx) -> t -> t
  val top_indices: t -> t

  val to_cil: t -> GoblintCil.offset
  val to_exp: t -> GoblintCil.exp offs

  val to_cil_offset: t -> GoblintCil.offset
  (** Version of {!to_cil} which drops indices for {!ArrayDomain}. *)

  val cmp_zero_offset: t -> [`MustZero | `MustNonzero | `MayZero]

  val type_of: base:GoblintCil.typ -> t -> GoblintCil.typ
end

module type Lattice =
sig
  include Printable (** @closed *)
  include Lattice.S with type t := t (** @closed *)

  val of_exp: GoblintCil.exp offs -> t

  val to_index: ?typ:GoblintCil.typ -> t -> idx
  val semantic_equal: typ1:GoblintCil.typ -> t -> typ2:GoblintCil.typ -> t -> bool option
end

module type Offset =
sig
  type nonrec 'i t = 'i t [@@deriving eq, ord, hash]

  (** Domains for offset indices. *)
  module Index:
  sig
    include module type of Index

    module Unit: Printable with type t = unit
    (** Unit index.
        Usually represents an arbitrary index. *)

    module Exp: Printable with type t = GoblintCil.exp
  end

  exception Type_of_error of GoblintCil.typ * string

  module type Printable = Printable
  module type Lattice = Lattice

  module MakePrintable (Idx: Index.Printable): Printable with type idx = Idx.t
  module MakeLattice (Idx: Index.Lattice): Lattice with type idx = Idx.t

  (** Offset instantiated with {!Index.Unit}. *)
  module Unit:
  sig
    include Printable with type idx = unit
    val of_offs : 'i offs -> t
    val of_cil : GoblintCil.offset -> t
  end

  (** Offset instantiated with {!Index.Exp}. *)
  module Exp:
  sig
    include Printable with type idx = GoblintCil.exp
    val of_cil : GoblintCil.offset -> t
    val to_cil : t -> GoblintCil.offset
  end

  (** Special index expression for some unknown index.
      Weakly updates array in assignment.
      Used for exp.fast_global_inits. *)
  val any_index_exp: GoblintCil.exp

  (** Special index expression for all indices.
      Strongly updates array in assignment.
      Used for Goblint-specific witness invariants. *)
  val all_index_exp: GoblintCil.exp
end
