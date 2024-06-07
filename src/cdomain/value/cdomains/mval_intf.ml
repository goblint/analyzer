module type Printable =
sig
  type idx
  (** Type of indices in mvalue offset. *)

  type t = GoblintCil.varinfo * idx Offset.t
  include Printable.S with type t := t (** @closed *)

  val is_definite: t -> bool
  (** Whether offset of mvalue has only definite integer indexing (and fields). *)

  val add_offset: t -> idx Offset.t -> t
  (** [add_offset m o] appends [o] to [m]. *)

  val prefix: t -> t -> idx Offset.t option
  (** [prefix m1 m2] checks if [m1] is a prefix of [m2].

      @return [Some o] if it is (such that the variables are equal and [add_offset m1 o = m2]), [None] if it is not. *)

  val top_indices: t -> t
  (** Change all indices to top indices. *)

  val to_cil: t -> GoblintCil.lval
  (** Convert to CIL lvalue. *)

  val to_cil_exp: t -> GoblintCil.exp
  (** Convert to CIL lvalue expression. *)

  val type_of: t -> GoblintCil.typ
  (** Type of mvalue. *)
end

module type Lattice =
sig
  include Printable (** @closed *)
  include Lattice.S with type t := t (** @closed *)

  val semantic_equal: t -> t -> bool option
  (** Check semantic equality of two mvalues.

      @return [Some true] if definitely equal, [Some false] if definitely not equal, [None] if unknown. *)
end

module type Mval =
sig
  module type Printable = Printable
  module type Lattice = Lattice

  module MakePrintable (Offs: Offset.Printable): Printable with type idx = Offs.idx
  (** Make {!Printable} mvalue from {{!Offset.Printable} printable offset}. *)

  module MakeLattice (Offs: Offset.Lattice): Lattice with type idx = Offs.idx
  (** Make mvalue {!Lattice} from {{!Offset.Lattice} offset lattice}. *)

  (** Mvalue with {!Offset.Unit} indices in offset. *)
  module Unit: Printable with type idx = unit

  (** Mvalue with {!Offset.Exp} indices in offset. *)
  module Exp: Printable with type idx = GoblintCil.exp
end
