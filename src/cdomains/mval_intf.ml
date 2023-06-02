module type Printable =
sig
  type idx
  type t = GoblintCil.varinfo * idx Offset.t
  include Printable.S with type t := t (** @closed *)
  include MapDomain.Groupable with type t := t (** @closed *)

  val is_definite: t -> bool
  val add_offset: t -> idx Offset.t -> t
  val prefix: t -> t -> idx Offset.t option
  val top_indices: t -> t

  val to_cil: t -> GoblintCil.lval
  val to_cil_exp: t -> GoblintCil.exp

  val type_of: t -> GoblintCil.typ
end

module type Lattice =
sig
  include Printable (** @closed *)
  include Lattice.S with type t := t (** @closed *)

  val semantic_equal: t -> t -> bool option
end

module type Mval =
sig
  module type Printable = Printable
  module type Lattice = Lattice

  module MakePrintable (Offs: Offset.Printable): Printable with type idx = Offs.idx
  module MakeLattice (Offs: Offset.Lattice): Lattice with type idx = Offs.idx

  (** Mval instantiated with {!Offset.Unit}. *)
  module Unit: Printable with type idx = unit

  (** Mval instantiated with {!Offset.Unit}. *)
  module Exp: Printable with type idx = GoblintCil.exp
end
