module ID = IntDomain.IntDomTuple
module FD = FloatDomain.FloatDomTupleImpl

module IndexDomain = (* TODO: hide type t = ID.t, require explicit (un)lift *)
struct
  include IntDomain.IntDomWithDefaultIkind (ID) (IntDomain.PtrDiffIkind) (* TODO: add ptrdiff cast into to_int? *)
  let lift x = ID.cast_to ~kind:Internal (Cilfacade.ptrdiff_ikind ()) x (* TODO: proper castkind *)
  let unlift x = x
end

module SizeDomain:
sig
  include IntDomain.ZDefault (* hide type t = ID.t, require explicit (un)lift *)
  val lift: ID.t -> t
  val unlift: t -> ID.t

  val of_index: IndexDomain.t -> t
  (** Convert an index to size, {e assuming} it is non-negative.

      This is useful for comparing an index with a size.
      A negative index check must be done separately, if needed.
      This conversion is more precise than size to index, which may overflow. *)
end =
struct
  include IntDomain.IntDomWithDefaultIkind (ID) (struct let ikind () = !GoblintCil.kindOfSizeOf end) (* TODO: add ptrdiff cast into to_int? *)
  let lift x = ID.cast_to ~kind:Internal !GoblintCil.kindOfSizeOf x (* TODO: proper castkind *)
  let unlift x = x

  let of_index index =
    let index_nonneg = IndexDomain.meet index (IndexDomain.starting Z.zero) in (* exclude negative indices *)
    lift (IndexDomain.unlift index_nonneg) (* cast in lift should never underflow *)
end

module Offs = Offset.MakeLattice (IndexDomain)
module Mval = Mval.MakeLattice (Offs)
module AD = AddressDomain.AddressSet (Mval) (ID)
module Addr =
struct
  include AD.Addr
  module Offs = Offs
  module Mval = Mval
end
