module ID = IntDomain.IntDomTuple
module FD = FloatDomain.FloatDomTupleImpl
module IndexDomain = IntDomain.IntDomWithDefaultIkind (ID) (IntDomain.PtrDiffIkind) (* TODO: add ptrdiff cast into to_int? *)

module SizeDomain:
sig
  include IntDomain.ZDefault (* hide type t = ID.t, require explicit (un)lift *)
  val lift: ID.t -> t
  val unlift: t -> ID.t
end =
struct
  include IntDomain.IntDomWithDefaultIkind (ID) (struct let ikind () = !GoblintCil.kindOfSizeOf end) (* TODO: add ptrdiff cast into to_int? *)
  let lift x = ID.cast_to ~kind:Internal !GoblintCil.kindOfSizeOf x (* TODO: proper castkind *)
  let unlift x = x
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
