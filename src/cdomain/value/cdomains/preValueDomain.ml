module ID = IntDomain.IntDomTuple
module FD = FloatDomain.FloatDomTupleImpl
module IndexDomain = IntDomain.IntDomWithDefaultIkind (ID) (IntDomain.PtrDiffIkind) (* TODO: add ptrdiff cast into to_int? *)
module SizeDomain = IntDomain.IntDomWithDefaultIkind (ID) (struct let ikind () = !GoblintCil.kindOfSizeOf end) (* TODO: add ptrdiff cast into to_int? *)
module Offs = Offset.MakeLattice (IndexDomain)
module Mval = Mval.MakeLattice (Offs)
module AD = AddressDomain.AddressSet (Mval) (ID)
module Addr =
struct
  include AD.Addr
  module Offs = Offs
  module Mval = Mval
end
