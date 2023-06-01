module ID = IntDomain.IntDomTuple
module FD = FloatDomain.FloatDomTupleImpl
module IndexDomain = IntDomain.IntDomWithDefaultIkind (ID) (IntDomain.PtrDiffIkind) (* TODO: add ptrdiff cast into to_int? *)
module Offs = Offset.MakeLattice (IndexDomain)
module AD = AddressDomain.AddressSet (Offs) (ID)
module Addr = AD.Addr
