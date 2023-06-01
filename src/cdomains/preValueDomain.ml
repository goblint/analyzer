module ID = IntDomain.IntDomTuple
module FD = FloatDomain.FloatDomTupleImpl
module IndexDomain = IntDomain.IntDomWithDefaultIkind (ID) (IntDomain.PtrDiffIkind) (* TODO: add ptrdiff cast into to_int? *)
module AD = AddressDomain.AddressSet (IndexDomain)
module Addr = AddressDomain.NormalLat (Offset.MakeLattice (IndexDomain))
