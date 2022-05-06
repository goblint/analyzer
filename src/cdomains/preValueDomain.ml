module ID = IntDomain.IntDomTuple
module FD = FloatDomain.FloatDomTuple
module IndexDomain = IntDomain.IntDomWithDefaultIkind (ID) (IntDomain.PtrDiffIkind)
module AD = AddressDomain.AddressSet (IndexDomain)
module Addr = Lval.NormalLat (IndexDomain)
