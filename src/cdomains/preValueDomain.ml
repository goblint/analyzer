module ID = IntDomain.IntDomTuple
module FD = FloatDomain.FloatDomainImpl
module IndexDomain = IntDomain.IntDomWithDefaultIkind (ID) (IntDomain.PtrDiffIkind)
module AD = AddressDomain.AddressSet (IndexDomain)
module Addr = Lval.NormalLat (IndexDomain)
