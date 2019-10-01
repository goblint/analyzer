open OUnit
open ArrayDomain_deprecated
open GeneralArrayTest

module I = Int64

module Idx = IntDomain.Trier
module Val = IntDomain.Trier

module D     = PreciseMapArrayDomain(struct let n = Some 12 end)(Val)(Idx)
module ATD   = ArrayTestDomain(D)
module GTPMA = GeneralTests(D)(ATD)


module PMapATest (TD : GeneralArrayTest.S with type t = D.t) =
struct
  open TD

  let  test =
    [ ("test_cache_mem" >:: GTPMA.test_cache_mem);
    ]

end


module STPMA = PMapATest(ATD)


(* all tests together *)
let  test () = "pMapArrayDomainTest" >:::
  GTPMA.test @ STPMA.test



