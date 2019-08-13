open OUnit
open ArrayDomain_deprecated
open GeneralArrayTest

module I = Int64

module Idx = IntDomain.Trier
module Val = IntDomain.Trier

module D     = LooseMapArrayDomain(struct let n = Some 12 end)(Val)(Idx)
module ATD   = ArrayTestDomain(D)
module GTLMA = GeneralTests(D)(ATD)


module LMapATest (TD : GeneralArrayTest.S with type t = D.t) =
struct
  open TD

  let  test = 
    [ ("test_cache_mem" >:: GTLMA.test_cache_mem);
    ]
end


module STLMA = LMapATest(ATD)


(* all tests together *)
let  test () = "lMapArrayDomainTest" >::: 
  GTLMA.test @ STLMA.test


