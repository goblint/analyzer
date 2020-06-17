open OUnit
open ArrayDomain_deprecated
open GeneralArrayTest

module I = Int64

module Idx = IntDomain.DefExc
module Val = IntDomain.DefExc

module D = NativeArrayEx(Val)(Idx)



module NatTest (TD : GeneralArrayTest.S with type t = D.t) =
struct
  open TD

  let test_big_nat () =
    let a = ref (D.make 201 (get_int_d (-2))) in
    a := set_v !a 1   42;
    a := set_v !a 100 42;
    a := set_v !a 200 42;
    assert_equal ~printer:(string_of_int) (-2) (get_v !a 0);
    assert_equal ~printer:(string_of_int)  42  (get_v !a 1);
    assert_equal ~printer:(string_of_int)  42  (get_v !a 100);
    assert_equal ~printer:(string_of_int)  42  (get_v !a 200)


  let  test =
    [ ("test_big_nat" >:: test_big_nat );
    ]
end


module ATD   = ArrayTestDomain(D)
module GTNat = GeneralTests(D)(ATD)
module STNat = NatTest(ATD)


(* all tests together *)
let  test () = "nativeArrayDomainTest" >:::
  GTNat.test @ STNat.test
