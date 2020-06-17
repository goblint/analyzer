open OUnit
open ArrayDomain_deprecated
open GeneralArrayTest

module I = Int64

module Idx = IntDomain.DefExc
module Val = IntDomain.DefExc

module D = Collapsing(Val)(Idx) (* collapses at len>25 *)


module ColTest (TD : GeneralArrayTest.S with type t = D.t) =
struct
  open TD

  let test_small_col () =
    let a = ref (D.make 21 (get_int_d (-2))) in
    a := set_v !a 1  42;
    a := set_v !a 10 42;
    a := set_v !a 20 42;
    assert_equal ~printer:(string_of_int) (-2) (get_v !a 0);
    assert_equal ~printer:(string_of_int)  42  (get_v !a 1);
    assert_equal ~printer:(string_of_int)  42  (get_v !a 10);
    assert_equal ~printer:(string_of_int)  42  (get_v !a 20)

  let test_big_col () =
    let a = ref (D.make 201 (get_int_d (-3))) in
    assert_equal ~printer:(string_of_int) (-1) (get_v !a 0);
    a := set_v !a 1   0;
    assert_equal ~printer:(string_of_int) (0) (get_v !a 1);
    assert_bool "safe collapsing" (D.leq !a (set_v !a 3 42));
    a := set_v !a 100 (-1);
    assert_equal ~cmp:D.equal ~printer:(D.short max_int) (D.top ()) (!a)

  let  test =
    [ ("test_small_col" >:: test_small_col );
      ("test_big_col"   >:: test_big_col   );
    ]

end


module ATD   = ArrayTestDomain(D)
module GTCol = GeneralTests(D)(ATD)
module STCol = ColTest(ATD)


(* all tests together *)
let  test () = "collapsingArrayDomainTest" >:::
  GTCol.test @ STCol.test
