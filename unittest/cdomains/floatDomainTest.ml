open OUnit2
open Round

module FloatInterval =
struct
  module FI = FloatDomain.FloatInterval
  module IT = IntDomain.IntDomTuple

  let fi_zero = FI.of_const 0.
  let fi_one = FI.of_const 1.
  let fi_neg_one = FI.of_const (-.1.)
  let itb_true = IT.of_interval IBool (Big_int_Z.big_int_of_int 1, Big_int_Z.big_int_of_int 1)
  let itb_false = IT.of_interval IBool (Big_int_Z.big_int_of_int 0, Big_int_Z.big_int_of_int 0)
  let itb_unknown = IT.of_interval IBool (Big_int_Z.big_int_of_int 0, Big_int_Z.big_int_of_int 1)

  let assert_equal v1 v2 =
    assert_equal ~cmp:FI.equal ~printer:FI.show v1 v2

  let itb_xor v1 v2 = (**returns true, if both IntDomainTuple bool results are either unknown or different *)
    ((IT.equal v1 itb_unknown) && (IT.equal v2 itb_unknown)) || ((IT.equal v1 itb_true) && (IT.equal v2 itb_false)) || ((IT.equal v1 itb_false) && (IT.equal v2 itb_true))

  (**interval tests *)
  let test_FI_nan _ = 
    assert_equal (FI.top ()) (FI.of_const Float.nan)

  let test_FI_add1 _ =
    assert_equal fi_one (FI.add fi_zero fi_one)

  let test_FI_add2 _ =
    assert_equal fi_zero (FI.add fi_one fi_neg_one)

  (**interval tests using QCheck arbitraries *)
  let test_FI_not_bot =
    QCheck.Test.make ~name:"test_FI_not_bot" (FI.arbitrary ()) (fun arg -> 
        not (FI.is_bot arg))

  let test_FI_of_const_not_bot =
    QCheck.Test.make ~name:"test_FI_of_const_not_bot" QCheck.float (fun arg -> 
        not (FI.is_bot (FI.of_const arg)))

  let test_FI_div_zero_result_top =
    QCheck.Test.make ~name:"test_FI_div_zero_result_top" (FI.arbitrary ()) (fun arg ->
        FI.is_top (FI.div arg (FI.of_const 0.)))

  let test_FI_accurate_neg =
    QCheck.Test.make ~name:"test_FI_accurate_neg" QCheck.float (fun arg ->
        FI.equal (FI.of_const (-.arg)) (FI.neg (FI.of_const arg)))

  let test_FI_lt_xor_ge =
    QCheck.Test.make ~name:"test_FI_lt_xor_ge" (QCheck.pair (FI.arbitrary ()) (FI.arbitrary ())) (fun (arg1, arg2) ->
        itb_xor (FI.lt arg1 arg2) (FI.ge arg1 arg2))

  let test_FI_gt_xor_le =
    QCheck.Test.make ~name:"test_FI_lt_xor_ge" (QCheck.pair (FI.arbitrary ()) (FI.arbitrary ())) (fun (arg1, arg2) ->
        itb_xor (FI.gt arg1 arg2) (FI.le arg1 arg2))

  let test_FI_eq_xor_ne =
    QCheck.Test.make ~name:"test_FI_lt_xor_ge" (QCheck.pair (FI.arbitrary ()) (FI.arbitrary ())) (fun (arg1, arg2) ->
        itb_xor (FI.eq arg1 arg2) (FI.ne arg1 arg2))

  let test_FI_add =
    QCheck.Test.make ~name:"test_FI_add" (QCheck.pair QCheck.float QCheck.float) (fun (arg1, arg2) ->
        let result = FI.add (FI.of_const arg1) (FI.of_const arg2) in
        (FI.leq (FI.of_const (add Up arg1 arg2)) result) && (FI.leq (FI.of_const (add Down arg1 arg2)) result))

  let test_FI_sub =
    QCheck.Test.make ~name:"test_FI_sub" (QCheck.pair QCheck.float QCheck.float) (fun (arg1, arg2) ->
        let result = FI.sub (FI.of_const arg1) (FI.of_const arg2) in
        (FI.leq (FI.of_const (sub Up arg1 arg2)) result) && (FI.leq (FI.of_const (sub Down arg1 arg2)) result))

  let test_FI_mul =
    QCheck.Test.make ~name:"test_FI_mul" (QCheck.pair QCheck.float QCheck.float) (fun (arg1, arg2) ->
        let result = FI.mul (FI.of_const arg1) (FI.of_const arg2) in
        (FI.leq (FI.of_const (mul Up arg1 arg2)) result) && (FI.leq (FI.of_const (mul Down arg1 arg2)) result))

  let test_FI_div =
    QCheck.Test.make ~name:"test_FI_div" (QCheck.pair QCheck.float QCheck.float) (fun (arg1, arg2) ->
        let result = FI.div (FI.of_const arg1) (FI.of_const arg2) in
        (FI.leq (FI.of_const (div Up arg1 arg2)) result) && (FI.leq (FI.of_const (div Down arg1 arg2)) result))

  let test () = [
    "test_FI_nan" >:: test_FI_nan;
    "test_FI_add1" >:: test_FI_add1;
    "test_FI_add2" >:: test_FI_add2;
  ]

  let test_qcheck () = QCheck_ounit.to_ounit2_test_list [
      test_FI_not_bot;
      test_FI_of_const_not_bot;
      test_FI_div_zero_result_top;
      test_FI_accurate_neg;
      test_FI_lt_xor_ge;
      test_FI_gt_xor_le;
      test_FI_eq_xor_ne;
      test_FI_add;
      test_FI_sub;
      test_FI_mul;
      test_FI_div;
    ]
end

let test () = "floatDomainTest" >:::
              [ 
                "float_interval" >::: FloatInterval.test ();
                "float_interval_qcheck" >::: FloatInterval.test_qcheck ();
              ]