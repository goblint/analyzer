open Goblint_lib
open OUnit2
open FloatOps

module FloatInterval(Float_t: CFloatType)(Domain_t: FloatDomain.FloatDomainBase) =
struct
  module FI = Domain_t
  module IT = IntDomain.IntDomTuple

  let to_float = Float_t.to_float
  let of_float = Float_t.of_float
  let add = Float_t.add
  let sub = Float_t.sub
  let mul = Float_t.mul
  let div = Float_t.div

  let pred x = Option.get (to_float (Float_t.pred (of_float Nearest x)))
  let succ x = Option.get (to_float (Float_t.succ (of_float Nearest x)))

  let fmax = Option.get (to_float Float_t.upper_bound)
  let fmin = Option.get (to_float Float_t.lower_bound)
  let fsmall = Option.get (to_float Float_t.smallest)

  let fi_zero = FI.of_const 0.
  let fi_one = FI.of_const 1.
  let fi_neg_one = FI.of_const (-.1.)
  let itb_true = IT.of_int IBool (Big_int_Z.big_int_of_int 1)
  let itb_false = IT.of_int IBool (Big_int_Z.big_int_of_int 0)
  let itb_unknown = IT.of_interval IBool (Big_int_Z.big_int_of_int 0, Big_int_Z.big_int_of_int 1)

  let assert_equal v1 v2 =
    assert_equal ~cmp:FI.equal ~printer:FI.show v1 v2

  let itb_xor v1 v2 = (**returns true, if both IntDomainTuple bool results are either unknown or different *)
    ((IT.equal v1 itb_unknown) && (IT.equal v2 itb_unknown)) || ((IT.equal v1 itb_true) && (IT.equal v2 itb_false)) || ((IT.equal v1 itb_false) && (IT.equal v2 itb_true))

  (**interval tests *)
  let test_FI_nan _ =
    assert_equal (FI.top ()) (FI.of_const Float.nan)


  let test_FI_add_specific _ =
    let (+) = FI.add in
    let (=) a b = assert_equal b a in
    begin
      (FI.of_const (-. 0.)) = fi_zero;
      fi_zero + fi_one = fi_one;
      fi_neg_one + fi_one = fi_zero;
      fi_one + (FI.of_const fmax) = FI.top ();
      fi_neg_one + (FI.of_const fmin) = FI.top ();
      fi_neg_one + (FI.of_const fmax) = (FI.of_interval ((pred fmax), fmax));
      fi_one + (FI.of_const fmin) = (FI.of_interval (fmin, succ fmin));
      FI.top () + FI.top () = FI.top ();
      (FI.of_const fmin) + (FI.of_const fmax) = fi_zero;
      (FI.of_const fsmall) + (FI.of_const fsmall) = FI.of_const (fsmall +. fsmall);
      let one_plus_fsmall = Option.get (to_float (Float_t.add Up (Float_t.of_float Up 1.) Float_t.smallest)) in
      (FI.of_const fsmall) + (FI.of_const 1.) = FI.of_interval (1., one_plus_fsmall);
      (FI.of_interval (1., 2.)) + (FI.of_interval (2., 3.)) = FI.of_interval (3., 5.);
      (FI.of_interval (-. 2., 3.)) + (FI.of_interval (-. 100., 20.)) = FI.of_interval (-. 102., 23.);
    end

  let test_FI_sub_specific _ =
    let (-) = FI.sub in
    let (=) a b = assert_equal b a in
    begin
      fi_zero - fi_one = fi_neg_one;
      fi_neg_one - fi_one = FI.of_const (-. 2.);
      fi_one - (FI.of_const fmin) = FI.top ();
      fi_neg_one - (FI.of_const fmax) = FI.top ();
      (FI.of_const fmax) - fi_one = (FI.of_interval ((pred fmax), fmax));
      (FI.of_const fmin) - fi_neg_one = (FI.of_interval (fmin, succ fmin));
      FI.top () - FI.top () = FI.top ();
      (FI.of_const fmax) - (FI.of_const fmax) = fi_zero;
      (FI.of_const fsmall) - (FI.of_const fsmall) = fi_zero;
      (FI.of_const fsmall) - (FI.of_const 1.) = FI.of_interval (-. 1., succ (-. 1.));
      (FI.of_interval (-. 2., 3.)) - (FI.of_interval (-. 100., 20.)) = FI.of_interval (-. 22., 103.);
      (FI.of_const (-. 0.)) - fi_zero = fi_zero
    end

  let test_FI_mul_specific _ =
    let ( * ) = FI.mul in
    let (=) a b = assert_equal b a in
    begin
      fi_zero * fi_one = fi_zero;
      (FI.of_const 2.) * (FI.of_const fmin) = FI.top ();
      (FI.of_const 2.) * (FI.of_const fmax) = FI.top ();
      (FI.of_const fsmall) * (FI.of_const fmax) = FI.of_const (fsmall *. fmax);
      FI.top () * FI.top () = FI.top ();
      (FI.of_const fmax) * fi_zero = fi_zero;
      (FI.of_const fsmall) * fi_zero = fi_zero;
      (FI.of_const fsmall) * fi_one = FI.of_const fsmall;
      (FI.of_const fmax) * fi_one = FI.of_const fmax;
      (FI.of_const 2.) * (FI.of_const 0.5) = fi_one;
      (FI.of_interval (-. 2., 3.)) * (FI.of_interval (-. 100., 20.)) = FI.of_interval (-. 300., 200.);

      let up = if Float_t.name <> "float" then succ 1.00000000000000222 else succ (succ 1.00000000000000111 *. succ 1.00000000000000111) in
      begin
        (FI.of_const 1.00000000000000111) * (FI.of_const 1.00000000000000111) = FI.of_interval (1.00000000000000222 , up);
        (FI.of_const (-. 1.00000000000000111)) * (FI.of_const 1.00000000000000111) = FI.of_interval (-. up, -. 1.00000000000000222)
      end
    end

  let test_FI_div_specific _ =
    let (/) = FI.div in
    let (=) a b = assert_equal b a in
    begin
      fi_zero / fi_one = fi_zero;
      (FI.of_const 2.) / fi_zero = FI.nan ();
      fi_zero / fi_zero = FI.nan ();
      (FI.of_const fmax) / (FI.of_const fsmall) = FI.top ();
      (FI.of_const fmin) / (FI.of_const fsmall) = FI.top ();
      FI.top () / FI.top () = FI.top ();
      fi_zero / fi_one = fi_zero;
      (FI.of_const fsmall) / fi_one = FI.of_const fsmall;
      (FI.of_const fsmall) / (FI.of_const fsmall) = fi_one;
      (FI.of_const fmax) / (FI.of_const fmax) = fi_one;
      (FI.of_const fmax) / fi_one = FI.of_const fmax;
      (FI.of_const 2.) / (FI.of_const 0.5) = (FI.of_const 4.);
      (FI.of_const 4.) / (FI.of_const 2.) = (FI.of_const 2.);
      (FI.of_interval (-. 2., 3.)) / (FI.of_interval (-. 100., 20.)) = FI.top ();
      (FI.of_interval (6., 10.)) / (FI.of_interval (2., 3.)) = (FI.of_interval (2., 5.));

      (FI.of_const 1.) / (FI.of_const 3.) = (FI.of_interval (pred 0.333333333333333370340767487505, 0.333333333333333370340767487505));
      (FI.of_const (-. 1.)) / (FI.of_const 3.) = (FI.of_interval (-. 0.333333333333333370340767487505, succ (-. 0.333333333333333370340767487505)))
    end

  let test_FI_casti2f_specific _ =
    let cast_bool a b =
      assert_equal b (FI.of_int (IT.of_int IBool (Big_int_Z.big_int_of_int a))) in
    begin
      cast_bool 0 fi_zero;
      cast_bool 1 fi_one
    end;
    let cast a b = assert_equal b (FI.of_int a) in
    begin
      GobConfig.set_bool "ana.int.interval" true;
      cast (IT.top_of IInt) (FI.of_interval (-2147483648.,2147483647.));
      cast (IT.top_of IBool) (FI.of_interval (0., 1.));
      cast (IT.of_int IInt Big_int_Z.zero_big_int) fi_zero;
      cast (IT.of_int IInt Big_int_Z.unit_big_int) fi_one;
      (* no IChar because char has unknown signedness (particularly, unsigned on arm64) *)
      cast (IT.of_interval IUChar (Big_int_Z.big_int_of_int 0, Big_int_Z.big_int_of_int 128)) (FI.of_interval (0., 128.));
      cast (IT.of_interval ISChar (Big_int_Z.big_int_of_int (-8), Big_int_Z.big_int_of_int (-1))) (FI.of_interval (-. 8., - 1.));
      cast (IT.of_interval IUInt (Big_int_Z.big_int_of_int 2, Big_int_Z.big_int_of_int 100)) (FI.of_interval (2., 100.));
      cast (IT.of_interval IInt (Big_int_Z.big_int_of_int (- 100), Big_int_Z.big_int_of_int 100)) (FI.of_interval (-. 100., 100.));
      cast (IT.of_interval IUShort (Big_int_Z.big_int_of_int 2, Big_int_Z.big_int_of_int 100)) (FI.of_interval (2., 100.));
      cast (IT.of_interval IShort (Big_int_Z.big_int_of_int (- 100), Big_int_Z.big_int_of_int 100)) (FI.of_interval (-. 100., 100.));

      cast (IT.of_interval IULong (Big_int_Z.zero_big_int, Big_int_Z.big_int_of_int64 Int64.max_int)) (FI.of_interval (0., 9223372036854775807.));
      cast (IT.of_interval IULong (Big_int_Z.zero_big_int, Big_int_Z.big_int_of_int64 (9223372036854775806L))) (FI.of_interval (0., 9223372036854775807.));
      cast (IT.of_interval ILong (Big_int_Z.big_int_of_int64 Int64.min_int, Big_int_Z.zero_big_int)) (FI.of_interval (-. 9223372036854775808., 0.));
      cast (IT.of_interval ILong (Big_int_Z.big_int_of_int (- 100), Big_int_Z.big_int_of_int 100)) (FI.of_interval (-. 100., 100.));
      cast (IT.of_interval IULongLong (Big_int_Z.zero_big_int, Big_int_Z.big_int_of_int64 Int64.max_int)) (FI.of_interval (0., 9223372036854775807.));
      cast (IT.of_interval IULongLong (Big_int_Z.zero_big_int, Big_int_Z.big_int_of_int64 (9223372036854775806L))) (FI.of_interval (0., 9223372036854775807.));
      cast (IT.of_interval ILongLong (Big_int_Z.big_int_of_int64 Int64.min_int, Big_int_Z.zero_big_int)) (FI.of_interval (-. 9223372036854775808., 0.));
      cast (IT.of_interval ILongLong (Big_int_Z.big_int_of_int (- 100), Big_int_Z.big_int_of_int 100)) (FI.of_interval (-. 100., 100.));
      GobConfig.set_bool "ana.int.interval" false;
    end

  let test_FI_castf2i_specific _ =
    let cast ikind a b =
      OUnit2.assert_equal ~cmp:IT.equal ~printer:IT.show b (FI.to_int ikind a) in
    begin
      GobConfig.set_bool "ana.int.interval" true;
      cast IInt (FI.of_interval (-2147483648.,2147483647.)) (IT.top_of IInt);
      cast IInt (FI.of_interval (-9999999999.,9999999999.)) (IT.top_of IInt);
      cast IInt (FI.of_interval (-10.1,20.9)) (IT.of_interval IInt ( Big_int_Z.big_int_of_int (-10),  Big_int_Z.big_int_of_int 20));
      cast IBool (FI.of_interval (0.,1.)) (IT.top_of IBool);
      cast IBool (FI.of_interval (-9999999999.,9999999999.)) (IT.top_of IBool);
      cast IBool fi_one (IT.of_bool IBool true);
      cast IBool fi_zero (IT.of_bool IBool false);

      (* no IChar because char has unknown signedness (particularly, unsigned on arm64) *)
      cast IUChar (FI.of_interval (0.123, 128.999)) (IT.of_interval IUChar (Big_int_Z.big_int_of_int 0, Big_int_Z.big_int_of_int 128));
      cast ISChar (FI.of_interval (-. 8.0000000, 127.)) (IT.of_interval ISChar (Big_int_Z.big_int_of_int (-8), Big_int_Z.big_int_of_int 127));
      cast IUInt (FI.of_interval (2., 100.)) (IT.of_interval IUInt (Big_int_Z.big_int_of_int 2, Big_int_Z.big_int_of_int 100));
      cast IInt (FI.of_interval (-. 100.2, 100.1)) (IT.of_interval IInt (Big_int_Z.big_int_of_int (- 100), Big_int_Z.big_int_of_int 100));
      cast IUShort (FI.of_interval (2., 100.)) (IT.of_interval IUShort (Big_int_Z.big_int_of_int 2, Big_int_Z.big_int_of_int 100));
      cast IShort (FI.of_interval (-. 100., 100.)) (IT.of_interval IShort (Big_int_Z.big_int_of_int (- 100), Big_int_Z.big_int_of_int 100));

      cast IULong (FI.of_interval (0., 9223372036854775808.)) (IT.of_interval IULong (Big_int_Z.zero_big_int, Big_int_Z.big_int_of_string "9223372036854775808"));
      cast ILong (FI.of_interval (-. 9223372036854775808., 0.)) (IT.of_interval ILong (Big_int_Z.big_int_of_string "-9223372036854775808", Big_int_Z.zero_big_int));
      cast ILong (FI.of_interval (-. 100.99999, 100.99999)) (IT.of_interval ILong (Big_int_Z.big_int_of_int (- 100), Big_int_Z.big_int_of_int 100));
      cast IULongLong (FI.of_interval (0., 9223372036854775808.)) (IT.of_interval IULongLong (Big_int_Z.zero_big_int, Big_int_Z.big_int_of_string "9223372036854775808"));
      cast ILongLong (FI.of_interval (-. 9223372036854775808., 0.)) (IT.of_interval ILongLong ((Big_int_Z.big_int_of_string "-9223372036854775808"), Big_int_Z.zero_big_int));
      cast ILongLong  (FI.of_interval (-. 100., 100.)) (IT.of_interval ILongLong (Big_int_Z.big_int_of_int (- 100), Big_int_Z.big_int_of_int 100));
      GobConfig.set_bool "ana.int.interval" false;
    end

  let test_FI_meet_specific _ =
    let check_meet a b c =
      assert_equal c (FI.meet a b) in
    begin
      check_meet (FI.top ()) (FI.top ()) (FI.top ());
      check_meet (FI.top ()) fi_one fi_one;
      check_meet fi_zero fi_one (FI.bot ());
      check_meet (FI.of_interval (0., 10.)) (FI.of_interval (5., 20.)) (FI.of_interval (5., 10.));
    end

  let test_FI_join_specific _ =
    let check_join a b c =
      assert_equal c (FI.join a b) in
    begin
      check_join (FI.top ()) (FI.top ()) (FI.top ());
      check_join (FI.top ()) fi_one (FI.top ());
      check_join (FI.of_interval (0., 10.)) (FI.of_interval (5., 20.)) (FI.of_interval (0., 20.));
    end

  let test_FI_leq_specific _ =
    let check_leq flag a b =
      OUnit2.assert_equal flag (FI.leq a b) in
    begin
      check_leq true (FI.top ()) (FI.top ());
      check_leq true fi_one fi_one;
      check_leq false fi_one fi_zero;
      check_leq true (FI.of_interval (5., 20.)) (FI.of_interval (0., 20.));
      check_leq false (FI.of_interval (0., 20.)) (FI.of_interval (5., 20.));
      check_leq true (FI.of_interval (1., 19.)) (FI.of_interval (0., 20.));
      check_leq false (FI.of_interval (0., 20.)) (FI.of_interval (20.0001, 20.0002));
    end

  let test_FI_widen_specific _ =
    let check_widen a b c =
      assert_equal c (FI.widen a b) in
    begin
      check_widen (FI.top ()) (FI.top ()) (FI.top ());
      check_widen fi_zero (FI.top ()) (FI.top ());
      check_widen (FI.top ()) fi_one (FI.top ());
      check_widen fi_zero fi_one (FI.of_interval (0., fmax));
      check_widen fi_one fi_zero (FI.of_interval (fmin, 1.));
      check_widen fi_one (FI.of_interval (0., 2.)) (FI.of_interval (fmin, fmax));
    end

  let test_FI_narrow_specific _ =
    let check_narrow a b c =
      assert_equal c (FI.narrow a b) in
    begin
      check_narrow (FI.top ()) (FI.top ()) (FI.top ());
      check_narrow fi_zero (FI.top ()) fi_zero;
      check_narrow (FI.top ()) fi_zero fi_zero;
      check_narrow fi_zero fi_one fi_zero;
    end

  let test_FI_ArithmeticOnFloatBot _ =
    begin
      assert_raises (FloatDomain.ArithmeticOnFloatBot ("minimal "^(FI.show (FI.bot ())))) (fun() -> (FI.minimal (FI.bot ())));
      assert_raises (FloatDomain.ArithmeticOnFloatBot ("to_int "^(FI.show (FI.bot ())))) (fun() -> (FI.to_int IInt (FI.bot ())));
      assert_raises (FloatDomain.ArithmeticOnFloatBot ((FI.show (FI.bot ()))^" op "^(FI.show fi_zero))) (fun() -> (FI.add (FI.bot ()) fi_zero));
      assert_raises (FloatDomain.ArithmeticOnFloatBot ((FI.show (FI.bot ()))^" op "^(FI.show fi_zero))) (fun() -> (FI.lt (FI.bot ()) fi_zero));
      assert_raises (FloatDomain.ArithmeticOnFloatBot ("unop "^(FI.show (FI.bot ())))) (fun() -> (FI.acos (FI.bot ())));
    end

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
    QCheck.Test.make ~name:"test_FI_gt_xor_le" (QCheck.pair (FI.arbitrary ()) (FI.arbitrary ())) (fun (arg1, arg2) ->
        itb_xor (FI.gt arg1 arg2) (FI.le arg1 arg2))

  let test_FI_eq_xor_ne =
    QCheck.Test.make ~name:"test_FI_eq_xor_ne" (QCheck.pair (FI.arbitrary ()) (FI.arbitrary ())) (fun (arg1, arg2) ->
        itb_xor (FI.eq arg1 arg2) (FI.ne arg1 arg2))

  let test_FI_add =
    QCheck.Test.make ~name:"test_FI_add" (QCheck.pair QCheck.float QCheck.float) (fun (arg1, arg2) ->
        let result = FI.add (FI.of_const arg1) (FI.of_const arg2) in
        (FI.leq (FI.of_const (Option.get (to_float (add Up (of_float Nearest arg1) (of_float Nearest arg2))))) result) &&
        (FI.leq (FI.of_const (Option.get (to_float (add Down (of_float Nearest arg1) (of_float Nearest arg2))))) result))

  let test_FI_sub =
    QCheck.Test.make ~name:"test_FI_sub" (QCheck.pair QCheck.float QCheck.float) (fun (arg1, arg2) ->
        let result = FI.sub (FI.of_const arg1) (FI.of_const arg2) in
        (FI.leq (FI.of_const (Option.get (to_float (sub Up (of_float Nearest arg1) (of_float Nearest arg2))))) result) &&
        (FI.leq (FI.of_const (Option.get (to_float (sub Down (of_float Nearest arg1) (of_float Nearest arg2))))) result))

  let test_FI_mul =
    QCheck.Test.make ~name:"test_FI_mul" (QCheck.pair QCheck.float QCheck.float) (fun (arg1, arg2) ->
        let result = FI.mul (FI.of_const arg1) (FI.of_const arg2) in
        (FI.leq (FI.of_const (Option.get (to_float (mul Up (of_float Nearest arg1) (of_float Nearest arg2))))) result) &&
        (FI.leq (FI.of_const (Option.get (to_float (mul Down (of_float Nearest arg1) (of_float Nearest arg2))))) result))


  let test_FI_div =
    QCheck.Test.make ~name:"test_FI_div" (QCheck.pair QCheck.float QCheck.float) (fun (arg1, arg2) ->
        let result = FI.div (FI.of_const arg1) (FI.of_const arg2) in
        (FI.leq (FI.of_const (Option.get (to_float (div Up (of_float Nearest arg1) (of_float Nearest arg2))))) result) &&
        (FI.leq (FI.of_const (Option.get (to_float (div Down (of_float Nearest arg1) (of_float Nearest arg2))))) result))


  let test () = [
    "test_FI_nan" >:: test_FI_nan;
    "test_FI_add_specific" >:: test_FI_add_specific;
    "test_FI_sub_specific" >:: test_FI_sub_specific;
    "test_FI_mul_specific" >:: test_FI_mul_specific;
    "test_FI_div_specific" >:: test_FI_div_specific;
    "test_FI_casti2f_specific" >:: test_FI_casti2f_specific;
    "test_FI_castf2i_specific" >:: test_FI_castf2i_specific;
    (* "test_FI_castf2f_specific" >::  *)
    "test_FI_join_specific" >:: test_FI_meet_specific;
    "test_FI_meet_specific" >:: test_FI_join_specific;
    "test_FI_meet_specific" >:: test_FI_leq_specific;
    "test_FI_widen_specific" >:: test_FI_widen_specific;
    "test_FI_narrow_specific" >:: test_FI_narrow_specific;
    "test_FI_ArithmeticOnFloatBot" >:: test_FI_ArithmeticOnFloatBot;
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

module FloatIntervalTest32 = FloatInterval(CFloat)(FloatDomain.F32Interval)
module FloatIntervalTest64 = FloatInterval(CDouble)(FloatDomain.F64Interval)

let test () =
  "floatDomainTest" >:::
  [
    "float_interval32" >::: FloatIntervalTest32.test ();
    "float_interval_qcheck32" >::: FloatIntervalTest32.test_qcheck ();
    "float_interval64" >::: FloatIntervalTest64.test ();
    "float_interval_qcheck64" >::: FloatIntervalTest64.test_qcheck ();
  ]
