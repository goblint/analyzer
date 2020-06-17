open OUnit
open Int64

module IntTest (I:IntDomain.S) =
struct
  let izero      = I.of_int zero
  let ione       = I.of_int one
  let iminus_one = I.of_int minus_one
  let itwo       = I.of_int (of_int 2)
  let iminus_two = I.of_int (of_int (-2))
  let i4         = I.of_int (of_int 4)
  let i5         = I.of_int (of_int 5)
  let in5        = I.of_int (of_int (-5))
  let i42        = I.of_int (of_int 42)
  let itrue      = I.of_bool true
  let ifalse     = I.of_bool false


  let test_int_comp () =
    assert_equal ~printer:(I.short 80) izero (I.of_int zero);
    assert_equal ~printer:(I.short 80) ione  (I.of_int one);
    assert_equal ~printer:(I.short 80) itrue (I.of_bool true);
    assert_equal ~printer:(I.short 80) ifalse(I.of_bool false);
    assert_bool "IntDomain cannot hold 1" (I.is_int ione) ;
    assert_bool "IntDomain cannot hold 0" (I.is_int izero) ;
    assert_equal (Some one ) (I.to_int ione);
    assert_equal (Some zero) (I.to_int izero);
    assert_equal (Some zero) (I.to_int ifalse)


  let test_bool () =
    assert_equal (Some true ) (I.to_bool ione);
    assert_equal (Some false) (I.to_bool izero);
    assert_bool "0 isn't bool" (I.is_bool izero);
    assert_bool "1 isn't bool" (I.is_bool ione);
    assert_bool "true isn't bool" (I.is_bool itrue);
    assert_bool "false isn't bool" (I.is_bool ifalse);
    assert_equal ~printer:(I.short 80) itrue  (I.lt ione  itwo);
    assert_equal ~printer:(I.short 80) ifalse (I.gt ione  itwo);
    assert_equal ~printer:(I.short 80) itrue  (I.le ione  ione);
    assert_equal ~printer:(I.short 80) ifalse (I.ge izero itwo);
    assert_equal ~printer:(I.short 80) itrue  (I.eq izero izero);
    assert_equal ~printer:(I.short 80) ifalse (I.ne ione  ione)


  let test_neg () =
    assert_equal ~printer:(I.short 80) in5 (I.neg i5);
    assert_equal ~printer:(I.short 80) i5 (I.neg (I.neg i5));
    assert_equal ~printer:(I.short 80) izero (I.neg izero)


  let test_add () =
    assert_equal ~printer:(I.short 80) ione (I.add izero ione);
    assert_equal ~printer:(I.short 80) ione (I.add ione  izero);
    assert_equal ~printer:(I.short 80) izero(I.add izero izero)


  let test_sub () =
    assert_equal ~printer:(I.short 80) ione (I.sub izero iminus_one);
    assert_equal ~printer:(I.short 80) ione (I.sub ione  izero);
    assert_equal ~printer:(I.short 80) izero(I.sub izero izero)


  let test_mul () =
    assert_equal ~printer:(I.short 80) izero(I.mul izero iminus_one);
    assert_equal ~printer:(I.short 80) izero(I.mul izero izero);
    assert_equal ~printer:(I.short 80) ione (I.mul ione  ione);
    assert_equal ~printer:(I.short 80) i42  (I.mul ione  i42)


  let test_div () =
    assert_equal ~printer:(I.short 80) ione (I.div ione ione);
    assert_equal ~printer:(I.short 80) ione (I.div i5 i5);
    assert_equal ~printer:(I.short 80) i5   (I.div i5 ione);
    assert_equal ~printer:(I.short 80) in5  (I.div i5 iminus_one);
    assert_bool "div_by_0" (try I.is_bot (I.div i5 izero) with Division_by_zero -> true)


  let test_rem () =
    assert_equal ~printer:(I.short 80) ione (I.rem ione  i5);
    assert_equal ~printer:(I.short 80) izero(I.rem izero i5);
    assert_equal ~printer:(I.short 80) itwo (I.rem i42   i5);
    assert_equal ~printer:(I.short 80) itwo (I.rem i42   in5)


  let test_bit () =
    assert_equal ~printer:(I.short 80) iminus_one (I.bitnot izero);
    assert_equal ~printer:(I.short 80) iminus_two (I.bitnot ione);
    assert_equal ~printer:(I.short 80) i5   (I.bitand i5 i5);
    assert_equal ~printer:(I.short 80) i4   (I.bitand i5 i4);
    assert_equal ~printer:(I.short 80) i5   (I.bitor  i4 ione);
    assert_equal ~printer:(I.short 80) ione (I.bitxor i4 i5);
    assert_equal ~printer:(I.short 80) itwo (I.shift_left  ione ione );
    assert_equal ~printer:(I.short 80) ione (I.shift_left  ione izero);
    assert_equal ~printer:(I.short 80) ione (I.shift_right itwo ione);
    assert_equal ~printer:(I.short 80) izero(I.shift_right ione ione)


  let test () =
    [ ("test_int_comp" >:: test_int_comp );
      ("test_bool"     >:: test_bool     );
      ("test_neg"      >:: test_neg      );
      ("test_add"      >:: test_add      );
      ("test_sub"      >:: test_sub      );
      ("test_mul"      >:: test_mul      );
      ("test_div"      >:: test_div      );
      ("test_rem"      >:: test_rem      );
      ("test_bit"      >:: test_bit      );
    ]

end

module A = IntTest (IntDomain.Integers)
module B = IntTest (IntDomain.Flattened)
module C = IntTest (IntDomain.DefExc)
module T = struct
  include IntDomain.DefExc
  let of_excl_list xs = of_excl_list Cil.ILong xs
end

let tzero      = T.of_int zero
let tone       = T.of_int one
let tminus_one = T.of_int minus_one
let ttwo       = T.of_int (of_int 2)
let tminus_two = T.of_int (of_int (-2))
let t4         = T.of_int (of_int 4)
let t5         = T.of_int (of_int 5)
let tn5        = T.of_int (of_int (-5))
let t42        = T.of_int (of_int 42)
let ttrue      = T.of_bool true
let tfalse     = T.of_bool false
let ttop       = T.top ()
let tbot       = T.bot ()
let tex0       = T.of_excl_list [zero]
let tex1       = T.of_excl_list [one ]
let tex10      = T.of_excl_list [zero; one]
let tex01      = T.of_excl_list [one; zero]

let test_bot () =
  assert_bool "bot != bot" (T.is_bot tbot);
  assert_bool "top != top" (T.is_top ttop);
  assert_bool "top == bot" (not (T.is_bot ttop));
  assert_bool "bot == top" (not (T.is_top tbot));
  assert_bool "0 == top" (not (T.is_top tzero));
  assert_bool "1 == top" (not (T.is_top tone))

let test_join () =
  assert_equal ~printer:(T.short 80) tone (T.join tbot  tone);
  assert_equal ~printer:(T.short 80) tzero(T.join tbot  tzero);
  assert_equal ~printer:(T.short 80) tone (T.join tone  tbot);
  assert_equal ~printer:(T.short 80) tzero(T.join tzero tbot);
  assert_equal ~printer:(T.short 80) ttop (T.join ttop  tone);
  assert_equal ~printer:(T.short 80) ttop (T.join ttop  tzero);
  assert_equal ~printer:(T.short 80) ttop (T.join tone  ttop);
  assert_equal ~printer:(T.short 80) ttop (T.join tzero ttop);
  assert_equal ~printer:(T.short 80) tbot (T.join tbot  tbot);
  assert_equal ~printer:(T.short 80) tone (T.join tone  tone);
  assert_equal ~printer:(T.short 80) tex0 (T.join tone  ttwo);
  assert_equal ~printer:(T.short 80) ttop (T.join tone  tzero);
  assert_equal ~printer:(T.short 80) tex0 (T.join tex0  tex0);
  assert_equal ~printer:(T.short 80) tex1 (T.join tex1  tex1);
  assert_equal ~printer:(T.short 80) ttop (T.join tex1  tex0);
  assert_equal ~printer:(T.short 80) ttop (T.join tex0  tex1);
  assert_equal ~printer:(T.short 80) tex0 (T.join tone  ttwo);
  assert_equal ~printer:(T.short 80) tex1 (T.join tex1  tzero);
  assert_equal ~printer:(T.short 80) tex0 (T.join tex0  tone );
  assert_equal ~printer:(T.short 80) tex1 (T.join tex1  tzero);
  assert_equal ~printer:(T.short 80) tex0 (T.join tex0  tone )

let test_meet () =
  assert_equal ~printer:(T.short 80) tbot (T.meet tbot  tone);
  assert_equal ~printer:(T.short 80) tbot (T.meet tbot  tzero);
  assert_equal ~printer:(T.short 80) tbot (T.meet tone  tbot);
  assert_equal ~printer:(T.short 80) tbot (T.meet tzero tbot);
  assert_equal ~printer:(T.short 80) tone (T.meet ttop  tone);
  assert_equal ~printer:(T.short 80) tzero(T.meet ttop  tzero);
  assert_equal ~printer:(T.short 80) tone (T.meet tone  ttop);
  assert_equal ~printer:(T.short 80) tzero(T.meet tzero ttop);
  assert_equal ~printer:(T.short 80) tbot (T.meet tbot  tbot);
  assert_equal ~printer:(T.short 80) tone (T.meet tone  tone);
  assert_equal ~printer:(T.short 80) tbot (T.meet tone  ttwo);
  assert_equal ~printer:(T.short 80) tbot (T.meet tone  tzero);
  assert_equal ~printer:(T.short 80) tex0 (T.meet tex0  tex0);
  assert_equal ~printer:(T.short 80) tex1 (T.meet tex1  tex1);
  assert_equal ~printer:(T.short 80) tex10(T.meet tex1  tex0);
  assert_equal ~printer:(T.short 80) tex01(T.meet tex0  tex1);
  assert_equal ~printer:(T.short 80) tzero(T.meet tex1  tzero);
  assert_equal ~printer:(T.short 80) tone (T.meet tex0  tone );
  assert_equal ~printer:(T.short 80) tzero(T.meet tex1  tzero);
  assert_equal ~printer:(T.short 80) tone (T.meet tex0  tone )

let test_ex_set () =
  assert_equal (Some [zero; one]) (T.to_excl_list tex10);
  assert_equal (Some [zero; one]) (T.to_excl_list tex01);
  assert_bool  "Not [1;0] is not excl set" (T.is_excl_list tex10);
  assert_bool  "bot is excl set" (not (T.is_excl_list tbot));
  assert_bool  "42  is excl set" (not (T.is_excl_list t42));
  assert_equal (Some true) (T.to_bool tex0);
  assert_equal (Some true) (T.to_bool tex10);
  assert_equal None (T.to_bool tex1)

let test () = "intDomainTest" >:::
              [ "int_Integers"  >::: A.test ();
                "int_Flattened" >::: B.test ();
                "int_DefExc"     >::: C.test ();
                "test_bot"      >::  test_bot;
                "test_join"     >::  test_join;
                "test_meet"     >::  test_meet;
                "test_excl_list">::  test_ex_set;
              ]
