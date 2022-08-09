open Goblint_lib
open OUnit2
open GoblintCil
open Z

module IntTest (I:IntDomainProperties.OldS) =
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


  let test_int_comp _ =
    assert_equal ~printer:I.show izero (I.of_int zero);
    assert_equal ~printer:I.show ione  (I.of_int one);
    assert_equal ~printer:I.show itrue (I.of_bool true);
    assert_equal ~printer:I.show ifalse(I.of_bool false);
    assert_bool "IntDomain cannot hold 1" (I.is_int ione) ;
    assert_bool "IntDomain cannot hold 0" (I.is_int izero) ;
    assert_equal (Some one ) (I.to_int ione);
    assert_equal (Some zero) (I.to_int izero);
    assert_equal (Some zero) (I.to_int ifalse)


  let test_bool _ =
    assert_equal (Some true ) (I.to_bool ione);
    assert_equal (Some false) (I.to_bool izero);
    assert_bool "0 isn't bool" (I.is_bool izero);
    assert_bool "1 isn't bool" (I.is_bool ione);
    assert_bool "true isn't bool" (I.is_bool itrue);
    assert_bool "false isn't bool" (I.is_bool ifalse);
    assert_equal ~printer:I.show itrue  (I.lt ione  itwo);
    assert_equal ~printer:I.show ifalse (I.gt ione  itwo);
    assert_equal ~printer:I.show itrue  (I.le ione  ione);
    assert_equal ~printer:I.show ifalse (I.ge izero itwo);
    assert_equal ~printer:I.show itrue  (I.eq izero izero);
    assert_equal ~printer:I.show ifalse (I.ne ione  ione)


  let test_neg _ =
    assert_equal ~printer:I.show in5 (I.neg i5);
    assert_equal ~printer:I.show i5 (I.neg (I.neg i5));
    assert_equal ~printer:I.show izero (I.neg izero)


  let test_add _ =
    assert_equal ~printer:I.show ione (I.add izero ione);
    assert_equal ~printer:I.show ione (I.add ione  izero);
    assert_equal ~printer:I.show izero(I.add izero izero)


  let test_sub _ =
    assert_equal ~printer:I.show ione (I.sub izero iminus_one);
    assert_equal ~printer:I.show ione (I.sub ione  izero);
    assert_equal ~printer:I.show izero(I.sub izero izero)


  let test_mul _ =
    assert_equal ~printer:I.show izero(I.mul izero iminus_one);
    assert_equal ~printer:I.show izero(I.mul izero izero);
    assert_equal ~printer:I.show ione (I.mul ione  ione);
    assert_equal ~printer:I.show i42  (I.mul ione  i42)


  let test_div _ =
    assert_equal ~printer:I.show ione (I.div ione ione);
    assert_equal ~printer:I.show ione (I.div i5 i5);
    assert_equal ~printer:I.show i5   (I.div i5 ione);
    assert_equal ~printer:I.show in5  (I.div i5 iminus_one);
    assert_bool "div_by_0" (try I.is_top (I.div i5 izero) with Division_by_zero -> true)


  let test_rem _ =
    assert_equal ~printer:I.show ione (I.rem ione  i5);
    assert_equal ~printer:I.show izero(I.rem izero i5);
    assert_equal ~printer:I.show itwo (I.rem i42   i5);
    assert_equal ~printer:I.show itwo (I.rem i42   in5)


  let test_bit _ =
    assert_equal ~printer:I.show iminus_one (I.bitnot izero);
    assert_equal ~printer:I.show iminus_two (I.bitnot ione);
    assert_equal ~printer:I.show i5   (I.bitand i5 i5);
    assert_equal ~printer:I.show i4   (I.bitand i5 i4);
    assert_equal ~printer:I.show i5   (I.bitor  i4 ione);
    assert_equal ~printer:I.show ione (I.bitxor i4 i5);
    assert_equal ~printer:I.show itwo (I.shift_left  ione ione );
    assert_equal ~printer:I.show ione (I.shift_left  ione izero);
    assert_equal ~printer:I.show ione (I.shift_right itwo ione);
    assert_equal ~printer:I.show izero(I.shift_right ione ione)


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

module Ikind = struct let ikind () = Cil.ILong end
module A = IntTest (IntDomain.Integers (IntOps.BigIntOps))
module B = IntTest (IntDomain.FlattenedBI)
module C = IntTest (IntDomainProperties.WithIkind (IntDomain.DefExc) (Ikind))
module T = struct
  include IntDomainProperties.WithIkind (IntDomain.DefExc) (Ikind)
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

let test_bot _ =
  assert_bool "bot != bot" (T.is_bot tbot);
  assert_bool "top != top" (T.is_top ttop);
  assert_bool "top == bot" (not (T.is_bot ttop));
  assert_bool "bot == top" (not (T.is_top tbot));
  assert_bool "0 == top" (not (T.is_top tzero));
  assert_bool "1 == top" (not (T.is_top tone))

let test_join _ =
  assert_equal ~printer:T.show tone (T.join tbot  tone);
  assert_equal ~printer:T.show tzero(T.join tbot  tzero);
  assert_equal ~printer:T.show tone (T.join tone  tbot);
  assert_equal ~printer:T.show tzero(T.join tzero tbot);
  assert_equal ~printer:T.show ttop (T.join ttop  tone);
  assert_equal ~printer:T.show ttop (T.join ttop  tzero);
  assert_equal ~printer:T.show ttop (T.join tone  ttop);
  assert_equal ~printer:T.show ttop (T.join tzero ttop);
  assert_equal ~printer:T.show tbot (T.join tbot  tbot);
  assert_equal ~printer:T.show tone (T.join tone  tone);
  (* assert_equal ~printer:T.show tex0 (T.join tone  ttwo); *) (* TODO: now more precise range *)
  (* assert_equal ~printer:T.show ttop (T.join tone  tzero); *) (* TODO: now more precise range *)
  assert_equal ~printer:T.show tex0 (T.join tex0  tex0);
  assert_equal ~printer:T.show tex1 (T.join tex1  tex1);
  assert_equal ~printer:T.show ttop (T.join tex1  tex0);
  assert_equal ~printer:T.show ttop (T.join tex0  tex1);
  (* assert_equal ~printer:T.show tex0 (T.join tone  ttwo); *) (* TODO: now more precise range *)
  assert_equal ~printer:T.show tex1 (T.join tex1  tzero);
  assert_equal ~printer:T.show tex0 (T.join tex0  tone );
  assert_equal ~printer:T.show tex1 (T.join tex1  tzero);
  assert_equal ~printer:T.show tex0 (T.join tex0  tone )

let test_meet _ =
  assert_equal ~printer:T.show tbot (T.meet tbot  tone);
  assert_equal ~printer:T.show tbot (T.meet tbot  tzero);
  assert_equal ~printer:T.show tbot (T.meet tone  tbot);
  assert_equal ~printer:T.show tbot (T.meet tzero tbot);
  assert_equal ~printer:T.show tone (T.meet ttop  tone);
  assert_equal ~printer:T.show tzero(T.meet ttop  tzero);
  assert_equal ~printer:T.show tone (T.meet tone  ttop);
  assert_equal ~printer:T.show tzero(T.meet tzero ttop);
  assert_equal ~printer:T.show tbot (T.meet tbot  tbot);
  assert_equal ~printer:T.show tone (T.meet tone  tone);
  assert_equal ~printer:T.show tbot (T.meet tone  ttwo);
  assert_equal ~printer:T.show tbot (T.meet tone  tzero);
  assert_equal ~printer:T.show tex0 (T.meet tex0  tex0);
  assert_equal ~printer:T.show tex1 (T.meet tex1  tex1);
  assert_equal ~printer:T.show tex10(T.meet tex1  tex0);
  assert_equal ~printer:T.show tex01(T.meet tex0  tex1);
  assert_equal ~printer:T.show tzero(T.meet tex1  tzero);
  assert_equal ~printer:T.show tone (T.meet tex0  tone );
  assert_equal ~printer:T.show tzero(T.meet tex1  tzero);
  assert_equal ~printer:T.show tone (T.meet tex0  tone )

let test_ex_set _ =
  assert_equal (Some [zero; one]) (T.to_excl_list tex10 |> Option.map fst);
  assert_equal (Some [zero; one]) (T.to_excl_list tex01 |> Option.map fst);
  assert_bool  "Not [1;0] is not excl set" (T.is_excl_list tex10);
  assert_bool  "bot is excl set" (not (T.is_excl_list tbot));
  assert_bool  "42  is excl set" (not (T.is_excl_list t42));
  assert_equal (Some true) (T.to_bool tex0);
  assert_equal (Some true) (T.to_bool tex10);
  assert_equal None (T.to_bool tex1)

module Interval =
struct
  module I = IntDomain.Interval

  let assert_equal x y =
    assert_equal ~cmp:I.equal ~printer:I.show x y

  let test_interval_rem _ =
    let ik = Cil.IInt in
    assert_equal (I.of_int ik Z.zero) (I.rem ik (I.of_int ik Z.minus_one) (I.of_int ik Z.one))

  let test () = [
    "test_interval_rem" >:: test_interval_rem;
  ]
end

module Congruence =
struct
  module C = IntDomain.Congruence

  let assert_equal x y =
    assert_equal ~cmp:C.equal ~printer:C.show x y

  let test_shift_left _ =
    let ik = Cil.IBool in
    assert_equal (C.top_of ik) (C.join ik (C.of_int ik Z.zero) (C.of_int ik Z.one));
    assert_equal (C.top_of ik) (C.shift_left ik (C.of_int ik Z.one) (C.top_of ik))

  let test () = [
    "test_shift_left" >:: test_shift_left;
  ]
end

module IntDomTuple =
struct
  let exists0 =
    let open Batteries in
    let to_list x = Tuple4.enum x |> List.of_enum |> List.filter_map identity in
    let f g = g identity % to_list in
    List.(f exists)

  let exists1 = function
    | (Some true, _, _, _)
    | (_, Some true, _, _)
    | (_, _, Some true, _)
    | (_, _, _, Some true) ->
      true
    | _ ->
      false

  let bool_option = QCheck.option QCheck.bool
  let arb = QCheck.quad bool_option bool_option bool_option bool_option

  let test_exists = QCheck.Test.make ~name:"exists" arb (fun args ->
      exists0 args = exists1 args
    )

  let for_all0 =
    let open Batteries in
    let to_list x = Tuple4.enum x |> List.of_enum |> List.filter_map identity in
    let f g = g identity % to_list in
    List.(f for_all)

  let for_all1 = function
    | (Some false, _, _, _)
    | (_, Some false, _, _)
    | (_, _, Some false, _)
    | (_, _, _, Some false) ->
      false
    | _ ->
      true

  let test_for_all = QCheck.Test.make ~name:"for_all" arb (fun args ->
      for_all0 args = for_all1 args
    )


  let test () = QCheck_ounit.to_ounit2_test_list [
      test_exists;
      test_for_all;
    ]
end

let test () = "intDomainTest" >:::
              [ "int_Integers"  >::: A.test ();
                "int_Flattened" >::: B.test ();
                "int_DefExc"     >::: C.test ();
                "test_bot"      >::  test_bot;
                "test_join"     >::  test_join;
                "test_meet"     >::  test_meet;
                "test_excl_list">::  test_ex_set;
                "interval" >::: Interval.test ();
                "congruence" >::: Congruence.test ();
                "intDomTuple" >::: IntDomTuple.test ();
              ]
