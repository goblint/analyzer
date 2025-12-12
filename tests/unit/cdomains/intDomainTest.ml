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

  let assert_poly_equal = assert_equal
  let assert_equal x y =
    assert_equal ~cmp:I.equal ~printer:I.show x y


  let test_int_comp _ =
    assert_equal izero (I.of_int zero);
    assert_equal ione  (I.of_int one);
    assert_equal itrue (I.of_bool true);
    assert_equal ifalse(I.of_bool false);
    assert_poly_equal (Some one ) (I.to_int ione);
    assert_poly_equal (Some zero) (I.to_int izero);
    assert_poly_equal (Some zero) (I.to_int ifalse)


  let test_bool _ =
    assert_poly_equal (Some true ) (I.to_bool ione);
    assert_poly_equal (Some false) (I.to_bool izero);
    assert_poly_equal (Some true ) (I.to_bool itrue);
    assert_poly_equal (Some false) (I.to_bool ifalse);
    assert_equal itrue  (I.lt ione  itwo);
    assert_equal ifalse (I.gt ione  itwo);
    assert_equal itrue  (I.le ione  ione);
    assert_equal ifalse (I.ge izero itwo);
    assert_equal itrue  (I.eq izero izero);
    assert_equal ifalse (I.ne ione  ione)


  let test_neg _ =
    assert_equal in5 (I.neg i5);
    assert_equal i5 (I.neg (I.neg i5));
    assert_equal izero (I.neg izero)


  let test_add _ =
    assert_equal ione (I.add izero ione);
    assert_equal ione (I.add ione  izero);
    assert_equal izero(I.add izero izero)


  let test_sub _ =
    assert_equal ione (I.sub izero iminus_one);
    assert_equal ione (I.sub ione  izero);
    assert_equal izero(I.sub izero izero)


  let test_mul _ =
    assert_equal izero(I.mul izero iminus_one);
    assert_equal izero(I.mul izero izero);
    assert_equal ione (I.mul ione  ione);
    assert_equal i42  (I.mul ione  i42)


  let test_div _ =
    assert_equal ione (I.div ione ione);
    assert_equal ione (I.div i5 i5);
    assert_equal i5   (I.div i5 ione);
    assert_equal in5  (I.div i5 iminus_one);
    assert_bool "div_by_0" (try I.is_top (I.div i5 izero) with Division_by_zero -> true)


  let test_rem _ =
    assert_equal ione (I.rem ione  i5);
    assert_equal izero(I.rem izero i5);
    assert_equal itwo (I.rem i42   i5);
    assert_equal itwo (I.rem i42   in5)


  let test_bit _ =
    assert_equal iminus_one (I.lognot izero);
    assert_equal iminus_two (I.lognot ione);
    assert_equal i5   (I.logand i5 i5);
    assert_equal i4   (I.logand i5 i4);
    assert_equal i5   (I.logor  i4 ione);
    assert_equal ione (I.logxor i4 i5);
    assert_equal itwo (I.shift_left  ione ione );
    assert_equal ione (I.shift_left  ione izero);
    assert_equal ione (I.shift_right itwo ione);
    assert_equal izero(I.shift_right ione ione)


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
module B = IntTest (IntDomain.Flat (IntDomain.Integers (IntOps.BigIntOps)))
module C = IntTest (IntDomainProperties.WithIkind (IntDomainProperties.MakeS2 (IntDomain.DefExc)) (Ikind))

module D = struct
  module T = struct
    include IntDomainProperties.WithIkind (IntDomainProperties.MakeS2 (IntDomain.DefExc)) (Ikind)
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

  let assert_poly_equal = assert_equal
  let assert_equal x y = assert_equal ~cmp: T.equal ~printer:T.show x y

  let test_join _ =
    assert_equal tone (T.join tbot  tone);
    assert_equal tzero(T.join tbot  tzero);
    assert_equal tone (T.join tone  tbot);
    assert_equal tzero(T.join tzero tbot);
    assert_equal ttop (T.join ttop  tone);
    assert_equal ttop (T.join ttop  tzero);
    assert_equal ttop (T.join tone  ttop);
    assert_equal ttop (T.join tzero ttop);
    assert_equal tbot (T.join tbot  tbot);
    assert_equal tone (T.join tone  tone);
    (* assert_equal tex0 (T.join tone  ttwo); *) (* TODO: now more precise range *)
    (* assert_equal ttop (T.join tone  tzero); *) (* TODO: now more precise range *)
    assert_equal tex0 (T.join tex0  tex0);
    assert_equal tex1 (T.join tex1  tex1);
    assert_equal ttop (T.join tex1  tex0);
    assert_equal ttop (T.join tex0  tex1);
    (* assert_equal ~printer:T.show tex0 (T.join tone  ttwo); *) (* TODO: now more precise range *)
    assert_equal tex1 (T.join tex1  tzero);
    assert_equal tex0 (T.join tex0  tone );
    assert_equal tex1 (T.join tex1  tzero);
    assert_equal tex0 (T.join tex0  tone )

  let test_meet _ =
    assert_equal tbot (T.meet tbot  tone);
    assert_equal tbot (T.meet tbot  tzero);
    assert_equal tbot (T.meet tone  tbot);
    assert_equal tbot (T.meet tzero tbot);
    assert_equal tone (T.meet ttop  tone);
    assert_equal tzero(T.meet ttop  tzero);
    assert_equal tone (T.meet tone  ttop);
    assert_equal tzero(T.meet tzero ttop);
    assert_equal tbot (T.meet tbot  tbot);
    assert_equal tone (T.meet tone  tone);
    assert_equal tbot (T.meet tone  ttwo);
    assert_equal tbot (T.meet tone  tzero);
    assert_equal tex0 (T.meet tex0  tex0);
    assert_equal tex1 (T.meet tex1  tex1);
    assert_equal tex10(T.meet tex1  tex0);
    assert_equal tex01(T.meet tex0  tex1);
    assert_equal tzero(T.meet tex1  tzero);
    assert_equal tone (T.meet tex0  tone );
    assert_equal tzero(T.meet tex1  tzero);
    assert_equal tone (T.meet tex0  tone )

  let test_ex_set _ =
    assert_poly_equal (Some [zero; one]) (T.to_excl_list tex10 |> Option.map fst);
    assert_poly_equal (Some [zero; one]) (T.to_excl_list tex01 |> Option.map fst);
    assert_bool  "Not [1;0] is not excl set" (T.is_excl_list tex10);
    assert_bool  "bot is excl set" (not (T.is_excl_list tbot));
    assert_bool  "42  is excl set" (not (T.is_excl_list t42));
    assert_poly_equal (Some true) (T.to_bool tex0);
    assert_poly_equal (Some true) (T.to_bool tex10);
    assert_poly_equal None (T.to_bool tex1)
end

module IntervalTest (I : IntDomain.SOverflow with type int_t = Z.t) =
struct
  module I = IntDomain.SOverflowUnlifter (I)
  let ik      = Cil.IInt
  let i65536  = I.of_interval ik (Z.zero, of_int 65536)
  let i65537  = I.of_interval ik (Z.zero, of_int 65537)
  let imax    = I.of_interval ik (Z.zero, of_int 2147483647)
  let imin    = I.of_interval ik (of_int (-2147483648), Z.zero)

  let assert_equal x y =
    assert_equal ~cmp:I.equal ~printer:I.show x y

  let test_interval_rem _ =
    assert_equal (I.of_int ik Z.zero) (I.rem ik (I.of_int ik Z.minus_one) (I.of_int ik Z.one))

  let test_interval_widen _ =
    GobConfig.set_bool "ana.int.interval_threshold_widening" true;
    GobConfig.set_string "ana.int.interval_threshold_widening_constants" "comparisons";
    assert_equal imax (I.widen ik i65536 i65537);
    assert_equal imax (I.widen ik i65536 imax)

  let test_interval_narrow _ =
    GobConfig.set_bool "ana.int.interval_threshold_widening" true;
    GobConfig.set_string "ana.int.interval_threshold_widening_constants" "comparisons";
    let i_zero_one = I.of_interval ik (Z.zero, Z.one) in
    let i_zero_five = I.of_interval ik (Z.zero, of_int 5) in
    let to_widen = I.of_interval ik (Z.zero, Z.zero) in
    (* this should widen to [0, x], where x is the next largest threshold above 5 or the maximal int*)
    let widened = I.widen ik to_widen i_zero_five in
    (* either way, narrowing from [0, x] to [0, 1] should be possible *)
    let narrowed = I.narrow ik widened i_zero_one in
    (* however, narrowing should not allow [0, x] to grow *)
    let narrowed2 = I.narrow ik widened imax in
    assert_equal i_zero_one narrowed;
    assert_equal widened narrowed2;

    (* the same tests, but for lower bounds *)
    let i_minus_one_zero = I.of_interval ik (Z.minus_one, Z.zero) in
    let i_minus_five_zero = I.of_interval ik (of_int (-5), Z.zero) in
    let widened = I.widen ik to_widen i_minus_five_zero in
    let narrowed = I.narrow ik widened i_minus_one_zero in
    let narrowed2 = I.narrow ik widened imin in
    assert_equal i_minus_one_zero narrowed;
    assert_equal widened narrowed2

  let test_interval_logand _ =
    assert_equal (I.of_interval ik (Z.zero, Z.of_int 4)) (I.logand ik (I.of_interval ik (Z.of_int (-4), Z.of_int (-2))) (I.of_int ik (Z.of_int 4)))

  let test () = [
    "test_interval_rem" >:: test_interval_rem;
    "test_interval_widen" >:: test_interval_widen;
    "test_interval_narrow" >:: test_interval_narrow;
    "test_interval_logand" >:: test_interval_logand;
  ]
end

module BitfieldTest (I : IntDomain.SOverflow with type int_t = Z.t) =
struct
  module I_ = I
  module I = IntDomain.SOverflowUnlifter (I)

  let ik = Cil.IInt
  let ik_lst = [Cil.IChar; Cil.IUChar; Cil.IShort; Cil.IUShort; ik; Cil.IUInt;]

  let assert_equal x y =
    OUnit.assert_equal ~printer:I.show x y

  let test_of_int_to_int _ =
    let b1 = I.of_int ik (of_int 17) in
    OUnit.assert_equal 17 (I.to_int b1 |> Option.get |> to_int)

  let test_to_int_of_int _ =
    OUnit.assert_equal None (I.to_int (I.bot_of ik));
    OUnit.assert_equal (of_int 13) (I.to_int (I.of_int ik (of_int 13)) |> Option.get);
    OUnit.assert_equal None (I.to_int (I.top_of ik));
    OUnit.assert_equal None (I.to_int (I.join ik (I.of_int ik (of_int 13)) (I.of_int ik (of_int 14))))

  let test_equal_to _ =
    let b1 = I.join ik (I.of_int ik (of_int 4)) (I.of_int ik (of_int 2)) in
    OUnit.assert_equal `Top (I.equal_to (Z.of_int 4) b1);
    OUnit.assert_equal `Top (I.equal_to (Z.of_int 2) b1);

    OUnit.assert_equal `Top (I.equal_to (Z.of_int 0) b1);
    OUnit.assert_equal `Top (I.equal_to (Z.of_int 6) b1);

    OUnit.assert_equal `Neq (I.equal_to (Z.of_int 1) b1);
    OUnit.assert_equal `Neq (I.equal_to (Z.of_int 3) b1);
    OUnit.assert_equal `Neq (I.equal_to (Z.of_int 5) b1);

    let b2 =I.of_int ik (of_int 123) in
    OUnit.assert_equal `Eq (I.equal_to (Z.of_int 123) b2)

  let test_join _ =
    let b1 = I.of_int ik (of_int 9) in
    let b2 = I.of_int ik (of_int 2) in
    let bjoin = I.join ik b1 b2 in

    assert_bool "num1 leq join" (I.leq b1 bjoin);
    assert_bool "num2 leq join" (I.leq b2 bjoin);

    OUnit.assert_equal `Top (I.equal_to (Z.of_int 9) bjoin);
    OUnit.assert_equal `Top (I.equal_to (Z.of_int 2) bjoin);
    OUnit.assert_equal `Top (I.equal_to (Z.of_int 11) bjoin)

  let test_meet _ =
    let b1 = I.of_int ik (of_int 5) in
    let b2 = I.of_int ik (of_int 3) in
    let bf12 = I.join ik b1 b2 in

    let b3 = I.of_int ik (of_int 7) in
    let b4 = I.of_int ik (of_int 4) in
    let bf34 = I.join ik b3 b4 in

    let bmeet2 = I.meet ik bf12 bf34 in

    OUnit.assert_equal `Top (I.equal_to (Z.of_int 5) bmeet2);
    OUnit.assert_equal `Top (I.equal_to (Z.of_int 7) bmeet2)

  let test_leq_1 _ =
    let b1 = I.of_int ik (of_int 13) in
    let b2 = I.of_int ik (of_int 5) in

    let bjoin = I.join ik b1 b2 in

    OUnit.assert_bool "13 leq 13" (I.leq b1 b1);
    OUnit.assert_bool "5 leq 5" (I.leq b2 b2);

    OUnit.assert_bool "5 leq 13" (I.leq b2 bjoin);
    OUnit.assert_bool "not 13 leq 5" (not (I.leq bjoin b2))

  let test_leq_2 _ =
    let b1 = I.of_int ik (of_int 7) in

    OUnit.assert_bool "bot leq 7" (I.leq (I.bot_of ik) b1);
    OUnit.assert_bool "7 leq top" (I.leq b1 (I.top_of ik))

  let test_wrap_1 _ =
    let z = of_int 31376 in
    let b_uint8 = I.of_int IUChar z in
    let b_sint8 = I.of_int IUChar z in
    let b_uint16 = I.of_int IUShort z in
    let b_sint16 = I.of_int IUShort z in

    (* See https://www.simonv.fr/TypesConvert/?integers *)
    assert_equal (I.of_int IUChar (of_int 144)) b_uint8;
    assert_equal (I.of_int IUChar (of_int (-112))) b_sint8;
    assert_equal (I.of_int IUShort (of_int 31376)) b_uint16;
    assert_equal (I.of_int IUShort (of_int 31376)) b_sint16

  let test_wrap_2 _ =
    let z1 = of_int 30867 in
    let z2 = of_int 30870 in
    let join_cast_unsigned = I.join IUChar (I.of_int IUChar z1) (I.of_int IUChar z2) in

    let expected_unsigned = I.join IUChar (I.of_int IUChar (of_int 147)) (I.of_int IUChar (of_int 150)) in

    let expected_signed = I.join IUChar (I.of_int IUChar (of_int (-106))) (I.of_int IUChar (of_int (-109))) in

    assert_equal expected_unsigned join_cast_unsigned;
    assert_equal expected_signed join_cast_unsigned

  let test_widen_1 _ =
    let b1 = I.of_int ik (of_int 3) in
    let b2 = I.of_int ik (of_int 17) in

    (* widen both masks *)
    assert_equal (I.top_of ik) (I.widen ik b1 b2);

    (* no widening *)
    let bjoin = I.join ik b1 b2 in
    assert_equal bjoin (I.widen ik bjoin b1)


  let test_widen_2 _ =
    let b1 = I.of_int ik (of_int 123613) in
    let b2 = I.of_int ik (of_int 613261) in

    (* no widening needed *)
    assert_bool "join leq widen" (I.leq (I.join ik b1 b2) (I.widen ik b1 b2))

  let assert_of_interval lb ub =
    let intvl = (of_int lb, of_int ub) in
    let bf = I.of_interval ik intvl in
    let print_err_message i = "Missing value: " ^ string_of_int i ^ " in [" ^ string_of_int lb ^ ", " ^ string_of_int ub ^ "]" in
    for i = lb to ub do
      assert_bool (print_err_message i) (I.equal_to (of_int i) bf = `Top)
    done

  let test_of_interval _ =
    assert_of_interval 3 17;
    assert_of_interval (-17) (-3);
    assert_of_interval (-3) 17;
    assert_of_interval (-17) 3

  let test_of_bool _ =
    let b1 = I.of_bool ik true in
    let b2 = I.of_bool ik false in

    assert_bool "true" (I.equal_to (of_int 1) b1 = `Eq);
    assert_bool "false" (I.equal_to (of_int 0) b2 = `Eq)

  let test_to_bool _ =
    let ik = IUInt in
    let b1 = I.of_int ik (of_int 3) in
    let b2 = I.of_int ik (of_int (-6)) in
    let b3 = I.of_int ik (of_int 0) in

    let b12 = I.join ik b1 b2 in
    let b13 = I.join ik b1 b3 in
    let b23 = I.join ik b2 b3 in

    assert_bool "3" (I.to_bool b1 = Some true);
    assert_bool "-6" (I.to_bool b2 = Some true);
    assert_bool "0" (I.to_bool b3 = Some false);

    assert_bool "3 | -6" (I.to_bool b12 = Some true);
    assert_bool "3 | 0" (I.to_bool b13 = None);
    assert_bool "-6 | 0" (I.to_bool b23 = None)

  let test_cast_to _ =
    let b1 = I.of_int ik (of_int 1234) in

    assert_equal (I.of_int IUChar (of_int (210))) (I.cast_to IUChar b1);
    assert_equal (I.of_int IUChar (of_int (-46))) (I.cast_to IUChar b1);

    assert_equal (I.of_int IUInt128 (of_int 1234)) (I.cast_to IUInt128 b1)

  (* Bitwise  *)

  let test_logxor _ =
    let b1 = I.of_int ik (of_int 5) in
    let b2 = I.of_int ik (of_int 17) in

    assert_equal (I.of_int ik (of_int 20)) (I.logxor ik b1 b2);

    let b12 = I.join ik b1 b2 in
    let b3 = I.of_int ik (of_int 13) in
    assert_bool "8 ?= 13 xor (5 | 17)" (I.equal_to (of_int 8) (I.logxor ik b12 b3) = `Top);
    assert_bool "28 ?= 13 xor (5 | 17)" (I.equal_to (of_int 28) (I.logxor ik b12 b3) = `Top)

  let test_logand _ =
    let b1 = I.of_int ik (of_int 7) in
    let b2 = I.of_int ik (of_int 13) in

    assert_equal (I.of_int ik (of_int 5)) (I.logand ik b1 b2);

    let b12 = I.join ik b1 b2 in
    let b3 = I.of_int ik (of_int 12) in
    assert_bool "4 ?= 12 and (7 | 12)" (I.equal_to (of_int 4) (I.logand ik b12 b3) = `Top);
    assert_bool "12 ?= 12 and (7 | 12)" (I.equal_to (of_int 12) (I.logand ik b12 b3) = `Top)


  let test_logor _ =
    let b1 = I.of_int ik (of_int 5) in
    let b2 = I.of_int ik (of_int 17) in

    assert_equal (I.of_int ik (of_int 21)) (I.logor ik b1 b2);

    let b12 = I.join ik b1 b2 in
    let b3 = I.of_int ik (of_int 13) in
    assert_bool "13 ?= 13 or (5 | 17)" (I.equal_to (of_int 13) (I.logor ik b12 b3) = `Top);
    assert_bool "29 ?= 13 or (5 | 17)" (I.equal_to (of_int 29) (I.logor ik b12 b3) = `Top)

  let test_lognot _ =
    let b1 = I.of_int ik (of_int 4) in
    let b2 = I.of_int ik (of_int 12) in

    (* assumes two's complement *)
    assert_equal (I.of_int ik (of_int (-5))) (I.lognot ik b1);

    let b12= I.join ik b1 b2 in
    assert_bool "-13 ?= not (4 | 12)" (I.equal_to (of_int (-13)) (I.lognot ik b12) = `Top);
    assert_bool "-5 ?= not (4 | 12)" (I.equal_to (of_int (-5)) (I.lognot ik b12) = `Top)

  let of_list ik is = List.fold_left (fun acc x -> I.join ik acc (I.of_int ik x)) (I.bot ()) is

  let precision ik = snd @@ IntDomain.Size.bits ik
  let over_precision ik = Int.succ @@ precision ik
  let under_precision ik = Int.pred @@ precision ik

  let assert_shift ?(rev_cond=false) ?(expected_ov_info=None) shift ik a b expected =
    let module I = I_ in
    let symb, shift_op_bf, shift_op_int = match shift with
      | `L -> "<<", I.shift_left ik, Int.shift_left
      | `R -> ">>", I.shift_right ik, Int.shift_right
    in
    let of_list (is: int list) : I.t = of_list ik (List.map of_int is) in
    let get_param x : I.t = match x with
      | `B bf -> bf
      | `I is -> of_list is
    in
    let string_of_param x = match x with
      | `B bf -> I.show bf
      | `I is -> Printf.sprintf "[%s]" (String.concat ", " @@ List.map string_of_int is)
    in
    let bf_a, bf_b, expected = get_param a, get_param b, get_param expected in
    let result, ov_info = (shift_op_bf bf_a bf_b) in
    let output_string = Printf.sprintf "test (%s) shift %s %s %s failed: was: %s but should%s be: %s"
        (CilType.Ikind.show ik)
        (string_of_param a) symb (string_of_param b)
        (I.show result) (if rev_cond then " not" else "") (I.show expected)
    in
    let assertion = I.equal result expected in
    let assertion = if rev_cond then not assertion else assertion in
    assert_bool output_string assertion;
    if Option.is_some expected_ov_info then
      let ov_printer (ov_info : IntDomain.overflow_info) = Printf.sprintf "{underflow=%b; overflow=%b}" ov_info.underflow ov_info.overflow in
      let err_msg = Printf.sprintf "In (%s) shift %s %s %s" (CilType.Ikind.show ik) (string_of_param a) symb (string_of_param b) in
      OUnit.assert_equal ~msg:err_msg ~printer:ov_printer (Option.get expected_ov_info) ov_info


  let assert_shift_left ?(rev_cond=false) ?(ov_info=None) = assert_shift ~rev_cond:rev_cond ~expected_ov_info:ov_info `L
  let assert_shift_right ?(rev_cond=false) ?(ov_info=None) = assert_shift ~rev_cond:rev_cond ~expected_ov_info:ov_info `R

  let gen_sized_set size_gen gen =
    let open QCheck2.Gen in
    map (List.sort_uniq Int.compare) (list_size size_gen gen)

  let test_shift ik name c_op a_op =
    let shift_test_printer (a,b) = Printf.sprintf "a: [%s] b: [%s]"
        (String.concat ", " (List.map string_of_int a))
        (String.concat ", " (List.map string_of_int b))
    in
    let of_list ik is = of_list ik (List.map of_int is) in
    let open QCheck2 in let open Gen in
    let a_gen ik =
      let min_ik, max_ik = Batteries.Tuple2.mapn Z.to_int (IntDomain.Size.range ik) in
      gen_sized_set (1 -- precision ik) (min_ik -- max_ik)
    in
    let b_gen ik =
      gen_sized_set (1 -- (Z.log2up @@ Z.of_int @@ precision ik)) (0 -- under_precision ik) (* only shifts that are smaller than precision *)
    in
    let test_case_gen = Gen.pair (a_gen ik) (b_gen ik)
    in
    Test.make ~name:name ~print:shift_test_printer
      test_case_gen
      (fun (a,b) ->
         let expected_subset = GobList.cartesian_map c_op a b |> of_list ik in
         let result = a_op ik (of_list ik a) (of_list ik b) in
         I.leq expected_subset result
      )

  let test_shift_left = List.fold_left (fun acc ik -> test_shift ik
                                           (Printf.sprintf "test_shift_left_ik_%s" (CilType.Ikind.show ik)) Int.shift_left I.shift_left :: acc
                                       ) [] ik_lst |> QCheck_ounit.to_ounit2_test_list

  let test_shift_right = List.fold_left (fun acc ik -> test_shift ik
                                            (Printf.sprintf "test_shift_right_ik_%s" (CilType.Ikind.show ik)) Int.shift_right I.shift_right :: acc
                                        ) [] ik_lst |> QCheck_ounit.to_ounit2_test_list

  let bot = `B (I.bot_of ik)
  let top = `B (I.top_of ik)

  let isSigned = GoblintCil.Cil.isSigned

  let max_of ik = Z.to_int @@ snd @@ IntDomain.Size.range ik
  let min_of ik = Z.to_int @@ fst @@ IntDomain.Size.range ik

  let ov_overflow : IntDomain.overflow_info option = Some ({underflow=false; overflow=true})
  let ov_underflow : IntDomain.overflow_info option = Some ({underflow=true; overflow=false})
  let no_ov : IntDomain.overflow_info option = Some ({underflow=false; overflow=false})

  let one ik = I.of_int ik @@ Z.of_int 1

  let test_shift_left =
    let highest_bit_set ?(is_neg=false) ik =
      let pos = Int.pred @@ snd @@ IntDomain.Size.bits ik in
      (if isSigned ik && is_neg
       then Z.neg @@ Z.shift_left Z.one pos
       else Z.shift_left Z.one pos
      ) |> Z.to_int
    in
    [
      "property_test_shift_left" >::: test_shift_left;
      "shift_left_edge_cases" >:: fun _ ->
        assert_shift_left ik (`I [1]) (`I [1; 2]) (`I [1; 2; 4; 8]);
        assert_shift_left ~ov_info:ov_underflow ik (`I [-1000]) (`I [64]) top;

        List.iter (fun ik ->
            assert_shift_left ik bot bot bot;

            assert_shift_left ik (`I [0]) top (`I [0]);

            if isSigned ik
            then (
              assert_shift_left ik (`I [1]) (`I [-1]) top; (* Negative shifts are undefined behavior *)
              assert_shift_left ik (`I [-1]) top top;
              assert_shift_left ik top (`I [-1]) top;

              assert_shift_left ~ov_info:no_ov ik (`I [1]) (`I [under_precision ik]) (`I [highest_bit_set ik]);
              assert_shift_left ~ov_info:ov_overflow ik (`I [1]) (`I [precision ik]) top;
              assert_shift_left ~ov_info:ov_overflow ik (`I [1]) (`I [over_precision ik]) top;

              assert_shift_left ~ov_info:no_ov ik (`I [-1]) (`I [under_precision ik]) (`I [highest_bit_set ~is_neg:true ik]);
              assert_shift_left ~ov_info:no_ov ik (`I [-1]) (`I [precision ik]) (`I [Z.to_int @@ IntDomain.Size.cast ik @@ Z.shift_left Z.one (precision ik)]);
              assert_shift_left ~ov_info:ov_underflow ik (`I [-1]) (`I [over_precision ik]) top;
            ) else (
              (* See C11 N2310 at 6.5.7 *)
              assert_shift_left ~ov_info:no_ov ik (`I [1]) (`I [under_precision ik]) (`I [highest_bit_set ik]);
              assert_shift_left ~ov_info:ov_overflow ik (`I [1]) (`I [precision ik]) (`I [0]);
              assert_shift_left ~ov_info:ov_overflow ik (`I [1]) (`I [over_precision ik]) (`I [0]);
            )

          ) ik_lst
    ]

  let test_shift_right =
    [
      "property_test_shift_right" >::: test_shift_right;
      "shift_right_edge_cases" >:: fun _ ->
        assert_shift_right ik (`I [10]) (`I [1; 2]) (`I [10; 7; 5; 1]);

        List.iter (fun ik ->
            assert_shift_right ik bot bot bot;

            assert_shift_right ik (`I [0]) top (`I [0]);

            if isSigned ik
            then (
              assert_shift_right ~rev_cond:true ik (`I [max_of ik]) top top; (* the sign bit shouldn't be set with right shifts if its unset *)

              assert_shift_right ik (`I [2]) (`I [-1]) top; (* Negative shifts are undefined behavior *)
              assert_shift_right ik (`I [min_of ik]) top top; (* implementation-defined sign-bit handling *)

              assert_shift_right ~ov_info:no_ov ik (`I [max_of ik]) (`I [under_precision ik]) (`I [1]);
              assert_shift_right ~ov_info:no_ov ik (`I [max_of ik]) (`I [precision ik]) (`I [0]);
              assert_shift_right ~ov_info:no_ov ik (`I [max_of ik]) (`I [over_precision ik]) (`I [0]);

              assert_shift_right ~ov_info:no_ov ik ~rev_cond:true (`I [min_of ik]) (`I [under_precision ik]) top;
              assert_shift_right ~ov_info:no_ov ik ~rev_cond:true (`I [min_of ik]) (`I [precision ik]) top;
              assert_shift_right ~ov_info:no_ov ik (`I [min_of ik]) (`I [over_precision ik]) top;
            ) else (
              (* See C11 N2310 at 6.5.7 *)
              assert_shift_right ~ov_info:no_ov ik (`I [max_of ik]) (`I [under_precision ik]) (`I [1]);
              assert_shift_right ~ov_info:no_ov ik (`I [max_of ik]) (`I [precision ik]) (`I [0]);
              assert_shift_right ~ov_info:no_ov ik (`I [max_of ik]) (`I [over_precision ik]) (`I [0]);
            )

          ) ik_lst

    ]


  (* Arith *)

  let print_err_message bf1 bf2 bfr =
    I.show bfr ^ " on input " ^ I.show bf1 ^ " and " ^ I.show bf2

  let ik_arithu = Cil.IUChar

  let ik_ariths = Cil.IChar

  let result_list op is1 is2 = List.concat (List.map (fun x -> List.map (op x) is2) is1)

  let generate_test ?(debug=false) opc opa ik is1 is2 =
    let zs1 = List.map Z.of_int is1 in
    let zs2 = List.map Z.of_int is2 in
    let res = of_list ik (result_list opc zs1 zs2) in
    let bs1 = of_list ik zs1 in
    let bs2 = of_list ik zs2 in
    let bsr = opa ik bs1 bs2 in
    OUnit2.assert_equal ~cmp:I.leq ~printer:(print_err_message bs1 bs2) res bsr

  let c1 = [99]
  let c2 = [186]
  let c3 = [-64]
  let c4 = [-104]

  let is1 = [8; 45; 89; 128]
  let is2 = [5; 69; 72; 192]
  let is3 = [-11; -42; -99; -120]
  let is4 = [-16; -64; -87; -111]
  let is5 = [-64; -14; 22; 86]

  let testsuite = [c1;c2;c3;c4;is1;is2;is3;is4]
  let testsuite_unsigned = [c1;c2;is1;is2]

  let arith_testsuite ?(debug=false) opc opa ts ik =
    List.iter (fun x -> List.iter (generate_test opc opa ik x) ts) ts

  let test_add _ =
    let _ = arith_testsuite Z.add I.add testsuite ik_arithu in
    let _ = arith_testsuite Z.add I.add testsuite ik_ariths in
    ()

  let test_sub _ =
    let _ = arith_testsuite Z.sub I.sub testsuite ik_arithu in
    let _ = arith_testsuite Z.sub I.sub testsuite ik_ariths in
    ()

  let test_mul _ =
    let _ = arith_testsuite Z.mul I.mul testsuite ik_arithu in
    let _ = arith_testsuite Z.mul I.mul testsuite ik_ariths in
    ()

  let test_div _ =
    let _ = arith_testsuite Z.div I.div testsuite_unsigned ik_arithu in
    let _ = arith_testsuite Z.div I.div testsuite IShort in
    ()

  let test_rem _ =
    let _ = arith_testsuite Z.rem I.rem testsuite_unsigned ik_arithu in
    let _ = arith_testsuite Z.rem I.rem testsuite IShort in
    ()

  let test_neg _ =
    let print_neg_err_message bfi bfr =
      I.show bfr ^ " on input " ^ I.show bfi
    in
    let generate_test_neg opc opa ik is =
      let zs = List.map Z.of_int is in
      let res = of_list ik (List.map opc zs) in
      let bs = of_list ik zs in
      OUnit2.assert_equal ~cmp:I.leq ~printer:(print_neg_err_message bs) res (opa ik bs)
    in
    let neg_testsuite opc opa ik =
      let testsuite = [c1;c2;c3;c4;is1;is2;is3;is4] in
      List.map (generate_test_neg opc opa ik) testsuite
    in
    let _ = neg_testsuite Z.neg I.neg ik_arithu in
    let _ = neg_testsuite Z.neg I.neg ik_ariths in
    ()

  (* Comparisons *)

  let test_eq _ =
    let b1 = I.of_int ik (of_int 5) in
    let b2 = I.of_int ik (of_int 17) in

    assert_bool "5 == 5" (I.eq ik b1 b1 = I.of_bool ik true);
    assert_bool "5 == 17" (I.eq ik b1 b2 = I.of_bool ik false);

    let b12 = I.join ik b1 b2 in
    assert_bool "5 == (5 | 17)" (I.eq ik b1 b12 = (I.join ik (I.of_bool ik true) (I.of_bool ik false)))

  let test_ne _ =
    let b1 = I.of_int ik (of_int 5) in
    let b2 = I.of_int ik (of_int 17) in

    assert_bool "5 != 5" (I.ne ik b1 b1 = I.of_bool ik false);
    assert_bool "5 != 17" (I.ne ik b1 b2 = I.of_bool ik true);

    let b12 = I.join ik b1 b2 in
    assert_bool "5 != (5 | 17)" (I.ne ik b1 b12 = (I.join ik (I.of_bool ik false) (I.of_bool ik true)))

  let test_le _ =
    let b1 = I.of_int ik (of_int 5) in
    let b2 = I.of_int ik (of_int 14) in

    assert_bool "5 <= 5" (I.le ik b1 b1 = I.of_bool ik true);
    assert_bool "5 <= 14" (I.le ik b1 b2 = I.of_bool ik true);
    assert_bool "14 <= 5" (I.le ik b2 b1 = I.of_bool ik false);

    let b12 = I.join ik b1 b2 in

    let b3 = I.of_int ik (of_int 17) in
    assert_bool "17 <= (5 | 14)" (I.le ik b3 b12 = I.of_bool ik false);

    let b4 = I.of_int ik (of_int 13) in
    assert_bool "13 <= (5 | 14)" (I.le ik b4 b12 = (I.join ik (I.of_bool ik false) (I.of_bool ik true)));

    let b5 = I.of_int ik (of_int 5) in
    assert_bool "5 <= (5 | 14)" (I.le ik b5 b12 = I.join ik (I.of_bool ik true) (I.of_bool ik false));

    let b6 = I.of_int ik (of_int 4) in
    assert_bool "4 <= (5 | 14)" (I.le ik b6 b12 = I.of_bool ik true)


  let test_ge _ =
    let b1 = I.of_int ik (of_int 5) in
    let b2 = I.of_int ik (of_int 14) in

    assert_bool "5 >= 5" (I.ge ik b1 b1 = I.of_bool ik true);
    assert_bool "5 >= 14" (I.ge ik b1 b2 = I.of_bool ik false);
    assert_bool "14 >= 5" (I.ge ik b2 b1 = I.of_bool ik true);

    let b12 = I.join ik b1 b2 in

    let b3 = I.of_int ik (of_int 2) in
    assert_bool "2 >= (5 | 14)" (I.ge ik b3 b12 = I.of_bool ik false);

    let b4 = I.of_int ik (of_int 13) in
    assert_bool "13 >= (5 | 14)" (I.ge ik b4 b12 = (I.join ik (I.of_bool ik true) (I.of_bool ik false)));

    let b6 = I.of_int ik (of_int 15) in
    assert_bool "15 >= (5 | 14)" (I.ge ik b6 b12 = I.of_bool ik true)

  let test_lt _ =
    let b1 = I.of_int ik (of_int 7) in
    let b2 = I.of_int ik (of_int 13) in

    assert_bool "7 < 7" (I.lt ik b1 b1 = I.of_bool ik false);
    assert_bool "7 < 13" (I.lt ik b1 b2 = I.of_bool ik true);

    let b12 = I.join ik b1 b2 in
    let b3 = I.of_int ik (of_int 4) in
    assert_bool "4 < (7 | 13)" (I.lt ik b3 b12 = I.of_bool ik true);

    let b4 = I.of_int ik (of_int 8) in
    assert_bool "8 < (7 | 13)" (I.lt ik b4 b12 = I.join ik (I.of_bool ik false) (I.of_bool ik true))

  let test_gt _ =
    let b1 = I.of_int ik (of_int 5) in
    let b2 = I.of_int ik (of_int 14) in

    assert_bool "5 > 5" (I.gt ik b1 b1 = I.of_bool ik false);
    assert_bool "5 > 14" (I.gt ik b1 b2 = I.of_bool ik false);
    assert_bool "14 > 5" (I.gt ik b2 b1 = I.of_bool ik true);

    let b12 = I.join ik b1 b2 in

    let b3 = I.of_int ik (of_int 2) in
    assert_bool "2 > (5 | 14)" (I.gt ik b3 b12 = I.of_bool ik false);

    let b4 = I.of_int ik (of_int 13) in
    assert_bool "13 > (5 | 14)" (I.gt ik b4 b12 = (I.join ik (I.of_bool ik false) (I.of_bool ik true)));

    let b5 = I.of_int ik (of_int 5) in
    assert_bool "5 > (5 | 14)" (I.gt ik b5 b12 = I.join ik (I.of_bool ik false) (I.of_bool ik true));

    let b6 = I.of_int ik (of_int 4) in
    assert_bool "4 > (5 | 14)" (I.gt ik b6 b12 = (I.of_bool ik false) )

  let test_starting _ =
    let bf1 = I.starting ik (of_int 17) in

    assert_bool "17" (I.equal_to (of_int 17) bf1 = `Top);
    assert_bool "18" (I.equal_to (of_int 18) bf1 = `Top);

    assert_bool "-3" (I.equal_to (of_int (-3)) bf1 = `Neq);

    let bf2 = I.starting ik (of_int (-17)) in

    assert_bool "-16" (I.equal_to (of_int (-16)) bf2 = `Top);
    assert_bool "-17" (I.equal_to (of_int (-17)) bf2 = `Top)


  let test_ending _ =
    let bf = I.ending ik (of_int 17) in

    assert_bool "-4" (I.equal_to (of_int (-4)) bf = `Top);
    assert_bool "16" (I.equal_to (of_int 16) bf = `Top);

    let bf2 = I.ending ik (of_int (-17)) in

    assert_bool "-16" (I.equal_to (of_int (-16)) bf2 = `Top);
    assert_bool "-18" (I.equal_to (of_int (-18)) bf2 = `Top);

    assert_bool "17" (I.equal_to (of_int 17) bf2 = `Neq)

  let test_refine_with_congruence _ =
    let bf = I.top_of ik in

    let bf_refined1= I.refine_with_congruence ik bf (Some (Z.of_int 3, Z.of_int 4)) in
    assert_bool "3" (I.equal_to (of_int 3) bf_refined1 = `Top);
    let bf_refined3= I.refine_with_congruence ik bf (Some (Z.of_int 5, Z.of_int 0)) in
    assert_bool "5" (I.equal_to (of_int 5) bf_refined3 = `Eq)

  let test_refine_with_inclusion_list _ =
    let bf = I.top_of ik in

    let list = List.map of_int [-2;3;23; 26] in
    let bf_refined = I.refine_with_incl_list ik bf (Some list) in

    List.iter (fun i -> assert_bool (Z.to_string i) (I.equal_to i bf_refined = `Top)) list

  (*
  let test_refine_with_exclusion_list _ = failwith "TODO"
  *)

  let test () =[
    "test_of_int_to_int" >:: test_of_int_to_int;
    "test_to_int_of_int" >:: test_to_int_of_int;
    "test_equal_to" >:: test_equal_to;

    "test_join" >:: test_join;
    "test_meet" >:: test_meet;

    "test_leq_1" >:: test_leq_1;
    "test_leq_2" >:: test_leq_2;

    "test_wrap_1" >:: test_wrap_1;
    "test_wrap_2" >:: test_wrap_2;

    "test_widen_1" >:: test_widen_1;
    "test_widen_2" >:: test_widen_2;


    "test_of_interval" >:: test_of_interval;
    "test_of_bool" >:: test_of_bool;
    "test_to_bool" >:: test_to_bool;
    "test_cast_to" >:: test_cast_to;

    "test_logxor" >:: test_logxor;
    "test_logand" >:: test_logand;
    "test_logor" >:: test_logor;
    "test_lognot" >:: test_lognot;

    "test_shift_left" >::: test_shift_left;
    "test_shift_right" >::: test_shift_right;

    "test_add" >:: test_add;
    "test_sub" >:: test_sub;
    "test_mul" >:: test_mul;
    "test_div" >:: test_div;
    "test_rem" >:: test_rem;


    "test_eq" >:: test_eq;
    "test_ne" >:: test_ne;
    "test_le" >:: test_le;
    "test_ge" >:: test_ge;
    "test_lt" >:: test_lt;
    "test_gt" >:: test_gt;

    "test_starting" >:: test_starting;
    "test_ending" >:: test_ending;

    "test_refine_with_congruence" >:: test_refine_with_congruence;
    "test_refine_with_inclusion_list" >:: test_refine_with_inclusion_list;
  ]

end

module Interval    = IntervalTest (IntDomain.Interval)
module Bitfield    = BitfieldTest (IntDomain.Bitfield)
module IntervalSet = IntervalTest (IntDomain.IntervalSet)

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
    let to_list (a,b,c,d) = List.filter_map identity [a;b;c;d] in
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
    let to_list (a,b,c,d) = List.filter_map identity [a;b;c;d] in
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


let test () =
  "intDomainTest" >::: [
    "int_Integers"  >::: A.test ();
    "int_Flattened" >::: B.test ();
    "int_DefExc"     >::: C.test ();
    "test_bot"      >::  D.test_bot;
    "test_join"     >::  D.test_join;
    "test_meet"     >::  D.test_meet;
    "test_excl_list">::  D.test_ex_set;
    "interval" >::: Interval.test ();
    "bitfield" >::: Bitfield.test ();
    "intervalSet" >::: IntervalSet.test ();
    "congruence" >::: Congruence.test ();
    "intDomTuple" >::: IntDomTuple.test ();
  ]
