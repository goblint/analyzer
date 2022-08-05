open OUnit2

module ID = IntDomain.IntDomWithDefaultIkind (IntDomain.IntDomLifter (IntDomain.DefExc)) (IntDomain.PtrDiffIkind)
module LV = Lval.NormalLat (ID)

let ikind = IntDomain.PtrDiffIkind.ikind ()

let a_var = Cil.makeGlobalVar "a" Cil.intPtrType
let a_lv = LV.from_var a_var
let i_0 = ID.of_int ikind (Z.of_int 0)
let a_lv_0 = LV.from_var_offset (a_var, `Index (i_0, `NoOffset))
let i_1 = ID.of_int ikind (Z.of_int 1)
let a_lv_1 = LV.from_var_offset (a_var, `Index (i_1, `NoOffset))
let i_top = ID.join i_0 i_1
let a_lv_top = LV.from_var_offset (a_var, `Index (i_top, `NoOffset))
let i_not_0 = ID.join i_1 (ID.of_int ikind (Z.of_int 2))
let a_lv_not_0 = LV.from_var_offset (a_var, `Index (i_not_0, `NoOffset))

let s = Cil.mkCompInfo true "s" (fun self ->
    [
      ("fst", Cil.intType, None, [], Cil.locUnknown);
      ("snd", Cil.intType, None, [], Cil.locUnknown);
    ]
  ) []
let f_fst = List.nth s.cfields 0
let f_snd = List.nth s.cfields 1
let a_lv_fst = LV.from_var_offset (a_var, `Field (f_fst, `NoOffset))
let a_lv_snd = LV.from_var_offset (a_var, `Field (f_snd, `NoOffset))


let assert_leq x y =
  assert_bool (Format.sprintf "expected: %s leq: %s" (LV.show x) (LV.show y)) (LV.leq x y)
let assert_not_leq x y =
  assert_bool (Format.sprintf "expected: %s not leq: %s" (LV.show x) (LV.show y)) (not (LV.leq x y))

let assert_equal x y =
  assert_equal ~cmp:LV.equal ~printer:LV.show x y


let test_equal_0 _ =
  assert_equal a_lv a_lv_0;
  assert_equal a_lv_0 a_lv;
  assert_equal a_lv a_lv_fst;
  assert_equal a_lv_fst a_lv;
  assert_equal a_lv_0 a_lv_fst;
  assert_equal a_lv_fst a_lv_0

let test_compare_0 _ =
  assert_bool "test_compare_0" @@ (LV.compare a_lv a_lv_0 = 0);
  assert_bool "test_compare_0" @@ (LV.compare a_lv_0 a_lv = 0);
  assert_bool "test_compare_0" @@ (LV.compare a_lv a_lv_fst = 0);
  assert_bool "test_compare_0" @@ (LV.compare a_lv_fst a_lv = 0);
  assert_bool "test_compare_0" @@ (LV.compare a_lv_0 a_lv_fst = 0);
  assert_bool "test_compare_0" @@ (LV.compare a_lv_fst a_lv_0 = 0)

let test_hash_0 _ =
  assert_bool "test_hash_0" @@ (LV.hash a_lv = LV.hash a_lv_0);
  assert_bool "test_hash_0" @@ (LV.hash a_lv = LV.hash a_lv_fst)

let test_leq_0 _ =
  assert_leq a_lv a_lv_0;
  assert_leq a_lv a_lv_top;
  assert_leq a_lv_0 a_lv;
  assert_leq a_lv_0 a_lv_top;
  assert_not_leq a_lv a_lv_1;
  assert_not_leq a_lv_1 a_lv;
  assert_not_leq a_lv_0 a_lv_1;
  assert_not_leq a_lv_1 a_lv_0;

  assert_leq a_lv a_lv_fst;
  assert_leq a_lv_0 a_lv_fst;
  assert_leq a_lv_fst a_lv;
  assert_leq a_lv_fst a_lv_0;
  assert_leq a_lv_fst a_lv_top;
  assert_not_leq a_lv_fst a_lv_1;
  assert_not_leq a_lv_1 a_lv_fst

let test_join_0 _ =
  assert_equal a_lv_0 (LV.join a_lv a_lv_0);
  assert_equal a_lv_0 (LV.join a_lv_0 a_lv);
  assert_equal a_lv_top (LV.join a_lv_0 a_lv_1);
  assert_equal a_lv_top (LV.join a_lv a_lv_1);

  assert_equal a_lv_fst (LV.join a_lv a_lv_fst);
  assert_equal a_lv_fst (LV.join a_lv_fst a_lv);
  assert_equal a_lv_fst (LV.join a_lv_0 a_lv_fst);
  assert_equal a_lv_fst (LV.join a_lv_fst a_lv_0);
  assert_equal a_lv_top (LV.join a_lv_fst a_lv_1);

  assert_equal a_lv_top (LV.join a_lv a_lv_not_0)

let test_meet_0 _ =
  assert_equal a_lv (LV.meet a_lv a_lv_0);
  assert_equal a_lv (LV.meet a_lv_0 a_lv);
  assert_equal a_lv_0 (LV.meet a_lv_0 a_lv_top);
  assert_equal a_lv_0 (LV.meet a_lv_top a_lv_0);
  assert_equal a_lv (LV.meet a_lv a_lv_top);
  assert_equal a_lv (LV.meet a_lv_top a_lv);

  assert_equal a_lv_fst (LV.meet a_lv a_lv_fst);
  assert_equal a_lv_fst (LV.meet a_lv_fst a_lv);
  assert_equal a_lv_fst (LV.meet a_lv_0 a_lv_fst);
  assert_equal a_lv_fst (LV.meet a_lv_fst a_lv_0);
  assert_equal a_lv_fst (LV.meet a_lv_fst a_lv_top);
  assert_equal a_lv_fst (LV.meet a_lv_top a_lv_fst)

let test_leq_not_0 _ =
  assert_leq a_lv_1 a_lv_not_0;
  OUnit.assert_equal ~printer:[%show: [`Eq | `Neq | `Top]] `Neq (ID.equal_to (Z.of_int 0) i_not_0);
  OUnit.assert_equal ~printer:[%show: [`MustZero | `MustNonzero | `MayZero]] `MustNonzero (LV.Offs.cmp_zero_offset (`Index (i_not_0, `NoOffset)));
  assert_not_leq a_lv a_lv_not_0;
  assert_not_leq a_lv_0 a_lv_not_0

let test () =
  "lvalTest" >::: [
    "test_equal_0" >:: test_equal_0;
    "test_compare_0" >:: test_compare_0;
    "test_hash_0" >:: test_hash_0;
    "test_join_0" >:: test_join_0;
    "test_meet_0" >:: test_meet_0;
    "test_leq_0" >:: test_leq_0;
    "test_leq_not_0" >:: test_leq_not_0;
  ]
