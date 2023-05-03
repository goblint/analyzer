open Goblint_lib
open OUnit2
open GoblintCil

module ID = IntDomain.IntDomWithDefaultIkind (IntDomain.IntDomLifter (IntDomain.DefExc)) (IntDomain.PtrDiffIkind)
module LV = Lval.NormalLat (ID)

let ikind = IntDomain.PtrDiffIkind.ikind ()

let a_var = Cil.makeGlobalVar "a" Cil.intPtrType
let a_lv = LV.from_var ~is_modular:false a_var
let i_0 = ID.of_int ikind Z.zero
let a_lv_0 = LV.from_var_offset ~is_modular:false (a_var, `Index (i_0, `NoOffset))
let i_1 = ID.of_int ikind Z.one
let a_lv_1 = LV.from_var_offset ~is_modular:false (a_var, `Index (i_1, `NoOffset))
let i_top = ID.join i_0 i_1
let a_lv_top = LV.from_var_offset ~is_modular:false (a_var, `Index (i_top, `NoOffset))
let i_not_0 = ID.join i_1 (ID.of_int ikind (Z.of_int 2))
let a_lv_not_0 = LV.from_var_offset ~is_modular:false (a_var, `Index (i_not_0, `NoOffset))


let assert_leq x y =
  assert_bool (Format.sprintf "expected: %s leq: %s" (LV.show x) (LV.show y)) (LV.leq x y)
let assert_not_leq x y =
  assert_bool (Format.sprintf "expected: %s not leq: %s" (LV.show x) (LV.show y)) (not (LV.leq x y))

let assert_equal x y =
  assert_equal ~cmp:LV.equal ~printer:LV.show x y

let test_join_0 _ =
  assert_equal a_lv_top (LV.join a_lv_0 a_lv_1)

let test_leq_not_0 _ =
  assert_leq a_lv_1 a_lv_not_0;
  OUnit.assert_equal ~printer:[%show: [`Eq | `Neq | `Top]] `Neq (ID.equal_to Z.zero i_not_0);
  OUnit.assert_equal ~printer:[%show: [`MustZero | `MustNonzero | `MayZero]] `MustNonzero (LV.Offs.cmp_zero_offset (`Index (i_not_0, `NoOffset)));
  assert_not_leq a_lv a_lv_not_0;
  assert_not_leq a_lv_0 a_lv_not_0

let test () =
  "lvalTest" >::: [
    "test_join_0" >:: test_join_0;
    "test_leq_not_0" >:: test_leq_not_0;
  ]
