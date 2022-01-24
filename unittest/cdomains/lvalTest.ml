open OUnit

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

let test_equal_0 () =
  assert_bool "test_equal_0" @@ LV.equal a_lv a_lv_0

let test_compare_0 () =
  assert_bool "test_compare_0" @@ (LV.compare a_lv a_lv_0 = 0) (* TODO *)

let test_hash_0 () =
  assert_bool "test_hash_0" @@ (LV.hash a_lv = LV.hash a_lv_0)

let test_leq_0 () =
  assert_bool "test_leq_0 1" @@ LV.leq a_lv a_lv_0;
  assert_bool "test_leq_0 2" @@ LV.leq a_lv_0 a_lv

let test_join_0 () =
  assert_equal ~cmp:LV.equal ~printer:LV.show ~msg:"1" a_lv_top (LV.join a_lv_0 a_lv_1);
  assert_equal ~cmp:LV.equal ~printer:LV.show ~msg:"2" a_lv_top (LV.join a_lv a_lv_1) (* TODO *)

let test () =
  "lvalTest" >::: [
    "test_equal_0" >:: test_equal_0;
    "test_compare_0" >:: test_compare_0;
    "test_hash_0" >:: test_hash_0;
    "test_join_0" >:: test_join_0;
  ]
