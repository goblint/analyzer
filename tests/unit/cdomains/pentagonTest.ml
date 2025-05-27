(* To run this (and all other unit tests), type `dune runtest tests/unit/`. *)

open OUnit2
open Apron

(* Test cases for the Intervals module *)
let test_order_single _ =
    let i1 = (Z.of_int 3, Z.of_int 7) in
    let i2 = (Z.of_int 1, Z.of_int 10) in
    assert_bool "i1 should be less than or equal to i2" (leq_single i1 i2);
    assert_bool "i2 should not be less than or equal to i1" (not (leq_single i2 i1))

let test_bottom_single _ =
    let i = (Z.of_int 5, Z.of_int 3) in
    assert_bool "Interval should be bottom" (is_bot_single i)

let test_top_single _ =
    let i = top_single () in
    assert_bool "Interval should be top" (is_top_single i)

let test_join_single _ =
    let i1 = (Z.of_int 3, Z.of_int 7) in
    let i2 = (Z.of_int 1, Z.of_int 10) in
    let result = join_single i1 i2 in
    assert_equal (Z.of_int 1, Z.of_int 10) result ~msg:"join_single failed"

let test_meet_single _ =
    let i1 = (Z.of_int 3, Z.of_int 7) in
    let i2 = (Z.of_int 1, Z.of_int 5) in
    let result = meet_single i1 i2 in
    assert_equal (Some (Z.of_int 3, Z.of_int 5)) result ~msg:"meet_single failed";
    let i3 = (Z.of_int 8, Z.of_int 10) in
    let result_none = meet_single i1 i3 in
    assert_equal None result_none ~msg:"meet_single should return None for disjoint intervals"

let test_widening_single _ =
    let i1 = (Z.of_int 3, Z.of_int 7) in
    let i2 = (Z.of_int 1, Z.of_int 10) in
    let result = widen_single i1 i2 in
    assert_equal (Z.of_int 3, Z.of_int max_int) result ~msg:"widen_single failed"

let test_order_env _ =
    let env1 = VarMap.empty |> VarMap.add 1 (Z.of_int 3, Z.of_int 7) in
    let env2 = VarMap.empty |> VarMap.add 1 (Z.of_int 1, Z.of_int 10) in
    assert_bool "env1 should be less than or equal to env2" (leq env1 env2);
    assert_bool "env2 should not be less than or equal to env1" (not (leq env2 env1))

let test_bottom_env _ =
    let env = VarMap.empty |> VarMap.add 1 (Z.of_int 5, Z.of_int 3) in
    assert_bool "Environment should be bottom" (is_bot env)

let test_top_env _ =
    let env = top () in
    assert_bool "Environment should be top" (is_top env)

let test_join_env _ =
    let env1 = VarMap.empty |> VarMap.add 1 (Z.of_int 3, Z.of_int 7) in
    let env2 = VarMap.empty |> VarMap.add 1 (Z.of_int 1, Z.of_int 10) in
    let result = join env1 env2 in
    let expected = VarMap.empty |> VarMap.add 1 (Z.of_int 1, Z.of_int 10) in
    assert_equal expected result ~msg:"join_env failed"

let test_meet_env _ =
    let env1 = VarMap.empty |> VarMap.add 1 (Z.of_int 3, Z.of_int 7) in
    let env2 = VarMap.empty |> VarMap.add 1 (Z.of_int 1, Z.of_int 5) in
    let result = meet env1 env2 in
    let expected = VarMap.empty |> VarMap.add 1 (Z.of_int 3, Z.of_int 5) in
    assert_equal expected result ~msg:"meet_env failed"

let test_widening_env _ =
    let env1 = VarMap.empty |> VarMap.add 1 (Z.of_int 3, Z.of_int 7) in
    let env2 = VarMap.empty |> VarMap.add 1 (Z.of_int 1, Z.of_int 10) in
    let result = widen env1 env2 in
    let expected = VarMap.empty |> VarMap.add 1 (Z.of_int 3, Z.of_int max_int) in
    assert_equal expected result ~msg:"widen_env failed"

let test_suite =
  "IntervalsTests" >::: [
    "test_order_single" >:: test_order_single;
    "test_bottom_single" >:: test_bottom_single;
    "test_top_single" >:: test_top_single;
    "test_join_single" >:: test_join_single;
    "test_meet_single" >:: test_meet_single;
    "test_widening_single" >:: test_widening_single;
    "test_order_env" >:: test_order_env;
    "test_bottom_env" >:: test_bottom_env;
    "test_top_env" >:: test_top_env;
    "test_join_env" >:: test_join_env;
    "test_meet_env" >:: test_meet_env;
    "test_widening_env" >:: test_widening_env;
  ]

let () = run_test_tt_main (test_suite)