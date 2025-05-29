(* To run this (and all other unit tests), type `dune runtest tests/unit/`. *)
open OUnit2
open Goblint_lib
open Batteries
open PentagonDomain

module INTERVALS = PentagonDomain.INTERVALS
module SUB = PentagonDomain.SUB

(* Test cases for the Intervals module *)
let test_order_single _ =
  let i1 = (Z.of_int 3, Z.of_int 7) in
  let i2 = (Z.of_int 1, Z.of_int 10) in
  assert_bool 
    "i1 should be less than or equal to i2"
    (INTERVALS.leq_single i1 i2);
  assert_bool
    "i2 should not be less than or equal to i1"
    (not (INTERVALS.leq_single i2 i1))

let test_bottom_single _ =
  let i = (Z.of_int 5, Z.of_int 3) in
  assert_bool "Interval should be bottom" (INTERVALS.is_bot_single i)

let test_top_single _ =
  let i = INTERVALS.top_single () in
  assert_bool "Interval should be top" (INTERVALS.is_top_single i)

let test_join_single _ =
  let i1 = (Z.of_int 3, Z.of_int 7) in
  let i2 = (Z.of_int 1, Z.of_int 10) in
  let result = INTERVALS.join_single i1 i2 in
  assert_equal (Z.of_int 1, Z.of_int 10) result ~msg:"join_single failed"

let test_meet_single _ =
  let i1 = (Z.of_int 3, Z.of_int 7) in
  let i2 = (Z.of_int 1, Z.of_int 5) in
  let result = INTERVALS.meet_single i1 i2 in
  assert_equal (Some (Z.of_int 3, Z.of_int 5)) result ~msg:"meet_single failed";
  let i3 = (Z.of_int 8, Z.of_int 10) in
  let result_none = INTERVALS.meet_single i1 i3 in
  assert_equal 
    None
    result_none
    ~msg:"meet_single should return None for disjoint intervals"

let test_widening_single _ =
  let i1 = (Z.of_int 3, Z.of_int 7) in
  let i2 = (Z.of_int 1, Z.of_int 10) in
  let result = INTERVALS.widen_single i1 i2 in
  assert_equal
    (INTERVALS.top_single ())
    result
    ~msg:(Z.to_string (fst result) ^ ", " ^ Z.to_string (snd result))

let test_order_env _ =
  let env1 = 
    INTERVALS.VarMap.empty |>
    INTERVALS.VarMap.add 1 (Z.of_int 3, Z.of_int 7) in
  let env2 =
    INTERVALS.VarMap.empty |>
    INTERVALS.VarMap.add 1 (Z.of_int 1, Z.of_int 10) in
  assert_bool
    "env1 should be less than or equal to env2"
    (INTERVALS.leq env1 env2);
  assert_bool
    "env2 should not be less than or equal to env1"
    (not (INTERVALS.leq env2 env1))

let test_bottom_env _ =
  let env =
    INTERVALS.VarMap.empty |>
    INTERVALS.VarMap.add 1 (Z.of_int 5, Z.of_int 3) in
  assert_bool
    "Environment should be bottom"
    (INTERVALS.is_bot env)

let test_top_env _ =
  let env = INTERVALS.top () in
  assert_bool "Environment should be top" (INTERVALS.is_top env)

let test_join_env _ =
  let env1 = 
    INTERVALS.VarMap.empty |>
    INTERVALS.VarMap.add 1 (Z.of_int 3, Z.of_int 7) in
  let env2 = 
    INTERVALS.VarMap.empty |>
    INTERVALS.VarMap.add 1 (Z.of_int 1, Z.of_int 10) in
  let result =
    INTERVALS.join env1 env2 in
  let expected =
    INTERVALS.VarMap.empty |>
    INTERVALS.VarMap.add 1 (Z.of_int 1, Z.of_int 10) in
  assert_equal expected result ~msg:"join_env failed"

let test_meet_env _ =
  let env1 =
    INTERVALS.VarMap.empty |>
    INTERVALS.VarMap.add 1 (Z.of_int 3, Z.of_int 7) in
  let env2 =
    INTERVALS.VarMap.empty |>
    INTERVALS.VarMap.add 1 (Z.of_int 1, Z.of_int 5) in
  let result =
    INTERVALS.meet env1 env2 in
  let expected =
    INTERVALS.VarMap.empty |>
    INTERVALS.VarMap.add 1 (Z.of_int 3, Z.of_int 5) in
  assert_equal expected result ~msg:"meet_env failed"

let test_widening_env _ =
  let env1 =
    INTERVALS.VarMap.empty |>
    INTERVALS.VarMap.add 1 (Z.of_int 3, Z.of_int 7) in
  let env2 =
    INTERVALS.VarMap.empty |>
    INTERVALS.VarMap.add 1 (Z.of_int 1, Z.of_int 10) in
  let result = INTERVALS.widen env1 env2 in
  let expected = 
    INTERVALS.VarMap.empty |>
    INTERVALS.VarMap.add 1 (Z.of_int 3, Z.of_int max_int) in
  assert_equal expected result ~msg:"widen_env failed"



(* Test cases for the SUB module *)
let test_sub_equal _ =
  let sub1 = [SUB.VarSet.singleton 2] in
  let sub2 = [SUB.VarSet.singleton 2] in
  let sub3 = [SUB.VarSet.singleton 3] in
  assert_bool "sub1 should be equal to sub2" (SUB.equal sub1 sub2);
  assert_bool "sub1 should not be equal to sub3" (not (SUB.equal sub1 sub3))

let test_sub_leq _ =
  let sub = [
    SUB.VarSet.of_list [2; 3; 4];
    SUB.VarSet.of_list [7; 6]
  ] in
  let equal_sub = [
    SUB.VarSet.of_list [2; 3; 4];
    SUB.VarSet.of_list [7; 6]
  ] in
  let uncomparable_sub = [
    SUB.VarSet.singleton 3
  ] in
  let less_specific_sub = [
    SUB.VarSet.of_list [4];
    SUB.VarSet.of_list [6]
  ] in

  assert_bool
    "sub should be less than or equal to an equal sub" (SUB.leq sub equal_sub);
  assert_bool
    "sub should not be less than or equal to an uncomparable sub"
    (not (SUB.leq sub uncomparable_sub));
  assert_bool
    "sub should be less than or equal to a sub containing \
     less precise information"
    (SUB.leq sub less_specific_sub);
  assert_bool 
    "sub should not be less than or equal to a sub \
     containing more precise information"
    (not (SUB.leq less_specific_sub sub));;

let test_sub_to_string _ = 
  let sub_string = 
    SUB.to_string [SUB.VarSet.of_list [2;3;4;28]] in
  print_string sub_string; assert_equal sub_string "{\n1 -> {2,3,4,28}\n}\n";;

let test_sub_dim_add _ =
  let dim_change = ({dim = [|0; 1; 1; 2; 3|]; intdim = 5; realdim = 0 }: Apron.Dim.change) in
  let sub = [
    SUB.VarSet.singleton 2;
    SUB.VarSet.singleton 3;
    SUB.VarSet.singleton 5
  ] in 
  (*
  0_
    1 -> {2}
  1_
    2 -> {3}
  2_
    3 -> {5}
  3_
  *)
  let expected_sub = [
    SUB.VarSet.empty;  (* insert 0 *)
    SUB.VarSet.singleton 2;
    SUB.VarSet.empty;  (* insert 1 *)
    SUB.VarSet.empty;  (* insert 1 *)
    SUB.VarSet.singleton 3;
    SUB.VarSet.empty;  (* insert 2 *)
    SUB.VarSet.singleton 5;
    SUB.VarSet.empty   (* insert 3 *)
  ]
  in 
  (*
  1 -> {} // new
  2 -> {2, 3}
  3 -> {} // new
  4 -> {} // new
  5 -> {3}
  6 -> {} // new
  7 -> {5}
  8 -> {} // new
  *)
  let resulting_sub = SUB.dim_add dim_change sub in
  assert_equal expected_sub resulting_sub;;


let test () =
  "PentagonTests" >::: [
    (*"test_order_single" >:: test_order_single;
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
      "test_widening_env" >:: test_widening_env; *)
    "test_sub_equal" >:: test_sub_equal;
    "test_sub_leq" >:: test_sub_leq;
    "test_sub_dim_add" >:: test_sub_dim_add;
    "test_sub_to_string" >:: test_sub_to_string;
  ]

