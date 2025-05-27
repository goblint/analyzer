(* To run this (and all other unit tests), type `dune runtest tests/unit/`. *)
open OUnit2
open Goblint_lib
open PentagonDomain

module INTERVALS = PentagonDomain.INTERVALS
module SUB = PentagonDomain.SUB

(* Test cases for the Intervals module *)
let test_order_single _ =
  let i1 = (Z.of_int 3, Z.of_int 7) in
  let i2 = (Z.of_int 1, Z.of_int 10) in
  assert_bool "i1 should be less than or equal to i2" (INTERVALS.leq_single i1 i2);
  assert_bool "i2 should not be less than or equal to i1" (not (INTERVALS.leq_single i2 i1))

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
  assert_equal None result_none ~msg:"meet_single should return None for disjoint intervals"

let test_widening_single _ =
  let i1 = (Z.of_int 3, Z.of_int 7) in
  let i2 = (Z.of_int 1, Z.of_int 10) in
  let result = INTERVALS.widen_single i1 i2 in
  assert_equal (INTERVALS.top_single ()) result ~msg:(Z.to_string (fst result) ^ ", " ^ Z.to_string (snd result))

let test_order_env _ =
  let env1 = INTERVALS.VarMap.empty |> INTERVALS.VarMap.add 1 (Z.of_int 3, Z.of_int 7) in
  let env2 = INTERVALS.VarMap.empty |> INTERVALS.VarMap.add 1 (Z.of_int 1, Z.of_int 10) in
  assert_bool "env1 should be less than or equal to env2" (INTERVALS.leq env1 env2);
  assert_bool "env2 should not be less than or equal to env1" (not (INTERVALS.leq env2 env1))

let test_bottom_env _ =
  let env = INTERVALS.VarMap.empty |> INTERVALS.VarMap.add 1 (Z.of_int 5, Z.of_int 3) in
  assert_bool "Environment should be bottom" (INTERVALS.is_bot env)

let test_top_env _ =
  let env = INTERVALS.top () in
  assert_bool "Environment should be top" (INTERVALS.is_top env)

let test_join_env _ =
  let env1 = INTERVALS.VarMap.empty |> INTERVALS.VarMap.add 1 (Z.of_int 3, Z.of_int 7) in
  let env2 = INTERVALS.VarMap.empty |> INTERVALS.VarMap.add 1 (Z.of_int 1, Z.of_int 10) in
  let result = INTERVALS.join env1 env2 in
  let expected = INTERVALS.VarMap.empty |> INTERVALS.VarMap.add 1 (Z.of_int 1, Z.of_int 10) in
  assert_equal expected result ~msg:"join_env failed"

let test_meet_env _ =
  let env1 = INTERVALS.VarMap.empty |> INTERVALS.VarMap.add 1 (Z.of_int 3, Z.of_int 7) in
  let env2 = INTERVALS.VarMap.empty |> INTERVALS.VarMap.add 1 (Z.of_int 1, Z.of_int 5) in
  let result = INTERVALS.meet env1 env2 in
  let expected = INTERVALS.VarMap.empty |> INTERVALS.VarMap.add 1 (Z.of_int 3, Z.of_int 5) in
  assert_equal expected result ~msg:"meet_env failed"

let test_widening_env _ =
  let env1 = INTERVALS.VarMap.empty |> INTERVALS.VarMap.add 1 (Z.of_int 3, Z.of_int 7) in
  let env2 = INTERVALS.VarMap.empty |> INTERVALS.VarMap.add 1 (Z.of_int 1, Z.of_int 10) in
  let result = INTERVALS.widen env1 env2 in
  let expected = INTERVALS.VarMap.empty |> INTERVALS.VarMap.add 1 (Z.of_int 3, Z.of_int max_int) in
  assert_equal expected result ~msg:"widen_env failed"



(* Test cases for the SUB module *)
let test_sub_equal _ =
  let sub1 = ({ d = Some (SUB.VarMap.empty |> SUB.VarMap.add 1 (SUB.VarSet.singleton 2)); env = SUB.VarMan.empty_env }: SUB.t) in
  let sub2 = ({ d = Some (SUB.VarMap.empty |> SUB.VarMap.add 1 (SUB.VarSet.singleton 2)); env = SUB.VarMan.empty_env }: SUB.t) in
  let sub3 = ({ d = Some (SUB.VarMap.empty |> SUB.VarMap.add 1 (SUB.VarSet.singleton 3)); env = SUB.VarMan.empty_env }: SUB.t) in
  assert_bool "sub1 should be equal to sub2" (SUB.equal sub1 sub2);
  assert_bool "sub1 should not be equal to sub3" (not (SUB.equal sub1 sub3))
let test_sub_is_bot _ =
  let sub = ({ d = Some SUB.VarMap.empty; env = SUB.VarMan.empty_env }: SUB.t) in
  assert_bool "Empty sub should be bottom" (SUB.is_bot sub);
  let non_bot_sub = ({ d = Some (SUB.VarMap.empty |> SUB.VarMap.add 1 (SUB.VarSet.singleton 2)); env = SUB.VarMan.empty_env }: SUB.t) in
  assert_bool "Non-empty sub should not be bottom" (not (SUB.is_bot non_bot_sub))

let test_sub_leq _ =
  let sub1 = ({d = Some(SUB.VarMap.empty |> SUB.VarMap.add 1 (SUB.VarSet.singleton 2)); env = SUB.VarMan.empty_env}: SUB.t)  in
  let sub2 = ({d = Some(SUB.VarMap.empty |> SUB.VarMap.add 1 (SUB.VarSet.singleton 2)); env = SUB.VarMan.empty_env}: SUB.t) in
  let sub3 = ({d = Some(SUB.VarMap.empty |> SUB.VarMap.add 1 (SUB.VarSet.singleton 3)); env = SUB.VarMan.empty_env}: SUB.t) in
  assert_bool "sub1 should be less than or equal to sub2" (SUB.leq sub1 sub2);
  assert_bool "sub1 should not be less than or equal to sub3" (not (SUB.leq sub1 sub3))

let test_sub_join _ =
  let sub1 = ({ d = Some (SUB.VarMap.empty |> SUB.VarMap.add 1 (SUB.VarSet.of_list [2; 3])); env = SUB.VarMan.empty_env }: SUB.t) in
  let sub2 = ({ d = Some (SUB.VarMap.empty |> SUB.VarMap.add 1 (SUB.VarSet.singleton 2)); env = SUB.VarMan.empty_env }: SUB.t) in
  let result = SUB.join sub1 sub2 in
  let expected = ({ d = Some (SUB.VarMap.empty |> SUB.VarMap.add 1 (SUB.VarSet.singleton 2)); env = SUB.VarMan.empty_env }: SUB.t) in
  assert_bool "join failed" (SUB.equal result expected)

let test_sub_meet _ =
  let sub1 = ({ d = Some (SUB.VarMap.empty |> SUB.VarMap.add 1 (SUB.VarSet.singleton 2)); env = SUB.VarMan.empty_env }: SUB.t) in
  let sub2 = ({ d = Some (SUB.VarMap.empty |> SUB.VarMap.add 1 (SUB.VarSet.singleton 3)); env = SUB.VarMan.empty_env }: SUB.t) in
  let result = SUB.meet sub1 sub2 in
  let expected = ({ d = Some (SUB.VarMap.empty |> SUB.VarMap.add 1 (SUB.VarSet.of_list [2; 3])); env = SUB.VarMan.empty_env }: SUB.t) in
  assert_bool "meet failed" (SUB.equal result expected)

let test_sub_widening _ =
  let sub1 = ({ d = Some (SUB.VarMap.empty |> SUB.VarMap.add 1 (SUB.VarSet.singleton 2)); env = SUB.VarMan.empty_env }: SUB.t) in
  let sub2 = ({ d = Some (SUB.VarMap.empty |> SUB.VarMap.add 1 (SUB.VarSet.of_list [2; 3])); env = SUB.VarMan.empty_env }: SUB.t) in
  let result = SUB.widen sub1 sub2 in
  let expected = ({ d = Some (SUB.VarMap.empty |> SUB.VarMap.add 1 (SUB.VarSet.of_list [2; 3])); env = SUB.VarMan.empty_env }: SUB.t) in
  assert_bool "widening failed" (SUB.equal result expected)

let test_sub_top _ =
  let top_sub = SUB.top () in
  assert_bool "Top sub should not be bottom" (not (SUB.is_bot top_sub));
  assert_bool "Top sub should be equal to itself" (SUB.equal top_sub top_sub)

let testing _ = assert_equal true true

let test () =
  "PentagonTests" >::: [
    "noop" >:: testing;
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
    "test_sub_equal" >:: test_sub_equal;
    "test_sub_is_bot" >:: test_sub_is_bot;
    "test_sub_leq" >:: test_sub_leq;
    "test_sub_join" >:: test_sub_join;
    "test_sub_meet" >:: test_sub_meet;
    "test_sub_widening" >:: test_sub_widening;
    "test_sub_top" >:: test_sub_top;
  ]

