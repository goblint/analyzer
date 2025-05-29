(* To run this (and all other unit tests), type `dune runtest tests/unit/`. *)
open OUnit2
open Goblint_lib
open Batteries
open PentagonDomain

module INTERVALS = PentagonDomain.INTERVALS
module SUB = PentagonDomain.SUB

(* Test cases for the Intervals module *)



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

let test_sub_dim_add_1 _ =

  let dim_change = ({dim = [|0; 1; 1; 2; 3|]; intdim = 5; realdim = 0 }: Apron.Dim.change) in
  let sub = [
    (*0*) SUB.VarSet.singleton 2;
    (*1*) SUB.VarSet.singleton 2;
    (*2*) SUB.VarSet.singleton 5;
    (*3*) SUB.VarSet.singleton 0
  ] in 
  let expected_sub = [
    (*0*) SUB.VarSet.empty;(* insert 0 *)
    (*1*) SUB.VarSet.singleton 6;   (* prev 0 *)
    (*2*) SUB.VarSet.empty;(* insert 1 *)
    (*3*) SUB.VarSet.empty;(* insert 1 *)
    (*4*) SUB.VarSet.singleton 6;   (* prev 1 *)
    (*5*) SUB.VarSet.empty;(* insert 2 *)
    (*6*) SUB.VarSet.singleton 5;   (* prev 2 *)
    (*7*) SUB.VarSet.empty;(* insert 3 *)
    (*8*) SUB.VarSet.singleton 1    (* prev 3 *)
  ]
  in
  let resulting_sub = SUB.dim_add dim_change sub in
  assert_equal ~msg:(
    "expected:" ^ SUB.to_string expected_sub ^
    "\ngot:" ^ SUB.to_string resulting_sub
  ) expected_sub resulting_sub;;

let test_sub_dim_add_2 _ =
  let dim_change = ({dim = [|0; 3; 3; 3; 3; 3|]; intdim = 5; realdim = 0 }: Apron.Dim.change) in
  let sub = [
    (*0*) SUB.VarSet.singleton 3;
    (*1*) SUB.VarSet.empty;
    (*2*) SUB.VarSet.empty;
    (*3*) SUB.VarSet.singleton 0;
    (*4*)
  ] in 
  let expected_sub = [
    (*0*) SUB.VarSet.empty;
    (*1*) SUB.VarSet.singleton 9;
    (*2*) SUB.VarSet.empty;
    (*3*) SUB.VarSet.empty;
    (*4*) SUB.VarSet.empty;
    (*5*) SUB.VarSet.empty;
    (*6*) SUB.VarSet.empty;
    (*7*) SUB.VarSet.empty;
    (*8*) SUB.VarSet.empty;
    (*9*) SUB.VarSet.singleton 1;
    (*10*)
  ]
  in
  let resulting_sub = SUB.dim_add dim_change sub in
  assert_equal ~msg:(
    "expected:" ^ SUB.to_string expected_sub ^
    "\ngot:" ^ SUB.to_string resulting_sub
  ) expected_sub resulting_sub;;



let noop _ = assert_bool "" true

let test () =
  "PentagonTests" >::: [
    "noop" >:: noop;
    "test_sub_equal" >:: test_sub_equal;
    (* "test_sub_leq" >:: test_sub_leq; *)
    "test_sub_dim_add_1" >:: test_sub_dim_add_1;
    "test_sub_dim_add_2" >:: test_sub_dim_add_2;
  ]

