(* To run this (and all other unit tests), type `dune runtest tests/unit/`. *)
open OUnit2
open Goblint_lib
open Batteries
open PentagonDomain

module INTERVALS = PentagonDomain.INTERVALS
module SUB = PentagonDomain.SUB
module PNTG = PentagonDomain.D

(* Test cases for the Intervals module *)
let test_intv_equal _ =
  let intv = [
    INTERVALS.create_single max_int min_int;
    INTERVALS.create_single 1 5;
    INTERVALS.top_single ()] 
  in
  let intv_equal = [
    INTERVALS.create_single max_int min_int;
    INTERVALS.create_single 1 5;
    INTERVALS.top_single ()] 
  in
  let intv_not_equal = [
    (Z.of_int max_int, Z.of_int min_int);
    INTERVALS.top_single ()
  ]
  in
  let intv_slightly_not_equal = [
    INTERVALS.create_single max_int min_int;
    INTERVALS.create_single 1 4;
    INTERVALS.top_single ()]
  in
  assert_bool "intv should be equal to intv_equal" (INTERVALS.equal intv intv_equal);
  assert_bool "intv should not be equal to intv_not_equal" (not (INTERVALS.equal intv intv_not_equal));
  assert_bool "sub1 should be equal to sub2" (not (INTERVALS.equal intv intv_slightly_not_equal));;

let test_intv_dim_add_1 _ =   
  let dim_change = ({dim = [|0; 1; 1; 2; 4|]; intdim = 5; realdim = 0 }: Apron.Dim.change) in
  let intv = [
    (* 0 *) INTERVALS.create_single 1 3; (* prev 0 *)
    (* 1 *) INTERVALS.create_single (-3) 25; (* prev 1 *)
    (* 2 *) INTERVALS.create_single 2 2; (* prev 2 *)
    (* 3 *) INTERVALS.create_single (-1) max_int; (* prev 3 *)
  ] in
  let expected_intv = [
    (* 0 *) INTERVALS.top_single ();
    (* 1 *) INTERVALS.create_single 1 3; (* prev 0 *)
    (* 2 *) INTERVALS.top_single ();
    (* 3 *) INTERVALS.top_single ();
    (* 4 *) INTERVALS.create_single (-3) 25; (* prev 1 *)
    (* 5 *) INTERVALS.top_single ();
    (* 6 *) INTERVALS.create_single 2 2; (* prev 2 *)
    (* 7 *) INTERVALS.create_single (-1) max_int; (* prev 3 *)
    (* 8 *) INTERVALS.top_single ();
  ]
  in
  let resulting_intv = INTERVALS.dim_add dim_change intv in
  assert_equal ~msg:( 
    "expected:" ^ INTERVALS.to_string expected_intv ^
    "\ngot:" ^ INTERVALS.to_string resulting_intv
  ) expected_intv resulting_intv;;

let test_intv_dim_add_2 _ =
  let dim_change = ({dim = [|0; 3; 3; 3; 3; 3; 4|]; intdim = 5; realdim = 0 }: Apron.Dim.change) in
  let intv = [
    (*0*) (Z.of_int 1, Z.of_int 3);
    (*1*) (Z.of_int (-1), Z.of_int 1);
    (*2*) INTERVALS.top_single ();
    (*3*) (Z.of_int (-100), Z.of_int max_int);
    (*4*)
  ] in 
  let expected_intv = [
    (* 0 *) INTERVALS.top_single ();
    (* 1 *) (Z.of_int 1, Z.of_int 3); (* prev 0 *)
    (* 2 *) (Z.of_int (-1), Z.of_int 1);  (* prev 1 *)
    (* 3 *) INTERVALS.top_single ();  (* prev 2 *)
    (* 4 *) INTERVALS.top_single ();
    (* 5 *) INTERVALS.top_single ();
    (* 6 *) INTERVALS.top_single ();
    (* 7 *) INTERVALS.top_single ();
    (* 8 *) INTERVALS.top_single ();
    (* 9 *) (Z.of_int (-100), Z.of_int max_int);  (* prev 3 *)
    (* 10 *) INTERVALS.top_single ();
    (* 11 *)
  ]
  in
  let resulting_intv = INTERVALS.dim_add dim_change intv in
  assert_equal ~msg:(
    "expected:" ^ INTERVALS.to_string expected_intv ^
    "\ngot:" ^ INTERVALS.to_string resulting_intv
  ) expected_intv resulting_intv;;

let test_intv_dim_remove_1 _ = 
  let dim_change = ({dim = [|0; 2; 3; 5; 7|]; intdim = 5; realdim = 0 }: Apron.Dim.change) in
  let intv = [
    (* 0 *) INTERVALS.top_single ();
    (* 1 *) INTERVALS.create_single 1 3;
    (* 2 *) INTERVALS.top_single ();
    (* 3 *) INTERVALS.top_single ();
    (* 4 *) INTERVALS.create_single (-3) 25;
    (* 5 *) INTERVALS.top_single ();
    (* 6 *) INTERVALS.create_single 2 2;
    (* 7 *) INTERVALS.top_single ();
    (* 8 *) INTERVALS.create_single (-1) max_int;
  ] in
  let expected_intv = [
    (* 0 *) INTERVALS.create_single 1 3;
    (* 1 *) INTERVALS.create_single (-3) 25;
    (* 2 *) INTERVALS.create_single 2 2;
    (* 3 *) INTERVALS.create_single (-1) max_int;
  ] 
  in
  let resulting_intv = INTERVALS.dim_remove dim_change intv in
  assert_equal ~msg:(
    "expected:" ^ INTERVALS.to_string expected_intv ^
    "\ngot:" ^ INTERVALS.to_string resulting_intv
  ) expected_intv resulting_intv;;


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

let test_sub_dim_remove_1 _ =
  let dim_change = ({dim = [|0; 2; 3; 5; 7|]; intdim = 5; realdim = 0 }: Apron.Dim.change) in
  (* If dim contains duplicates, then its not well-formed. *)
  (* let dim_change_with_duplicates = ({dim = [|0; 2; 2; 3; 5; 7|]; intdim = 5; realdim = 0 }: Apron.Dim.change) in *)
  let sub = [
    (*0*) SUB.VarSet.empty;
    (*1*) SUB.VarSet.singleton 6;
    (*2*) SUB.VarSet.empty;
    (*3*) SUB.VarSet.empty;
    (*4*) SUB.VarSet.singleton 6;
    (*5*) SUB.VarSet.empty;
    (*6*) SUB.VarSet.singleton 5;
    (*7*) SUB.VarSet.empty;
    (*8*) SUB.VarSet.singleton 1
  ] 
  in
  let expected_sub = [
    (*0*) SUB.VarSet.singleton 2;
    (*1*) SUB.VarSet.singleton 2;
    (*2*) SUB.VarSet.empty;
    (*3*) SUB.VarSet.singleton 0
  ] 
  in
  let resulting_sub = SUB.dim_remove dim_change sub in
  assert_equal ~msg:(
    "expected:" ^ SUB.to_string expected_sub ^
    "\ngot:" ^ SUB.to_string resulting_sub
  ) expected_sub resulting_sub;;


(* Test cases for the D module (PNTG) *)

let test_pntg_meet _ = 
  let pntg_1 = {}:
                 let pntg_2 = {}

let noop _ = assert_bool "" true

let test () =
  "PentagonTests" >::: [
    "noop" >:: noop;

    "test_intv_equal" >:: test_intv_equal;
    "test_intv_dim_add_1" >:: test_intv_dim_add_1;
    "test_intv_dim_add_2" >:: test_intv_dim_add_2;
    "test_intv_dim_remove_2" >:: test_intv_dim_remove_1;

    "test_sub_equal" >:: test_sub_equal;
    "test_sub_dim_add_1" >:: test_sub_dim_add_1;
    "test_sub_dim_add_2" >:: test_sub_dim_add_2;
    "test_sub_dim_remove_1" >:: test_sub_dim_remove_1;
  ]

