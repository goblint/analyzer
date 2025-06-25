(* To run this (and all other unit tests), type `dune runtest tests/unit/`. *)
(*open OUnit2
  open Goblint_lib
  open Batteries
  open PentagonDomain
  open Intv

  (* Test cases for the Intervals module *)
  let assert_equal expected result = 
  OUnit2.assert_equal ~cmp: Intv.equal ~msg:(
    "expected:" ^ Intv.to_string expected ^
    "\ngot:" ^ Intv.to_string result
  ) expected result;;
  let test_intv_equal _ =
  let intv =
    [
      Interval.create max_int min_int;
      Interval.create 1 5;
      Interval.top ();
    ]
  in
  let intv_equal =
    [
      Interval.create max_int min_int;
      Interval.create 1 5;
      Interval.top ();
    ]
  in
  let intv_not_equal =
    [ Interval.bot (); Interval.top () ]
  in
  let intv_slightly_not_equal =
    [
      Interval.create max_int min_int;
      Interval.create 1 4;
      Interval.top ();
    ]
  in
  assert_equal intv intv_equal;
  assert_bool "intv should not be equal to intv_not_equal"
    (not (Intv.equal intv intv_not_equal));
  assert_bool "sub1 should be equal to sub2"
    (not (Intv.equal intv intv_slightly_not_equal))

  let test_intv_dim_add_1 _ =
  let dim_change =
    ({ dim = [| 0; 1; 1; 2; 4 |]; intdim = 5; realdim = 0 } : Apron.Dim.change)
  in
  let intv =
    [
      (* 0 *) Interval.create 1 3;
      (* 1 *) Interval.create (-3) 25;
      (* 2 *) Interval.create 2 2;
      (* 3 *) Interval.create (-1) max_int;
    ]
  in
  let expected_intv =
    [
      (* 0 *) Interval.top ();
      (* 1 *) Interval.create 1 3; (* prev 0 *)
      (* 2 *) Interval.top ();
      (* 3 *) Interval.top ();
      (* 4 *) Interval.create (-3) 25; (* prev 1 *)
      (* 5 *) Interval.top ();
      (* 6 *) Interval.create 2 2; (* prev 2 *)
      (* 7 *) Interval.create (-1) max_int; (* prev 3 *)
      (* 8 *) Interval.top ();
    ]
  in
  let resulting_intv = Intv.dim_add dim_change intv in
  assert_equal expected_intv resulting_intv

  let test_intv_dim_add_2 _ =
  let dim_change =
    ({ dim = [| 0; 3; 3; 3; 3; 3; 4 |]; intdim = 5; realdim = 0 }
     : Apron.Dim.change)
  in
  let intv =
    [
      (*0*) (ZExt.of_int 1, ZExt.of_int 3);
      (*1*) (ZExt.of_int (-1), ZExt.of_int 1);
      (*2*) Interval.top ();
      (*3*) (ZExt.of_int (-100), ZExt.of_int max_int);
      (*4*)
    ]
  in
  let expected_intv =
    [
      (* 0 *) Interval.top ();
      (* 1 *) (ZExt.of_int 1, ZExt.of_int 3); (* prev 0 *)
      (* 2 *) (ZExt.of_int (-1), ZExt.of_int 1); (* prev 1 *)
      (* 3 *) Interval.top (); (* prev 2 *)
      (* 4 *) Interval.top ();
      (* 5 *) Interval.top ();
      (* 6 *) Interval.top ();
      (* 7 *) Interval.top ();
      (* 8 *) Interval.top ();
      (* 9 *) (ZExt.of_int (-100), ZExt.of_int max_int); (* prev 3 *)
      (* 10 *) Interval.top ();
      (* 11 *)
    ]
  in
  let resulting_intv = Intv.dim_add dim_change intv in
  assert_equal expected_intv resulting_intv

  let test_intv_dim_remove_1 _ =
  let dim_change =
    ({ dim = [| 0; 2; 3; 5; 7 |]; intdim = 5; realdim = 0 } : Apron.Dim.change)
  in
  let intv =
    [
      (* 0 *) Interval.top ();
      (* 1 *) Interval.create 1 3;
      (* 2 *) Interval.top ();
      (* 3 *) Interval.top ();
      (* 4 *) Interval.create (-3) 25;
      (* 5 *) Interval.top ();
      (* 6 *) Interval.create 2 2;
      (* 7 *) Interval.top ();
      (* 8 *) Interval.create (-1) max_int;
    ]
  in
  let expected_intv =
    [
      (* 0 *) Interval.create 1 3;
      (* 1 *) Interval.create (-3) 25;
      (* 2 *) Interval.create 2 2;
      (* 3 *) Interval.create (-1) max_int;
    ]
  in
  let resulting_intv = Intv.dim_remove dim_change intv in
  assert_equal expected_intv resulting_intv

  (* Test cases for the SUB module *)
  let assert_equal expected_sub resulting_sub = 
  OUnit2.assert_equal 
    ~cmp: SUB.equal
    ~msg: ("expected:" ^ SUB.to_string expected_sub ^ "\ngot:" ^ SUB.to_string resulting_sub)
    expected_sub
    resulting_sub

  let test_sub_equal _ =
  let sub1 = [ SUB.VarSet.singleton 2 ] in
  let sub2 = [ SUB.VarSet.singleton 2 ] in
  let sub3 = [ SUB.VarSet.singleton 3 ] in
  assert_bool "sub1 should be equal to sub2" (SUB.equal sub1 sub2);
  assert_bool "sub1 should not be equal to sub3" (not (SUB.equal sub1 sub3))

  let test_sub_leq _ =
  let sub = [ SUB.VarSet.of_list [ 2; 3; 4 ]; SUB.VarSet.of_list [ 7; 6 ] ] in
  let equal_sub =
    [ SUB.VarSet.of_list [ 2; 3; 4 ]; SUB.VarSet.of_list [ 7; 6 ] ]
  in
  let uncomparable_sub = [ SUB.VarSet.singleton 3 ] in
  let less_specific_sub =
    [ SUB.VarSet.of_list [ 4 ]; SUB.VarSet.of_list [ 6 ] ]
  in

  assert_bool "sub should be less than or equal to an equal sub"
    (SUB.leq sub equal_sub);
  assert_bool "sub should not be less than or equal to an uncomparable sub"
    (not (SUB.leq sub uncomparable_sub));
  assert_bool
    "sub should be less than or equal to a sub containing less precise \
     information"
    (SUB.leq sub less_specific_sub);
  assert_bool
    "sub should not be less than or equal to a sub containing more precise \
     information"
    (not (SUB.leq less_specific_sub sub))

  let test_sub_dim_add_1 _ =
  let dim_change =
    ({ dim = [| 0; 1; 1; 2; 3 |]; intdim = 5; realdim = 0 } : Apron.Dim.change)
  in
  let sub =
    [
      (*0*) SUB.VarSet.singleton 2;
      (*1*) SUB.VarSet.singleton 2;
      (*2*) SUB.VarSet.singleton 5;
      (*3*) SUB.VarSet.singleton 0;
    ]
  in
  let expected_sub =
    [
      (*0*) SUB.VarSet.empty; (* insert 0 *)
      (*1*) SUB.VarSet.singleton 6; (* prev 0 *)
      (*2*) SUB.VarSet.empty; (* insert 1 *)
      (*3*) SUB.VarSet.empty; (* insert 1 *)
      (*4*) SUB.VarSet.singleton 6; (* prev 1 *)
      (*5*) SUB.VarSet.empty; (* insert 2 *)
      (*6*) SUB.VarSet.singleton 5; (* prev 2 *)
      (*7*) SUB.VarSet.empty; (* insert 3 *)
      (*8*) SUB.VarSet.singleton 1 (* prev 3 *);
    ]
  in
  let resulting_sub = SUB.dim_add dim_change sub in
  assert_equal expected_sub resulting_sub

  let test_sub_dim_add_2 _ =
  let dim_change =
    ({ dim = [| 0; 3; 3; 3; 3; 3 |]; intdim = 5; realdim = 0 }
     : Apron.Dim.change)
  in
  let sub =
    [
      (*0*) SUB.VarSet.singleton 3;
      (*1*) SUB.VarSet.empty;
      (*2*) SUB.VarSet.empty;
      (*3*) SUB.VarSet.singleton 0;
      (*4*)
    ]
  in
  let expected_sub =
    [
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
  assert_equal expected_sub resulting_sub

  let test_sub_dim_remove_1 _ =
  let dim_change =
    ({ dim = [| 0; 2; 3; 5; 7 |]; intdim = 5; realdim = 0 } : Apron.Dim.change)
  in
  (* If dim contains duplicates, then its not well-formed. *)
  (* let dim_change_with_duplicates = ({dim = [|0; 2; 2; 3; 5; 7|]; intdim = 5; realdim = 0 }: Apron.Dim.change) in *)
  let sub =
    [
      (*0*) SUB.VarSet.empty;
      (*1*) SUB.VarSet.singleton 6;
      (*2*) SUB.VarSet.empty;
      (*3*) SUB.VarSet.empty;
      (*4*) SUB.VarSet.singleton 6;
      (*5*) SUB.VarSet.empty;
      (*6*) SUB.VarSet.singleton 5;
      (*7*) SUB.VarSet.empty;
      (*8*) SUB.VarSet.singleton 1;
    ]
  in
  let expected_sub =
    [
      (*0*) SUB.VarSet.singleton 2;
      (*1*) SUB.VarSet.singleton 2;
      (*2*) SUB.VarSet.empty;
      (*3*) SUB.VarSet.singleton 0;
    ]
  in
  let resulting_sub = SUB.dim_remove dim_change sub in
  assert_equal expected_sub resulting_sub

  (* Test cases for the D module (PNTG) *)

  let assert_equal expected result = 
  OUnit2.assert_equal ~cmp: D.equal ~msg:(
    "expected:" ^ D.to_string expected ^
    "\ngot:" ^ D.to_string result
  ) expected result;;

  let test_pntg_widen _ =
  let env =
    Apron.Environment.make
      (Array.init 3 (fun i -> Apron.Var.of_string (string_of_int i)))
      [||]
  in
  let (pntg1: D.t) = { d = 
                         Some({
                             intv = [
                               Interval.create 0 100;
                               Interval.create 100 200;
                               Interval.create 200 300;];
                             sub = [
                               SUB.VarSet.empty |> SUB.VarSet.add 1;
                               SUB.VarSet.empty |> SUB.VarSet.add 2;
                               SUB.VarSet.empty;
                               SUB.VarSet.empty;]
                           }); env } in
  let (pntg2: D.t) = { d = Some({
      intv = [
        Interval.create 40 60;
        Interval.create 120 201;
        Interval.create 199 301;
      ]; sub = [
          SUB.VarSet.empty |> SUB.VarSet.add 1 |> SUB.VarSet.add 2;
          SUB.VarSet.empty |> SUB.VarSet.add 2;
          SUB.VarSet.empty;
          SUB.VarSet.empty;
        ] }); env } in
  let (resulting_pntg : D.t) = D.widen pntg1 pntg2 in

  let expected_intvs =
    [
      Interval.create 40 60;
      Interval.create 120 max_int;
      Interval.create min_int max_int;
    ]
  in
  let expected_sub =
    [
      SUB.VarSet.empty |> SUB.VarSet.add 1 |> SUB.VarSet.add 2;
      SUB.VarSet.empty |> SUB.VarSet.add 2;
      SUB.VarSet.empty;
      SUB.VarSet.empty;
    ]
  in
  let (expected_pntg' : PentagonDomain.PNTG.t) =
    { intv = expected_intvs; sub = expected_sub }
  in
  let (expected_pntg : D.t) = { d = Some expected_pntg'; env } in

  assert_equal expected_pntg resulting_pntg

  let test_pntg_leq_1 _ =
  let env =
    Apron.Environment.make
      (Array.init 4 (fun i -> Apron.Var.of_string (string_of_int i)))
      [||]
  in
  let intvs1 =
    [
      Interval.create 0 2;
      Interval.create 2 3;
      Interval.create 3 3;
      Interval.create 1 5;
    ]
  in
  (* [0 < {1}; 1 < {2}; 2 < {}; 3 < {}] *)
  let sub1 =
    [
      SUB.VarSet.empty |> SUB.VarSet.add 1;
      SUB.VarSet.empty |> SUB.VarSet.add 2;
      SUB.VarSet.empty;
      SUB.VarSet.empty;
    ]
  in
  let intvs2 = List.init 4 (fun i -> Interval.create 0 (i + 2)) in
  (* [0 < {1, 2}; 1 < {2}; 2 < {}; 3 < {}] *)
  let sub2 =
    [
      SUB.VarSet.empty |> SUB.VarSet.add 1 |> SUB.VarSet.add 2;
      SUB.VarSet.empty |> SUB.VarSet.add 2;
      SUB.VarSet.empty;
      SUB.VarSet.empty;
    ]
  in
  let (pntg1 : PentagonDomain.PNTG.t) = { intv = intvs1; sub = sub1 } in
  let (pntg2 : PentagonDomain.PNTG.t) = { intv = intvs2; sub = sub2 } in
  let (d1 : D.t) = { d = Some pntg1; env } in
  let (d2 : D.t) = { d = Some pntg2; env } in
  (* 0 < 2 is missing in sub1, but implied by intvs1: [0,2] < [3,3] *)
  assert_bool "" (D.leq d1 d2)

  let test_pntg_leq_2 _ =
  let env =
    Apron.Environment.make
      (Array.init 4 (fun i -> Apron.Var.of_string (string_of_int i)))
      [||]
  in
  let intvs1 = List.init 4 (fun i -> Interval.create 0 (i + 2)) in
  (* [0 < {1}; 1 < {2}; 2 < {}; 3 < {}] *)
  let sub1 =
    [
      SUB.VarSet.empty |> SUB.VarSet.add 1;
      SUB.VarSet.empty |> SUB.VarSet.add 2;
      SUB.VarSet.empty;
      SUB.VarSet.empty;
    ]
  in
  let intvs2 = List.init 4 (fun i -> Interval.create 0 (i + 2)) in
  (* [0 < {1, 2}; 1 < {2}; 2 < {}; 3 < {}] *)
  let sub2 =
    [
      SUB.VarSet.empty |> SUB.VarSet.add 1 |> SUB.VarSet.add 2;
      SUB.VarSet.empty |> SUB.VarSet.add 2;
      SUB.VarSet.empty;
      SUB.VarSet.empty;
    ]
  in
  let (pntg1 : PentagonDomain.PNTG.t) = { intv = intvs1; sub = sub1 } in
  let (pntg2 : PentagonDomain.PNTG.t) = { intv = intvs2; sub = sub2 } in
  let (d1 : D.t) = { d = Some pntg1; env } in
  let (d2 : D.t) = { d = Some pntg2; env } in
  (* 0 < 2 is missing in sub1, would be implied by transitivity, but we must not check for that *)
  assert_bool "" (not (D.leq d1 d2))
  (* Test cases for the D module (PNTG) *)

  (** Check behaviour of meet and bot *)
  let test_pntg_meet_bots _ =
  let cs = Interval.create in
  let assert_equal expected result = 
    assert_equal expected result
  in
  let pntg_1 = D.bot () in
  let pntg_2 = D.bot () in
  let pntg_3 =
    ({
      d =
        Some
          {
            intv = [ cs 1 2; cs 2 4 ];
            sub = [ SUB.VarSet.singleton 1; SUB.VarSet.empty ];
          };
      env = D.empty_env;
    }
      : D.t)
  in

  let result = (D.meet pntg_1 pntg_2) in
  let expected = (D.bot ()) in
  assert_equal expected result;

  let result = (D.meet pntg_1 pntg_3) in
  let expected = D.bot () in
  assert_equal expected result;

  let result = D.meet pntg_3 pntg_2 in
  let expected = D.bot () in
  assert_equal expected result;;

  (** Meet empty pentagons *)
  let test_pntg_meet _ =
  let cs = Interval.create in
  let env1 = Apron.Environment.make (Array.init 2 (fun i -> Apron.Var.of_string (string_of_int i))) [||] in 
  let env2 = Apron.Environment.make (Array.init 3 (fun i -> Apron.Var.of_string (string_of_int i))) [||] in 
  let pntg_1 =
    ({ d = Some { intv = [cs 1 2; cs (-3) 5]; sub = [SUB.VarSet.singleton 2; SUB.VarSet.empty] }; env = env1 } : D.t)
  in
  let pntg_2 =
    ({ d = Some { intv = [cs (-2) 5; Interval.top (); cs min_int max_int]; sub = [SUB.VarSet.empty; SUB.VarSet.singleton 1; SUB.VarSet.empty] }; env = env2 } : D.t)
  in
  let expected =
    ({ d = Some { intv = [cs 1 2; cs (-3) 5; cs min_int max_int]; sub = [SUB.VarSet.singleton 2; SUB.VarSet.singleton 1; SUB.VarSet.empty]};  env = Apron.Environment.lce env1 env2 } : D.t)
  in
  let result = D.meet pntg_1 pntg_2 in
  assert_equal expected result


  let noop _ = assert_bool "" true

  let test () =
  "PentagonTests"
  >::: [
    "noop" >:: noop;
    "test_intv_equal" >:: test_intv_equal;
    "test_intv_dim_add_1" >:: test_intv_dim_add_1;
    "test_intv_dim_add_2" >:: test_intv_dim_add_2;
    "test_intv_dim_remove_2" >:: test_intv_dim_remove_1;
    "test_sub_equal" >:: test_sub_equal;
    "test_sub_dim_add_1" >:: test_sub_dim_add_1;
    "test_sub_dim_add_2" >:: test_sub_dim_add_2;
    "test_sub_dim_remove_1" >:: test_sub_dim_remove_1;
    "test_pntg_leq_1" >:: test_pntg_leq_1;
    "test_pntg_leq_2" >:: test_pntg_leq_2;
    "test_pntg_widen" >:: test_pntg_widen;
    "test_pntg_meet_bots" >:: test_pntg_meet_bots;
    "test_pntg_meet" >:: test_pntg_meet;
  ]
*)