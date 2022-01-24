open OUnit2

let all_tests = ("" >:::
  [ IntDomainTest.test ();
    MapDomainTest.test ();
    SolverTest.test ();
    (* etc *)
  ])

let () = run_test_tt_main all_tests
