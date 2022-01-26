open OUnit2

let all_tests = ("" >:::
  [ IntDomainTest.test ();
    MapDomainTest.test ();
    SolverTest.test ();
    LvalTest.test ();
    (* etc *)
    "domaintest" >::: QCheck_ounit.to_ounit2_test_list Maindomaintest.all_testsuite
  ])

let () = run_test_tt_main all_tests
