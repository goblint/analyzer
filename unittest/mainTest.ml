open OUnit2

let all_tests = ("" >:::
  [ IntDomainTest.test ();
    FloatDomainTest.test ();
    MapDomainTest.test ();
    SolverTest.test ();
    LvalTest.test ();
    CompilationDatabaseTest.tests;
    (* etc *)
    "domaintest" >::: QCheck_ounit.to_ounit2_test_list Maindomaintest.all_testsuite
  ])

let all_tests_debug = ("" >::: [FloatDomainTest.test ()]) (* TODO: TO BE REMOVED only for personal Debugging *)

let () = run_test_tt_main all_tests_debug (* TODO: change back to all_tests *)
