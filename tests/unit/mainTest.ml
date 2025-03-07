open OUnit2

let all_tests =
  "" >::: [
    IntDomainTest.test ();
    FloatDomainTest.test ();
    MapDomainTest.test ();
    SolverTest.test ();
    LvalTest.test ();
    SparseMatrixImplementationTest.test () ;
    CompilationDatabaseTest.tests;
    LibraryDslTest.tests;
    CilfacadeTest.tests;
    (* etc *)
    "domaintest" >::: QCheck_ounit.to_ounit2_test_list Maindomaintest.all_testsuite;
    IntOpsTest.tests;
    ThreadIdDomainTest.tests;
  ]

let () =
  print_string "\027[0;1munit: \027[0;0;00m";
  run_test_tt_main all_tests
