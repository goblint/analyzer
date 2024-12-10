open OUnit2

let all_tests =
  "" >::: [
    IntDomainTest.test ();
    FloatDomainTest.test ();
    MapDomainTest.test ();
    SolverTest.test ();
    LvalTest.test ();
    SparseMatrixImplementationTest.tests ;
    CompilationDatabaseTest.tests;
    LibraryDslTest.tests;
    CilfacadeTest.tests;
    (* etc *)
    "domaintest" >::: QCheck_ounit.to_ounit2_test_list Maindomaintest.all_testsuite;
    IntOpsTest.tests;
    (* SparseMatrixImplementationTest.tests; *) (* Uncomment this to add the sparse matrix tests to all tests *)
  ]

let subset_tests = "" >::: [SparseMatrixImplementationTest.tests]

let () =
  print_string "\027[0;1munit: \027[0;0;00m";
  run_test_tt_main subset_tests (* Remove this and uncomment the line below to run all tests.*)
 (* run_test_tt_main all_tests *)
