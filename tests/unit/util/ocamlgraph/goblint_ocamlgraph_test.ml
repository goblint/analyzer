open OUnit2

let all_tests =
  "goblint.ocamlgraph" >::: [
    ColoringTest.tests;
  ]

let () =
  print_string "\027[0;1mgoblint.ocamlgraph: \027[0;0;00m";
  run_test_tt_main all_tests
