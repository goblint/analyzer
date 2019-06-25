open OUnit

module U = Testutils

let all_tests _ = ("" >:::
  [ NativeArrayDomainTest.test ();
    CollapsingArrayDomainTest.test ();
    IntDomainTest.test ();
    MapDomainTest.test ();
    PMapArrayDomainTest.test ();
    LMapArrayDomainTest.test ();
    SolverTest.test ();
    (* etc *)
  ])

let _ =
  (* first we need to load the default config which is done at the toplevel in Defaults *)
  let module Ignore = Defaults in
  let verbose = ref false in
  let set_verbose _ = verbose := true in
  Arg.parse
    [("-verbose", Arg.Unit set_verbose, "Run the test in verbose mode.");]
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    ("usage: " ^ Sys.argv.(0) ^ " [-verbose]");
  if not (U.was_successful (run_test_tt ~verbose:!verbose (all_tests ()))) then
    exit 1
