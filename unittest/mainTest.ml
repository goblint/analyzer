open OUnit

module U = Testutils

let all_tests _ = ("Tests for goblin" >:::
  [ NativeArrayDomainTest.test ();
    CollapsingArrayDomainTest.test ();
    IntDomainTest.test ();
    MapDomainTest.test ();
    PMapArrayDomainTest.test ();
    LMapArrayDomainTest.test ();
    (* etc *)
  ])


let _ =
  let verbose = ref false in
  let set_verbose _ = verbose := true in
  Arg.parse
    [("-verbose", Arg.Unit set_verbose, "Run the test in verbose mode.");]
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    ("usage: " ^ Sys.argv.(0) ^ " [-verbose]");
  if not (U.was_successful (run_test_tt ~verbose:!verbose (all_tests ()))) then
    exit 1
