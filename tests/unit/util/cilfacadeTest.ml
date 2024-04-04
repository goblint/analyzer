open Goblint_lib
open OUnit2
open Cilfacade

let test_split_anoncomp_name _ =
  let assert_equal = assert_equal ~printer:[%show: bool * string option * int] in
  assert_equal (false, Some "pthread_mutexattr_t", 488594144) (split_anoncomp_name "__anonunion_pthread_mutexattr_t_488594144");
  assert_equal (true, Some "__once_flag", 1234) (split_anoncomp_name "__anonstruct___once_flag_1234");
  assert_equal (false, None, 50) (split_anoncomp_name "__anonunion_50")

let tests =
  "cilfacadeTest" >::: [
    "split_anoncomp_name" >:: test_split_anoncomp_name;
  ]
