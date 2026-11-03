open OUnit2
open GoblintCil

module U = LoopUnrolling0

let with_copyof f _ =
  U.reset_copyof ();
  Fun.protect ~finally:U.reset_copyof f

let test_numbering () =
  let original = mkEmptyStmt () in
  let copy = mkEmptyStmt () in
  let nested_copy = mkEmptyStmt () in
  let unrelated = mkEmptyStmt () in
  let pairs = [copy, original; nested_copy, copy] in
  let fd = emptyFunction "test" in
  fd.sbody <- mkBlock [original; copy; nested_copy; unrelated];
  computeCFGInfo fd true;
  U.rebuild_copyof pairs;
  assert_bool "immediate provenance is preserved" (match U.find_copyof nested_copy with Some s -> s == copy | None -> false);
  assert_bool "nested copies resolve to the original" (U.find_original nested_copy == original);
  assert_bool "identical syntax is not identity" (U.find_original unrelated == unrelated);
  U.reset_copyof ();
  U.rebuild_copyof [];
  assert_bool "a new file discards old provenance" (U.find_original copy == copy)

let tests =
  "loopUnrollingTest" >::: [
    "numbering" >:: with_copyof test_numbering;
  ]
