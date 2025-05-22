(*
  To run this (and all other unit tests), type `dune runtest tests/unit/`. 
*)

open OUnit2
open Apron


let testing _ =
  assert_equal true true


let testing2 _ = assert_equal true true

let test () =
  "PentagonTest-Apron"
  >::: [
    "noop" >:: testing;
    "Title" >:: testing2;
  ]