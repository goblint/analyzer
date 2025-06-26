(* To run this (and all other unit tests), type `dune runtest tests/unit/`. *)
open OUnit2
open Goblint_lib
open Batteries
open PentagonDomain


let stringing _ =
  let s = "{ boxes = ( ) { 0->[1, 1]; 1->[2, 2]; 2->[-I32, +I32] }; sub = (⊤) { 0->∅; 1->{0#, 2#}; 2->{0#} } }" in
  let expected = "{ boxes = ( ) { one->[1, 1]; two->[2, 2]; three->[-I32, +I32] }; sub = (⊤) { one->∅; two->{one, three}; three->{one} } }" in
  let vars = [|"one"; "two"; "three" |] in
  (* First pass to adjust boxes. *)
  let boxes_re = Str.regexp {|\([0-9]+\)->|} in
  let subs_re = Str.regexp {|\([0-9]+\)#|} in
  Str.global_substitute boxes_re (
    fun m -> 
      let idx = int_of_string (Str.matched_group 1 m) in
      if idx < Array.length vars then
        (vars.(idx) ^ "->")
      else
        failwith "D.to_string hit unknown variable!"
  ) s |>
  (* Second pass to adjust subs. *)
  Str.global_substitute subs_re (
    fun m -> 
      let idx = int_of_string (Str.matched_group 1 m) in
      if idx < Array.length vars then
        vars.(idx)
      else
        failwith "D.to_string hit unknown variable!"
  ) |>
  assert_equal expected

let noop _ = assert_bool "" true

let test () =
  "PentagonTests"
  >::: [
    "noop" >:: noop;
    "string" >:: stringing;
  ]
