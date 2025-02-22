open OUnit2

(* Note: 
   There are currently no tests independent of apron.
   The .apron tests use apron's Mpqf module as an implementation of RatOps.
   There are currently no other RatOps implementations, so this file is empty.
*)
let test () =
  "SparseMatrixImplementationTest-No-Apron"
  >::: [
  ]