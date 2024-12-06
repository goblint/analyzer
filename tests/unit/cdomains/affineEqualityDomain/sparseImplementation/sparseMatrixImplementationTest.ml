(*
  To run this, type `dune runtest tests/unit/`. 
*)

open Goblint_lib
open OUnit2
open SparseVector
open ListMatrix
module D = SharedFunctions.Mpqf
module Vector = SparseVector (D)
module Matrix = ListMatrix (D) (SparseVector)

let normalize _ =
  let width = 4 in
  let int x = D.of_int x in
  let any_matrix =
    Matrix.append_row
      (Matrix.append_row
         (Matrix.append_row 
            (Matrix.empty ())
            (Vector.of_sparse_list width [ (0, int 2); (2, int 4); (3, int (-1)) ])
         )
         (Vector.of_sparse_list width [ (0, int 2); (1, int 3); (2, int 4) ])
      )
      (Vector.of_sparse_list width [ (0, int 1); (1, int 1); (2, int 2); (3, int 4) ]
      )
  in
  let normalized_matrix =
    Matrix.append_row
      (Matrix.append_row
         (Matrix.append_row 
            (Matrix.empty ())
            (Vector.of_sparse_list width [ (0,int 1); (2,int 2) ])
         )
         (Vector.of_sparse_list width [ (1,int 1) ])
      )
      (Vector.of_sparse_list width [ (3,int 1) ]
      )
  in
  match Matrix.normalize any_matrix with
  | None -> assert_failure "The matrix is normalizable but was not normalized!"
  | Some reduced_matrix -> assert_equal reduced_matrix normalized_matrix

let tests = "SparseMatrixImplementationTest" >::: [ "normalize" >:: normalize ]

let () =
  run_test_tt_main tests
