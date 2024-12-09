(*
  To run this, type `dune runtest tests/unit/`. 
*)

open Goblint_lib
open OUnit2
open SparseVector
open ListMatrix
open ArrayVector
open ArrayMatrix
module D = SharedFunctions.Mpqf
module Vector = SparseVector (D)
module Matrix = ListMatrix (D) (SparseVector)

let normalize _ =
  (*
    Example from a [Youtube video](https://www.youtube.com/watch?v=TYs4h-AoqyQ)
    but extended by a solution vector b = [0;0;25]^T.
  *)

  let width = 5 in
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
      (Vector.of_sparse_list width [ (0, int 1); (1, int 1); (2, int 2); (3, int 4); (4, int 25)]
      )
  in
  let normalized_matrix =
    Matrix.append_row
      (Matrix.append_row
         (Matrix.append_row 
            (Matrix.empty ())
            (Vector.of_sparse_list width [ (0,int 1); (2,int 2); (4, int 3)])
         )
         (Vector.of_sparse_list width [ (1,int 1); (4, int (-2)) ])
      )
      (Vector.of_sparse_list width [ (3,int 1); (4, int 6)]
      )
  in
  match Matrix.normalize any_matrix with
  | None -> assert_failure "The matrix is normalizable but was not normalized!"
  | Some reduced_matrix -> assert_equal reduced_matrix normalized_matrix

let tests = "SparseMatrixImplementationTest" >::: [ "normalize" >:: normalize ]

let () =
  run_test_tt_main tests
