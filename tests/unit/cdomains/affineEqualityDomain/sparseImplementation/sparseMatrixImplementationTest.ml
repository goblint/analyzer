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

(** This function runs the equality assertion with the solution after normalizing the matrix. *)
let normalize_and_assert (matrix : Matrix.t) (solution : Matrix.t) =
  let get_dimensions m = (Matrix.num_rows m, Matrix.num_cols m) in
  let do_dimensions_match dim1 dim2 =
    match (dim1, dim2) with (r1, c1), (r2, c2) -> r1 == r2 && c1 == c2
  in
  let matrix_dim = get_dimensions matrix in
  let solution_dim = get_dimensions solution in
  if not (do_dimensions_match solution_dim matrix_dim) then
    failwith
      "The matrix to normalize and the solution have different dimensions!"
  else
    match Matrix.normalize matrix with
    | None ->
        assert_failure "The matrix is normalizable but was not normalized!"
    | Some reduced_matrix -> assert_equal reduced_matrix solution

(**
  Example from a [Youtube video](https://www.youtube.com/watch?v=TYs4h-AoqyQ)
  but extended by a solution vector b = [0;0;25]^T.
*)
let standard_normalize _ =
  let width = 5 in
  let int x = D.of_int x in
  let any_matrix =
    Matrix.append_row
      (Matrix.append_row
         (Matrix.append_row (Matrix.empty ())
            (Vector.of_sparse_list width
               [ (0, int 2); (2, int 4); (3, int (-1)) ]))
         (Vector.of_sparse_list width [ (0, int 2); (1, int 3); (2, int 4) ]))
      (Vector.of_sparse_list width
         [ (0, int 1); (1, int 1); (2, int 2); (3, int 4); (4, int 25) ])
  in
  let normalized_matrix =
    Matrix.append_row
      (Matrix.append_row
         (Matrix.append_row (Matrix.empty ())
            (Vector.of_sparse_list width [ (0, int 1); (2, int 2); (4, int 3) ]))
         (Vector.of_sparse_list width [ (1, int 1); (4, int (-2)) ]))
      (Vector.of_sparse_list width [ (3, int 1); (4, int 6) ])
  in
  normalize_and_assert any_matrix normalized_matrix

let should_just_sort_normalize _ =
  let width = 10 in
  let int x = D.of_int x in
  let chaotic_matrix =
    Matrix.append_row
      (Matrix.append_row
         (Matrix.append_row
            (Matrix.append_row (Matrix.empty ())
               (Vector.of_sparse_list width [ (2, int 1) ]))
            (Vector.of_sparse_list width [ (5, int 1) ]))
         (Vector.of_sparse_list width [ (0, int 1); (3, int 1) ]))
      (Vector.of_sparse_list width [ (1, int 1); (4, int (-3)) ])
  in

  let sorted_matrix =
    Matrix.append_row
      (Matrix.append_row
         (Matrix.append_row
            (Matrix.append_row (Matrix.empty ())
               (Vector.of_sparse_list width [ (0, int 1); (3, int 1) ]))
            (Vector.of_sparse_list width [ (1, int 1); (4, int (-3)) ]))
         (Vector.of_sparse_list width [ (2, int 1) ]))
      (Vector.of_sparse_list width [ (5, int 1) ])
  in
  normalize_and_assert chaotic_matrix sorted_matrix

let tests =
  "SparseMatrixImplementationTest"
  >::: [
         "standard" >:: standard_normalize;
         "should_sort" >:: should_just_sort_normalize;
       ]

let () = run_test_tt_main tests
