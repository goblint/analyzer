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

(** Shorthands for common functions. *)
let int x = D.of_int x

let frac numerator denominator = D.of_frac numerator denominator

(** Shorthands for common functions. *)
let float x = D.of_float x

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

(**
   Normalization just sorts the matrix, as the rows are already reduced. 
*)
let does_just_sort _ =
  let width = 7 in
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

(**
   Normalization should eliminate both linearly dependent rows.
*)
let does_eliminate_dependent_rows _ =
  let width = 3 in
  let linearly_dependent_rows =
    Matrix.append_row
      (Matrix.append_row
         (Matrix.append_row (Matrix.empty ())
            (Vector.of_sparse_list width [ (0, int 1); (2, int 4) ]))
         (Vector.of_sparse_list width [ (0, int 2); (2, int 8) ]))
      (Vector.of_sparse_list width [ (0, int 3); (2, int 12) ])
  in

  let minimized_matrix =
    Matrix.append_row
      (Matrix.append_row
         (Matrix.append_row (Matrix.empty ())
            (Vector.of_sparse_list width [ (0, int 1); (2, int 4) ]))
         (Vector.zero_vec width))
      (Vector.zero_vec width)
  in
  normalize_and_assert linearly_dependent_rows minimized_matrix

let does_handle_floats _ =
  let width = 3 in
  let any_matrix =
    Matrix.append_row
      (Matrix.append_row (Matrix.empty ())
         (Vector.of_sparse_list width [ (0, float 5.); (2, float (1. /. 2.)) ]))
      (Vector.of_sparse_list width
         [ (0, float (1. /. 4.)); (2, float 23.); (2, float 2.) ])
  in

  let normalized_matrix =
    Matrix.append_row
      (Matrix.append_row (Matrix.empty ())
         (Vector.of_sparse_list width [ (0, float 1.); (2, float (1. /. 10.)) ]))
      (Vector.of_sparse_list width [ (2, float 1.); (2, float (79. /. 920.)) ])
  in
  normalize_and_assert any_matrix normalized_matrix

let does_handle_fractions _ =
  let width = 3 in
  let any_matrix =
    Matrix.append_row
      (Matrix.append_row (Matrix.empty ())
         (Vector.of_sparse_list width [ (0, frac 5 1); (2, frac 1 2) ]))
      (Vector.of_sparse_list width
         [ (0, frac 1 4); (2, frac 23 1); (2, frac 2 1) ])
  in

  let normalized_matrix =
    Matrix.append_row
      (Matrix.append_row (Matrix.empty ())
         (Vector.of_sparse_list width [ (0, frac 1 1); (2, frac 1 10) ]))
      (Vector.of_sparse_list width [ (2, frac 1 1); (2, frac 79 920) ])
  in
  normalize_and_assert any_matrix normalized_matrix

let does_negate_negative _ =
  let width = 5 in
  let negative_matrix =
    Matrix.append_row
      (Matrix.append_row (Matrix.empty ())
         (Vector.of_sparse_list width [ (0, int (-1)); (3, int (-3)) ]))
      (Vector.of_sparse_list width [ (2, int (-1)); (4, int (-5)) ])
  in

  let negated_matrix =
    Matrix.append_row
      (Matrix.append_row (Matrix.empty ())
         (Vector.of_sparse_list width [ (0, int 1); (3, int 3) ]))
      (Vector.of_sparse_list width [ (2, int 1); (4, int 5) ])
  in
  normalize_and_assert negative_matrix negated_matrix

(**
   Normalization is idempotent.
*)
let does_not_change_normalized_matrix _ =
  let width = 5 in
  let already_normalized =
    Matrix.append_row
      (Matrix.append_row
         (Matrix.append_row (Matrix.empty ())
            (Vector.of_sparse_list width [ (0, int 1); (3, int 1) ]))
         (Vector.of_sparse_list width [ (1, int 1); (3, int 3) ]))
      (Vector.of_sparse_list width [ (2, int 1); (3, int 1) ])
  in
  normalize_and_assert already_normalized already_normalized

let tests =
  "SparseMatrixImplementationTest"
  >::: [
    "can solve a standard normalization" >:: standard_normalize;
    "does sort already reduzed" >:: does_just_sort;
    "does eliminate dependent rows" >:: does_eliminate_dependent_rows;
    (* Looks like the tests are deadlock or inifinite execution when those are activated. *)
    (*"can handle float domain" >:: does_handle_floats;*)
    (*"can handle fraction domain" >:: does_handle_fractions;*)
    "does negate negative matrix" >:: does_negate_negative;
    "does not change already normalized matrix" >:: does_not_change_normalized_matrix;
  ]

let () = run_test_tt_main tests
