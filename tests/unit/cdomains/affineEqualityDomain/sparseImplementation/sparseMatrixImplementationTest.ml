(*
  To run this (and all other unit tests), type `dune runtest tests/unit/`. 
*)

open Goblint_lib
open OUnit2
open SparseVector
open ListMatrix

module D = SharedFunctions.Mpqf
module Vector = SparseVector (D)
module Matrix = ListMatrix (D) (SparseVector)
include RatOps.ConvenienceOps(D)

(** Shorthands for common functions. *)
let int x = D.of_int x

let frac numerator denominator = D.of_frac numerator denominator

(** Shorthands for common functions. *)
let float x = D.of_float x

let make_matrix_of_2d_list l =
  List.fold_left
    (fun acc row -> Matrix.append_row acc (Vector.of_list row))
    (Matrix.empty ())
    l

(** This function runs the equality assertion with the solution after normalizing the matrix. *)
let normalize_and_assert (matrix : Matrix.t) (solution : Matrix.t) =
  let get_dimensions m = (Matrix.num_rows m, Matrix.num_cols m) in
  let do_dimensions_match (r1, c1) (r2, c2) = r1 = r2 && c1 = c2 in
  let matrix_dim = get_dimensions matrix in
  let solution_dim = get_dimensions solution in
  if not (do_dimensions_match solution_dim matrix_dim) then
    failwith
      "The matrix to normalize and the solution have different dimensions!"
  else
    match Matrix.normalize matrix with
    | None -> assert_failure "The normalization returned None."
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
         [ (0, float (1. /. 4.)); (1, float 23.); (2, float 2.) ])
  in

  let normalized_matrix =
    Matrix.append_row
      (Matrix.append_row (Matrix.empty ())
         (Vector.of_sparse_list width [ (0, float 1.); (2, frac 1 10) ]))
      (Vector.of_sparse_list width [ (1, float 1.); (2, frac 79 920) ])
  in
  normalize_and_assert any_matrix normalized_matrix

let does_handle_fractions _ =
  let width = 3 in
  let any_matrix =
    Matrix.append_row
      (Matrix.append_row (Matrix.empty ())
         (Vector.of_sparse_list width [ (0, frac 5 1); (2, frac 1 2) ]))
      (Vector.of_sparse_list width
         [ (0, frac 1 4); (1, frac 23 1); (2, frac 2 1) ])
  in

  let normalized_matrix =
    Matrix.append_row
      (Matrix.append_row (Matrix.empty ())
         (Vector.of_sparse_list width [ (0, frac 1 1); (2, frac 1 10) ]))
      (Vector.of_sparse_list width [ (1, frac 1 1); (2, frac 79 920) ])
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

let is_covered_by_simple _ =
  let m1 = Matrix.init_with_vec (Vector.of_list [int 1; int 1; int 2; int 10]) in
  let m2 = Matrix.append_row 
      (Matrix.append_row 
         (Matrix.empty ()) 
         (Vector.of_list [int 1; int 1; int 0; int 6])) 
      (Vector.of_list [int 0; int 0; int 1; int 2]) in
  let result = Matrix.is_covered_by m1 m2 in
  assert_bool "Matrix m1 is covered by m2, but was false" result

let is_covered_by_vector_first_row _ =
  let m1 = Matrix.init_with_vec (Vector.of_list [int 1; int 2; int 0; int 7]) in
  let m2 = Matrix.append_row 
      (Matrix.append_row 
         (Matrix.empty ()) 
         (Vector.of_list [int 1; int 2; int 0; int 7])) 
      (Vector.of_list [int 0; int 0; int 1; int 2]) in
  let result = Matrix.is_covered_by m1 m2 in
  assert_bool "Matrix m1 is covered by m2, but was false" result

let is_zero_vec_covered _ = 
  let m1 = Matrix.init_with_vec (Vector.zero_vec 4) in
  let m2 = Matrix.append_row 
      (Matrix.append_row 
         (Matrix.empty ()) 
         (Vector.of_list [int 1; int 2; int 0; int 7])) 
      (Vector.of_list [int 0; int 0; int 1; int 2]) in
  let result = Matrix.is_covered_by m1 m2 in
  assert_bool "Matrix m1 is covered by m2, but was false" result

let is_not_covered _ =
  let m1 = Matrix.init_with_vec (Vector.of_list [int 1; int 1; int 2; int 10]) in
  let m2 = Matrix.append_row 
      (Matrix.append_row 
         (Matrix.empty ()) 
         (Vector.of_list [int 1; int 1; int 0; int 6])) 
      (Vector.of_list [int 0; int 0; int 1; int 3]) in
  let result = Matrix.is_covered_by m2 m1 in
  assert_bool "Matrix m1 is not covered by m2, but was true" (not result)

let is_covered_big _ =
  let m1 = make_matrix_of_2d_list @@
    [[int 1; int 0; int 0; int 0; int 0; int (-1); int 0];
     [int 0; int 1; int 0; int 0; int 0; int (-2); int 0];
     [int 0; int 0; int 1;  (frac (-1) 3); frac 1 3; int 0; frac 1 3]] in

  let m2 = make_matrix_of_2d_list @@
    [[int 1; int 0; int 0; int 0; int 0; int 0; int 0];
     [int 0; int 1; int 0; int 0; int 0; int 0; int 0];
     [int 0; int 0; int 1; frac (-1) 3; frac 1 3; int 0; frac 1 3];
     [int 0; int 0; int 0; int 0; int 0; int 1; int 0]] in

  let result = Matrix.is_covered_by m1 m2 in
  assert_bool "Matrix m1 is covered by m2, but was false" (result)

let is_covered_big2 _ = 
  let m1 = make_matrix_of_2d_list @@
    [[int 1; int 0; int 0; int 0; int 0; int 1; int 0]
    ] in

  let m2 = make_matrix_of_2d_list @@
    [[int 1; int 0; int 0; int 0; int 0; int 0; int 0];
     [int 0; int 1; int 0; int 0; int 0; int 0; int 0];
     [int 0; int 0; int 0; int 0; int 0; int 1; int 0]] in

  let result = Matrix.is_covered_by m1 m2 in
  assert_bool "Matrix m1 is covered by m2, but was false" (result)
(**
   Normalization works on an empty matrix.
*)
let normalize_empty _ =
  let width = 3 in
  let empty_matrix =
    Matrix.append_row
      (Matrix.append_row
         (Matrix.append_row (Matrix.empty ()) (Vector.zero_vec width))
         (Vector.zero_vec width))
      (Vector.zero_vec width)
  in
  normalize_and_assert empty_matrix empty_matrix

let normalize_two_columns _ =
  let width = 2 in
  let two_col_matrix =
    Matrix.append_row
      (Matrix.append_row (Matrix.empty ())
         (Vector.of_sparse_list width [ (0, int 3); (1, int 2) ]))
      (Vector.of_sparse_list width [ (0, int 9); (1, int 6) ])
  in
  let normalized_matrix =
    Matrix.append_row
      (Matrix.append_row (Matrix.empty ())
         (Vector.of_sparse_list width
            [ (0, int 1); (1, D.div (int 2) (int 3)) ]))
      (Vector.of_sparse_list width [])
  in
  normalize_and_assert two_col_matrix normalized_matrix

let int_domain_to_rational _ =
  let width = 3 in
  let int_matrix =
    Matrix.append_row
      (Matrix.append_row (Matrix.empty ())
         (Vector.of_sparse_list width [ (0, int 3); (2, int 1) ]))
      (Vector.of_sparse_list width [ (0, int 2); (1, int 1); (2, int 1) ])
  in
  let normalized_matrix =
    Matrix.append_row
      (Matrix.append_row (Matrix.empty ())
         (Vector.of_sparse_list width
            [ (0, int 1); (2, D.div (int 1) (int 3)) ]))
      (Vector.of_sparse_list width [ (1, int 1); (2, D.div (int 1) (int 3)) ])
  in
  normalize_and_assert int_matrix normalized_matrix


let vectorMap2i _ =
  let v1 = Vector.of_list [int 0; int 1; int 0; int 2; int 3; int 0; int 4; int 0; int 1] in
  let v2 = Vector.of_list [int 4; int 0; int 0; int 0; int 5; int 6; int 0; int 0; int 2] in
  let result = Vector.map2i (fun i x y -> (int i) *: (x +: y)) v1 v2 in
  let expected = Vector.of_list [int 0; int 1; int 0; int 6; int 32; int 30; int 24; int 0; int 24] in
  assert_equal expected result


let vectorMap2i_empty _ = 
  let v1 = Vector.of_list [] in
  let v2 = Vector.of_list [] in
  let result = Vector.map2i (fun i x y -> (int i) *: (x +: y)) v1 v2 in
  let expected = Vector.of_list [] in
  assert_equal expected result

let vectorMap2i_one_zero _ = 
  let v1 = Vector.of_list [int 0; int 0; int 0; int 0] in
  let v2 = Vector.of_list [int 1; int 2; int 3; int 4] in
  let result = Vector.map2i (fun i x y -> (int i) *: (x +: y)) v1 v2 in
  let expected = Vector.of_list [int 0; int 2; int 6; int 12] in
  assert_equal expected result


let vectorMap_zero_preserving_normal _ =
  let v1 = Vector.of_list [int 0; int 1; int 2; int 0; int 0; int 4; int 5; int 0; int 0;] in
  let result = Vector.map_f_preserves_zero (fun x -> x *: x) v1 in
  let expected = Vector.of_list [int 0; int 1; int 4; int 0; int 0; int 16; int 25; int 0; int 0;] in
  assert_equal expected result


let get_col_upper_triangular _ =
  let m = make_matrix_of_2d_list @@
    [[int 1; int 0; int 0; int 0; int 0; int 0; int 0];
     [int 0; int 1; int 0; int 0; int 0; int 0; int 0];
     [int 0; int 0; int 1; frac (-1) 3; int 0; frac 1 3; int 1]] in

  let result = Matrix.get_col_upper_triangular m 5 in

  let expected = Vector.of_list [int 0; int 0; frac 1 3] in

  assert_equal expected result

let test () =
  "SparseMatrixImplementationTest"
  >::: [
    "can solve a standard normalization" >:: standard_normalize;
    "does sort already reduzed" >:: does_just_sort;
    "does eliminate dependent rows" >:: does_eliminate_dependent_rows;
    "can handle float domain" >:: does_handle_floats;
    "can handle fraction domain" >:: does_handle_fractions;
    "does negate negative matrix" >:: does_negate_negative;
    "does not change already normalized matrix" >:: does_not_change_normalized_matrix;
    "m1 is covered by m2" >:: is_covered_by_simple;
    "m1 is covered by m2 with vector in first row" >:: is_covered_by_vector_first_row;
    "zero vector is covered by m2" >:: is_zero_vec_covered;
    "m1 is not covered by m2" >:: is_not_covered;
    "m1 is covered by m2 with big matrix" >:: is_covered_big;
    "does not change an empty matrix" >:: normalize_empty;
    "can correctly normalize a two column matrix" >:: normalize_two_columns;
    "can handle a rational solution" >:: int_domain_to_rational;
    "m1 is covered by m2 with big matrix2" >:: is_covered_big2;
    "map2i two vectors" >:: vectorMap2i;
    "map2i two empty vectors" >:: vectorMap2i_empty;
    "map2i one zero vector" >:: vectorMap2i_one_zero;
    "map zero preserving normal" >:: vectorMap_zero_preserving_normal;
    "get column when matrix in rref" >:: get_col_upper_triangular;
  ]