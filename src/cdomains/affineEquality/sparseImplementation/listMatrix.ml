open AbstractMatrix
open AbstractVector
open RatOps
open ConvenienceOps

open BatList
module List = BatList

(** Sparse matrix implementation.
    It provides a normalization function to reduce a matrix into reduced row echelon form.
    Operations exploit that the input matrix/matrices are in reduced row echelon form already. *)
module ListMatrix: AbstractMatrix =
  functor (A: RatOps) (V: AbstractVector) ->
  struct
    include ConvenienceOps(A)
    module V = V(A)

    type t = V.t list (*List of rows*)
    [@@deriving eq, ord, hash]

    let show x =
      List.fold_left (^) "" (List.map (fun x -> (V.show x)) x)

    let empty () = []

    let num_rows = List.length

    let is_empty m =
      num_rows m = 0
    (*This should be different if the implimentation is sound*)
    (*m.column_count = 0*)

    let num_cols m = if m = [] then 0 else V.length (hd m)

    let copy m = m
    (* Lists are immutable, so this should suffice? A.t is mutuable currently, but is treated like its not in ArrayMatrix*)

    let copy m =
      Timing.wrap "copy" (copy) m

    let add_empty_columns m cols = 
      let () = Printf.printf "Before add_empty_columns m:\n%s\n" (show m) in
      let cols = Array.to_list cols in 
      let sorted_cols = List.sort Stdlib.compare cols in
      let rec count_sorted_occ acc cols last count = 
        match cols with
        | [] -> acc
        | (x :: xs) when x = last -> count_sorted_occ acc xs x (count + 1)
        | (x :: xs) -> count_sorted_occ ((last, count) :: acc) xs x 1
      in
      let occ_cols = count_sorted_occ [] sorted_cols (-1) 0 in
      let () = Printf.printf "After add_empty_columns m:\n%s\n" (show (List.map (fun row -> V.insert_zero_at_indices row occ_cols) m)) in
      List.map (fun row -> V.insert_zero_at_indices row occ_cols) m

    let add_empty_columns m cols =
      Timing.wrap "add_empty_cols" (add_empty_columns m) cols

    let append_row m row  =
      let () = Printf.printf "Before append_row m:\n%s\n" (show m) in
      let () = Printf.printf "After append_row m:\n%s\n" (show ( m @ [row])) in
      m @ [row]

    let get_row m n =
      List.nth m n

    let remove_row m n =
      let () = Printf.printf "Before remove_row %i of m:\n%s\n" n (show m) in
      List.remove_at n m

    let get_col m n =
      let () = Printf.printf "get_col %i of m:\n%s\n%s\n" n (show m) (V.show (V.of_list @@ List.map (fun row -> V.nth row n) m)) in
      V.of_list @@ List.map (fun row -> V.nth row n) m (* builds full col including zeros, maybe use sparselist instead? *)

    let get_col m n =
      Timing.wrap "get_col" (get_col m) n

    let set_col m new_col n = 
      let () = Printf.printf "Before set_col m:\n%s\n" (show m) in
      (* List.mapi (fun row_idx row -> V.set_nth row n (V.nth new_col row_idx)) m *)
      let () = Printf.printf "After set_col m:\n%s\n" (show (List.map2 (fun row value -> V.set_nth row n value) m (V.to_list new_col))) in
      List.map2 (fun row value -> V.set_nth row n value) m (V.to_list new_col)

    let append_matrices m1 m2  = (* keeps dimensions of first matrix, what if dimensions differ?*)
      m1 @ m2

    let equal m1 m2 = Timing.wrap "equal" (equal m1) m2

    let sub_rows (minu : V.t) (subt : V.t) : V.t =
      V.map2_f_preserves_zero (-:) minu subt

    let div_row (row : V.t) (pivot : A.t) : V.t =
      V.map_f_preserves_zero (fun a -> a /: pivot) row

    let swap_rows m j k =
      List.mapi (fun i row -> if i = j then List.nth m k else if i = k then List.nth m j else row) m

    let sub_scaled_row row1 row2 s =
      V.map2_f_preserves_zero (fun x y -> x -: s *: y) row1 row2

    let reduce_col m j = 
      let () = Printf.printf "Matrix: reduce_col %i of m:\n%s\n" j (show m) in
      if is_empty m then m 
      else
        let rec find_pivot idx entries = (* Finds non-zero element in column j and returns pair of row idx and the pivot value *)
          match entries with
          | [] -> None
          | row :: rest -> let value = V.nth row j in 
            if value =: A.zero then find_pivot (idx - 1) rest else Some (idx, value)
        in
        match (find_pivot (num_rows m - 1) (List.rev m)) with
        | None -> m (* column is already filled with zeroes *)
        | Some (row_idx, pivot) -> 
          let pivot_row = List.nth m row_idx in
          List.mapi (fun idx row ->
              if idx = row_idx then 
                V.zero_vec (num_cols m)
              else
                let row_value = V.nth row j in 
                if row_value = A.zero then row
                else (let s = row_value /: pivot in
                      sub_scaled_row row pivot_row s)            
            ) m

    let reduce_col_with_vec m j v = 
      let () = Printf.printf "Before reduce_col_with_vec %i with vec %s of m:\n%s\n" j (V.show v) (show m) in
      let pivot_element = V.nth v j in
      if pivot_element = A.zero then m
      else List.mapi (fun idx row ->
          let row_value = V.nth row j in 
          if row_value = A.zero then row
          else (let s = row_value /: pivot_element in
                V.map2_f_preserves_zero (fun x y -> x -: s *: y) row v)            
        ) m

    let del_col m j =
      if num_cols m = 1 then empty () 
      else 
        List.map (fun row -> V.remove_nth row j) m

    let del_cols m cols =
      let cols = Array.to_list cols in (* TODO: Is it possible to use list for Apron dimchange? *)
      let sorted_cols = List.sort_uniq Stdlib.compare cols in (* Apron Docs:  Repetitions are meaningless (and are not correct specification) *)
      if (List.length cols) = num_cols m then empty() 
      else
        List.map (fun row -> V.remove_at_indices row sorted_cols) m

    let del_cols m cols = Timing.wrap "del_cols" (del_cols m) cols

    let map2i f m v =
      let () = Printf.printf "Before map2i m:\n%sv:%s\n" (show m) (V.show v) in
      let rec map2i_min i acc m v =
        match m, v with
        | [], _  -> List.rev acc
        | row :: rs, [] -> List.rev_append (row :: acc) rs
        | row :: rs, value :: vs -> map2i_min (i + 1) (f i row value :: acc) rs vs
      in  
      let () = Printf.printf "After map2i m:\n%s\n" (show (map2i_min 0 [] m (V.to_list v))) in
      map2i_min 0 [] m (V.to_list v)

    let remove_zero_rows m =
      List.filter (fun row -> not (V.is_zero_vec row)) m

    let init_with_vec v =
      [v]

    let get_pivot_positions m = 
      List.mapi (fun i row -> V.findi (fun z -> z =: A.one) row) m

    let sub_rows (minu : V.t) (subt : V.t) : V.t =
      V.map2_f_preserves_zero (-:) minu subt

    let div_row (row : V.t) (pivot : A.t) : V.t =
      V.map_f_preserves_zero (fun a -> a /: pivot) row

    let swap_rows m j k =
      List.mapi (fun i row -> if i = j then List.nth m k else if i = k then List.nth m j else row) m

    let normalize m =
      let () = Printf.printf "Before normalizing we have m:\n%s" (show m) in
      let col_count = num_cols m in
      let dec_mat_2D (m : t) (row_idx : int) (col_idx : int) : t = 
        List.filteri_map (fun i row -> if i < row_idx then None else Some (V.starting_from_nth col_idx row)) m
      in
      (* Function for finding first pivot in an extracted part of the matrix (row_idx and col_idx indicate which part of the original matrix) *)
      let find_first_pivot m' row_idx col_idx =
        if col_idx >= col_count then None else 
          (* Finding pivot by extracting the minimum column index of the first non zero value of each row*)
          let (piv_row, piv_col, piv_val) = List.fold_lefti (fun (cur_row, cur_col, cur_val) i row  -> 
              let row_first_non_zero = V.findi_val_opt ((<>:) A.zero) row in
              match row_first_non_zero with
              | None -> (cur_row, cur_col, cur_val)
              | Some (idx, value) -> let () = Printf.printf "We found first non-zero at index %i in row %i\n" idx i in
                if idx < cur_col then (row_idx + i, idx, value) else (cur_row, cur_col, cur_val)
            ) (num_rows m', num_cols m', A.zero) m' (* Initializing with max, so num_cols m indicates that pivot is not found *)
          in
          if piv_col = (num_cols m') then None else Some (piv_row, piv_col + col_idx, piv_val)
      in
      let affeq_rows_are_valid m = (* Check if the semantics of an rref-affeq matrix are correct *)
        let col_count = num_cols m in
        let row_is_valid row = (* TODO: Vector findi_opt *)
          match V.findi_val_opt ((<>:) A.zero) row with
          | Some (idx, _) -> if idx < col_count - 1 then true else false (* If all cofactors of the affeq are zero, but the constant is non-zero, the row is invalid *)
          | None -> true (* Full zero row is valid *)
        in
        List.for_all row_is_valid m in
      let rec main_loop m m' row_idx col_idx = 
        if col_idx = (col_count - 2) then m  (* In this case the whole bottom of the matrix starting from row_index is Zero, so it is normalized *)
        else
          match find_first_pivot m' row_idx col_idx with
          | None -> m (* No pivot found means already normalized*)
          | Some (piv_row_idx, piv_col_idx, piv_val) -> (
              let () = Printf.printf "The current matrix is: \n%s and the pivot is (%i, %i, %s)\n" (show m) piv_row_idx piv_col_idx (A.to_string piv_val) in
              let m = if piv_row_idx <> row_idx then swap_rows m row_idx piv_row_idx else m in
              let normalized_m = List.mapi (fun idx row -> if idx = row_idx then div_row row piv_val else row) m in
              let piv_row = (List.nth normalized_m row_idx) in
              let subtracted_m = List.mapi (fun idx row -> if idx <> row_idx then 
                                               let scale = V.nth row piv_col_idx in
                                               sub_scaled_row row piv_row scale else row) normalized_m in
              let m' = dec_mat_2D m (row_idx + 1) (piv_col_idx + 1) in
              main_loop subtracted_m m' (row_idx + 1) (piv_col_idx + 1)) (* We start at piv_col_idx + 1 because every other col before that is zero at the bottom*)
      in 
      let m' = main_loop m m 0 0 in
      let () = Printf.printf "After normalizing we have m:\n%s" (show m') in
      if affeq_rows_are_valid m' then Some m' else None (* TODO: We can check this for each row, using the helper function row_is_invalid *)


    (*This function yields the same result as appending vector v to m and normalizing it afterwards would. However, it is usually faster than performing those ops manually.*)
    (*m must be in rref form and contain the same num of cols as v*)
    (*If m is empty then v is simply normalized and returned*)
    (* TODO: OPTIMIZE! *)
    let rref_vec m v =
      normalize @@ append_matrices m (init_with_vec v)

    (*Similar to rref_vec_with but takes two matrices instead.*)
    (*ToDo Could become inefficient for large matrices since pivot_elements are always recalculated + many row additions*)
    (*TODO: OPTIMIZE!*)
    let rref_matrix m1 m2 =
      normalize @@ append_matrices m1 m2


    let delete_row_with_pivots row pivots m2 = 
      failwith "TODO"

    (* Assumes that the first row of the matrix is already the pivot row fitting to the vector. *)
    let rec is_linearly_independent_rref v m = 
      match m with
      | [] -> not @@ V.is_zero_vec v
      | x::xs ->
        let pivot_opt = V.findi_val_opt ((<>:) A.zero) v in
        match pivot_opt with
        | None -> false (* When we found no pivot, the vector is already A.zero. *)
        | Some (pivot_id, pivot) ->
          let new_v = V.map2_f_preserves_zero (fun v1 v2 -> v1 -: (pivot *: v2)) v x in
          is_linearly_independent_rref new_v xs

    let is_covered_by m1 m2 =
      if num_rows m1 > num_rows m2 then false else
        let rec is_covered_by_helper m1 m2 = 
          match m1 with 
          | [] -> true
          | v1::vs1 -> 
            let first_non_zero = V.findi_val_opt ((<>:) A.zero) v1 in
            match first_non_zero with
            | None -> true  (* vs1 must also be zero-vectors because of rref *)
            | Some (idx, _) -> 
              let m' = List.drop_while (fun v2 -> 
                  match V.findi_val_opt ((<>:) A.zero) v2 with
                  | None -> true  (* In this case, m2 only has zero rows after that *)
                  | Some (idx', _) -> idx' < idx
                ) m2 in (* Only consider the part of m2 where the pivot is at a position useful for deleting first_non_zero of v1*)
              let linearly_indep = is_linearly_independent_rref v1 m' in 
              if linearly_indep then false else is_covered_by_helper vs1 m'
        in is_covered_by_helper m1 m2

    let is_covered_by m1 m2 = Timing.wrap "is_covered_by" (is_covered_by m1) m2

    let find_opt f m =
      List.find_opt f m

    let map2 f m v =
      let () = Printf.printf "Before map2 we have m:\n%s\n" (show m) in
      let vector_length = V.length v in
      let () = Printf.printf "After map2 we have m:\n%s\n" (show (List.mapi (fun index row -> if index < vector_length then f row (V.nth v index) else row ) m)) in
      List.mapi (fun index row -> if index < vector_length then f row (V.nth v index) else row ) m


    (* ------------------------- Deprecated ------------------------*)

    let rref_vec_with m v =
      (*This function yields the same result as appending vector v to m and normalizing it afterwards would. However, it is usually faster than performing those ops manually.*)
      (*m must be in rref form and contain the same num of cols as v*)
      (*If m is empty then v is simply normalized and returned*)
      failwith "deprecated"

    let rref_vec_with m v = Timing.wrap "rref_vec_with" (rref_vec_with m) v
    let rref_with m =
      failwith "deprecated"

    let reduce_col_with m j =
      failwith "deprecated"

    let reduce_col_with m j  = Timing.wrap "reduce_col_with" (reduce_col_with m) j


    let rref_with m = Timing.wrap "rref_with" rref_with m

    let normalize_with m = 
      failwith "deprecated"

    let normalize_with m = Timing.wrap "normalize_with" normalize_with m


    let set_col_with m new_col n =
      failwith "deprecated"

    let set_col_with m new_col n = Timing.wrap "set_col" (set_col_with m new_col) n

    let map2_with f m v =
      failwith "deprecated"

    let map2_with f m v = Timing.wrap "map2_with" (map2_with f m) v

    let map2i_with f m v =
      failwith "deprecated"

    let map2i_with f m v = Timing.wrap "map2i_with" (map2i_with f m) v

    let rref_matrix_with m1 m2 =
      (*Similar to rref_vec_with but takes two matrices instead.*)
      (*ToDo Could become inefficient for large matrices since pivot_elements are always recalculated + many row additions*)
      failwith "deprecated"

    let rref_matrix_with m1 m2 = Timing.wrap "rref_matrix_with" (rref_matrix_with m1) m2

  end