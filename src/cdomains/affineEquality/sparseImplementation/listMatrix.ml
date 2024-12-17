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

    (* This only works if Array.modifyi has been removed from dim_add *)
    let add_empty_columns (m : t) (cols : int array) : t =    
      let cols = Array.to_list cols in 
      let sorted_cols = List.sort Stdlib.compare cols in
      let rec count_sorted_occ acc cols last count =
        match cols with
        | [] -> if count > 0 then (last, count) :: acc else acc
        | x :: xs when x = last -> count_sorted_occ acc xs x (count + 1)
        | x :: xs -> let acc = if count > 0 then (last, count) :: acc else acc in
          count_sorted_occ acc xs x 1      
      in
      let occ_cols = List.rev @@ count_sorted_occ [] sorted_cols 0 0 in
      List.map (fun row -> V.insert_zero_at_indices row occ_cols (List.length cols)) m

    let add_empty_columns m cols =
      Timing.wrap "add_empty_cols" (add_empty_columns m) cols

    let append_row m row  =
      m @ [row]

    let get_row m n =
      List.nth m n

    let remove_row m n =
      List.remove_at n m

    let get_col m n =
      (*let () = Printf.printf "get_col %i of m:\n%s\n%s\n" n (show m) (V.show (V.of_list @@ List.map (fun row -> V.nth row n) m)) in*)
      V.of_list @@ List.map (fun row -> V.nth row n) m (* builds full col including zeros, maybe use sparselist instead? *)

    let get_col m n =
      Timing.wrap "get_col" (get_col m) n

    let set_col m new_col n = 
      (* List.mapi (fun row_idx row -> V.set_nth row n (V.nth new_col row_idx)) m *)
      List.map2 (fun row value -> V.set_nth row n value) m (V.to_list new_col)

    let append_matrices m1 m2  = (* keeps dimensions of first matrix, what if dimensions differ?*)
      m1 @ m2

    let equal m1 m2 = Timing.wrap "equal" (equal m1) m2

    let div_row (row : V.t) (pivot : A.t) : V.t =
      V.map_f_preserves_zero (fun a -> a /: pivot) row

    let swap_rows m j k =
      List.mapi (fun i row -> if i = j then List.nth m k else if i = k then List.nth m j else row) m

    let sub_scaled_row row1 row2 s =
      V.map2_f_preserves_zero (fun x y -> x -: (s *: y)) row1 row2

    (* Reduces the jth column with the last row that has a non-zero element in this column. *)
    let reduce_col m j = 
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
          let pivot_row = List.nth m row_idx in (* use the pivot row to reduce all entries in column j to zero, then "delete" the pivot row *)
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
      let pivot_element = V.nth v j in
      if pivot_element = A.zero then m
      else List.mapi (fun idx row ->
          let row_value = V.nth row j in 
          if row_value =: A.zero then row
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
      if (List.length sorted_cols) = num_cols m then empty() 
      else
        List.map (fun row -> V.remove_at_indices row sorted_cols) m

    let del_cols m cols = Timing.wrap "del_cols" (del_cols m) cols

    let map2i f m v =
      let rec map2i_min i acc m v =
        match m, v with
        | [], _  -> List.rev acc
        | row :: rs, [] -> List.rev_append (row :: acc) rs
        | row :: rs, value :: vs -> map2i_min (i + 1) (f i row value :: acc) rs vs
      in  
      map2i_min 0 [] m (V.to_list v)

    let remove_zero_rows m =
      List.filter (fun row -> not (V.is_zero_vec row)) m

    let init_with_vec v =
      [v]

    let normalize m =
      let col_count = num_cols m in
      let dec_mat_2D (m : t) (row_idx : int) (col_idx : int) : t = 
        List.filteri_map (fun i row -> if i < row_idx then None else Some (V.starting_from_nth col_idx row)) m
      in
      let dec_mat_2D m row_idx col_idx = Timing.wrap "dec_mat_2D" (dec_mat_2D m row_idx) col_idx in
      (* Function for finding first pivot in an extracted part of the matrix (row_idx and col_idx indicate which part of the original matrix) *)
      (* The last column represents the constant in the affeq *)
      let find_first_pivot m' row_idx col_idx =
        if col_idx >= col_count then None else 
          let max_piv_col_idx = num_cols m' - 2 in (* col at num_cols - 1 is the constant of the affeq *)
          (* Finding pivot by extracting the minimum column index of the first non zero value of each row*)
          let (piv_row, piv_col, piv_val) = List.fold_lefti (fun (cur_row, cur_col, cur_val) i row  -> 
              let row_first_non_zero = V.find_first_non_zero row in
              match row_first_non_zero with
              | None -> (cur_row, cur_col, cur_val)
              | Some (idx, value) -> (* let () = Printf.printf "We found first non-zero at index %i in row %i\n" idx i in *)
                if idx < cur_col then (i, idx, value) else (cur_row, cur_col, cur_val)
            ) (num_rows m', max_piv_col_idx + 1, A.zero) m' (* Initializing with max, so num_cols m indicates that pivot is not found *)
          in
          if piv_col = (max_piv_col_idx + 1) then None else Some (row_idx + piv_row, col_idx + piv_col, piv_val)
      in
      let find_first_pivot m' row_idx col_idx = Timing.wrap "find_first_pivot" (find_first_pivot m' row_idx) col_idx in
      let affeq_rows_are_valid m = (* Check if the semantics of an rref-affeq matrix are correct *)
        let col_count = num_cols m in
        let row_is_valid row = (* TODO: Vector findi_opt *)
          match V.find_first_non_zero row with
          | Some (idx, _) -> if idx < col_count - 1 then true else false (* If all cofactors of the affeq are zero, but the constant is non-zero, the row is invalid *)
          | None -> true (* Full zero row is valid *)
        in
        List.for_all row_is_valid m in
      let rec main_loop m m' row_idx col_idx = 
        if col_idx >= (col_count - 1) then m  (* In this case the whole bottom of the matrix starting from row_index is Zero, so it is normalized *)
        else
          match find_first_pivot m' row_idx col_idx with
          | None -> m (* No pivot found means already normalized*)
          | Some (piv_row_idx, piv_col_idx, piv_val) -> (
              (* let () = Printf.printf "The current matrix is: \n%s and the pivot is (%i, %i, %s)\n" (show m) piv_row_idx piv_col_idx (A.to_string piv_val) in *)
              let m = if piv_row_idx <> row_idx then swap_rows m row_idx piv_row_idx else m in
              let normalized_m = List.mapi (fun idx row -> if idx = row_idx then div_row row piv_val else row) m in
              let piv_row = (List.nth normalized_m row_idx) in
              let subtracted_m = List.mapi (fun idx row -> if idx <> row_idx then 
                                               let scale = V.nth row piv_col_idx in
                                               sub_scaled_row row piv_row scale else row) normalized_m in
              let m' = dec_mat_2D subtracted_m (row_idx + 1) (piv_col_idx + 1) in
              main_loop subtracted_m m' (row_idx + 1) (piv_col_idx + 1)) (* We start at piv_col_idx + 1 because every other col before that is zero at the bottom*)
      in 
      let m' = main_loop m m 0 0 in
      if affeq_rows_are_valid m' then Some m' else None (* TODO: We can check this for each row, using the helper function row_is_invalid *)

    (* This function return a tuple of row index and pivot position (column) in m *)
    (* TODO: maybe we could use a Hashmap instead of a list? *)
    let get_pivot_positions (m : t) : (int * int) list =
      List.rev @@ List.fold_lefti (
        fun acc i row -> match V.find_first_non_zero row with
          | None -> acc
          | Some (pivot_col, _) -> (i, pivot_col) :: acc
      ) [] m

    (* Inserts the vector v with pivot at piv_idx at the correct position in m. m has to be in rref form. *)
    let insert_v_according_to_piv m v piv_idx pivot_positions = 
      match List.find_opt (fun (row_idx, piv_col) -> piv_col > piv_idx) pivot_positions with
      | None -> append_row m v
      | Some (row_idx, _) -> let (before, after) = List.split_at row_idx m in (* TODO: Optimize *)
        before @ (v :: after)

    (* This function yields the same result as appending v to m, normalizing and removing zero rows would. *)
    (* m must be in rref form and must contain the same number of columns as v *)
    let rref_vec m v =
      if is_empty m then (* In this case, v is normalized and returned *)
        match V.find_first_non_zero v with 
        | None -> None
        | Some (_, value) -> 
          let normalized_v = V.map_f_preserves_zero (fun x -> x /: value) v in
          Some (init_with_vec normalized_v)
      else (* We try to normalize v and check if a contradiction arises. If not, we insert v at the appropriate place in m (depending on the pivot) *)
        let pivot_positions = get_pivot_positions m in
        let v_after_elim = List.fold_left (
            fun acc (row_idx, pivot_position) ->
              let v_at_piv = V.nth acc pivot_position in 
              if v_at_piv =: A.zero then 
                acc
              else
                let piv_row = List.nth m row_idx in
                sub_scaled_row acc piv_row v_at_piv
          ) v pivot_positions
        in 
        match V.find_first_non_zero v_after_elim with (* now we check for contradictions and finally insert v *)
        | None -> Some m (* v is zero vector and was therefore already covered by m *)
        | Some (idx, value) -> 
          if idx = (num_cols m - 1) then 
            None
          else
            let normalized_v = V.map_f_preserves_zero (fun x -> x /: value) v_after_elim in
            Some (insert_v_according_to_piv m normalized_v idx pivot_positions)

    let rref_vec m v = Timing.wrap "rref_vec" (rref_vec m) v
    
    (* This should yield the same result as appending m2 to m1, normalizing and removing zero rows. However, it is usually faster. *)
    (* Both input matrices are assumed to be in rref form *)
    let rref_matrix (m1 : t) (m2 : t) =
      let big_m, small_m = if num_rows m1 > num_rows m2 then m1, m2 else m2, m1 in
      fst @@ List.fold_while (fun acc _ -> Option.is_some acc) 
        (fun acc_big_m small -> rref_vec (Option.get acc_big_m) small ) (Some big_m) small_m (* TODO: pivot_positions are recalculated at each step, but since they need to be adjusted after each step it might not make sense to keep track of them here.*)

    let rref_matrix m1 m2 = Timing.wrap "rref_matrix" (rref_matrix m1) m2

    (* Performs a partial rref reduction to check if concatenating both matrices and afterwards normalizing them would yield a matrix <> m2 *)
    (* Both input matrices must be in rref form! *)
    let is_covered_by m1 m2 =
      let rec is_linearly_independent_rref v m = 
        let pivot_opt = V.findi_val_opt ((<>:) A.zero) v in
        match pivot_opt with
        | None -> false (* When we found no pivot, the vector is already A.zero. *)
        | Some (pivot_id, pivot) ->
          let m' = List.drop_while (fun v2 -> 
              match V.findi_val_opt ((<>:) A.zero) v2 with
              | None -> true  (* In this case, m2 only has zero rows after that *)
              | Some (idx', _) -> idx' < pivot_id
            ) m in 
          match m' with
          | [] -> not @@ V.is_zero_vec v
          | x::xs ->
            let new_v = V.map2_f_preserves_zero (fun v1 v2 -> v1 -: (pivot *: v2)) v x in
            is_linearly_independent_rref new_v m'
      in
      if num_rows m1 > num_rows m2 then false else
        let rec is_covered_by_helper m1 m2 = 
          match m1 with 
          | [] -> true
          | v1::vs1 -> 
            let first_non_zero = V.findi_val_opt ((<>:) A.zero) v1 in
            match first_non_zero with
            | None -> true  (* vs1 must also be zero-vectors because of rref *)
            | Some (idx, _) -> 
              let linearly_indep = is_linearly_independent_rref v1 m2 in
              if linearly_indep then false else is_covered_by_helper vs1 m2
        in is_covered_by_helper m1 m2

    let is_covered_by m1 m2 = Timing.wrap "is_covered_by" (is_covered_by m1) m2

    let find_opt f m =
      List.find_opt f m

    let map2 f m v =
      let vector_length = V.length v in
      List.mapi (fun index row -> if index < vector_length then f row (V.nth v index) else row ) m
  end