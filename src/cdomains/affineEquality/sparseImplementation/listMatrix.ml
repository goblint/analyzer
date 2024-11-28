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
      List.fold_left (^) "" (List.map (fun x -> (V.show x) ^ "\n") x)

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

    let add_empty_columns m cols = failwith "TODO"
    (*let colsL = List.sort (fun a b -> a-b) (Array.to_list cols) in
      let emptyT = A.zero in
      let rec list_of_all_before_index idx cols =
        match  cols with
        | x::xs -> 
          if x < idx 
          then 
            let (h,t) = list_of_all_before_index idx xs in
            (x::h, t)
          else ([],x::xs)
        | [] -> ([],[])
      in
      (*This could easily be abstracted into the above functions, but its nice to have 
        it here for readability and debugging*)
      let rec make_empty_entries_for_idxs idxs =
        match idxs with
        | x::xs -> (x, emptyT)::(make_empty_entries_for_idxs xs) 
        | [] -> []
      in
      let rec add_column_element r cols = 
        match r with
        | (idx, _)::xs -> 
          let (bef,aft) = list_of_all_before_index idx cols in
          (make_empty_entries_for_idxs bef)@(add_column_element xs aft)
        | [] -> []
      in
      let rec add_empty_columns_on_list m cols =
        match m with
        | x::xs -> (add_column_element x cols)::(add_empty_columns_on_list xs cols)
        | [] -> []
      in tM (add_empty_columns_on_list m.entries colsL) (m.column_count + Array.length cols)*)

    let add_empty_columns m cols =
      Timing.wrap "add_empty_cols" (add_empty_columns m) cols

    let append_row m row  =
      m @ [row]

    let get_row m n =
      List.nth m n

    let remove_row m n =
      List.remove_at n m

    let get_col m n =
      V.of_list @@ List.map (fun row -> V.nth row n) m (* builds full col including zeros, maybe use sparselist instead? *)

    let get_col m n =
      Timing.wrap "get_col" (get_col m) n

    let set_col_with m new_col n =
      failwith "deprecated"

    let set_col_with m new_col n = Timing.wrap "set_col" (set_col_with m new_col) n

    let set_col m new_col n = 
      List.mapi (fun row_idx row -> V.set_nth row n (V.nth new_col row_idx)) m 

    let append_matrices m1 m2  = (* keeps dimensions of first matrix, what if dimensions differ?*)
      m1 @ m2

    let equal m1 m2 = Timing.wrap "equal" (equal m1) m2

    let reduce_col_with m j =
      failwith "deprecated"

    let reduce_col_with m j  = Timing.wrap "reduce_col_with" (reduce_col_with m) j
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
          let pivot_row = List.nth m row_idx in
          List.mapi (fun idx row ->
              if idx = row_idx then 
                V.zero_vec (num_cols m)
              else
                let row_value = V.nth row j in 
                if row_value = A.zero then row
                else (let s = row_value /: pivot in
                      V.map2_preserve_zero (fun x y -> x -: s *: y) row pivot_row)            
            ) m

    let del_col m j =
      if num_cols m = 1 then empty () 
      else 
        List.map (fun row -> V.remove_nth row j) m

    let del_cols m cols =
      if (Array.length cols) = num_cols m then empty() 
      else
        let cols = Array.to_list cols in 
        let sorted_cols = List.sort_uniq Stdlib.compare cols in (* Apron Docs:  Repetitions are meaningless (and are not correct specification), maybe use List instead?*)
        List.map (fun row -> V.remove_at_indices row sorted_cols) m

    let del_cols m cols = Timing.wrap "del_cols" (del_cols m) cols

    let map2i f m v =
      let vector_length = V.length v in
      List.mapi (fun index row -> if index < vector_length then f index row (V.nth v index) else row) m

    let remove_zero_rows m =
      List.filter (fun row -> not (V.is_zero_vec row)) m

    let rref_with m =
      failwith "deprecated"

    let rref_with m = Timing.wrap "rref_with" rref_with m

    let init_with_vec v =
      [v]

    let reduce_col_with_vec m j v = 
      failwith "TODO"

    let get_pivot_positions m = 
      failwith "TODO"

    let normalize_with m = 
      failwith "deprecated"

    let normalize_with m = Timing.wrap "normalize_with" normalize_with m

    let sub_rows (minu : V.t) (subt : V.t) : V.t =
      V.map2_preserve_zero (-:) minu subt

    let div_row (row : V.t) (pivot : A.t) : V.t =
      V.map_preserve_zero (fun a -> a /: pivot) row

    let swap_rows m j k =
      List.mapi (fun i row -> if i = j then List.nth m k else if i = k then List.nth m k else row) m

    let normalize (m : t) : t Option.t =
      let col_count = num_cols m in

      let dec_mat_2D (m : t) (row_idx : int) (col_idx : int) : t = 
        m 
      in
      (* Function for finding a pivot in an extracted part of the matrix (row_idx and col_idx indicate which part of the original matrix) *)
      let rec find_pivot (m : t) (row_idx : int) (col_idx : int) : (int * int * A.t) Option.t =
        if col_idx >= col_count then None else 
          let get_first_non_zero v =  (* Returns (col_idx, value) of first non-zero row*)
            let v' = V.to_sparse_list v in
            match v' with
            | [] -> None
            | (idx, value)::_ -> Some (idx, value)
          in
          (* Finding pivot by extracting the minimum column index of the first non zero value of each row*)
          let (piv_row, piv_col, piv_val) = List.fold_lefti (fun (cur_row, cur_col, cur_val) i row  -> 
              let row_first_non_zero = get_first_non_zero row in
              match row_first_non_zero with
              | None -> (cur_row, cur_col, cur_val)
              | Some (col_idx, value) -> if col_idx < cur_col then (row_idx + i, col_idx, value) else (cur_row, cur_col, cur_val)
            ) (num_rows m, num_cols m, A.zero) m (* Initializing with max, so num_cols m indicates that pivot is not found *)
          in
          if piv_col = (num_cols m) then None else Some (piv_row, piv_col, piv_val)
      in
      let rec main_loop (m : t) (m' : t) (row_idx : int) (col_idx : int) : t = 
        if col_idx = (col_count - 1) then m  (* In this case the whole bottom of the matrix starting from row_index is Zero, so it is normalized *)
        else
          match find_pivot m' row_idx col_idx with
          | None -> m (* No pivot found means already normalized*)
          | Some (piv_row_idx, piv_col_idx, piv_val) -> (
              let m = if piv_row_idx <> row_idx then swap_rows m row_idx piv_row_idx else m in
              let normalized_m = List.mapi (fun idx row -> if idx = row_idx then div_row row piv_val else row) m in
              let piv_row = (List.nth normalized_m row_idx) in
              let subtracted_m = List.mapi (fun idx row -> if idx <> row_idx then sub_rows row piv_row else row) normalized_m in
              let m' = dec_mat_2D m (row_idx + 1) (piv_col_idx + 1) in
              main_loop subtracted_m m' (row_idx + 1) (piv_col_idx + 1)) (* We start at piv_col_idx + 1 because every other col before that is zero at the bottom*)
      in 
      let m' = main_loop m m 0 0 in
      Some m'

    let rref_vec_helper m pivot_positions v =
      failwith "TODO"

    let rref_vec_with m v =
      (*This function yields the same result as appending vector v to m and normalizing it afterwards would. However, it is usually faster than performing those ops manually.*)
      (*m must be in rref form and contain the same num of cols as v*)
      (*If m is empty then v is simply normalized and returned*)
      failwith "deprecated"

    let rref_vec_with m v = Timing.wrap "rref_vec_with" (rref_vec_with m) v

    (*This function yields the same result as appending vector v to m and normalizing it afterwards would. However, it is usually faster than performing those ops manually.*)
    (*m must be in rref form and contain the same num of cols as v*)
    (*If m is empty then v is simply normalized and returned*)
    (* TODO: OPTIMIZE! *)
    let rref_vec m v =
      normalize @@ append_matrices m (init_with_vec v)

    let rref_matrix_with m1 m2 =
      (*Similar to rref_vec_with but takes two matrices instead.*)
      (*ToDo Could become inefficient for large matrices since pivot_elements are always recalculated + many row additions*)
      failwith "deprecated"

    let rref_matrix_with m1 m2 = Timing.wrap "rref_matrix_with" (rref_matrix_with m1) m2

    (*Similar to rref_vec_with but takes two matrices instead.*)
    (*ToDo Could become inefficient for large matrices since pivot_elements are always recalculated + many row additions*)
    (*TODO: OPTIMIZE!*)
    let rref_matrix m1 m2 =
      normalize @@ append_matrices m1 m2

    let is_covered_by m1 m2 =
      failwith "TODO"

    let is_covered_by m1 m2 = Timing.wrap "is_covered_by" (is_covered_by m1) m2

    let find_opt f m =
      List.find_opt f m

    let map2 f m v =
      let vector_length = V.length v in
      List.mapi (fun index row -> if index < vector_length then f row (V.nth v index) else row ) m

    let map2_with f m v =
      failwith "deprecated"

    let map2_with f m v = Timing.wrap "map2_with" (map2_with f m) v

    let map2i_with f m v =
      failwith "Do not use!"

    let map2i_with f m v = Timing.wrap "map2i_with" (map2i_with f m) v
  end