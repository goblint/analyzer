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

    let add_empty_columns m cols =
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
      failwith "TODO"

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
      failwith "Do not use!"

    let set_col_with m new_col n = Timing.wrap "set_col" (set_col_with m new_col) n

    let set_col m new_col n = 
      List.mapi (fun row_idx row -> V.set_nth row n (V.nth new_col row_idx)) m 

    let append_matrices m1 m2  = (* keeps dimensions of first matrix, what if dimensions differ?*)
      m1 @ m2

    let equal m1 m2 = Timing.wrap "equal" (equal m1) m2

    let reduce_col_with m j =
      failwith "Do not use!"

    let reduce_col_with m j  = Timing.wrap "reduce_col_with" (reduce_col_with m) j
    let reduce_col m j =
      if is_empty m then m 
      else
        let rec find_pivot idx entries = (* Finds non-zero element in column j and returns pair of row idx and the pivot value *)
          match entries with
          | [] -> None
          | row :: rest -> match (List.assoc_opt j row) with
            | None -> find_pivot (idx - 1) rest
            | Some value -> Some (idx, value)
        in
        match (find_pivot (num_rows m - 1) (List.rev m.entries)) with
        | None -> m (* column is already filled with zeroes *)
        | Some (row_idx, pivot) -> 
          let pivot_row = List.nth m.entries row_idx in
          let entries' = 
            List.mapi(fun idx row ->
                if idx = row_idx then 
                  [] 
                else
                  match (List.assoc_opt j row) with (* Find column element in row and, if it exists, subtract row *)
                  | None -> row
                  | Some row_value -> (let s = row_value /: pivot in
                                       let rec merge acc piv_row cur_row = 
                                         match piv_row, cur_row with 
                                         | [], [] -> acc 
                                         | [], (i, value) :: rest -> merge ((i, value) :: acc) piv_row rest
                                         | (i, value) :: rest, [] -> let new_value = A.zero -: s *: value in merge ((i, new_value) :: acc) rest cur_row
                                         | (i, piv_val) :: piv_rest, (j, cur_val) :: cur_rest -> 
                                           if i = j then 
                                             let new_value = cur_val -: s *: piv_val in merge ((i, new_value) :: acc) piv_rest cur_rest 
                                           else if i < j then 
                                             let new_value = A.zero -: s *: piv_val in merge ((i, new_value) :: acc) piv_rest cur_row
                                           else 
                                             merge ((j, cur_val) :: acc) piv_row cur_rest
                                       in List.rev @@ merge [] pivot_row row)
              ) m.entries
          in 
          {entries = entries'; column_count = m.column_count}

    let del_col m j =
      List.map (fun row -> V.remove_nth row j) m

    let del_cols m cols =
      if (Array.length cols) = num_cols m then empty() else
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
      failwith "Do not use!"

    let rref_with m = Timing.wrap "rref_with" rref_with m

    let init_with_vec v =
      [v]

    let reduce_col_with_vec m j v = 
      failwith "TODO"

    let get_pivot_positions m = 
      failwith "TODO"

    let rref_vec m pivot_positions v =
      failwith "TODO"


    let rref_vec_with m v =
      (*This function yields the same result as appending vector v to m and normalizing it afterwards would. However, it is usually faster than performing those ops manually.*)
      (*m must be in rref form and contain the same num of cols as v*)
      (*If m is empty then v is simply normalized and returned*)
      failwith "Do not use!"

    let rref_vec_with m v = Timing.wrap "rref_vec_with" (rref_vec_with m) v

    let rref_vec m v = failwith "TODO"

    let rref_matrix_with m1 m2 =
      (*Similar to rref_vec_with but takes two matrices instead.*)
      (*ToDo Could become inefficient for large matrices since pivot_elements are always recalculated + many row additions*)
      failwith "Do not use!"

    let rref_matrix_with m1 m2 = Timing.wrap "rref_matrix_with" (rref_matrix_with m1) m2

    let rref_matrix m1 m2 = failwith "TODO"

    let normalize_with m = 
      failwith "Do not use!"

    let normalize_with m = Timing.wrap "normalize_with" normalize_with m

    let normalize m =
      let entries = m.entries in
      let col_count = m.column_count in
      let swap_rows m r1_idx r2_idx =
        List.mapi (fun i row -> 
            if i = r1_idx then List.nth m r2_idx
            else if i = r2_idx then List.nth m r1_idx
            else row
          ) entries
      in 
      let rec sub_rows minu subt : (int * A.t) list =
        match minu, subt with
        | ((xidx, xv)::xs, (yidx,yv)::ys) -> (
            match xidx - yidx with
            | d when d = 0 && xv <> yv -> (xidx, xv -: yv)::(sub_rows xs ys)
            | d when d < 0 -> (xidx, xv)::(sub_rows xs ((yidx, yv)::ys))
            | d when d > 0 -> (yidx, A.zero -: yv)::(sub_rows ((xidx, xv)::xs) ys)
            | _ -> sub_rows xs ys ) (* remove row when is (0, 0) *)
        | ([], (yidx, yv)::ys) -> (yidx, A.zero -: yv)::(sub_rows [] ys)
        | ((xidx, xv)::xs, []) -> (xidx, xv)::(sub_rows xs [])
        | ([],[]) -> []
      in
      let div_row row pivot =
        List.map (fun (idx, value) -> (idx, value /: pivot)) row
      in
      let dec_mat_2D m = 
        m 
      in
      let rec find_pivot_in_col m row_idx col_idx = 
        match m with
        | ((idx, value)::_)::xs -> if idx = col_idx then Some (row_idx, value) else find_pivot_in_col xs (row_idx + 1) col_idx
        | ([])::xs -> find_pivot_in_col xs (row_idx + 1) col_idx
        | [] -> None
      in 
      (* let rec find_pivot m col_idx row_idx =
         if col_idx >= col_count then None else 
          match find_pivot_in_col m col_idx row_idx with
          | Some (row_idx, value) -> Some (row_idx, value)
          | None -> find_pivot m (col_idx + 1) row_idx
         in *)
      let rec main_loop m m' row_idx col_idx : (int * A.t) list list = 
        if col_idx = (col_count - 1) (* In this case the whole bottom of the matrix starting from row_index is Zero, so it is normalized *)
        then m
        else
          match find_pivot_in_col m' row_idx col_idx with
          | None -> main_loop m m' row_idx (col_idx + 1)
          | Some (piv_row_idx, piv_val) -> (
              let m = if piv_row_idx <> row_idx then swap_rows m row_idx piv_row_idx else m in
              let normalized_m = List.mapi (fun idx row -> if idx = row_idx then div_row row piv_val else row) m in
              let piv_row = (List.nth normalized_m row_idx) in
              let subtracted_m = List.mapi (fun idx row -> if idx <> row_idx then sub_rows row piv_row else row) normalized_m in
              let m' = dec_mat_2D m in
              main_loop subtracted_m m' (row_idx + 1) (col_idx + 1)
            )

      in 
      let e' = main_loop m.entries m.entries 0 0 in
      Some {entries = e'; column_count = m.column_count}


    let is_covered_by m1 m2 =
      failwith "TODO"

    let is_covered_by m1 m2 = Timing.wrap "is_covered_by" (is_covered_by m1) m2

    let find_opt f m =
      List.find_opt f m

    let map2 f m v =
      let vector_length = V.length v in
      List.mapi (fun index row -> if index < vector_length then f row (V.nth v index) else row ) m

    let map2_with f m v =
      failwith "Do not use!"

    let map2_with f m v = Timing.wrap "map2_with" (map2_with f m) v

    let map2i_with f m v =
      failwith "Do not use!"

    let map2i_with f m v = Timing.wrap "map2i_with" (map2i_with f m) v
  end