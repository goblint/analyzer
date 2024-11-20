open AbstractMatrix
open AbstractVector
open RatOps
open ConvenienceOps

(** Sparse matrix implementation.
    It provides a normalization function to reduce a matrix into reduced row echelon form.
    Operations exploit that the input matrix/matrices are in reduced row echelon form already. *)
module SparseMatrix: AbstractMatrix =
  functor (A: RatOps) (V: AbstractVector) ->
  struct
    include ConvenienceOps(A)
    module V = V(A)

    (* Array of arrays implementation. One array per row containing tuple of column index and value *)
    type t = {
      entries : (int * A.t) list list;
      column_count : int
    } [@@deriving eq, ord, hash]

    let show x =
      List.fold_left (^) " " (List.map (fun row -> V.show @@ V.of_sparse_list row x.column_count) x.entries)

    let empty () =
      {entries = []; column_count = 0}

    let num_rows m =
      List.length m.entries

    let is_empty m =
      num_rows m = 0
    (*This should be different if the implimentation is sound*)
    (*m.column_count = 0*)

    let num_cols m =
      m.column_count

    let copy m =
      {entries = m.entries; column_count = m.column_count} (* Lists are immutable, so this should suffice? A.t is mutuable currently, but is treated like its not in ArrayMatrix*)

    let copy m =
      Timing.wrap "copy" (copy) m

    let add_empty_columns m cols =
      let colsL = List.sort (fun a b -> a-b) (Array.to_list cols) in
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
      in {entries = add_empty_columns_on_list m.entries colsL; column_count = m.column_count + Array.length cols}

    let add_empty_columns m cols =
      Timing.wrap "add_empty_cols" (add_empty_columns m) cols

    let append_row m row  =
      {entries = m.entries @ [V.to_sparse_list row]; column_count = m.column_count}

    let get_row m n =
      V.of_sparse_list (List.nth m.entries n) m.column_count

    let remove_row m n =
      let rec aux idx entries = match idx, entries with
        | i, x :: xs when i = n -> xs
        | _, x :: xs -> x :: aux (idx + 1) xs
        | _, _ -> failwith "trying to remove out of bounds row"
      in 
      let new_entries = aux 0 m.entries in
      let new_col_count = if new_entries = [] then 0 else m.column_count in
      {entries = new_entries; column_count = new_col_count}

    let get_col m n =
      (* Uses the fact that row is sorted to return Zero when index n is exceeded *)
      let rec get_col_from_row row =
        match row with
        | [] -> A.zero
        | (col_idx, value)::_ when col_idx = n -> value
        | (col_idx, _)::_ when col_idx > n -> A.zero
        | _::cs -> get_col_from_row cs
      in
      V.of_list @@ List.map (fun row -> get_col_from_row row ) m.entries

    let get_col m n =
      Timing.wrap "get_col" (get_col m) n

    let set_col_with m new_col n =
      failwith "TODO"

    let set_col_with m new_col n = Timing.wrap "set_col" (set_col_with m new_col) n

    let set_col m new_col n = 
      let rec set_col_in_row row value =
        match row with
        | [] -> if value =: A.zero then [] else [(n, value)]
        | (col_idx, v)::cs when col_idx > n -> if value =: A.zero then (col_idx, v)::cs else (n, value)::(col_idx, v)::cs
        | (col_idx, v)::cs when col_idx = n -> if value =: A.zero then cs else (n, value)::cs 
        | (col_idx, v)::cs -> (col_idx, v)::(set_col_in_row cs value)
      in
      let new_entries = List.mapi (fun row_idx row -> 
          let value = V.nth new_col row_idx in
          set_col_in_row row value
        ) m.entries in
      {entries = new_entries; column_count = m.column_count}

    let append_matrices m1 m2  =
      failwith "TODO"

    let equal m1 m2 = Timing.wrap "equal" (equal m1) m2

    let reduce_col_with m j =
      failwith "TODO"

    let reduce_col_with m j  = Timing.wrap "reduce_col_with" (reduce_col_with m) j
    let reduce_col m j =
      failwith "TODO"

    let del_col m j =
      if is_empty m then m else
        let del_col_from_row row =
          List.filter_map (fun (col_idx, value) ->
              if col_idx = j then
                None
              else if col_idx > j then
                Some (col_idx - 1, value)
              else
                Some (col_idx, value)
            ) row
        in
        let new_entries = List.map (fun row -> del_col_from_row row) m.entries in
        {entries = new_entries; column_count = m.column_count - 1}

    (* TODO: Might be more efficient to check for each entry how much to reduce their index and not by recursively calling del_col *)
    let del_cols m cols =
      let cols = Array.to_list cols in (* TODO: Get away from Arrays *)
      let to_delete_count = List.length cols in
      if to_delete_count = 0 || is_empty m then m
      else
      if num_cols m = to_delete_count then empty () else
        let sorted_cols = List.sort_uniq Stdlib.compare cols in (* Apron Docs:  Repetitions are meaningless (and are not correct specification)*)
        let rec del_cols_aux m cols deleted_col_count =
          match cols with
          | [] -> m
          | col_idx::cs ->
            let m' = del_col m (col_idx - deleted_col_count) in (* Taking already deleted cols into account because of new index *)
            del_cols_aux m' cs (deleted_col_count + 1)
        in del_cols_aux m sorted_cols 0

    let del_cols m cols = Timing.wrap "del_cols" (del_cols m) cols

    let map2i f m v =
      failwith "TODO"

    let remove_zero_rows m =
      failwith "TODO"

    let rref_with m =
      failwith "TODO"

    let rref_with m = Timing.wrap "rref_with" rref_with m

    let init_with_vec v =
      failwith "TODO"


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
      failwith "TODO"

    let rref_vec_with m v = Timing.wrap "rref_vec_with" (rref_vec_with m) v

    let rref_matrix_with m1 m2 =
      (*Similar to rref_vec_with but takes two matrices instead.*)
      (*ToDo Could become inefficient for large matrices since pivot_elements are always recalculated + many row additions*)
      failwith "TODO"

    let rref_matrix_with m1 m2 = Timing.wrap "rref_matrix_with" (rref_matrix_with m1) m2

    let normalize_with m = 
      failwith "TODO"

    let normalize_with m = Timing.wrap "normalize_with" normalize_with m

    let normalize m =
      failwith "TODO"

    let is_covered_by m1 m2 =
      failwith "TODO"

    let is_covered_by m1 m2 = Timing.wrap "is_covered_by" (is_covered_by m1) m2

    let find_opt f m =
      failwith "TODO"

    let map2 f m v =
      failwith "TODO"

    let map2_with f m v =
      failwith "TODO"

    let map2_with f m v = Timing.wrap "map2_with" (map2_with f m) v

    let map2i_with f m v =
      failwith "TODO"

    let map2i_with f m v = Timing.wrap "map2i_with" (map2i_with f m) v
  end