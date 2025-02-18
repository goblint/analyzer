open Matrix
open SparseVector
open RatOps
open ConvenienceOps

open BatList
module List = BatList

let timing_wrap = Vector.timing_wrap

module type SparseMatrix = 
sig
  include Matrix
  val get_col_upper_triangular: t -> int -> vec

  val swap_rows: t -> int -> int -> t

  val rref_vec: t -> vec -> t Option.t

  val rref_matrix: t -> t -> t Option.t
end

module type SparseMatrixFunctor = 
  functor (A: RatOps) (V: SparseVectorFunctor) ->
  sig
    include SparseMatrix with type vec := V(A).t and type num := A.t
  end

(** Matrix implementation that uses a list of (ideally sparse) vectors representing its rows.
    It provides a normalization function to reduce a matrix into reduced row echelon form.
    Operations exploit that the input matrix/matrices are in reduced row echelon form already. *)
module ListMatrix: SparseMatrixFunctor =
  functor (A: RatOps) (V: SparseVectorFunctor) ->
  struct
    include ConvenienceOps(A)
    module V = V(A)

    type t = V.t list (* List of rows *)
    [@@deriving eq, ord, hash]

    let show x =
      List.fold_left (^) "" (List.map (fun x -> (V.show x)) x)

    let copy m = m

    let equal m1 m2 = timing_wrap "equal" (equal m1) m2

    let copy m =
      timing_wrap "copy" (copy) m

    let empty () = []

    let is_empty = List.is_empty

    let num_rows = List.length

    let compare_num_rows = List.compare_lengths

    let num_cols m =
      if m = [] then 0 else V.length (hd m)

    let init_with_vec v =
      [v]

    let append_row m row  =
      m @ [row]

    let get_row = List.nth

    let remove_row m n =
      List.remove_at n m

    let remove_zero_rows m =
      List.filter (fun row -> not (V.is_zero_vec row)) m

    let swap_rows m j k =
      List.mapi (fun i row -> if i = j then List.nth m k else if i = k then List.nth m j else row) m

    let map2 f m v =
      let vector_length = V.length v in
      let vector_entries = V.to_sparse_list v in
      let rec map2_helper acc index m v = 
        match m, v with 
        | [], _ -> List.rev acc
        | row :: rs, [] when index >= vector_length -> List.rev_append acc m
        | row :: rs, [] ->  map2_helper ((f row A.zero):: acc) (index + 1) rs []
        | row :: rs, (i, value) :: vs -> 
          if i = index then 
            map2_helper ((f row value):: acc) (index + 1) rs vs 
          else
            map2_helper ((f row A.zero) :: acc) (index + 1) rs v 
      in
      map2_helper [] 0 m vector_entries

    let map2 f m v = timing_wrap "Matrix.map2" (map2 f m) v

    let map2i f m v =
      let vector_length = V.length v in
      let vector_entries = V.to_sparse_list v in
      let rec map2i_helper acc index m v = 
        match m, v with 
        | [], _ -> List.rev acc
        | row :: rs, [] when index >= vector_length -> List.rev_append acc m 
        | row :: rs, [] ->  map2i_helper ((f index row A.zero):: acc) (index + 1) rs []
        | row :: rs, (i, value) :: vs -> 
          if i = index then 
            map2i_helper ((f index row value):: acc) (index + 1) rs vs 
          else
            map2i_helper ((f index row A.zero) :: acc) (index + 1) rs v 
      in
      map2i_helper [] 0 m vector_entries

    let map2i f m v = timing_wrap "Matrix.map2i" (map2i f m) v

    (**
       [add_empty_columns m cols] extends the matrix [m] as specified in [cols].
       @param cols An apron dimchange array as defined here: https://antoinemine.github.io/Apron/doc/api/ocaml/Dim.html .
    *)
    let add_empty_columns m cols  =    
      let cols_list = Array.to_list cols in (* cols should adhere to apron specification as described here: https://antoinemine.github.io/Apron/doc/api/ocaml/Dim.html*)
      (* This creates a list of tuples, each specifying an index and how many zeros are to be inserted at that index. This is then used for each row *)
      let grouped_indices = List.group Int.compare cols_list in
      let occ_cols = List.map (fun group -> ((List.hd group, List.length group))) grouped_indices in
      List.map (fun row -> V.insert_zero_at_indices row occ_cols (Array.length cols)) m

    let add_empty_columns m cols =
      timing_wrap "add_empty_cols" (add_empty_columns m) cols

    (** 
       [get_col_upper_triangular m n] returns the [n]th column of [m]. [m] must be in upper triangular form.
       @param m A matrix in upper triangular form.
    *)
    let get_col_upper_triangular m n =
      let rec helper acc m i = 
        match m with
        | [] ->  acc 
        | row :: xs ->
          if i > n then 
            acc
          else
            let value = V.nth row n in if value <>: A.zero then helper ((i, value) :: acc) xs (i + 1) else helper acc xs (i + 1)
      in V.of_sparse_list (num_rows m) (List.rev @@ helper [] m 0)

    let get_col_upper_triangular m n = timing_wrap "get_col" (get_col_upper_triangular m) n

    let set_col m new_col n = 
      map2 (fun row value -> V.set_nth row n value) m new_col

    let set_col m new_col n = timing_wrap "set_col" (set_col m) new_col n

    (**
       [del_cols m cols] removes columns from [m] as specified by [c].
       @param c An apron dimchange array as defined here: https://antoinemine.github.io/Apron/doc/api/ocaml/Dim.html .
    *)
    let del_cols m cols =
      if (Array.length cols) = num_cols m then empty() 
      else
        let cols_list = Array.to_list cols in
        List.map (fun row -> V.remove_at_indices row cols_list) m

    let del_cols m cols = timing_wrap "del_cols" (del_cols m) cols

    let find_opt = List.find_opt

    let div_row row value =
      V.map_f_preserves_zero (fun a -> a /: value) row 

    let sub_scaled_row row1 row2 factor =
      V.map2_f_preserves_zero (fun x y -> x -: (factor *: y)) row1 row2

    (** 
       [get_pivot_positions m] returns a list of tuples of row index and pivot position (column index) of all rref-pivots of [m].
       @param m A matrix in rref. 
    *)
    let get_pivot_positions m =
      List.rev @@ List.fold_lefti (
        fun acc i row -> match V.find_first_non_zero row with
          | None -> acc
          | Some (pivot_col, _) -> (i, pivot_col, row) :: acc
      ) [] m

    let get_pivot_positions m =
      timing_wrap "get_pivot_positions" get_pivot_positions m

    (** 
       [reduce_col m j] reduces the [j]-th column in [m] with the last row that has a non-zero element in this column.
    *)
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

    (**
       [normalize m] transforms matrix [m] into reduced row echolon form (rref).
    *)
    let normalize m =
      let col_count = num_cols m in
      let cut_front_matrix m  row_idx col_idx = 
        List.filteri_map (fun i row -> if i < row_idx then None else Some (V.starting_from_nth row col_idx)) m
      in
      let cut_front_matrix m row_idx col_idx = timing_wrap "cut_front_matrix" (cut_front_matrix m row_idx) col_idx in
      (* Function for finding first pivot in an extracted part of the matrix (row_idx and col_idx indicate which part of the original matrix) *)
      (* The last column represents the constant in the affeq *)
      let find_first_pivot m row_idx col_idx =
        if col_idx >= col_count then None else 
          let max_piv_col_idx = num_cols m - 2 in (* col at num_cols - 1 is the constant of the affeq *)
          (* Finding pivot by extracting the minimum column index of the first non zero value of each row *)
          let (piv_row, piv_col, piv_val) = List.fold_lefti (fun (cur_row, cur_col, cur_val) i row  -> 
              let row_first_non_zero = V.find_first_non_zero row in
              match row_first_non_zero with
              | None -> (cur_row, cur_col, cur_val)
              | Some (idx, value) -> if idx < cur_col then (i, idx, value) else (cur_row, cur_col, cur_val)
            ) (num_rows m, max_piv_col_idx + 1, A.zero) m 
          in
          if piv_col = (max_piv_col_idx + 1) then None else Some (row_idx + piv_row, col_idx + piv_col, piv_val)
      in
      let find_first_pivot m row_idx col_idx = timing_wrap "find_first_pivot" (find_first_pivot m row_idx) col_idx in
      let affeq_rows_are_valid m = (* Check if the semantics of an rref-affeq matrix are correct *)
        let col_count = num_cols m in
        let row_is_valid row =
          match V.find_first_non_zero row with
          | Some (idx, _) -> if idx < col_count - 1 then true else false (* If all cofactors of the affeq are zero, but the constant is non-zero, the row is invalid *)
          | None -> true 
        in
        List.for_all row_is_valid m in
      (* Finds the best fitting pivot, moves the respective row at the correct position, and reduces the pivot column afterwards. Continues with next row after that until fully normalized *)
      let rec find_piv_and_reduce m m' row_idx col_idx = 
        if col_idx >= (col_count - 1) then m  (* In this case the whole bottom of the matrix starting from row_index has been reduced to Zero, so it is normalized *)
        else
          match find_first_pivot m' row_idx col_idx with
          | None -> m (* No pivot found means already normalized *)
          | Some (piv_row_idx, piv_col_idx, piv_val) -> (
              let m = if piv_row_idx <> row_idx then swap_rows m row_idx piv_row_idx else m in
              let normalized_m = List.mapi (fun idx row -> if idx = row_idx then div_row row piv_val else row) m in
              let piv_row = (List.nth normalized_m row_idx) in
              let subtracted_m = List.mapi (fun idx row -> if idx <> row_idx then 
                                               let scale = V.nth row piv_col_idx in
                                               sub_scaled_row row piv_row scale else row) normalized_m in
              let m' = cut_front_matrix subtracted_m (row_idx + 1) (piv_col_idx + 1) in
              find_piv_and_reduce subtracted_m m' (row_idx + 1) (piv_col_idx + 1)) (* We start at piv_col_idx + 1 because every other col before that is zero at the bottom*)
      in 
      let m' = find_piv_and_reduce m m 0 0 in
      if affeq_rows_are_valid m' then Some m' else None


    let normalize m = timing_wrap "normalize" normalize m

    (** 
       [reduce_col_with_vec m j v] sets the [j]-th column of [m] to zero by subtracting multiples of [v] from each row. *)
    let reduce_col_with_vec m j v = 
      let pivot_element = V.nth v j in
      if pivot_element = A.zero then m
      else List.mapi (fun idx row ->
          let row_value = V.nth row j in 
          if row_value =: A.zero then row
          else (let s = row_value /: pivot_element in
                V.map2_f_preserves_zero (fun x y -> x -: (s *: y)) row v)
        ) m

    (** 
       [insert_v_according_to_piv m v piv_idx pivot_position] inserts vector [v] into [m] such that rref is preserved.
        @param m A matrix in rref such that [(r, piv_idx)] is not in [pivot_positions] for all [r]. 
        @param v A vector such that [v(piv_idx) =: A.one] and [v(c) <>: A.zero] if [(r,c)] is not in [pivot_position] for all [r]. 
    *)
    let insert_v_according_to_piv m v piv_idx pivot_positions = 
      let reduced_m = reduce_col_with_vec m piv_idx v in
      match List.find_opt (fun (row_idx, piv_col, _) -> piv_col > piv_idx) pivot_positions with
      | None -> append_row reduced_m v
      | Some (row_idx, _, _) -> 
        let (before, after) = List.split_at row_idx reduced_m in 
        before @ (v :: after)

    let insert_v_according_to_piv m v piv_idx pivot_positions =
      timing_wrap "insert_v_according_to_piv" (insert_v_according_to_piv m v piv_idx) pivot_positions

    (** 
       [rref_vec m v] yields the same result as appending [v] to [m], then bringing [m] into rref and removing all zero rows.

       {i Faster than appending [v] to [m] and normalizing!}
       @param m A matrix in rref.
       @param v A vector with number of entries equal to the number of columns of [v].
    *)
    let rref_vec m v =
      if is_empty m then (* In this case, v is normalized and returned *)
        BatOption.map (fun (_, value) -> init_with_vec @@ div_row v value) (V.find_first_non_zero v)
      else (* We try to normalize v and check if a contradiction arises. If not, we insert v at the appropriate place in m (depending on the pivot) *)
        let pivot_positions = get_pivot_positions m in
        (* filtered_pivots are only the pivots which have a non-zero entry in the corresponding column of v. Only those are relevant to subtract from v *)
        let filtered_pivots = List.rev @@ fst @@ List.fold_left (fun (res, pivs_tail) (col_idx, value) ->
            let pivs_tail = List.drop_while (fun (_, piv_col, _) -> piv_col < col_idx) pivs_tail in (* Skipping until possible match of both cols  *)
            match pivs_tail with
            | [] -> (res, [])
            | (row_idx, piv_col, row) :: ps when piv_col = col_idx -> ((row_idx, piv_col, row, value) :: res, ps)
            | _ -> (res, pivs_tail)
          ) ([], pivot_positions) (V.to_sparse_list v) in
        let v_after_elim = List.fold_left (fun acc (row_idx, pivot_position, piv_row, v_at_piv) ->
            sub_scaled_row acc piv_row v_at_piv
          ) v filtered_pivots in
        match V.find_first_non_zero v_after_elim with (* now we check for contradictions and finally insert v *)
        | None -> Some m (* v is zero vector and was therefore already covered by m *)
        | Some (idx, value) ->
          if idx = (num_cols m - 1) then
            None
          else
            let normalized_v = V.map_f_preserves_zero (fun x -> x /: value) v_after_elim in
            Some (insert_v_according_to_piv m normalized_v idx pivot_positions)

    let rref_vec m v = timing_wrap "rref_vec" (rref_vec m) v

    (** [rref_matrix m m'] yields the same result as appending [m'] to [m], then bringing the resulting matrix into rref and removing all zero rows.

        {i Faster than appending [m'] to [m] and then normalizing!}
        @param m A matrix in rref.
        @param m' A matrix in rref.
    *)
    let rref_matrix m1 m2 =
      let big_m, small_m = if compare_num_rows m1 m2 > 0 then m1, m2 else m2, m1 in
      fst @@ List.fold_while (fun acc _ -> Option.is_some acc) 
        (fun acc_big_m small -> rref_vec (Option.get acc_big_m) small ) (Some big_m) small_m

    let rref_matrix m1 m2 = timing_wrap "rref_matrix" (rref_matrix m1) m2

    (**
       [is_coverd_by m m'] returns [true] iff every vector in [m] is a linear combination of vectors in [m'].
       @param m A matrix in rref.
       @param m' A matrix in rref. 
    *)
    let is_covered_by m1 m2 =
      let rec is_linearly_independent_rref v m = 
        let pivot_opt = V.find_first_non_zero v in
        match pivot_opt with
        | None -> false (* When we found no pivot, the vector is already A.zero. *)
        | Some (pivot_id, pivot) ->
          let m' = List.drop_while (fun v2 -> 
              match V.find_first_non_zero v2 with
              | None -> true  (* In this case, m2 only has zero rows after that *)
              | Some (idx', _) -> idx' < pivot_id
            ) m in 
          match m' with
          | [] -> not @@ V.is_zero_vec v
          | x::xs ->
            let new_v = V.map2_f_preserves_zero (fun v1 v2 -> v1 -: (pivot *: v2)) v x in
            is_linearly_independent_rref new_v m'
      in
      if compare_num_rows m1 m2 > 0 then false else
        let rec is_covered_by_helper m1 m2 = 
          match m1 with 
          | [] -> true
          | v1::vs1 -> 
            let first_non_zero = V.find_first_non_zero v1 in
            match first_non_zero with
            | None -> true  (* vs1 must also be zero-vectors because of rref *)
            | Some (idx, _) -> 
              let linearly_indep = is_linearly_independent_rref v1 m2 in
              if linearly_indep then false else is_covered_by_helper vs1 m2
        in is_covered_by_helper m1 m2

    let is_covered_by m1 m2 = timing_wrap "is_covered_by" (is_covered_by m1) m2

  end
