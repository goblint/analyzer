open Matrix
open SparseVector
open RatOps

open Batteries

module M = Messages

let timing_wrap = Vector.timing_wrap

module type SparseMatrix = 
sig
  include Matrix
  val get_col_upper_triangular: t -> int -> vec

  val swap_rows: t -> int -> int -> t

  val rref_vec: t -> vec -> t Option.t

  val rref_matrix: t -> t -> t Option.t

  val linear_disjunct: t -> t -> t
  (** [linear_disjunct m1 m2] returns a matrix that contains the linear disjunct of [m1] and [m2]. 
      The result is in rref. If [m1] and [m2] are not linearly disjunct, an exception is raised. *)
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
      if m = [] then 0 else V.length (List.hd m)

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
        let rec find_pivot idx entries = (* Finds non-zero element in column j and returns pair of row idx, the pivot value and the row *)
          match entries with
          | [] -> None
          | row :: rest -> let value = V.nth row j in 
            if value =: A.zero then find_pivot (idx - 1) rest else Some (idx, value, row)
        in
        match (find_pivot (num_rows m - 1) (List.rev m)) with
        | None -> m (* column is already filled with zeroes *)
        | Some (row_idx, pivot,pivot_row) -> 
          List.mapi (fun idx row ->  (* use the pivot row to reduce all entries in column j to zero, then "delete" the pivot row *)
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
      let res = 
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
      in
      if M.tracing then
        M.trace "rref_vec" "rref_vec: m:\n%s, v: %s => res:\n%s" (show m) (V.show v) (match res with None -> "None" | Some r -> show r)
      ;
      res

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

    (** Direct implementation of https://doi.org/10.1007/BF00268497 , chapter 5.2 Calculation of Linear Disjunction
        also available at https://www-apr.lip6.fr/~mine/enseignement/mpri/attic/2014-2015/exos/karr.pdf   
        only difference is the implementation being optimized for sparse matrices in row representation
    *)
    let linear_disjunct m1 m2 =
      let maxcols = num_cols m1 in
      let inverse_termorder = fun x y -> y - x in
      let rev_matrix = List.map (fun x -> V.of_sparse_list (V.length x) (List.rev @@ V.to_sparse_list x) ) in
      let del_col m i = List.map (fun v -> V.tail_afterindex v i) m in 
      let safe_get_row m i =
        try List.nth m i with
        | Invalid_argument _ -> V.zero_vec (num_cols m) (* if row is empty, we return zero *)
      in 
      let safe_remove_row m i =
        try remove_row m i with   (* remove_row can fail for sparse representations *)
        | Invalid_argument _ -> m (* if row is empty, we return the original matrix *)
      in 

      let col_and_rc m colidx rowidx = 
        let col = get_col_upper_triangular m colidx in col , 
                                                       try V.nth col rowidx with (* V.nth could be integrated into get_col for the last few bits of performance... *)
                                                       | Invalid_argument _ -> A.zero (* if col is empty, we return zero *)
      in 

      let push_col m colidx col = 
        List.mapi (fun idx row -> 
            match V.nth col idx with
            | valu when A.equal A.zero valu -> row (* if the value is zero, we do not change the row *)
            | valu -> V.push_first row colidx valu
            | exception _ -> row
          ) m
      in

      let case_two a r col_b =
        let a_r = get_row a r in
        let res = map2i (fun i x y -> if i < r then
                            V.map2_f_preserves_zero (fun u j -> u +: y *: j) x a_r
                          else x) a col_b in
        if M.tracing then M.trace "linear_disjunct_cases" "case_two: \na:\n%s, r:%d,\n col_b: %s, a_r: %s, => res:\n%s" (show a) r (V.show col_b) (V.show a_r) (show res);
        res
      in

      let case_three col1 col2 m1 m2 result ridx cidx = (* no new pivots at ridx/cidx *)
        let sub_and_lastterm c1 c2 = (* return last element/idx pair that differs*)
          let len = V.length c1 in
          let c1 = V.to_sparse_list c1 in
          let c2 = V.to_sparse_list c2 in
          let rec sub_and_last_aux (acclist,acc) c1 c2 =
            match c1, c2 with
            | (i,_)::_,_ when i >= ridx -> (acclist,acc) (* we are done, no more entries in c1 that are relevant *)
            | (i1, v1) :: xs1, (i2, v2) :: xs2 ->
              if i1 = i2 then
                let res = A.sub v1 v2 in
                if A.equal res A.zero then 
                  sub_and_last_aux ((i1,res)::acclist,acc) xs1 xs2
                else
                  sub_and_last_aux ((i1,res)::acclist,Some (i1,v1,v2)) xs1 xs2
              else if i1 < i2 then
                sub_and_last_aux ((i1,v1)::acclist,Some (i1,v1,A.zero)) xs1 ((i2, v2)::xs2)
              else (* i1 > i2 *)
                sub_and_last_aux ((i2,A.neg v2)::acclist,Some (i2,A.zero,v2))  ((i1, v1)::xs1) xs2
            | (i,v)::xs ,[] -> sub_and_last_aux ((i,v)::acclist,Some (i,v,A.zero)) xs []
            | [], (i,v)::xs -> sub_and_last_aux ((i,v)::acclist,Some (i,A.zero,v)) [] xs
            | _ -> (acclist,acc)
          in
          let resl,rest = sub_and_last_aux ([],None) c1 c2 in
          if M.tracing then M.trace "linear_disjunct_cases" "sub_and_last: ridx: %d c1: %s, c2: %s, resultlist: %s, result_pivot: %s" ridx (V.show col1) (V.show col2) (String.concat "," (List.map (fun (i,v) -> Printf.sprintf "(%d,%s)" i (A.to_string v)) resl)) (match rest with None -> "None" | Some (i,v1,v2) -> Printf.sprintf "(%d,%s,%s)" i (A.to_string v1) (A.to_string v2));
          V.of_sparse_list len (List.rev resl), rest
        in
        let coldiff,lastdiff = sub_and_lastterm col1 col2 in
        match lastdiff with
        | None -> 
          let sameinboth=get_col_upper_triangular m1 cidx in 
          if M.tracing then M.trace "linear_disjunct_cases" "case_three: no difference found, cidx: %d, ridx: %d, coldiff: %s, sameinboth: %s" 
              cidx ridx (V.show coldiff) (V.show sameinboth);
          (del_col m1 cidx,  del_col m2 cidx, push_col result cidx sameinboth, ridx) (* No difference found -> (del_col m1 cidx, del_col m2 cidx, push hd to result, ridx)*)
        | Some (idx,x,y) ->
          let r1 = safe_get_row m1 idx in
          let r2 = safe_get_row m2 idx in
          let resrow = safe_get_row result idx in
          let diff = x -: y in
          let multiply_by_t termorder m t =
            map2i (fun i x c -> if i <= ridx then
                      let beta = c /: diff in
                      V.map2_f_preserves_zero_helper (termorder) (fun u j -> u -: (beta *: j)) x t
                    else x) m coldiff
          in
          let transformed_res = multiply_by_t (inverse_termorder) result resrow in
          let transformed_a = multiply_by_t (-) m1 r1 in
          let alpha = get_col_upper_triangular transformed_a cidx in
          let res = push_col transformed_res cidx alpha in
          if M.tracing then M.trace "linear_disjunct_cases" "case_three: found difference at ridx: %d idx: %d, x: %s, y: %s, diff: %s, m1: \n%s, m2:\n%s, res:\n%s" 
              ridx idx (A.to_string x) (A.to_string y) (A.to_string diff) (show m1) (show m2) (show @@ rev_matrix res);
          safe_remove_row (transformed_a) idx, safe_remove_row (multiply_by_t (-) m2 r2) idx, safe_remove_row (res) idx, ridx - 1
      in

      let rec lindisjunc_aux currentrowindex currentcolindex m1 m2 result =
        if M.tracing then M.trace "linear_disjunct" "result so far: \n%s, currentrowindex: %d, currentcolindex: %d, m1: \n%s, m2:\n%s "
            (show @@ rev_matrix result) currentrowindex currentcolindex  (show m1) (show m2); 
        if currentcolindex >= maxcols then result
        else
          let col1, rc1 = col_and_rc m1 currentcolindex currentrowindex in
          let col2, rc2 = col_and_rc m2 currentcolindex currentrowindex in
          match Z.to_int @@ A.get_num rc1, Z.to_int @@ A.get_num rc2 with
          | 1, 1 -> lindisjunc_aux
                      (currentrowindex + 1) (currentcolindex+1)
                      (del_col m1 currentrowindex) (del_col m2 currentrowindex)
                      (List.mapi (fun idx row -> if idx = currentrowindex then V.push_first row currentcolindex A.one else row) result)
          | 1, 0 -> let beta = get_col_upper_triangular m2 currentcolindex in
            if M.tracing then M.trace "linear_disjunct_cases" "case 1,0: currentrowindex: %d, currentcolindex: %d, m1: \n%s, m2:\n%s , beta %s" currentrowindex currentcolindex (show m1) (show m2) (V.show beta);
            lindisjunc_aux
              (currentrowindex) (currentcolindex+1)
              (safe_remove_row (case_two m1 currentrowindex col2) currentrowindex) (safe_remove_row m2 currentrowindex)
              (safe_remove_row (push_col result currentcolindex beta) currentrowindex)
          | 0, 1 -> let beta = get_col_upper_triangular m1 currentcolindex in
            if M.tracing then M.trace "linear_disjunct_cases" "case 0,1: currentrowindex: %d, currentcolindex: %d, m1: \n%s, m2:\n%s , beta %s" currentrowindex currentcolindex (show m1) (show m2) (V.show beta);
            lindisjunc_aux
              (currentrowindex) (currentcolindex+1)
              (safe_remove_row m1 currentrowindex) (safe_remove_row (case_two m2 currentrowindex col1) currentrowindex)
              (safe_remove_row (push_col result currentcolindex beta) currentrowindex)
          | 0, 0 -> let m1 , m2, result, currentrowindex = case_three col1 col2 m1 m2 result currentrowindex currentcolindex in
            lindisjunc_aux currentrowindex (currentcolindex+1) m1 m2 result  (* we need to process m1, m2 and result *)
          | a,b -> failwith ("matrix not in rref m1: " ^ (string_of_int a) ^ (string_of_int b)^(show m1) ^ " m2: " ^ (show m2))
      in
      (* create a totally empty intial result, with dimensions rows x cols *)
      let pseudoempty = List.init (max (num_rows m1) (num_rows m1)) (fun _ -> V.zero_vec (num_cols m1)) in
      let res = rev_matrix @@ lindisjunc_aux 0 0 m1 m2 (pseudoempty) in
      if M.tracing then (
        let pleinly = String.concat "\n" (List.map (fun line -> String.concat "," (List.map (fun (idx,valu) ->  "("^string_of_int idx^","^A.to_string valu^")") (V.to_sparse_list line)) ) res) in 
        M.tracel "linear_disjunct" "linear_disjunct between \n%s and \n%s =>\n%s  \n(%s)" (show m1)  (show m2) (show res) pleinly);
      res

  end
