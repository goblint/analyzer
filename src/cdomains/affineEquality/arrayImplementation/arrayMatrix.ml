open Matrix
open ArrayVector
open RatOps

open Batteries

let timing_wrap = Vector.timing_wrap

module type ArrayMatrix =
sig
  include Matrix
  val get_col: t -> int -> vec

  val set_col_with: t -> vec -> int -> t

  val del_col: t -> int -> t

  val reduce_col_with: t -> int -> unit

  val normalize_with: t -> bool

  val rref_vec_with: t -> vec -> t Option.t

  val rref_matrix_with: t -> t -> t Option.t

  val map2_with: (vec -> num -> vec) -> t -> vec -> unit

  val map2i_with: (int -> vec -> num -> vec) -> t -> vec -> unit

  val append_matrices: t -> t -> t
end

(** Some functions inside have the suffix _with, which means that the function is not purely functional. *)
module type ArrayMatrixFunctor =
  functor (A: RatOps) (V: ArrayVectorFunctor) ->
  sig
    include ArrayMatrix with type vec := V(A).t and type num := A.t
  end

(** Array-based matrix implementation.
    It provides a normalization function to reduce a matrix into reduced row echelon form.
    Operations exploit that the input matrix/matrices are in reduced row echelon form already. *)
(* The functions that have the suffix _with are not purely functional and affect the program state beyond their return value.
    They were used in a previous version of the affineEqualityDomain.
    These calls were removed to transition to list-based matrices.*)
module ArrayMatrix: ArrayMatrixFunctor =
  functor (A: RatOps) (V: ArrayVectorFunctor) ->
  struct
    include ConvenienceOps(A)
    module V = V(A)

    type t = A.t array array [@@deriving eq, ord, hash]

    let show x =
      Array.fold_left (^) "" (Array.map (fun v -> V.show @@ V.of_array v) x)

    let empty () =
      Array.make_matrix 0 0 A.zero

    let num_rows m =
      Array.length m

    let compare_num_rows m1 m2 =
      Int.compare (Array.length m1) (Array.length m2)

    let is_empty m =
      (num_rows m = 0)

    let num_cols m =
      if is_empty m then 0 else Array.length m.(0)

    let copy m =
      let cp = Array.make_matrix (num_rows m) (num_cols m) A.zero in
      Array.iteri (fun i x -> Array.blit x 0 cp.(i) 0 (num_cols m)) m; cp

    let copy m = timing_wrap "copy" (copy) m

    let add_empty_columns m cols =
      let nnc = Array.length cols in
      if is_empty m || nnc = 0 then m else
        let nr, nc = num_rows m, num_cols m in
        let m' = Array.make_matrix nr (nc + nnc) A.zero in
        for i = 0 to nr - 1 do
          let offset = ref 0 in
          for j = 0 to nc - 1 do
            while  !offset < nnc &&  !offset + j = cols.(!offset) do incr offset done;
            m'.(i).(j + !offset) <- m.(i).(j);
          done
        done;
        m'

    let add_empty_columns m cols = timing_wrap "add_empty_cols" (add_empty_columns m) cols

    let append_row m row  =
      let size = num_rows m in
      let new_matrix = Array.make_matrix (size + 1) (num_cols m) A.zero in
      for i = 0 to size - 1 do
        new_matrix.(i) <- m.(i)
      done;
      new_matrix.(size) <- V.to_array row;
      new_matrix

    let get_row m n =
      V.of_array m.(n)

    let remove_row m n =
      let new_matrix = Array.make_matrix (num_rows m - 1) (num_cols m) A.zero in
      if not @@ is_empty new_matrix then
        if n = 0 then
          Array.blit m 1 new_matrix 0 (num_rows m - 1)
        else
          (Array.blit m 0 new_matrix 0 n;
           if n <> (num_rows m - 1) then
             Array.blit m (n + 1) new_matrix n (num_rows new_matrix - n)); new_matrix

    let get_col m n =
      V.of_array @@ Array.init (Array.length m) (fun i -> m.(i).(n))

    let get_col m n = timing_wrap "get_col" (get_col m) n

    let set_col_with m new_col n =
      for i = 0 to num_rows m - 1 do
        m.(i).(n) <- V.nth new_col i
      done; m

    let set_col_with m new_col n = timing_wrap "set_col" (set_col_with m new_col) n

    let set_col m new_col n =
      let copy = copy m in
      set_col_with copy new_col n

    let append_matrices m1 m2  =
      Array.append m1 m2

    let equal m1 m2 = timing_wrap "equal" (equal m1) m2

    let reduce_col_with m j =
      if not @@ is_empty m then
        (let r = ref (-1) in
         for i' = 0 to num_rows m - 1 do                            (* i' runs through all row indices *)
           let rev_i' = num_rows m - i' - 1 in                      (* rev_i' runs through all row indices in reverse order *)
           if !r < 0 && m.(rev_i').(j) <>: A.zero then r := rev_i'; (* if we found the j-column (non-zero element), store its row index in r *)
           if !r <> rev_i' then                                     (* exclude the non-zero element's row from the transformation *)
             let g = m.(rev_i').(j) in                              (* only act if rev_i'/j element is non-zero*)
             if g <>: A.zero then
               let s = g /: m.(!r).(j) in                           (* determine the kill factor*)
               for j' = 0 to num_cols m - 1 do                      (* kill the rev_i' element, eintrailing a modification of the rest columns*)
                 m.(rev_i').(j') <- m.(rev_i').(j') -: s *: m.(!r).(j')
               done
         done;
         if !r >= 0 then Array.fill m.(!r) 0 (num_cols m) A.zero)   (* kill row r *)

    let reduce_col_with m j  = timing_wrap "reduce_col_with" (reduce_col_with m) j
    let reduce_col m j =
      let copy = copy m in
      reduce_col_with copy j;
      copy

    let del_col m j =
      if is_empty m then m else
        let new_matrix = Array.make_matrix (num_rows m) (num_cols m - 1) A.zero in
        for i = 0 to num_rows m - 1 do
          new_matrix.(i) <- Array.remove_at j m.(i)
        done; new_matrix

    let del_cols m cols =
      let n_c = Array.length cols in
      if n_c = 0 || is_empty m then m                           (* if #toberemoved=0, return m *)
      else
        let m_r, m_c = num_rows m, num_cols m in
        if m_c = n_c then empty () else                         (* if #cols = #toberemoved, return empty *)
          let m' = Array.make_matrix m_r (m_c - n_c) A.zero in  (* else alloc smaller array m' *)
          for i = 0 to m_r - 1 do                               (* i iterates rows of m' *)
            let offset = ref 0 in                               (* offset keeps track of coloffset *)
            for j = 0 to (m_c - n_c) - 1 do                     (* j iterates cols of m' *)
              while  !offset < n_c &&  !offset + j = cols.(!offset) do incr offset done;
              m'.(i).(j) <- m.(i).(j + !offset);                (* copy m to m' *)
            done
          done;
          m'

    let del_cols m cols = timing_wrap "del_cols" (del_cols m) cols

    (* This does NOT have the same semantics as map2i_with. While map2i_with can deal with m and v having different lengths, map2i will raise Invalid_argument in that case*)
    let map2i f m v =
      let f' x (i,y) = V.to_array @@ f i (V.of_array x) y in
      let range_array = Array.init (V.length v) Fun.id in
      Array.map2 f' m (Array.combine range_array (V.to_array v))

    let remove_zero_rows m =
      Array.filter (fun x -> Array.exists (fun y -> y <>: A.zero) x) m

    let rref_with m =
      (*Based on Cousot - Principles of Abstract Interpretation (2021)*)
      let swap_rows i1 i2 =
        let tmp = m.(i1) in
        m.(i1) <- m.(i2);
        m.(i2) <- tmp;
      in
      let exception Unsolvable in
      let num_rows = num_rows m in
      let num_cols = num_cols m in
      try (
        for i = 0 to num_rows-1 do
          let exception Found in
          try (
            for j = i to num_cols -2 do
              for k = i to num_rows -1 do
                if m.(k).(j) <>: A.zero then
                  (
                    if k <> i then swap_rows k i;
                    let piv = m.(i).(j) in
                    Array.iteri(fun j' x -> m.(i).(j') <- x /: piv) m.(i);
                    for l = 0 to num_rows-1 do
                      if l <> i && m.(l).(j) <>: A.zero then (
                        let is_only_zero = ref true in
                        let m_lj = m.(l).(j) in
                        for k = 0 to num_cols - 2 do
                          m.(l).(k) <- m.(l).(k) -: m.(i).(k) *: m_lj /: m.(i).(j);
                          if m.(l).(k) <>: A.zero then is_only_zero := false;
                        done;
                        let k_end = num_cols - 1 in
                        m.(l).(k_end) <- m.(l).(k_end) -: m.(i).(k_end) *: m_lj /: m.(i).(j);
                        if !is_only_zero && m.(l).(k_end) <>: A.zero then raise Unsolvable;
                      )
                    done;
                    raise Found
                  )
              done;
            done;
          )
          with Found -> ()
        done;
        true)
      with Unsolvable -> false

    let rref_with m = timing_wrap "rref_with" rref_with m

    let init_with_vec v =
      let new_matrix = Array.make_matrix 1 (V.length v) A.zero in
      new_matrix.(0) <- (V.to_array v); new_matrix


    let reduce_col_with_vec m j v =
      for i = 0 to num_rows m - 1 do
        if m.(i).(j) <>: A.zero then
          let beta = m.(i).(j) /: v.(j) in
          Array.iteri (fun j' x ->  m.(i).(j') <- x -: beta *: v.(j')) m.(i)
      done

    let get_pivot_positions m =
      let pivot_elements = Array.make (num_rows m) 0
      in Array.iteri (fun i x -> pivot_elements.(i) <- Array.findi (fun z -> z =: A.one) x) m; pivot_elements

    let rref_vec m pivot_positions v =
      let insert = ref (-1) in
      for j = 0 to Array.length v -2 do
        if v.(j) <>: A.zero then
          match Array.bsearch  Int.ord pivot_positions j with
          | `At i -> let beta = v.(j) /: m.(i).(j) in
            Array.iteri (fun j' x -> v.(j') <- x -: beta *: m.(i).(j')) v
          | _ -> if !insert < 0 then (let v_i = v.(j) in
                                      Array.iteri (fun j' x -> v.(j') <- x /: v_i) v; insert := j;
                                      reduce_col_with_vec m j v)

      done;
      if !insert < 0 then (
        if v.(Array.length v - 1) <>: A.zero then None
        else Some m
      )
      else
        let new_m = Array.make_matrix (num_rows m + 1) (num_cols m) A.zero
        in let (i, j) = Array.pivot_split Int.ord pivot_positions !insert in
        if i = 0 && j = 0 then (new_m.(0) <- v; Array.blit m 0 new_m 1 (num_rows m))
        else if i = num_rows m && j = num_rows m then (Array.blit m 0  new_m 0 j; new_m.(j) <- v)
        else (Array.blit m 0 new_m 0 i; new_m.(i) <- v; Array.blit m i new_m (i + 1) (Array.length m - j));
        Some new_m


    let rref_vec_with m v =
      (*This function yields the same result as appending vector v to m, normalizing it and removing zero rows would. However, it is usually faster than performing those ops manually.*)
      (*m must be in rref form and contain the same num of cols as v*)
      (*If m is empty then v is simply normalized and returned*)
      let v = V.to_array v in
      if is_empty m then
        match Array.findi (fun x -> x <>: A.zero) v with
        | exception Not_found -> None
        | i -> if i = Array.length v - 1 then None else
            let v_i = v.(i) in
            Array.iteri (fun j x -> v.(j) <- x /: v_i) v; Some (init_with_vec @@ V.of_array v)
      else
        let pivot_elements = get_pivot_positions m in
        rref_vec m pivot_elements v

    let rref_vec_with m v = timing_wrap "rref_vec_with" (rref_vec_with m) v

    let rref_matrix_with m1 m2 =
      (*Similar to rref_vec_with but takes two matrices instead.*)
      (*ToDo Could become inefficient for large matrices since pivot_elements are always recalculated + many row additions*)
      let b_m, s_m = if num_rows m1 > num_rows m2 then m1, m2 else m2, m1 in
      let b = ref b_m in
      let exception Unsolvable in
      try (
        for i = 0 to num_rows s_m - 1 do
          let pivot_elements = get_pivot_positions !b in
          let res = rref_vec !b pivot_elements s_m.(i) in
          match res with
          | None -> raise Unsolvable
          | Some res -> b := res
        done;
        Some !b
      )
      with Unsolvable -> None

    let rref_matrix_with m1 m2 = timing_wrap "rref_matrix_with" (rref_matrix_with m1) m2

    let normalize_with m =
      rref_with m

    let normalize_with m = timing_wrap "normalize_with" normalize_with m

    let normalize m =
      let copy = copy m in
      if normalize_with copy then
        Some copy
      else
        None

    let is_covered_by m1 m2 =
      (*Performs a partial rref reduction to check if concatenating both matrices and afterwards normalizing them would yield a matrix <> m2 *)
      (*Both input matrices must be in rref form!*)
      if num_rows m1 > num_rows m2 then false else
        let p2 = lazy (get_pivot_positions m2) in
        try (
          for i = 0 to num_rows m1 - 1 do
            if Array.exists2 (<>:) m1.(i) m2.(i) then
              let m1_i = Array.copy m1.(i) in
              for j = 0 to Array.length m1_i - 2 do
                if m1_i.(j) <>: A.zero then
                  match Array.bsearch Int.ord (Lazy.force p2) j with
                  | `At pos -> let beta =  m1_i.(j) in
                    Array.iteri (fun j' x -> m1_i.(j') <- m1_i.(j') -: beta *: m2.(pos).(j') ) m1_i
                  | _ -> raise Stdlib.Exit;
              done;
              if m1_i. (num_cols m1 - 1) <>: A.zero then
                raise Stdlib.Exit
          done;
          true
        )
        with Stdlib.Exit -> false;;

    let is_covered_by m1 m2 = timing_wrap "is_covered_by" (is_covered_by m1) m2

    let find_opt f m =
      let f' x = f (V.of_array x) in Option.map V.of_array (Array.find_opt f' m)

    let map2 f m v =
      let f' x y = V.to_array @@ f (V.of_array x) y in Array.map2 f' m (V.to_array v)

    let map2_with f m v =
      if num_rows m = V.length v then
        Array.iter2i (fun i x y -> m.(i) <- V.to_array @@ f (V.of_array x) y) m (V.to_array v)
      else
        for i = 0 to Stdlib.min (num_rows m) (V.length v) -1  do
          m.(i) <- V.to_array @@ f (V.of_array m.(i)) (V.nth v i)
        done

    let map2_with f m v = timing_wrap "map2_with" (map2_with f m) v

    let map2i_with f m v =
      if num_rows m = V.length v then
        Array.iter2i (fun i x y -> m.(i) <- V.to_array @@ f i (V.of_array x) y) m (V.to_array v)
      else
        for i = 0 to Stdlib.min (num_rows m) (V.length v) -1 do
          m.(i) <- V.to_array @@ f i (V.of_array m.(i)) (V.nth v i)
        done

    let map2i_with f m v = timing_wrap "map2i_with" (map2i_with f m) v
  end