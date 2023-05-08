open Batteries
module Array = Batteries.Array
module M = Messages

(* let timing_wrap = Timing.wrap *)
(* Disable timing of VectorMatrix and AffineEqualityDomain.
   This is cleaner than a timing functor because the timed functions also call each other. *)
let timing_wrap _ f x = f x

(** Abstracts the functions of the Mpqf module for rationals from Apron that implements multi-precision rationals.
    One could later exchange "Mpqf" with a different module that provides the functions specified by this interface. *)
module type RatOps =
sig
  type t [@@deriving eq, ord, hash]
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val neg : t -> t
  val abs : t -> t
  val to_string:  t -> string
  val of_int: int -> t
  val zero: t
  val one: t
  val get_den: t -> IntOps.BigIntOps.t
  val get_num: t -> IntOps.BigIntOps.t
end

(** It provides more readable infix operators for the functions of RatOps.
    It is designed to be included by modules that make use of RatOps's functions. *)
module ConvenienceOps (A: RatOps) =
struct
  let ( *: ) = A.mul
  let (+:) = A.add
  let (-:) = A.sub
  let (/:) = A.div
  let (=:) x y = A.equal x y
  let (<>:) x y = not (A.equal x y)
  let (<:) x y = A.compare x y < 0
  let (>:) x y = A.compare x y > 0
  let (<=:) x y = A.compare x y <= 0
  let (>=:) x y = A.compare x y >= 0
  let of_int x = A.of_int x
end

(** High-level abstraction of a vector. *)
module type Vector =
sig
  type num
  type t [@@deriving eq, ord, hash]

  val show: t -> string

  val keep_vals: t -> int ->  t

  val remove_val: t -> int ->  t

  val set_val: t -> int -> num ->  t

  val set_val_with: t -> int -> num -> unit

  val insert_val: int -> num ->  t ->  t

  val apply_with_c: (num -> num -> num) -> num ->  t ->  t

  val apply_with_c_with: (num -> num -> num) -> num -> t -> unit

  val zero_vec: int -> t

  val nth: t -> int -> num

  val length: t -> int

  val map2: (num -> num -> num) -> t -> t -> t

  val map2_with: (num -> num -> num) -> t -> t -> unit

  val findi: (num -> bool) ->  t -> int

  val map: (num -> num) -> t -> t

  val map_with: (num -> num) -> t -> unit

  val compare_length_with: t -> int -> int

  val of_list: num list -> t

  val to_list: t -> num list

  val filteri: (int -> num -> bool) -> t -> t

  val append: t -> t -> t

  val exists: (num -> bool) -> t -> bool

  val rev: t -> t

  val rev_with: t -> unit

  val map2i: (int -> num -> num -> num) -> t -> t -> t

  val map2i_with: (int -> num -> num -> num) -> t -> t -> unit

  val mapi: (int -> num -> num) -> t -> t

  val mapi_with: (int -> num -> num) -> t -> unit

  val find2i: (num -> num -> bool) -> t -> t -> int

  val to_array: t -> num array

  val of_array: num array -> t

  val copy: t -> t
end

(** Some functions inside have the suffix _with, which means that the function has side effects. *)
module type AbstractVector =
  functor (A: RatOps) ->
  sig
    include Vector with type num:= A.t
  end


(** High-level abstraction of a matrix. *)
module type Matrix =
sig
  type num
  type vec
  type t [@@deriving eq, ord, hash]

  val empty: unit -> t (* TODO: needs unit? *)

  val is_empty: t -> bool

  val show: t -> string

  val add_empty_column: t -> int -> t

  val add_empty_columns: t -> int array -> t

  val append_row: t -> vec -> t

  val get_row: t -> int -> vec

  val del_col: t -> int -> t

  val del_cols: t -> int array -> t

  val remove_row: t -> int -> t

  val get_col: t -> int -> vec

  val append_matrices: t -> t -> t

  val num_rows: t -> int

  val num_cols: t -> int

  val reduce_col: t -> int -> t

  val reduce_col_with: t -> int -> unit

  val normalize: t -> t Option.t (*Gauss-Jordan Elimination to get matrix in reduced row echelon form (rref) + deletion of zero rows. None matrix has no solution*)

  val normalize_with: t -> bool

  val rref_vec_with: t -> vec -> t Option.t

  val rref_matrix_with: t -> t -> t Option.t

  val find_opt: (vec -> bool) -> t -> vec option

  val map2: (vec -> num -> vec) -> t -> vec -> t

  val map2_with: (vec -> num -> vec) -> t -> vec -> unit

  val map2i: (int -> vec-> num -> vec) -> t -> vec -> t

  val map2i_with: (int -> vec -> num -> vec) -> t -> vec -> unit

  val set_col: t -> vec -> int -> t

  val set_col_with: t -> vec -> int -> t

  val init_with_vec: vec -> t

  val remove_zero_rows: t -> t

  val is_covered_by: t -> t -> bool

  val copy: t -> t

end

(** Some functions inside have the suffix _with, which means that the function has side effects. *)
module type AbstractMatrix =
  functor (A: RatOps) (V: AbstractVector) ->
  sig
    include Matrix with type vec := V(A).t and type num := A.t
  end


(** Array-based vector implementation. *)
module ArrayVector: AbstractVector =
  functor (A: RatOps) ->
  struct
    include ConvenienceOps (A)
    include Array
    type t = A.t array [@@deriving eq, ord]
    let hash = Array.fold_left (fun acc a -> 31 * acc + A.hash a) 0 (* TODO: array in ppx_deriving_hash *)

    let show t =
      let t = Array.to_list t in
      let rec list_str l =
        match l with
        | [] -> "]"
        | x :: xs -> (A.to_string x) ^" "^(list_str xs)
      in
      "["^list_str t^"\n"

    let keep_vals v n =
      if n >= Array.length v then v else
        Array.filteri (fun i x -> i < n) v (* TODO: take? *)

    let compare_length_with v len =
      Int.compare (Array.length v) len

    let remove_val v n =
      if n >= Array.length v then failwith "n outside of Array range" else
        Array.init (Array.length v - 1) (fun i -> if i < n then Array.get v i else Array.get v (i + 1)) (* TODO: remove_at? *)

    let set_val_with v n new_val =
      if n >= Array.length v then failwith "n outside of Array range" else
        Array.set v n new_val

    let set_val v n new_val =
      let copy = copy v in
      set_val_with copy n new_val; copy

    let insert_val n new_val v =
      if n > Array.length v then failwith "n too large" else
        Array.init (Array.length v + 1) (fun i -> if i < n then Array.get v i else if i = n then new_val else Array.get v (i -1)) (* insert? *)

    let apply_with_c f c v =
      Array.map (fun x -> f x c) v

    let zero_vec n = Array.make n A.zero

    let nth = Array.get

    let map2i f v1 v2 = let f' i (v'1, v'2) = f i v'1 v'2 in Array.mapi f' (Array.combine v1 v2) (* TODO: iter2i? *)

    let map2i_with f v1 v2 = Array.iter2i (fun i x y -> v1.(i) <- f i x y) v1 v2

    let find2i f v1 v2 = let f' (v'1, v'2) = f v'1 v'2 in
      Array.findi f' (Array.combine v1 v2) (* TODO: iter2i? *)

    let to_array v = v

    let of_array v = v

    let apply_with_c_with f c v = Array.modify (fun x -> f x c) v

    let rev_with v = Array.rev_in_place v

    let map_with f v = Array.modify f v

    let map2_with f v1 v2 = Array.iter2i (fun i x y -> v1.(i) <- f x y) v1 v2

    let copy v = Array.copy v

    let mapi_with f v = Array.iteri (fun i x -> v.(i) <- f i x) v
  end

open Batteries.Array

(** Array-based matrix implementation.
    It provides a normalization function to reduce a matrix into reduced row echelon form.
    Operations exploit that the input matrix/matrices are in reduced row echelon form already. *)
module ArrayMatrix: AbstractMatrix =
  functor (A: RatOps) (V: AbstractVector) ->
  struct
    include ConvenienceOps(A)
    module V = V(A)

    type t = A.t array array [@@deriving eq, ord]

    let hash = Array.fold_left (Array.fold_left (fun acc a -> 31 * acc + A.hash a)) 0

    let show x =
      Array.fold_left (^) "" (Array.map (fun v -> V.show @@ V.of_array v) x)

    let empty () =
      Array.make_matrix 0 0 A.zero

    let num_rows m =
      Array.length m

    let is_empty m =
      (num_rows m = 0)

    let num_cols m =
      if is_empty m then 0 else Array.length m.(0)

    let copy m =
      let cp = Array.make_matrix (num_rows m) (num_cols m) A.zero in
      Array.iteri (fun i x -> Array.blit x 0 cp.(i) 0 (num_cols m)) m; cp

    let copy m = timing_wrap "copy" (copy) m

    let add_empty_column m n =
      if is_empty m then m else
        let nc = Array.length m.(0) in
        if n > nc then failwith "n too large" else
          let new_matrix = make_matrix (Array.length m) (Array.length m.(0) + 1) A.zero in
          Array.iteri (fun i r -> if n = 0 then Array.blit r 0 new_matrix.(i) 1 (nc - 1) else
                          Array.blit r 0 new_matrix.(i) 0 n; if n <> nc then Array.blit r n new_matrix.(i) (n + 1) (nc - n)) m;
          new_matrix

    let add_empty_columns m cols =
      let nnc = Array.length cols in
      if is_empty m || nnc = 0 then m else
        let nr, nc = num_rows m, num_cols m in
        let m' = make_matrix nr (nc + nnc) A.zero in
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
      let new_matrix = make_matrix (size + 1) (num_cols m) A.zero in
      for i = 0 to size - 1 do
        new_matrix.(i) <- m.(i)
      done;
      new_matrix.(size) <- V.to_array row;
      new_matrix

    let get_row m n =
      V.of_array m.(n)

    let remove_row m n =
      let new_matrix = make_matrix (num_rows m - 1) (num_cols m) A.zero in
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
         for i' = 0 to num_rows m - 1 do
           let rev_i' = num_rows m - i' - 1 in
           if !r < 0 && m.(rev_i').(j) <>: A.zero then r := rev_i';
           if !r <> rev_i' then
             let g = m.(rev_i').(j) in
             if g <>: A.zero then
               let s = g /: m.(!r).(j) in
               for j' = 0 to num_cols m - 1 do
                 m.(rev_i').(j') <- m.(rev_i').(j') -: s *: m.(!r).(j')
               done
         done;
         if !r >= 0 then Array.fill m.(!r) 0 (num_cols m) A.zero)

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
      if n_c = 0 || is_empty m then m
      else
        let m_r, m_c = num_rows m, num_cols m in
        if m_c = n_c then empty () else
          let m' = Array.make_matrix m_r (m_c - n_c) A.zero in
          for i = 0 to m_r - 1 do
            let offset = ref 0 in
            for j = 0 to (m_c - n_c) - 1 do
              while  !offset < n_c &&  !offset + j = cols.(!offset) do incr offset done;
              m'.(i).(j) <- m.(i).(j + !offset);
            done
          done;
          m'

    let del_cols m cols = timing_wrap "del_cols" (del_cols m) cols

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
      (*This function yields the same result as appending vector v to m and normalizing it afterwards would. However, it is usually faster than performing those ops manually.*)
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
                  | _ -> raise Exit;
              done;
              if m1_i. (num_cols m1 - 1) <>: A.zero then
                raise Exit
          done;
          true
        )
        with Exit -> false;;

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
