open Prelude
module Array = Batteries.Array
module M = Messages

module type RatOps =
sig
  type t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val neg : t -> t
  val abs : t -> t
  val cmp : t -> t -> int
  val to_string:  t -> string
  val of_int: int -> t
  val get_den: t -> IntOps.BigIntOps.t
  val get_num: t -> IntOps.BigIntOps.t
end

module Mpqf = struct
  include Mpqf

  let get_den x = Z_mlgmpidl.z_of_mpzf @@ Mpqf.get_den x

  let get_num x = Z_mlgmpidl.z_of_mpzf @@ Mpqf.get_num x
end

module ConvenienceOps (A: RatOps) =
struct
  let ( *: ) = A.mul
  let (+:) = A.add
  let (-:) = A.sub
  let (/:) = A.div
  let (=:) x y = A.cmp x y = 0
  let (<:) x y = A.cmp x y < 0
  let (>:) x y = A.cmp x y > 0
  let (<=:) x y = A.cmp x y <= 0
  let (>=:) x y = A.cmp x y >= 0
  let of_int x = A.of_int x
end

module type Vector =
sig
  type num
  type t

  val show: t -> string

  val equal:  t ->  t -> bool

  val keep_vals: t -> int ->  t

  val remove_val: t -> int ->  t

  val set_val: t -> int -> num ->  t

  val set_val_pt_with: t -> int -> num -> t

  val insert_val: int -> num ->  t ->  t

  val apply_with_c: (num -> num -> num) -> num ->  t ->  t

  val apply_with_c_pt_with: (num -> num -> num) -> num -> t -> t

  val zero_vec: int -> t

  val nth: t -> int -> num

  val length: t -> int

  val map2: (num -> num -> num) -> t -> t -> t

  val map2_pt_with: (num -> num -> num) -> t -> t -> t

  val findi: (num -> bool) ->  t -> int

  val map: (num -> num) -> t -> t

  val map_pt_with: (num -> num) -> t -> t

  val compare_length_with: t -> int -> int

  val of_list: num list -> t

  val to_list: t -> num list

  val filteri: (int -> num -> bool) -> t -> t

  val append: t -> t -> t

  val exists: (num -> bool) -> t -> bool

  val rev: t -> t

  val rev_pt_with: t -> t

  val map2i: (int -> num -> num -> num) -> t -> t -> t

  val map2i_pt_with: (int -> num -> num -> num) -> t -> t -> t

  val mapi: (int -> num -> num) -> t -> t

  val mapi_pt_with: (int -> num -> num) -> t -> t

  val find2i: (num -> num -> bool) -> t -> t -> int

  val to_array: t -> num array

  val of_array: num array -> t

  val copy_pt_with: t -> t
end

module type AbstractVector =
  functor (A: RatOps) ->
  sig
    include Vector with type num:= A.t
  end



module ListVector: AbstractVector =
  functor (A: RatOps) ->
  struct
    include List
    include ConvenienceOps (A)
    type t = A.t List.t

    let show t =
      let rec list_str l =
        match l with
        | [] -> "]"
        | x :: xs -> (A.to_string x) ^" "^(list_str xs)
      in
      "["^list_str t^"\n"

    let equal v1 v2 =
      List.equal (=:) v1 v2

    let keep_vals v n =
      List.filteri (fun i x -> i < n) v

    let remove_val v n =
      if n < 0 || n >= List.length v then failwith "Entry does not exist"
      else List.filteri (fun i x -> i <> n) v

    let set_val (v: A.t List.t) n (new_val: A.t) =
      if n < 0 || n >= List.length v then failwith "Entry does not exist"
      else List.mapi (fun i x -> if i = n then new_val else x) v

    let rec insert_val n vl v =
      match v with
      | [] -> if n = 0 then [vl] else failwith "Entry does not exist"
      | x :: xs -> if n > 0 then x :: insert_val (n-1) vl xs else vl :: x :: xs

    let apply_with_c op c v =
      List.map (fun x -> op x c) v

    let zero_vec size = List.init size (fun i -> of_int 0)

    let of_list l = l

    let to_list v = v

    let exists = exists

    let findi f v = let (n, _) = List.findi (fun i x -> f x) v in n

    let find2i f r s = let f' (x,y) = f x y in findi f' (List.combine r s)

    let to_array v = Array.of_list v

    let of_array v = Array.to_list v

    let set_val_pt_with = set_val
    let apply_with_c_pt_with = apply_with_c

    let map2_pt_with = map2

    let map_pt_with = map

    let rev_pt_with = rev

    let copy_pt_with v = v

    let mapi_pt_with = mapi

    let map2i_pt_with = map2i
  end

module type Matrix =
sig
  type num
  type vec
  type t

  val empty: unit -> t

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

  val equal: t -> t -> bool

  val reduce_col: t -> int -> t

  val reduce_col_pt_with: t -> int -> t

  val normalize: t -> t Option.t (*Gauss-Jordan Elimination to get matrix in reduced row echelon form (rref) + deletion of zero rows. None matrix has no solution*)

  val normalize_pt_with: t -> t Option.t

  val find_opt: (vec -> bool) -> t -> vec option

  val map2: (vec -> num -> vec) -> t -> vec -> t

  val map2_pt_with: (vec -> num -> vec) -> t -> vec -> t

  val map2i: (int -> vec-> num -> vec) -> t -> vec -> t

  val map2i_pt_with: (int -> vec -> num -> vec) -> t -> vec -> t

  val set_col: t -> vec -> int -> t

  val set_col_with: t -> vec -> int -> t

  val init_with_vec: vec -> t

  val remove_zero_rows: t -> t

  val is_covered_by: t -> t -> bool

  val copy_pt: t -> t

end

module type AbstractMatrix =
  functor (A: RatOps) (V: AbstractVector) ->
  sig
    include Matrix with type vec := V(A).t and type num := A.t
  end

module ListMatrix : AbstractMatrix =
  functor (A: RatOps) (V: AbstractVector) ->
  struct
    include ConvenienceOps(A)
    module V = V(A)
    type t = V.t list

    let show x =
      List.fold_left (^) "" (List.map (V.show) x)

    let empty () = []

    let is_empty m =
      m = empty ()

    let add_empty_column m pos =
      List.map (fun y -> V.insert_val pos (of_int 0) y) m

    let add_empty_columns m cols =
      Array.fold_left (fun m'  x -> add_empty_column m' x) m cols

    let append_row m row  =
      List.append m [row]

    let get_row m n =
      List.nth m n

    let num_rows m =
      List.length m

    let remove_row m n =
      if n < 0 || n >= num_rows m then failwith "Entry does not exist"
      else
        List.filteri (fun i x -> n <> i) m

    let get_col m n =
      V.of_list (List.map (fun x -> V.nth x n) m)

    let set_col m new_col n =
      List.mapi (fun i r -> V.set_val r n (V.nth new_col i)) m

    let append_matrices m1 m2  =
      List.append m1 m2

    let num_cols m =
      match m with
      | [] -> 0
      | x :: xs -> V.length x

    let equal m1 m2 =
      List.equal V.equal m1 m2

    let subtract_rows_c row1 row2 c =
      V.map2 (fun x y -> x -: (y *: c)) row1 row2

    let reduce_row_to_zero m row_n col_n =
      let red_row = get_row m row_n in
      let c = V.nth red_row col_n in
      List.map (fun x -> subtract_rows_c x red_row ((V.nth x col_n) /: c)) m

    let del_col m col_n =
      List.map (fun x -> V.remove_val x col_n) m

    let del_cols m cols =
      Array.fold_lefti (fun y i x -> del_col y (x - i)) m cols

    let reduce_col m col_n =
      match List.findi (fun i x -> V.nth x col_n <> of_int 0)  (List.rev m) with
      | exception Not_found -> m
      | (i, _) -> let len_i = List.length m - (i + 1)
        in reduce_row_to_zero m len_i col_n

    let map2i f m v =
      List.map2i f m (V.to_list v)

    let remove_zero_rows m =
      List.filter (fun x -> V.exists (fun y -> y <> of_int 0) x) m

    let normalize t =
      let exception NoSolution in
      let rec create_rref new_t curr_row =
        if curr_row >= num_rows new_t then new_t else
          let row = List.nth new_t curr_row in
          match V.findi (fun x -> x <> of_int 0) row with
          | exception Not_found -> create_rref (remove_row new_t curr_row) curr_row
          | i -> if V.compare_length_with row (i + 1) <= 0 then raise NoSolution
            else let p = V.map (fun x -> x /: (V.nth row i)) row in
              let col = get_col new_t i in
              let res = map2i (fun row_i y z -> if row_i <> curr_row then subtract_rows_c y p z else p) new_t col
              in create_rref res (curr_row + 1)
      in
      match create_rref t 0 with
      | c -> let sort v1 v2 =
               let f = V.findi (fun x -> x =: of_int 1) in
               let i1, i2 = (f v1), (f v2) in Int.compare i1 i2
        in Some (List.sort sort c)
      | exception NoSolution -> None

    let normalize t =
      let res = normalize t in
      if M.tracing then M.tracel "norm" "normalize: %s -> %s \n" (show t) (if Option.is_none res then "None" else show @@ Option.get res); res

    let is_covered_by m1 m2 =
      let m_c = append_matrices m1 m2 in
      match normalize m_c with
      | Some x when equal x m2 -> true
      | _ -> false

    let find_opt =
      List.find_opt

    let map2 f m v =
      List.map2 f m (V.to_list v)

    let init_with_vec v =
      [v]

    let normalize_pt_with = normalize

    let reduce_col_pt_with = reduce_col

    let copy_pt t = t

    let set_col_with = set_col

    let map2_pt_with f m v =
      let v' = let a_length, b_length = num_rows m, V.length v in
        match Int.compare a_length b_length with
        | -1 -> V.keep_vals v a_length
        |  1 -> V.append v @@ V.zero_vec (a_length - b_length)
        | _ -> v
      in
      map2 f m v'

    let map2i_pt_with f m v =
      let v' = let a_length, b_length = num_rows m, V.length v in
        match Int.compare a_length b_length with
        | -1 -> V.keep_vals v a_length
        |  1 -> V.append v @@ V.zero_vec (a_length - b_length)
        | _ -> v
      in
      map2i f m v'
  end


module ArrayVector: AbstractVector =
  functor (A: RatOps) ->
  struct
    include ConvenienceOps (A)
    include Array
    type t = A.t array

    let show t =
      let t = Array.to_list t in
      let rec list_str l =
        match l with
        | [] -> "]"
        | x :: xs -> (A.to_string x) ^" "^(list_str xs)
      in
      "["^list_str t^"\n"

    let equal v1 v2 =
      if Array.length v1 <> Array.length v2 then false else
        not @@ Array.exists2 (<>) v1 v2

    let keep_vals v n =
      if n >= Array.length v then v else
        Array.filteri (fun i x -> i < n) v

    let compare_length_with v len =
      Stdlib.compare (Array.length v) len

    let remove_val v n =
      if n >= Array.length v then failwith "n outside of Array range" else
        Array.init (Array.length v - 1) (fun i -> if i < n then Array.get v i else Array.get v (i + 1))

    let set_val_with v n new_val =
      if n >= Array.length v then failwith "n outside of Array range" else
        Array.set v n new_val

    let set_val v n new_val =
      let copy = copy v in
      set_val_with copy n new_val; copy

    let insert_val n new_val v =
      if n > Array.length v then failwith "n too large" else
        Array.init (Array.length v + 1) (fun i -> if i < n then Array.get v i else if i = n then new_val else Array.get v (i -1))

    let apply_with_c f c v =
      Array.map (fun x -> f x c) v

    let zero_vec n = Array.make n (of_int 0)

    let nth = Array.get

    let map2i f v1 v2 = let f' i (v'1, v'2) = f i v'1 v'2 in Array.mapi f' (Array.combine v1 v2)

    let map2i_pt_with f v1 v2 = Array.iter2i (fun i x y -> v1.(i) <- f i x y) v1 v2; v1

    let find2i f v1 v2 = let f' (v'1, v'2) = f v'1 v'2 in
      Array.findi f' (Array.combine v1 v2)

    let to_array v = v

    let of_array v = v

    let set_val_pt_with v i a = v.(i) <- a; v

    let apply_with_c_pt_with f c v = Array.modify (fun x -> f x c) v; v

    let rev_pt_with v = Array.rev_in_place v; v

    let map_pt_with f v = Array.modify f v; v

    let map2_pt_with f v1 v2 = Array.iter2i (fun i x y -> v1.(i) <- f x y) v1 v2; v1

    let copy_pt_with v = Array.copy v

    let mapi_pt_with f v = Array.iteri (fun i x -> v.(i) <- f i x) v; v
  end

open Batteries.Array

module ArrayMatrix: AbstractMatrix =
  functor (A: RatOps) (V: AbstractVector) ->
  struct
    include ConvenienceOps(A)
    module V = V(A)

    type t = A.t array array

    let show x =
      Array.fold_left (^) "" (Array.map (fun v -> V.show @@ V.of_array v) x)

    let empty () =
      Array.make_matrix 0 0 (of_int 0)

    let num_rows m =
      Array.length m

    let is_empty m =
      (num_rows m = 0)

    let num_cols m =
      if is_empty m then 0 else Array.length m.(0)

    let copy m =
      let cp = Array.make_matrix (num_rows m) (num_cols m) (of_int 0) in
      Array.iteri (fun i x -> Array.blit x 0 cp.(i) 0 (num_cols m)) m; cp

    let add_empty_column m n =
      if is_empty m then m else
        let nc = Array.length m.(0) in
        if n > nc then failwith "n too large" else
          let new_matrix = create_matrix (Array.length m) (Array.length m.(0) + 1) (of_int 0) in
          Array.iteri (fun i r -> if n = 0 then Array.blit r 0 new_matrix.(i) 1 (nc - 1) else
                          Array.blit r 0 new_matrix.(i) 0 n; if n <> nc then Array.blit r n new_matrix.(i) (n + 1) (nc - n)) m;
          new_matrix

    let add_empty_columns m cols =
      let nnc = Array.length cols in
      if is_empty m || nnc = 0 then m else
        let nr, nc = num_rows m, num_cols m in
        let m' = create_matrix nr (nc + nnc) (of_int 0) in
        for i = 0 to nr - 1 do
          let offset = ref 0 in
          for j = 0 to nc - 1 do
            while  !offset < nnc &&  !offset + j = cols.(!offset) do incr offset done;
            m'.(i).(j + !offset) <- m.(i).(j);
          done
        done;
        m'

    let append_row m row  =
      let size = num_rows m in
      let new_matrix = create_matrix (size + 1) (num_cols m) (of_int 0) in
      for i = 0 to size - 1 do
        new_matrix.(i) <- m.(i)
      done;
      new_matrix.(size) <- V.to_array row;
      new_matrix

    let get_row m n =
      V.of_array m.(n)

    let remove_row m n =
      let new_matrix = create_matrix (num_rows m - 1) (num_cols m) (of_int 0) in
      if not @@ is_empty new_matrix then
        if n = 0 then
          Array.blit m 1 new_matrix 0 (num_rows m - 1)
        else
          (Array.blit m 0 new_matrix 0 n;
           if n <> (num_rows m - 1) then
             Array.blit m (n + 1) new_matrix n (num_rows new_matrix - n)); new_matrix

    let get_col m n =
      V.of_array @@ Array.init (Array.length m) (fun i -> m.(i).(n))

    let set_col_with m new_col n =
      for i = 0 to num_rows m - 1 do
        m.(i).(n) <- V.nth new_col i
      done; m

    let set_col m new_col n =
      let copy = copy m in
      set_col_with copy new_col n

    let append_matrices m1 m2  =
      Array.append m1 m2

    let equal m1 m2 =
      Array.equal (=) m1 m2

    let reduce_col_pt_with m j =
      if not @@ is_empty m then
        (let r = ref (-1) in
         for i' = 0 to num_rows m - 1 do
           let rev_i' = num_rows m - i' - 1 in
           if !r < 0 && m.(rev_i').(j) <> of_int 0 then r := rev_i';
           if !r <> rev_i' then
             let g = m.(rev_i').(j) in
             if g <> of_int 0 then
               let s = g /: m.(!r).(j) in
               for j' = 0 to num_cols m - 1 do
                 m.(rev_i').(j') <- m.(rev_i').(j') -: s *: m.(!r).(j')
               done
         done;
         if !r >= 0 then Array.fill m.(!r) 0 (num_cols m) (of_int 0));
      m

    let reduce_col m j =
      let copy = copy m in
      reduce_col_pt_with copy j

    let del_col m j =
      if is_empty m then m else
        let new_matrix = Array.make_matrix (num_rows m) (num_cols m - 1) (of_int 0) in
        for i = 0 to num_rows m - 1 do
          new_matrix.(i) <- Array.remove_at j m.(i)
        done; new_matrix

    let del_cols m cols =
      let n_c = Array.length cols in
      if n_c = 0 || is_empty m then m
      else
        let m_r, m_c = num_rows m, num_cols m in
        if m_c - n_c = 0 then empty () else
          let m' = Array.create_matrix m_r (m_c - n_c) (of_int 0) in
          for i = 0 to m_r - 1 do
            let offset = ref 0 in
            for j = 0 to (m_c - n_c) - 1 do
              while  !offset < n_c &&  !offset + j = cols.(!offset) do incr offset done;
              m'.(i).(j) <- m.(i).(j + !offset);
            done
          done;
          m'

    let map2i f m v =
      let f' x (i,y) = V.to_array @@ f i (V.of_array x) y in
      let range_array = Array.init (V.length v) Fun.id in
      Array.map2 f' m (Array.combine range_array (V.to_array v))

    let remove_zero_rows m =
      Array.filter (fun x -> Array.exists (fun y -> y <> of_int 0) x) m

    let rref_with m =
      (*from https://rosettacode.org/wiki/Reduced_row_echelon_form#OCaml (Slightly modified) *)
      let swap_rows m i j =
        let tmp = m.(i) in
        m.(i) <- m.(j);
        m.(j) <- tmp;
      in
      try
        let lead = ref 0
        and rows = num_rows m
        and cols = num_cols m in
        for r = 0 to pred rows do
          if cols - 1 <= !lead then
            raise Exit;
          let i = ref r in
          while m.(!i).(!lead) = of_int 0 do
            incr i;
            if rows = !i then begin
              i := r;
              incr lead;
              if cols -1 = !lead then
                raise Exit;
            end
          done;
          swap_rows m !i r;
          let lv = m.(r).(!lead) in
          Array.iteri (fun i v -> m.(r).(i) <- v /: lv) m.(r);
          for i = 0 to pred rows do
            if i <> r then
              let lv = m.(i).(!lead) in
              Array.iteri (fun j iv -> m.(i).(j) <- iv -: lv *: m.(r).(j)) m.(i);
          done;
          incr lead;
        done
      with Exit -> ();;


    let normalize_pt_with m =
      rref_with m;
      let is_unsolvable_row v = match Array.findi (fun x -> x <> of_int 0) v with
        | exception Not_found -> false
        | i -> i = num_cols m - 1
      in
      if Array.exists (fun v -> is_unsolvable_row v) m then None
      else Some (remove_zero_rows m)

    let normalize m =
      let copy = copy m in
      normalize_pt_with copy

    let is_covered_by m1 m2 =
      (*Performs a partial rref reduction to check if concatenating both matrices and afterwards normalizing them would yield a matrix <> m2 *)
      (*Both input matrices must be in rref form!*)
      if num_rows m1 > num_rows m2 then false else
        try
          for i = 0 to num_rows m1 -1 do
            if Array.exists2 (<>) m1.(i) m2.(i) then (*Start to reduce row to zero*)
              (
                let m1_i = Array.copy m1.(i) in
                for j = 0 to num_cols m1 -1 do
                  if m1_i.(j) <> of_int 0 then (
                    if j = num_cols m1 -1 then raise Exit; (*Unsolvable*)
                    let found = ref 0 in
                    let exception Found in
                    try (
                      for i' = i to num_rows m2 -1 do
                        if m2.(i').(j) <> of_int 0 then
                          let beta = m1_i.(j) /: m2.(i').(j) in
                          Array.iteri (fun c x -> m1_i.(c) <- x -: beta *: m2.(i').(c)) m1_i;
                          raise Found;
                      done;)
                    with Found -> incr found;
                      if !found = 0 then raise Exit (*m1.(i).(j) can not be reduced to zero*)
                  )
                done
              )
          done; true
        with Exit -> false;;

    let find_opt f m =
      let f' x = f (V.of_array x) in Option.map V.of_array (Array.find_opt f' m)

    let map2 f m v =
      let f' x y = V.to_array @@ f (V.of_array x) y in Array.map2 f' m (V.to_array v)

    let map2_pt_with f m v =
      if num_rows m = V.length v then
        Array.iter2i (fun i x y -> m.(i) <- V.to_array @@ f (V.of_array x) y) m (V.to_array v)
      else
        for i = 0 to Stdlib.min (num_rows m) (V.length v) -1  do
          m.(i) <- V.to_array @@ f (V.of_array m.(i)) (V.nth v i)
        done; m

    let map2i_pt_with f m v =
      if num_rows m = V.length v then
        Array.iter2i (fun i x y -> m.(i) <- V.to_array @@ f i (V.of_array x) y) m (V.to_array v)
      else
        for i = 0 to Stdlib.min (num_rows m) (V.length v) -1 do
          m.(i) <- V.to_array @@ f i (V.of_array m.(i)) (V.nth v i)
        done; m

    let init_with_vec v =
      let new_matrix = Array.make_matrix 1 (V.length v) (of_int 0) in
      new_matrix.(0) <- (V.to_array v); new_matrix

    let copy_pt = copy
  end


let vector =
  lazy (
    let options =
      ["list", (module ListVector: AbstractVector);
       "array", (module ArrayVector: AbstractVector);]
    in
    let matrix = (GobConfig.get_string "ana.matrix") in
    match List.assoc_opt matrix options with
    | Some man -> man
    | None -> failwith @@ "Matrix " ^ matrix ^ " is not supported. Please check the ana.matrix setting."
  )

let get_vector () = Lazy.force vector

let matrix =
  lazy (
    let options =
      ["list", (module ListMatrix: AbstractMatrix);
       "array", (module ArrayMatrix: AbstractMatrix);]
    in
    let matrix = (GobConfig.get_string "ana.matrix") in
    match List.assoc_opt matrix options with
    | Some man -> man
    | None -> failwith @@ "Matrix " ^ matrix ^ " is not supported. Please check the ana.matrix setting."
  )

let get_matrix () = Lazy.force matrix
