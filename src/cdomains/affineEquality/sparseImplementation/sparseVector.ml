open AbstractVector
open RatOps
open ConvenienceOps

open BatList
open Batteries

module List = BatList
module Array = BatArray

module SparseVector: AbstractVector =
  functor (A: RatOps) ->
  struct
    include ConvenienceOps (A)

    type t = {
      entries: (int * A.t) list ;
      len: int
    }[@@deriving eq, ord, hash]

    let copy v = v 

    let of_list l = 
      let entries' = List.rev @@ List.fold_lefti (fun acc i x -> if x <> A.zero then (i, x) :: acc else acc) [] l
      in {entries = entries'; len = List.length l}

    let of_array a =
      let len' = Array.length a in
      let entries' = List.rev @@ Array.fold_lefti (fun acc i x -> if x <> A.zero then (i, x) :: acc else acc ) [] a in
      {entries = entries'; len = len'}

    let of_sparse_list col_count ls =
      {entries = ls; len = col_count}

    let to_list v = 
      let[@tail_mod_cons] rec extend_zero_aux i v' =
        match v', i with
        | (xi,xv)::xs, _ -> if xi = i then xv::(extend_zero_aux (i+1) xs) else A.zero ::(extend_zero_aux (i+1) v')
        | [], j when i < v.len -> A.zero :: (extend_zero_aux (i+1) v')
        | [], _ -> []
      in 
      (extend_zero_aux 0 v.entries)

    let to_array v = 
      let vec = Array.make v.len A.zero in 
      List.iter (fun (idx, value) -> vec.(idx) <- value) v.entries;
      vec

    let to_sparse_list v = 
      v.entries

    let show v = 
      let t = to_list v in 
      let rec list_str l =
        match l with
        | [] -> "]"
        | x :: xs -> (A.to_string x) ^ " " ^ (list_str xs)
      in
      "["^list_str t^"\n"

    let length v =
      v.len
    let compare_length_with v n = 
      Int.compare v.len n

    let zero_vec n = 
      {entries = []; len = n}

    let is_zero_vec v = (v.entries = [])

    (* Checks if exactly one variable is part of the affine equality, i.e. whether there is exactly one non-zero entry, excluding the constant at the end *)
    let is_const_vec v = 
      match v.entries with 
      | [] -> false
      | (idx, _) :: (const_idx , _) :: [] when const_idx = (v.len - 1) -> true
      | (idx, _)::[] when idx <> v.len -1 -> true 
      | _ -> false

    let nth v n = (* Note: This exception HAS TO BE THROWN! It is expected by the domain *)
      if n >= v.len then raise (Invalid_argument "Cannot access vector element (out of bounds)")
      else
        let rec nth v = match v with (* List.assoc would also work, but we use the fact that v is sorted *)
          | [] -> A.zero
          | (col_idx, value) :: xs when col_idx > n -> A.zero
          | (col_idx, value) :: xs when col_idx = n -> value
          | (col_idx, value) :: xs -> nth xs 
        in nth v.entries

    let set_nth v n num = (* TODO: Optimize! *)
      if n >= v.len then failwith "Out of bounds" 
      else
        let rec set_nth_helper vec acc =
          match vec with
          | [] -> if num <>: A.zero then List.rev ((n, num) :: acc) else List.rev acc
          | (idx, value) :: xs when n = idx -> if num <>: A.zero then List.rev_append ((idx, num) :: acc) xs else List.rev_append acc xs
          | (idx, value) :: xs when n < idx -> if num <>: A.zero then List.rev_append ((n, num) :: acc) vec else List.rev_append acc vec
          | x :: xs -> set_nth_helper xs (x :: acc)
        in
        {entries = set_nth_helper v.entries []; len = v.len}

    let insert_val_at n new_val v = 
      if n > v.len then failwith "n too large" else (* Does this happen? Otherwise we can omit this comparison, here right now to be semantically equivalent *)
        let entries' = List.rev @@ List.fold_left (fun acc (idx, value) -> 
            if idx < n then (idx, value) :: acc 
            else (if new_val <>: A.zero then (idx + 1, value) :: (n, new_val) :: acc else (idx + 1, value) :: acc)
          ) [] v.entries in
        {entries = entries'; len = v.len + 1}

    (* Note that idx is assumed to be sorted. *)
    let insert_zero_at_indices v idx count = 
      let rec add_indices_helper vec idx added_count acc = 
        match vec, idx with 
        | [], _ -> List.rev acc (* inserting at the end only means changing the dimension *)
        | ((col_idx, value) :: xs), [] -> add_indices_helper xs [] added_count ((col_idx + added_count, value) :: acc)
        | ((col_idx, value) :: xs), ((i, count) :: ys) when i = col_idx -> add_indices_helper vec ys (added_count + count) acc
        | ((col_idx, value) :: xs), ((i, count) :: ys) when i < col_idx -> add_indices_helper vec ys (added_count + count) acc
        | ((col_idx, value) :: xs), ((i, count) :: ys) -> add_indices_helper xs idx added_count ((col_idx + added_count, value) :: acc)
      in
      {entries = add_indices_helper v.entries idx 0 []; len = v.len + count}

    let remove_nth v n =
      if n >= v.len then failwith "Out of bounds"
      else
        let new_entries = List.filter_map (fun (col_idx, value) ->
            if col_idx = n then None
            else if col_idx > n 
            then Some (col_idx - 1, value)
            else Some (col_idx, value)
          ) v.entries in
        {entries = new_entries; len = v.len - 1}

    (* Note: It is assumed and necessary here that idx is sorted!!! *)
    let remove_at_indices v idx = 
      let rec remove_indices_helper vec idx deleted_count acc = 
        match vec, idx with 
        | [], [] -> List.rev acc
        | [], (y :: ys) when deleted_count >= v.len || y >= v.len -> failwith "remove at indices: no more columns to delete"
        | [], (y :: ys) -> remove_indices_helper [] ys (deleted_count + 1) acc (* Removing zero (also in next iteration, else failwith ) *)
        | ((col_idx, value) :: xs), [] -> remove_indices_helper xs [] deleted_count ((col_idx - deleted_count, value) :: acc)
        | ((col_idx, value) :: xs), (y :: ys) when y = col_idx -> remove_indices_helper xs ys (deleted_count + 1) acc
        | ((col_idx, value) :: xs), (y :: ys) when y < col_idx -> remove_indices_helper vec ys (deleted_count + 1) acc
        | ((col_idx, value) :: xs), (y :: ys) -> remove_indices_helper xs idx deleted_count ((col_idx - deleted_count, value) :: acc)
      in
      {entries = remove_indices_helper v.entries idx 0 []; len = v.len - List.length idx}

    let keep_vals v n = 
      if n >= v.len then v else
        {entries = List.take_while (fun (idx, _) -> idx < n) v.entries; len=n}

    let starting_from_nth n v =
      let entries' = List.filter_map (fun (idx, value) -> if idx < n then None else Some (idx - n, value)) v.entries in
      {entries = entries'; len = v.len - n}

    let find_opt f v = (* TODO: Do we need this? And optimize!!!*)
      List.find_opt f (to_list v)

    let findi f v = 
      if f A.zero then  
        fst @@ List.findi (fun i (idx, value) -> if idx > i then true else f value) v.entries (* Here fst is the iteration variable i, not the tuple idx *)
      else
        fst @@ List.find (fun (idx, value) -> f value) v.entries (* Here fst is the idx contained in the found tuple *)

    let find2i f v v' = (* TODO: optimize! *)
      fst @@ List.findi (fun _ (val1, val2) -> (uncurry f) (val1, val2)) (List.combine (to_list v) (to_list v'))

    (* Returns optional of (index * value) where f value evaluated to true *)
    let findi_val_opt f v =
      let rec find_zero_or_val vec last_col_idx =
        let f0 = f A.zero in
        match vec with
        | [] -> if f0 && v.len <> last_col_idx + 1 then Some (last_col_idx + 1, A.zero) else None
        | (idx, value) :: xs -> 
          if f0  && idx <> last_col_idx + 1 then Some (last_col_idx + 1, A.zero) 
          else if f value then Some (idx, value) 
          else find_zero_or_val xs idx
      in find_zero_or_val v.entries (-1)

    let findi_val_opt f v = Timing.wrap "findi_val_opt" (findi_val_opt f) v

    let find_first_non_zero v =
      if v.entries = [] then None
      else Some (List.hd v.entries)

    let find_first_non_zero v = Timing.wrap "find_first_non_zero" (find_first_non_zero) v

    let exists f v  = 
      let c = v.len in
      let rec exists_aux at f v =
        match v with
        | [] -> if at = 0 then false else f A.zero
        | (xi, xv)::xs -> if f xv then true else exists_aux (at - 1) f xs
      in (exists_aux c f v.entries)

    let exists2 f v1 v2 = (* TODO: optimize! *)
      List.exists2 f (to_list v1) (to_list v2)

    let filteri f v = (* TODO: optimize! *)
      of_list (List.filteri f (to_list v))

    let map f v = 
      let f_zero = f A.zero in
      let rec map2_helper acc vec i =
        match vec with
        | [] when i >= v.len || f_zero =: A.zero -> List.rev acc
        | []  -> map2_helper ((i, f_zero) :: acc) [] (i  + 1)
        | (idx, value) :: xs when idx = i -> let new_val = f value in if new_val <>: A.zero then map2_helper ((idx, new_val) :: acc) xs (i + 1) else map2_helper acc xs (i + 1)
        | (idx, _) :: xs when idx > i -> if f_zero <>: A.zero then map2_helper ((i, f_zero) :: acc) vec (i + 1) else map2_helper acc vec (i + 1)
        | (_, _) :: _ -> failwith "This should not happen"
      in
      {entries = map2_helper [] v.entries 0; len = v.len}

    let map_f_preserves_zero f v = (* map for functions f such that f 0 = 0 since f won't be applied to zero values. See also map *)
      let entries' = List.filter_map (
          fun (idx, value) -> let new_val = f value in 
            if new_val = A.zero then None else Some (idx, new_val)) v.entries in 
      {entries = entries'; len = v.len}

    let map_f_preserves_zero f v = Timing.wrap "map_f_preserves_zero" (map_f_preserves_zero f) v

    let mapi f v  = (* TODO: optimize! *)
      of_list (List.mapi f (to_list v))

    let mapi_f_preserves_zero f v =
      let entries' = List.filter_map (
          fun (idx, value) -> let new_val = f idx value in 
            if new_val = A.zero then None else Some (idx, new_val)) v.entries in 
      {entries = entries'; len = v.len}

    let map2 f v v' = (* TODO: Optimize! *)
      if v.len <> v'.len then failwith "Unequal vector length" else 
        of_list (List.map2 f (to_list v) (to_list v'))

    (* map for functions f such that f 0 0 = 0 since f won't be applied to if both values are zero. See also map *)
    let map2_f_preserves_zero f v1 v2 =
      let f_rem_zero acc idx e1 e2 =
        let r = f e1 e2 in 
        if r =: A.zero then acc else (idx, r)::acc
      in  
      let rec aux acc v1 v2 =
        match v1, v2 with 
        | [], [] -> acc 
        | [], (yidx, yval)::ys -> aux (f_rem_zero acc yidx A.zero yval) [] ys
        | (xidx, xval)::xs, [] -> aux (f_rem_zero acc xidx xval A.zero) xs []
        | (xidx, xval)::xs, (yidx, yval)::ys -> 
          match xidx - yidx with
          | d when d < 0 -> aux (f_rem_zero acc xidx xval A.zero) xs v2
          | d when d > 0 -> aux (f_rem_zero acc yidx A.zero yval) v1 ys
          | _            -> aux (f_rem_zero acc xidx xval yval) xs ys
      in
      if v1.len <> v2.len then raise (Invalid_argument "Unequal lengths") else 
        {entries = List.rev (aux [] v1.entries v2.entries); len = v1.len}

    let map2_f_preserves_zero f v1 v2 = Timing.wrap "map2_f_preserves_zero" (map2_f_preserves_zero f v1) v2

    let map2i f v1 v2 = (*might more memory efficient, but definitly not faster (asymptotically)*)
      if v1.len <> v2.len then raise (Invalid_argument "Unequal lengths") else
        (*of_list (List.map2i f (to_list v) (to_list v'))*)
        let f_rem_zero idx acc e1 e2 =
          let r = f idx e1 e2 in 
          if r =: A.zero then acc else (idx, r)::acc
        in  
        let rec aux acc vec1 vec2 i =
          match vec1, vec2 with 
          | [], [] when i = v1.len -> acc 
          | [], [] -> aux (f_rem_zero i acc A.zero A.zero) [] [] (i + 1)
          | [], (yidx, yval)::ys when i = yidx -> aux (f_rem_zero i acc A.zero yval) [] ys (i + 1)
          | (xidx, xval)::xs, [] when i = xidx -> aux (f_rem_zero i acc xval A.zero) xs [] (i + 1)
          | [], (_, _)::_ | (_, _)::_, [] -> aux (f_rem_zero i acc A.zero A.zero) vec1 vec2 (i + 1) (* When one vec is not zero_vec, but has implicit zeroes at front *)
          | (xidx, xval)::xs, (yidx, yval)::ys -> 
            if xidx <> i && yidx <> i then aux (f_rem_zero i acc A.zero A.zero) vec1 vec2 (i+1) (* When both vectors have implicit zeroes at front *)
            else
              match xidx - yidx with (* Here at least one of the idx is i, which is the smaller one *)
              | d when d < 0 -> aux (f_rem_zero i acc xval A.zero) xs vec2 (i + 1)
              | d when d > 0 -> aux (f_rem_zero i acc A.zero yval) vec1 ys (i + 1)
              | _            -> aux (f_rem_zero i acc xval yval) xs ys (i + 1)
        in
        {entries = List.rev (aux [] v1.entries v2.entries 0); len = v1.len}

    let map2i_f_preserves_zero f v v' = failwith "TODO"

    let fold_left_f_preserves_zero f acc v =
      List.fold_left (fun acc (_, value) -> f acc value) acc v.entries

    let fold_left2_f_preserves_zero f acc v v' =  
      let rec aux acc v1 v2 =
        match v1, v2 with 
        | [], [] -> acc 
        | [], (yidx, yval)::ys -> aux (f acc A.zero yval) [] ys
        | (xidx, xval)::xs, [] -> aux (f acc xval A.zero) xs []
        | (xidx, xval)::xs, (yidx, yval)::ys -> 
          match xidx - yidx with
          | d when d < 0 -> aux (f acc xval A.zero) xs v2
          | d when d > 0 -> aux (f acc A.zero yval) v1 ys
          | _            -> aux (f acc xval yval) xs ys
      in
      if v.len <> v'.len then raise (Invalid_argument "Unequal lengths") else 
        (aux acc v.entries v'.entries)

    let apply_with_c f c v  = (* TODO: optimize! *)
      of_list @@ List.map (fun value -> f value c) (to_list v)

    let apply_with_c_f_preserves_zero f c v =
      let entries' = List.filter_map (fun (idx, value) -> let new_val = f value c in if new_val =: A.zero then None else Some (idx, new_val)) v.entries in
      {entries = entries'; len = v.len}

    let rev v = 
      let entries' = List.rev @@ List.map (fun (idx, value) -> (v.len - 1 - idx, value)) v.entries in 
      {entries = entries'; len = v.len}

    let append v v' = 
      let entries' = v.entries @ List.map (fun (idx, value) -> (idx + v.len), value) v'.entries in
      {entries = entries'; len = v.len + v'.len}

    let exists f v  = (*TODO: optimize!; maybe we shouldn't if exists is never called with a =: A.zero*)
      let c = v.len in
      let rec exists_aux at f v =
        match v with
        | [] -> if at = 0 then false else f A.zero
        | (xi, xv)::xs -> if f xv then true else exists_aux (at - 1) f xs
      in (exists_aux c f v.entries)

    let exists2 f v1 v2 = (* TODO: optimize! *)
      List.exists2 f (to_list v1) (to_list v2)

    let rev v = 
      let entries' = List.rev @@ List.map (fun (idx, value) -> (v.len - 1 - idx, value)) v.entries in 
      {entries = entries'; len = v.len}

    let find2i f v v' =
      fst @@ List.findi (fun _ (val1, val2) -> (uncurry f) (val1, val2)) (List.combine (to_list v) (to_list v'))

    let find2i_f_false_at_zero f v v' = (*Very welcome to change the name*)
      let rec aux v1 v2 =
        match v1, v2 with 
        | [], [] -> raise Not_found
        | [], (yidx, yval)::ys -> if f A.zero yval then yidx else aux [] ys
        | (xidx, xval)::xs, [] -> if f xval A.zero then xidx else aux xs []
        | (xidx, xval)::xs, (yidx, yval)::ys -> 
          match xidx - yidx with
          | d when d < 0 -> if f xval A.zero then xidx else aux xs v2
          | d when d > 0 -> if f A.zero yval then yidx else aux v1 ys
          | _            -> if f xval yval then xidx else aux xs ys
      in
      if v.len <> v'.len then raise (Invalid_argument "Unequal lengths") else 
        aux v.entries v'.entries

    let to_array v = 
      let vec = Array.make v.len A.zero in 
      List.iter (fun (idx, value) -> vec.(idx) <- value) v.entries;
      vec

    let of_array a =
      let len' = Array.length a in
      let entries' = List.rev @@ Array.fold_lefti (fun acc i x -> if x <> A.zero then (i, x) :: acc else acc ) [] a in
      {entries = entries'; len = len'}

    let copy v = v 

    let of_sparse_list col_count ls =
      {entries = ls; len = col_count}

    let to_sparse_list v = 
      v.entries

    let find_opt f v = (* TODO: Do we need this? And optimize!!!*)
      List.find_opt f (to_list v)

    let starting_from_nth n v =
      let entries' = List.filter_map (fun (idx, value) -> if idx < n then None else Some (idx - n, value)) v.entries in
      {entries = entries'; len = v.len - n}

    let show v = 
      let t = to_list v in 
      let rec list_str l =
        match l with
        | [] -> "]"
        | x :: xs -> (A.to_string x) ^ " " ^ (list_str xs)
      in
      "["^list_str t^"\n"
  end 
