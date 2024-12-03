open AbstractVector
open RatOps
open ConvenienceOps

open BatList
open BatArray

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

    let to_vector e l = {entries=e; len=l}

    let keep_vals v n = 
      let rec keep_vals_vec v n =
        match v with 
        | x::xs -> if fst x > n then [] else x::(keep_vals_vec xs n)
        | [] -> []
      in
      if n >= v.len then v else (*could be left out but maybe performance??*)
        {entries = keep_vals_vec v.entries n; len=n}


    let remove_nth v n = 
      let dec_idx v = 
        List.map (fun (a,b) -> (a-1, b)) v
      in
      let rec remove_nth_vec v n =
        match v with 
        | x::xs -> 
          if fst x = n then dec_idx xs else 
          if fst x > n then dec_idx (x::xs) else
            x::(remove_nth_vec xs n)  
        | [] -> []
      in
      if n >= v.len then v else (*could be left out but maybe performance??*)
        {entries = remove_nth_vec v.entries n; len = v.len - 1}

    (* TODO: Which of both remmove_nth should we use *)
    let remove_nth v n =
      if n >= v.len then failwith "Out of bounds"
      else
        let new_entries = List.filter_map (fun (col_idx, value) ->
            if col_idx = n 
            then None
            else if col_idx > n 
            then Some (col_idx - 1, value)
            else Some (col_idx, value)
          ) v.entries in
        {entries = new_entries; len = v.len - 1}

    let remove_at_indices v idx = 
      let rec remove_indices_helper vec idx deleted_count = 
        match vec, idx with 
        | [], [] -> []
        | [], (y :: ys) -> failwith "remove at indices: no more columns to delete"
        | ((col_idx, value) :: xs), [] -> (col_idx - deleted_count, value) :: remove_indices_helper xs [] deleted_count
        | ((col_idx, value) :: xs), (y :: ys) when y = col_idx -> remove_indices_helper xs ys (deleted_count + 1)
        | ((col_idx, value) :: xs), (y :: ys) when y < col_idx -> remove_indices_helper vec ys (deleted_count + 1) 
        | ((col_idx, value) :: xs), (y :: ys) -> (col_idx - deleted_count, value) :: remove_indices_helper xs idx deleted_count
      in
      {entries = remove_indices_helper v.entries idx 0; len = v.len - List.length idx}

    let insert_zero_at_indices v idx = 
      let rec add_indices_helper vec idx added_count = 
        match vec, idx with 
        | [], [] -> []
        | [], (y :: ys) -> [] (* inserting at the end only means changing the dimension *)
        | ((col_idx, value) :: xs), [] -> (col_idx + added_count, value) :: add_indices_helper xs [] added_count
        | ((col_idx, value) :: xs), ((i, count) :: ys) when i = col_idx -> add_indices_helper vec ys (added_count + count)
        | ((col_idx, value) :: xs), ((i, count) :: ys) when i < col_idx -> (col_idx + added_count + count, value) :: add_indices_helper xs ys (added_count + count)
        | ((col_idx, value) :: xs), ((i, count) :: ys) -> (col_idx + added_count, value) :: add_indices_helper xs idx added_count
      in
      {entries = add_indices_helper v.entries idx 0; len = v.len + List.length idx}

    let set_nth v n num = 
      if n >= v.len then failwith "Out of bounds" 
      else
        let rev_entries', _ = List.fold_lefti (fun (acc, found) i (idx, value) -> 
            if found then ((idx, value) :: acc, true)
            else 
            if i = v.len - 1 then 
              if idx = n then (if num <>: A.zero then (n, num) :: acc, true else acc, true)
              else if idx > n then (if num <>: A.zero then (idx, value) :: (n, num) :: acc, true else (idx, value) :: acc, true)
              else failwith "Out of bounds (Should not be reachable)"
            else
            if idx < n then ((idx, value) :: acc, false)
            else if idx = n then (if num <>: A.zero then (n, num) :: acc , true else acc, true)
            else (if num <>: A.zero then (idx, value) :: (n, num) :: acc, true else (idx, value) :: acc, true)

          ) ([], false) v.entries in
        {entries = List.rev rev_entries'; len = v.len}

    let insert_val_at n new_val v = 
      if n > v.len then failwith "n too large" else (* Does this happen? Otherwise we can omit this comparison, here right now to be semantically equivalent *)
        let entries' = List.fold_left (fun acc (idx, value) -> 
            if idx < n then (idx, value) :: acc 
            else (idx + 1, value) :: acc 
          ) [] (List.rev v.entries) in
        {entries = List.sort (fun (i, _) (j, _) -> Int.compare i j) entries'; len = v.len + 1}

    let map_preserve_zero f v = (* map for functions f such that f 0 = 0 since f won't be applied to zero values. See also map *)
      let entries' = List.filter_map (
          fun (idx, value) -> let new_val = f value in 
            if new_val = A.zero then None else Some (idx, new_val)) v.entries in 
      {entries = entries'; len = v.len}

    (* map for functions f such that f 0 0 = 0 since f won't be applied to if both values are zero. See also map *)
    let map2_preserve_zero f v1 v2 = 
      let rec map2_nonzero_aux v1 v2 =
        match v1, v2 with 
        | [], [] -> []
        | x , y -> 
          let cons, xtail, ytail =
            match x, y with
            | (xidx, xv)::xs, (yidx,yv)::ys -> (
                match xidx - yidx with
                | d when d < 0 -> (xidx, f xv A.zero), xs, v2
                | d when d > 0 -> (yidx, f A.zero yv), v1, ys
                | _            -> (xidx, f xv yv)    , xs, ys
              )
            | (xidx, xv)::xs, [] -> (xidx, f xv A.zero), xs, []
            | [], (yidx, yv)::ys -> (yidx, f A.zero yv), [], ys
            | [],[] -> raise (Failure "Should never be reached")
          in 
          let res = if snd cons = A.zero then [] else [cons] in
          res@(map2_nonzero_aux xtail ytail)
      in 
      if v1.len <> v2.len then raise (Invalid_argument "Different lengths") else
        to_vector (map2_nonzero_aux v1.entries v2.entries) v1.len

    let fold_left_preserve_zero f acc v =
      List.fold_left (fun acc (_, value) -> f acc value) acc v.entries

    let fold_left2_preserve_zero f acc v v' =
      List.fold_left2 (fun acc (_, value) (_, value') -> f acc value value') acc v.entries v'.entries

    let apply_with_c f c v =
      let entries' = List.map (fun (idx, value) -> (idx, f value c)) v.entries in
      {entries = entries'; len = v.len}

    let zero_vec n = 
      {entries = []; len = n}

    let is_zero_vec v = (v.entries = [])

    let nth v n = 
      if n >= v.len then failwith "V.nth out of bounds"
      else
        let rec nth v = match v with (* List.assoc would also work, but we use the fact that v is sorted *)
          | [] -> A.zero
          | (col_idx, value) :: xs when col_idx > n -> A.zero
          | (col_idx, value) :: xs when col_idx = n -> value
          | (col_idx, value) :: xs -> nth xs 
        in nth v.entries

    let length v =
      v.len

    let of_list l = 
      let entries' = List.rev @@ List.fold_lefti (fun acc i x -> if x <> A.zero then (i, x) :: acc else acc) [] l
      in {entries = entries'; len = List.length l}

    let to_list v = 
      let[@tail_mod_cons] rec extend_zero_aux i v' =
        if i >= v.len then failwith "out of Bounds for to_list" else (*can probably be removed*)
          match v' with
          | (xi,xv)::xs -> if xi = i then xv::(extend_zero_aux (i+1) xs) else A.zero::(extend_zero_aux (i+1) v')
          | [] -> []
      in 
      (extend_zero_aux 0 v.entries)

    let map2 f v v' = 
      if v.len <> v'.len then failwith "Unequal vector length" else 
        of_list (List.map2 f (to_list v) (to_list v'))


    let findi f v = 
      if f A.zero then  
        fst @@ List.findi (fun i (idx, value) -> if idx > i then true else f value) v.entries (* Here fst is the iteration variable i, not the tuple idx *)
      else
        fst @@ List.find (fun (idx, value) -> f value) v.entries (* Here fst is the idx contained in the found tuple *)

    (* Returns optional of (index * value) where f evaluated to true *)
    let findi_val_opt f v =
      if f A.zero then  
        (
          let i, (col_idx, value) = List.findi (fun i (idx, value) -> if idx > i then true else f value) v.entries  in
          if i < col_idx then (* In this case, Zero was the first element found because iteration index i is smaller than "found" value *)
            Some (i, A.zero)
          else Some (col_idx, value)
        )
      else
        Some (List.find (fun (idx, value) -> f value) v.entries) 

    let map f v = 
      of_list (List.map f (to_list v))

    let compare_length_with v n = 
      Int.compare v.len n

    let filteri f v = (* TODO: optimize! *)
      of_list (List.filteri f (to_list v))

    let append v v' = 
      let entries' = v.entries @ List.map (fun (idx, value) -> (idx + v.len), value) v'.entries in
      {entries = entries'; len = v.len + v'.len}
          
    let exists f v  = 
      let c = v.len in
      let rec exists_aux at f v =
        match v with
        | [] -> if at = 0 then false else f A.zero
        | (xi, xv)::xs -> if f xv then true else exists_aux (at - 1) f xs
      in (exists_aux c f v.entries)

    let exists2 f v1 v2 = (* TODO: optimize! *)
      List.exists2 f (to_list v1) (to_list v2)

    let rev v = 
      let entries' = List.rev @@ List.map (fun (idx, value) -> (v.len - idx, value)) v.entries in 
      {entries = entries'; len = v.len}

    let map2i f v v' = (* TODO: optimize! *)
      of_list (List.map2i f (to_list v) (to_list v'))

    let mapi f v  = (* TODO: optimize! *)
      of_list (List.mapi f (to_list v))

    let find2i f v v' = (* TODO: optimize! *)
      failwith "TODO"

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

    let show v = 
      let t = to_list v in 
      let rec list_str l =
        match l with
        | [] -> "]"
        | x :: xs -> (A.to_string x) ^" "^(list_str xs)
      in
      "["^list_str t^"\n"

    (* ------------------- Deprecated ------------------- *)
    let mapi_with f v = 
      failwith "deprecated"

    let map2i_with f v v' = 
      failwith "deprecated"


    let rev_with v = 
      failwith "deprecated"

    let map_with f v  = 
      failwith "deprecated"


    let map2_with f v v' = 
      failwith "deprecated"

    let apply_with_c_with f m v = 
      failwith "deprecated"

    let set_nth_with = 
      failwith "deprecated"
  end 
