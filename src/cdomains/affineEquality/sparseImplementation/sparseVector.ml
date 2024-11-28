open AbstractVector
open RatOps
open ConvenienceOps

open BatList
module List = BatList

module SparseVector: AbstractVector =
  functor (A: RatOps) ->
  struct
    include ConvenienceOps (A)

    type t = {
      entries: (int * A.t) list ;
      len: int
    }[@@deriving eq, ord, hash]

    let tV e l = {entries=e; len=l}
    let show v = 
      failwith "TODO"

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
        | ((col_idx, value) :: xs), [] -> (col_idx - deleted_count, value) :: remove_indices_helper xs idx deleted_count
        | ((col_idx, value) :: xs), (y :: ys) when y = col_idx -> remove_indices_helper xs ys (deleted_count + 1)
        | ((col_idx, value) :: xs), (y :: ys) -> (col_idx - deleted_count, value) :: remove_indices_helper xs idx deleted_count
      in
      {entries = remove_indices_helper v.entries idx 0; len = v.len - List.length idx}

    let set_nth v n num = 
      if n >= v.len then failwith "Out of bounds" 
      else
        let new_entries = List.map (fun (col_idx, value) ->
            if col_idx = n then (col_idx, num) else (col_idx, value)
          ) v.entries
        in
        {entries= new_entries; len=v.len}

    let set_nth_with = 
      failwith "deprecated"

    let insert_val_at n new_val v = 
      if n > v.len then failwith "n too large" else (* Does this happen? Otherwise we can omit this comparison, here right now to be semantically equivalent *)
        let entries' = List.fold_left (fun acc (idx, value) -> 
            if idx < n then (idx, value) :: acc 
            else if idx = n then (n, new_val) :: (idx + 1, value) :: acc 
            else (idx + 1, value) :: acc
          ) [] (List.rev v.entries) in
        {entries = entries'; len = v.len + 1}

    let map_preserve_zero f v = tV ((List.map f) v.entries) v.len
    
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
        tV (map2_nonzero_aux v1.entries v2.entries) v1.len


    let apply_with_c f m v = 
      failwith "TODO"

    let apply_with_c_with f m v = 
      failwith "TODO"

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

    let map2 f v v' = 
      failwith "TODO"

    let map2_with f v v' = 
      failwith "TODO"

    let findi f v = 
      failwith "TODO"

    let map f v = 
      failwith "TODO"

    let map_with f v  = 
      failwith "TODO"

    let compare_length_with v n = 
      Int.compare v.len n

    let of_list l = 
      let entries' = List.rev @@ List.fold_lefti (fun acc i x -> if x <> A.zero then (i, x) :: acc else acc) [] l
      in {entries = entries'; len = List.length l}

    let to_list v = 
      let l = List.init v.len (fun _ -> A.zero) in 
      List.fold_left (fun acc (idx, value) -> (List.modify_at idx (fun _ -> value) acc)) l v.entries

    let filteri f v = 
      failwith "TODO"

    let append v v' = 
      let entries' = v.entries @ List.map (fun (idx, value) -> (idx + v.len), value) v'.entries in
      {entries = entries'; len = v.len + v'.len}

    let exists f v  = 
      failwith "TODO"

    let rev v = 
      failwith "TODO"

    let rev_with v = 
      failwith "TODO"

    let map2i f v v' = 
      failwith "TODO"

    let map2i_with f v v' = 
      failwith "TODO"

    let mapi f v  = 
      failwith "TODO"

    let mapi_with f v = 
      failwith "TODO"

    let find2i f v v' = 
      failwith "TODO"

    let to_array v = 
      failwith "TODO"

    let of_array a =
      failwith "TODO"

    let copy v = v 

    let of_sparse_list col_count ls =
      {entries = ls; len = col_count}

    let to_sparse_list v = 
      v.entries

    let find_opt f v =
      failwith "TODO: Do we need this?"

  end 