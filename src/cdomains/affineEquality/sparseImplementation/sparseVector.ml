open AbstractVector
open RatOps
open ConvenienceOps

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


    let remove_val v n = 
      let dec_idx v = 
        List.map (fun (a,b) -> (a-1, b)) v
      in
      let rec remove_val_vec v n =
        match v with 
        | x::xs -> 
          if fst x = n then dec_idx xs else 
          if fst x > n then dec_idx (x::xs) else
            x::(remove_val_vec xs n)  
        | [] -> []
      in
      if n >= v.len then v else (*could be left out but maybe performance??*)
        {entries = remove_val_vec v.entries n; len = v.len - 1}

    let set_val v n m = 
      let rec set_val_vec v n m =
        match v with 
        | x::xs -> if fst x = n then (n, m)::xs else 
          if fst x < n then x::(set_val_vec xs n m)
          else v
        | [] -> [] 
      in
      if n >= v.len then failwith "Out of bounds" else
        {entries=set_val_vec v.entries n m; len=v.len}

    let set_val_with = 
      failwith "deprecated"

    let insert_val n m t = 
      failwith "TODO"

    let mul_vec_scal v s = 
      {entries= (List.map (fun (idx, va) -> (idx, va *: s)) v.entries); len=v.len}


    let add_vec v1 v2 = 
      let rec add_vec m s =
        match m, s with
        | ((xidx, xv)::xs, (yidx,yv)::ys) -> (
            match xidx - yidx with
            | d when d = 0 && (xv +: yv = A.zero) -> (xidx, xv +: yv)::(add_vec xs ys)
            | d when d < 0 -> (xidx, xv)::(add_vec xs ((yidx, yv)::ys))
            | d when d > 0 -> (yidx, yv)::(add_vec ((xidx, xv)::xs) ys)
            | _ -> add_vec xs ys ) (* remove row when is (0, 0) *)
        | ([], y::ys) -> y::(add_vec [] ys)
        | (x::xs, []) -> x::(add_vec xs [])
        | ([],[]) -> []
      in 
      if v1.len <> v2.len then failwith "Different Vector length" else
        {entries= add_vec v1.entries v2.entries; len=v1.len}

    let sub_vec v1 v2 = (*change to duplicate def of add if performance*)
      add_vec v1 ({entries= (List.map (fun (idx, va) -> (idx, A.zero -: va)) v2.entries); len=v2.len})

    let apply_with_c f m v = 
      failwith "TODO"

    let apply_with_c_with f m v = 
      failwith "TODO"

    let zero_vec n = 
      failwith "TODO"

    let nth v n = 
      failwith "TODO"

    let length v =
      failwith "TODO"

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
      failwith "TODO"

    let of_list l = 
      failwith "TODO"

    let to_list v = 
      failwith "TODO"

    let filteri f v = 
      failwith "TODO"

    let append v v' = 
      failwith "TODO"

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

    let remove_nth v n =
      failwith "TODO"

    let find_opt f v =
      failwith "TODO"

  end 