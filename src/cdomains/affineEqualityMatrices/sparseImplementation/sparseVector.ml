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
      failwith "TODO"

    let set_val_with v n m =
      failwith "TODO"

    let insert_val n m t = 
      failwith "TODO"

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

    let of_sparse_list ls col_count =
      failwith "TODO"

    let to_sparse_list v = 
      failwith "TODO"

  end 