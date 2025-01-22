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

    let of_sparse_list count ls =
      {entries = ls; len = count}

    (** [to_list v] Returns a non-sparse list representing the vector v *)
    let to_list v =
      let rec extend_zero_aux i v' acc =
        match v' with
        | (index, value) :: xs -> if index = i then extend_zero_aux (i + 1) xs (value :: acc) else extend_zero_aux (i + 1) v' (A.zero :: acc)
        | [] -> if i < v.len then extend_zero_aux (i + 1) v' (A.zero :: acc) else List.rev acc
      in
      extend_zero_aux 0 v.entries []

    let to_array v = 
      let vec = Array.make v.len A.zero in 
      List.iter (fun (idx, value) -> vec.(idx) <- value) v.entries;
      vec

    let to_sparse_list v = 
      v.entries

    let show v = 
      let rec sparse_list_str i l =
        if i >= v.len then "]"
        else match l with
          | [] -> (A.to_string A.zero) ^" "^ (sparse_list_str (i + 1) l)
          | (idx, value) :: xs -> 
            if i = idx then (A.to_string value) ^" "^ sparse_list_str (i + 1) xs
            else (A.to_string A.zero) ^" "^ sparse_list_str (i + 1) l
      in
      "["^(sparse_list_str 0 v.entries)^"\n"

    let show v = Timing.wrap "V.show" (show) v

    let length v =
      v.len
    let compare_length_with v n = 
      Int.compare v.len n

    let zero_vec n = 
      {entries = []; len = n}

    let is_zero_vec v = (v.entries = [])

    (**
       [is_const_vec v] returns true if the v represents an affine equality over only one variable, i.e. a constant.
       Constant vectors are represented by a two element list, however this is not an iff relationship.*)
    let is_const_vec v = 
      match v.entries with 
      | [] -> false
      | (idx, _) :: (const_idx , _) :: [] when const_idx = (v.len - 1) -> true
      | (idx, _)::[] when idx <> v.len -1 -> true 
      | _ -> false

    (**
       [nth v n] returns the [n]-th entry of [v].
       @raise Invalid_argument if [n] is out of bounds.
    *)
    let nth v n = (* Note: This exception HAS TO BE THROWN! It is expected by the domain *)
      if n >= v.len then raise (Invalid_argument "Index out of bounds")
      else
        let rec nth v = match v with
          | [] -> A.zero
          | (idx, value) :: xs when idx > n -> A.zero
          | (idx, value) :: xs when idx = n -> value
          | (idx, value) :: xs -> nth xs
        in nth v.entries

    (**
       [set_nth v n num] returns [v] where the [n]-th entry has been set to [num].
       @raise Invalid_argument if [n] is out of bounds.
    *)
    let set_nth v n num = 
      if n >= v.len then raise (Invalid_argument "Index out of bounds")
      else
        let rec set_nth_helper vec acc =
          match vec with
          | [] -> if num <>: A.zero then List.rev ((n, num) :: acc) else List.rev acc
          | (idx, value) :: xs when n = idx -> if num <>: A.zero then List.rev_append ((idx, num) :: acc) xs else List.rev_append acc xs
          | (idx, value) :: xs when n < idx -> if num <>: A.zero then List.rev_append ((n, num) :: acc) vec else List.rev_append acc vec
          | x :: xs -> set_nth_helper xs (x :: acc)
        in
        {entries = set_nth_helper v.entries []; len = v.len}

    (**
       [insert_val_at v n num] returns [v] where the [num] is inserted into the [n]-th position, i.e. [v] at [n] = [num].
       @raise Invalid_argument if [n] is out of bounds.
    *)
    let insert_val_at v n num = 
      if n > v.len then raise (Invalid_argument "Index out of bounds") 
      else
        let entries' = List.rev @@ List.fold_left (fun acc (idx, value) -> 
            if idx < n then (idx, value) :: acc 
            else (if num <>: A.zero then (idx + 1, value) :: (n, num) :: acc else (idx + 1, value) :: acc)
          ) [] v.entries in
        {entries = entries'; len = v.len + 1}

    (**
       [insert_zero_at_indices v indices num_zeros] inserts [num_zeros] into [v].
       @param indices A {b sorted} list of tuples where [fst] gives the index of the insert and [snd] the number of zeros that will be inserted.
    *)
    let insert_zero_at_indices v indices num_zeros =
      let rec add_indices_helper vec cur_idx added_count acc =
        match vec, cur_idx with
        | [], _ -> List.rev acc (* inserting at the end only means changing the dimension *)
        | (idx, value) :: xs, [] -> add_indices_helper xs [] added_count ((idx + added_count, value) :: acc)
        | (idx, _) :: _, ((i, count) :: ys) when i <= idx -> add_indices_helper vec ys (added_count + count) acc
        | (idx, value) :: xs, ((i, count) :: ys) -> add_indices_helper xs cur_idx added_count ((idx + added_count, value) :: acc)
      in
      {entries = add_indices_helper v.entries indices 0 []; len = v.len + num_zeros}

    (**
       [remove_nth v n] returns [v] where the [n]-th entry is removed, decreasing the length of the vector by one.
       @raise Invalid_argument if [n] is out of bounds
    *)
    let remove_nth v n =
      if n >= v.len then raise (Invalid_argument "Index out of bounds")
      else
        let new_entries = List.filter_map (fun (idx, value) ->
            if idx = n then None
            else if idx > n
            then Some (idx - 1, value)
            else Some (idx, value)
          ) v.entries in
        {entries = new_entries; len = v.len - 1}

    (**
       [remove_at_indices v indices] returns [v] where all entries at the positions specified by [indices] are removed, decreasing its length by the length of [indices].
       @param indices A {b sorted} list of indices
       @raise Invalid_argument if [indices] would reach out of bounds for some index.
    *)
    let remove_at_indices v indices =
      let rec remove_indices_helper vec cur_idx deleted_count acc =
        match vec, cur_idx with
        | [], [] -> List.rev acc
        | [], (y :: ys) when deleted_count >= v.len || y >= v.len -> raise (Invalid_argument "Indices out of bounds")
        | [], (y :: ys) -> remove_indices_helper [] ys (deleted_count + 1) acc
        | (idx, value) :: xs, [] -> remove_indices_helper xs [] deleted_count ((idx - deleted_count, value) :: acc)
        | (idx, _) :: xs, (y :: ys) when y = idx -> remove_indices_helper xs ys (deleted_count + 1) acc
        | (idx, _) :: xs, (y :: ys) when y < idx -> remove_indices_helper vec ys (deleted_count + 1) acc
        | (idx, value) :: xs, (y :: ys) -> remove_indices_helper xs cur_idx deleted_count ((idx - deleted_count, value) :: acc)
      in
      {entries = remove_indices_helper v.entries indices 0 []; len = v.len - List.length indices}

    (** [keep_vals v n] returns returns a vector only containing the first [n] elements of [v] *)
    let keep_vals v n = 
      if n >= v.len then v else
        {entries = List.take_while (fun (idx, _) -> idx < n) v.entries; len=n}

    (** [starting_from_nth v n] returns a vector only containing the elements after the [n]th *)
    let starting_from_nth v n =
      let entries' = List.filter_map (fun (idx, value) -> if idx < n then None else Some (idx - n, value)) v.entries in
      {entries = entries'; len = v.len - n}

    let findi f v = 
      (* How this works:
         Case 1:
         A.zero is also to be found. The List.findi is used to have an iteration index i.
         If at one point i < idx, it means that a Zero element was skipped because of the sparse vector representation. In that case i is the index of the first Zero, so returning true indicates that it is found. 
         f value still has to be checked in case other elements than Zero are to be found.
         List.findi returns the index at which is was found in the tuple's first element. 
         Case 2:
         A.zero is not to be found. We just iterate over the sparse list then and the index is returned if an element is found.
      *)
      if f A.zero then  
        fst @@ List.findi (fun i (idx, value) -> if i < idx then true else f value) v.entries (* Here fst is the iteration variable i, not the tuple idx *)
      else
        fst @@ List.find (fun (idx, value) -> f value) v.entries (* Here fst is the idx contained in the found tuple *)

    (**
       [find2i_f_false_at_zero f v v'] returns the {b index} of the first pair of entries [e, e'] from [v, v'] where [f e e' = true ].

       Note that [f] {b must} be such that [f 0 0 = false]!

       {i One should use this function over its general counterpart whenever possible, as it is considerably faster!}
    *)
    let find2i_f_false_at_zero f v v' = (*Very welcome to change the name*)
      let rec aux v1 v2 =
        match v1, v2 with 
        | [], [] -> raise Not_found
        | [], (yidx, yval)::ys -> if f A.zero yval then yidx else aux [] ys
        | (xidx, xval) :: xs, [] -> if f xval A.zero then xidx else aux xs []
        | (xidx, xval) :: xs, (yidx, yval) :: ys ->
          match xidx - yidx with
          | d when d < 0 -> if f xval A.zero then xidx else aux xs v2
          | d when d > 0 -> if f A.zero yval then yidx else aux v1 ys
          | _            -> if f xval yval then xidx else aux xs ys
      in
      if v.len <> v'.len then raise (Invalid_argument "Unequal lengths") else
        aux v.entries v'.entries

    (**
       [findi_val_opt f v] returns the first entry [e] and its index where [f e = true], if such an entry exists.
       @return [(idx, e) option]
    *)
    let findi_val_opt f v =
      let rec find_zero_or_val vec last_idx =
        let f0 = f A.zero in
        match vec with
        | [] -> if f0 && v.len <> last_idx + 1 then Some (last_idx + 1, A.zero) else None
        | (idx, value) :: xs -> 
          if f0  && idx <> last_idx + 1 then Some (last_idx + 1, A.zero) 
          else if f value then Some (idx, value) 
          else find_zero_or_val xs idx
      in find_zero_or_val v.entries (-1)

    let findi_val_opt f v = Timing.wrap "findi_val_opt" (findi_val_opt f) v

    (**
       [find_first_non_zero v] returns the first entry [e] and its index where [e <>: 0], if such an entry exists.
       @return [(idx, e) option]
    *)
    let find_first_non_zero v =
      if v.entries = [] then None
      else Some (List.hd v.entries)

    let find_first_non_zero v = Timing.wrap "find_first_non_zero" (find_first_non_zero) v

    let exists f v  = 
      let c = v.len in
      let rec exists_aux at f v =
        match v with
        | [] -> if at = 0 then false else f A.zero
        | (xi, xv) :: xs -> if f xv then true else exists_aux (at - 1) f xs
      in (exists_aux c f v.entries)

    (**
       [map_f_preserves_zero f v] returns the mapping of [v] specified by [f].

       Note that [f] {b must} be such that [f 0 = 0]!

       {i One should use this function over its general counterpart whenever possible, as it is considerably faster!}
    *)
    let map_f_preserves_zero f v = (* map for functions f such that f 0 = 0 since f won't be applied to zero values. See also map *)
      let entries' = List.filter_map (
          fun (idx, value) -> let new_val = f value in 
            if new_val = A.zero then None else Some (idx, new_val)) v.entries in 
      {entries = entries'; len = v.len}

    let map_f_preserves_zero f v = Timing.wrap "map_f_preserves_zero" (map_f_preserves_zero f) v

    (**
       [mapi_f_preserves_zero f v] returns the mapping of [v] specified by [f].

       Note that [f] {b must} be such that [f i 0 = 0] for any index [i]!

       {i One should use this function over its general counterpart whenever possible, as it is considerably faster!}
    *)
    let mapi_f_preserves_zero f v =
      let entries' = List.filter_map (
          fun (idx, value) -> let new_val = f idx value in 
            if new_val = A.zero then None else Some (idx, new_val)) v.entries in 
      {entries = entries'; len = v.len}

    (**
       [map2_f_preserves_zero f v v'] returns the mapping of [v] and [v'] specified by [f].

       Note that [f] {b must} be such that [f 0 0 = 0]!

       {i One should use this function over its general counterpart whenever possible, as it is considerably faster!}

       @raise Invalid_argument if [v] and [v'] have unequal lengths
    *)
    let map2_f_preserves_zero f v v' =
      let f_rem_zero acc idx e1 e2 =
        let r = f e1 e2 in 
        if r =: A.zero then acc else (idx, r) :: acc
      in  
      let rec aux acc v1 v2 =
        match v1, v2 with 
        | [], [] -> acc 
        | [], (yidx, yval) :: ys -> aux (f_rem_zero acc yidx A.zero yval) [] ys
        | (xidx, xval) :: xs, [] -> aux (f_rem_zero acc xidx xval A.zero) xs []
        | (xidx, xval) :: xs, (yidx, yval) :: ys -> 
          match xidx - yidx with
          | d when d < 0 -> aux (f_rem_zero acc xidx xval A.zero) xs v2
          | d when d > 0 -> aux (f_rem_zero acc yidx A.zero yval) v1 ys
          | _            -> aux (f_rem_zero acc xidx xval yval) xs ys
      in
      if v.len <> v'.len then raise (Invalid_argument "Unequal lengths") else 
        {entries = List.rev (aux [] v.entries v'.entries); len = v.len}

    let map2_f_preserves_zero f v1 v2 = Timing.wrap "map2_f_preserves_zero" (map2_f_preserves_zero f v1) v2

    (**
       @raise Invalid_argument if [v] and [v'] have unequal lengths
    *)
    let map2i f v v' = (*might more memory efficient, but definitly not faster (asymptotically)*)
      if v.len <> v'.len then raise (Invalid_argument "Unequal lengths") else
        let f_rem_zero idx acc e1 e2 =
          let r = f idx e1 e2 in 
          if r =: A.zero then acc else (idx, r) :: acc
        in  
        let rec aux acc vec1 vec2 i =
          match vec1, vec2 with 
          | [], [] when i = v.len -> acc 
          | [], [] -> aux (f_rem_zero i acc A.zero A.zero) [] [] (i + 1)
          | [], (yidx, yval) :: ys when i = yidx -> aux (f_rem_zero i acc A.zero yval) [] ys (i + 1)
          | (xidx, xval) :: xs, [] when i = xidx -> aux (f_rem_zero i acc xval A.zero) xs [] (i + 1)
          | [], (_, _) :: _ | (_, _) :: _, [] -> aux (f_rem_zero i acc A.zero A.zero) vec1 vec2 (i + 1) (* When one vec is not zero_vec, but has implicit zeroes at front *)
          | (xidx, xval)::xs, (yidx, yval)::ys -> 
            if xidx <> i && yidx <> i then aux (f_rem_zero i acc A.zero A.zero) vec1 vec2 (i+1) (* When both vectors have implicit zeroes at front *)
            else
              match xidx - yidx with (* Here at least one of the idx is i, which is the smaller one *)
              | d when d < 0 -> aux (f_rem_zero i acc xval A.zero) xs vec2 (i + 1)
              | d when d > 0 -> aux (f_rem_zero i acc A.zero yval) vec1 ys (i + 1)
              | _            -> aux (f_rem_zero i acc xval yval) xs ys (i + 1)
        in
        {entries = List.rev (aux [] v.entries v'.entries 0); len = v.len}

    (**
       [fold_left_f_preserves_zero f acc v] returns the fold of [v] on [acc] specified by [f].

       Note that [f] {b must} be such that [f acc 0 = acc]!

       {i One should use this function over its general counterpart whenever possible, as it is considerably faster!}
    *)
    let fold_left_f_preserves_zero f acc v =
      List.fold_left (fun acc (_, value) -> f acc value) acc v.entries

    (**
       [fold_left2_f_preserves_zero f acc v v'] returns the fold of [v] and [v'] specified by [f].

       Note that [f] {b must} be such that [f acc 0 0 = acc]!

       {i One should use this function over its general counterpart whenever possible, as it is considerably faster!}

       @raise Invalid_argument if [v] and [v'] have unequal lengths
    *)
    let fold_left2_f_preserves_zero f acc v v' =  
      let rec aux acc v1 v2 =
        match v1, v2 with 
        | [], [] -> acc 
        | [], (yidx, yval) :: ys -> aux (f acc A.zero yval) [] ys
        | (xidx, xval) :: xs, [] -> aux (f acc xval A.zero) xs []
        | (xidx, xval) :: xs, (yidx, yval) :: ys -> 
          match xidx - yidx with
          | d when d < 0 -> aux (f acc xval A.zero) xs v2
          | d when d > 0 -> aux (f acc A.zero yval) v1 ys
          | _            -> aux (f acc xval yval) xs ys
      in
      if v.len <> v'.len then raise (Invalid_argument "Unequal lengths") else 
        (aux acc v.entries v'.entries)

    (**
       [apply_with_c_f_preserves_zero f c v] returns the mapping of [v] and [c] specified by [f].

       Note that [f] {b must} be such that [f 0 c = 0]!

       {i One should use this function over its general counterpart whenever possible, as it is considerably faster!}
    *)
    let apply_with_c_f_preserves_zero f c v =
      let entries' = List.filter_map (fun (idx, value) -> let new_val = f value c in if new_val =: A.zero then None else Some (idx, new_val)) v.entries in
      {entries = entries'; len = v.len}

    let rev v = 
      let entries' = List.rev @@ List.map (fun (idx, value) -> (v.len - 1 - idx, value)) v.entries in 
      {entries = entries'; len = v.len}

    let append v v' = 
      let entries' = v.entries @ List.map (fun (idx, value) -> (idx + v.len), value) v'.entries in
      {entries = entries'; len = v.len + v'.len}
  end 
