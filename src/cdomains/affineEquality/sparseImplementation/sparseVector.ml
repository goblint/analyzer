open Vector
open RatOps

module M = Messages

open Batteries

module type SparseVector =
sig
  include Vector
  val push_first: t -> int -> num -> t

  val is_zero_vec: t -> bool

  val tail_afterindex: t -> int -> t

  val insert_zero_at_indices: t -> (int * int) list -> int -> t

  val remove_at_indices: t -> int list -> t

  (* Returns the part of the vector starting from index n*)
  val starting_from_nth : t -> int -> t

  val find_first_non_zero : t -> (int * num) option

  val map_f_preserves_zero: (num -> num) -> t -> t

  val mapi_f_preserves_zero: (int -> num -> num) -> t -> t

  val map2_f_preserves_zero: (num -> num -> num) -> t ->  t -> t

  val map2_f_preserves_zero_helper: (int -> int -> int) -> (num -> num -> num) -> t ->  t -> t

  val find2i_f_false_at_zero: (num -> num -> bool) -> t -> t -> int

  val apply_with_c_f_preserves_zero: (num -> num -> num) -> num ->  t ->  t
end

module type SparseVectorFunctor =
  functor (A: RatOps) ->
  sig
    include SparseVector with type num:= A.t
  end

module SparseVector: SparseVectorFunctor =
  functor (A: RatOps) ->
  struct
    include ConvenienceOps (A)

    (** Only non-zero [entries] are stored in a list of tuples of (index, value). All values not contained in [entries] are implicitly [A.Zero]. *)
    type t = {
      entries: (int * A.t) list ;
      len: int
    }[@@deriving eq, ord, hash]

    let copy v = v

    (** [of_list l] returns a vector constructed from the non-sparse list [l] *)
    let of_list l =
      let entries = List.filteri_map (fun i x -> if x <> A.zero then Some (i, x) else None) l in
      let len = List.length l in
      {entries; len}

    (** [of_array a] returns a vector constructed from the non-sparse array [a] *)
    let of_array a =
      let entries = Array.fold_righti (fun i x acc -> if x <> A.zero then (i, x) :: acc else acc ) a [] in
      let len = Array.length a in
      {entries; len}

    (** [of_sparse_list len entries] returns a vector of length [len] constructed from the sorted sparse list [entries].
        A sparse list is a list of tuples of the form [(i, value)] which represent that the vector has an entry [value] at index [i].
        All non-specified entries are assumed to be [Zero]. The sparse list has to be sorted by the index [i].
    *)
    let of_sparse_list len entries =
      {entries; len}

    (** [to_list v] returns a non-sparse list representing the vector v *)
    let to_list v =
      let rec extend_zero_aux i acc = function
        | (index, value) :: xs when index = i -> extend_zero_aux (i + 1) (value :: acc) xs
        | ((_:: _) as v) -> extend_zero_aux (i + 1) (A.zero :: acc) v
        | [] -> if i < v.len then extend_zero_aux (i + 1) (A.zero :: acc) [] else List.rev acc
      in
      extend_zero_aux 0 [] v.entries

    let to_array v =
      let vec = Array.make v.len A.zero in
      List.iter (fun (idx, value) -> vec.(idx) <- value) v.entries;
      vec

    let to_sparse_list v =
      v.entries

    let pretty () v =
      let open GoblintCil.Pretty in
      let pretty_a () a = text (A.to_string a) in
      let rec sparse_list_str i () l: doc =
        if i >= v.len then nil
        else
          match l with
          | [] -> dprintf "%a %a" pretty_a A.zero (sparse_list_str (i + 1)) l
          | (idx, value) :: xs ->
            if i = idx then dprintf "%a %a" pretty_a value (sparse_list_str (i + 1)) xs
            else dprintf "%a %a" pretty_a A.zero (sparse_list_str (i + 1)) l
      in
      dprintf "[%a]" (sparse_list_str 0) v.entries

    let length v =
      v.len

    let compare_length_with v n =
      Int.compare v.len n

    let zero_vec len =
      {entries = []; len}

    let is_zero_vec v = (v.entries = [])

    (**
       [is_const_vec v] returns true if the v can be interpreted as an expression with one variable and possibly a constant, i.e. x_i  + c
    *)
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
        match List.find_opt (fun (idx, _) -> idx >= n) v.entries with
        | Some (idx, value) when idx = n -> value
        | _ -> A.zero

    let push_first v n num =
      if n >= v.len then raise (Invalid_argument "Index out of bounds")
      else let res =
             {v with entries = (n,num)::v.entries} in
        if M.tracing then M.trace "push_first" "pushed %s at index %d, new length: %d, resulting in %s" (A.to_string num) n res.len (res.entries |> List.map (fun (i, x) -> Printf.sprintf "(%d, %s)" i (A.to_string x)) |> String.concat ", ");
        res

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
        {v with entries = set_nth_helper v.entries []}

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
        [tail_afterindex v n] returns the vector starting after the [n]-th entry, i.e. all entries with index > [n].
        @raise Invalid_argument if [n] is out of bounds.
    *)
    let tail_afterindex v n =
      if n >= v.len then raise (Invalid_argument "Index out of bounds")
      else
        match v.entries with
        | [] -> v (* If the vector is empty, return it as is *)
        | (headidx,headval) :: _ ->
          if M.tracing then M.trace "tail_afterindex" "headidx: %d, n: %d, v.len: %d" headidx n v.len;
          if headidx > n then v
          else
            let entries = List.tl v.entries in
            {entries; len = v.len }

    (**
       [remove_nth v n] returns [v] where the [n]-th entry is removed, decreasing the length of the vector by one.
       @raise Invalid_argument if [n] is out of bounds
    *)
    let remove_nth v n =
      if n >= v.len then raise (Invalid_argument "Index out of bounds")
      else
        let entries = List.filter_map (fun (idx, value) ->
            if idx = n then None
            else if idx > n
            then Some (idx - 1, value)
            else Some (idx, value)
          ) v.entries in
        {entries; len = v.len - 1}

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
      let entries = List.filter_map (fun (idx, value) -> if idx < n then None else Some (idx - n, value)) v.entries in
      {entries; len = v.len - n}

    (**
       [find2i_f_false_at_zero f v v'] returns the {b index} of the first pair of entries [e, e'] from [v, v'] where [f e e' = true ].

       Note that [f] {b must} be such that [f 0 0 = false]!
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
       [find_first_non_zero v] returns the first entry [e] and its index where [e <>: 0], if such an entry exists.
       @return [(idx, e) option]
    *)
    let find_first_non_zero v =
      match v.entries with
      | [] -> None
      | x :: _ -> Some x

    let find_first_non_zero v = timing_wrap "find_first_non_zero" (find_first_non_zero) v

    (**
       [map_f_preserves_zero f v] returns the mapping of [v] specified by [f].

       Note that [f] {b must} be such that [f 0 = 0]!

    *)
    let map_f_preserves_zero f v = (* map for functions f such that f 0 = 0 since f won't be applied to zero values. See also map *)
      let entries' = List.filter_map (
          fun (idx, value) -> let new_val = f value in
            if new_val = A.zero then None else Some (idx, new_val)) v.entries in
      {v with entries = entries'}

    let map_f_preserves_zero f v = timing_wrap "map_f_preserves_zero" (map_f_preserves_zero f) v

    (**
       [mapi_f_preserves_zero f v] returns the mapping of [v] specified by [f].

       Note that [f] {b must} be such that [f i 0 = 0] for any index [i]!

    *)
    let mapi_f_preserves_zero f v =
      let entries' = List.filter_map (
          fun (idx, value) -> let new_val = f idx value in
            if new_val = A.zero then None else Some (idx, new_val)) v.entries in
      {v with entries = entries'}

    (**
       [map2_f_preserves_zero termorder f v v'] returns the mapping of [v] and [v'] specified by [f].

       Note that [f] {b must} be such that [f 0 0 = 0]!

       [termorder] is a function specifying, if the entries of [v] and [v'] are ordered in increasing or decreasing index order.

       @raise Invalid_argument if [v] and [v'] have unequal lengths
    *)
    let map2_f_preserves_zero_helper termorder f v v' =
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
          match termorder xidx yidx with
          | d when d < 0 -> aux (f_rem_zero acc xidx xval A.zero) xs v2
          | d when d > 0 -> aux (f_rem_zero acc yidx A.zero yval) v1 ys
          | _            -> aux (f_rem_zero acc xidx xval yval) xs ys
      in
      if v.len <> v'.len then raise (Invalid_argument "Unequal lengths") else
        {v with entries = List.rev (aux [] v.entries v'.entries)}

    (**
       [map2_f_preserves_zero f v v'] returns the mapping of [v] and [v'] specified by [f].

       Note that [f] {b must} be such that [f 0 0 = 0]!

       The entries of [v] and [v'] are assumed to be ordered in increasing index order.

       @raise Invalid_argument if [v] and [v'] have unequal lengths
    *)
    let map2_f_preserves_zero f v v'= map2_f_preserves_zero_helper (-) f v v'

    let map2_f_preserves_zero f v1 v2 = timing_wrap "map2_f_preserves_zero" (map2_f_preserves_zero f v1) v2

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
        {v with entries = List.rev (aux [] v.entries v'.entries 0)}

    (**
       [apply_with_c_f_preserves_zero f c v] returns the mapping of [v] and [c] specified by [f].

       Note that [f] {b must} be such that [f 0 c = 0]!
    *)
    let apply_with_c_f_preserves_zero f c v =
      let entries = List.filter_map (fun (idx, value) -> let new_val = f value c in if new_val =: A.zero then None else Some (idx, new_val)) v.entries in
      {entries; len = v.len}

    let rev v =
      let entries = List.rev_map (fun (idx, value) -> (v.len - 1 - idx, value)) v.entries in
      {entries; len = v.len}
  end
