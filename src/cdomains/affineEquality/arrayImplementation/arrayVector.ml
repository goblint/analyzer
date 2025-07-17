open Vector
open RatOps

open Batteries

module type ArrayVector = 
sig 
  include Vector
  val mapi_with: (int -> num -> num) -> t -> unit

  val map_with: (num -> num) -> t -> unit

  val map2_with: (num -> num -> num) -> t -> t -> unit

  val map2i_with: (int -> num -> num -> num) -> t -> t -> unit

  val filteri: (int -> num -> bool) -> t -> t

  val findi: (num -> bool) ->  t -> int

  val find2i: (num -> num -> bool) -> t -> t -> int

  val exists: (num -> bool) -> t -> bool

  val set_nth_with: t -> int -> num -> unit

  val insert_val_at: t -> int -> num ->  t

  val apply_with_c_with: (num -> num -> num) -> num -> t -> unit

  val rev_with: t -> unit

  val append: t -> t -> t
end

module type ArrayVectorFunctor =
  functor (A: RatOps) ->
  sig
    include ArrayVector with type num:= A.t
  end

(** Array-based vector implementation. *)
module ArrayVector: ArrayVectorFunctor =
  functor (A: RatOps) ->
  struct
    include ConvenienceOps (A)
    include Array
    type t = A.t array [@@deriving eq, ord, hash]

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

    let remove_nth v n =
      if n >= Array.length v then failwith "n outside of Array range" else
        Array.init (Array.length v - 1) (fun i -> if i < n then Array.get v i else Array.get v (i + 1)) (* TODO: remove_at? *)

    let set_nth_with v n new_val =
      if n >= Array.length v then failwith "n outside of Array range" else
        Array.set v n new_val

    let set_nth v n new_val =
      let copy = copy v in
      set_nth_with copy n new_val; copy

    let insert_val_at v n new_val =
      if n > Array.length v then failwith "n too large" else
        Array.init (Array.length v + 1) (fun i -> if i < n then Array.get v i else if i = n then new_val else Array.get v (i -1)) (* insert? *)

    let zero_vec n = Array.make n A.zero

    let is_const_vec v =
      compare_length_with (filteri (fun i x -> (*Inefficient*)
          compare_length_with v (i + 1) > 0 && x <>: A.zero) v) 1 = 0

    let nth = Array.get

    let map2i f v1 v2 =
      let f' i = uncurry (f i) in
      Array.mapi f' (Array.combine v1 v2) (* TODO: iter2i? *)

    let map2i_with f v1 v2 = Array.iter2i (fun i x y -> v1.(i) <- f i x y) v1 v2

    let find2i f v1 v2 =
      Array.findi (uncurry f) (Array.combine v1 v2) (* TODO: iter2i? *)

    let to_array v = v

    let of_array v = v

    let apply_with_c_with f c v = Array.modify (fun x -> f x c) v

    let rev_with v = Array.rev_in_place v

    let map_with f v = Array.modify f v

    let map2_with f v1 v2 = Array.iter2i (fun i x y -> v1.(i) <- f x y) v1 v2

    let copy v = Array.copy v

    let mapi_with f v = Array.iteri (fun i x -> v.(i) <- f i x) v

    let of_sparse_list col_count ls =
      let vec = Array.make col_count A.zero in
      List.iter (fun (idx, value) -> vec.(idx) <- value) ls;
      vec

    let to_sparse_list v = 
      let rec aux idx acc =
        if idx < 0 then acc
        else
          let value = v.(idx) in
          let acc = if value <> A.zero then (idx, value):: acc else acc in
          aux (idx - 1) acc
      in aux (length v - 1) []
  end