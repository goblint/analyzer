(** High-level abstraction of a vector. *)
module type Vector =
sig
  type num
  type t [@@deriving eq, ord, hash]

  val show: t -> string

  val keep_vals: t -> int ->  t

  val remove_nth: t -> int ->  t

  val set_nth: t -> int -> num ->  t

  val set_nth_with: t -> int -> num -> unit

  val insert_val_at: int -> num ->  t ->  t

  val apply_with_c: (num -> num -> num) -> num ->  t ->  t

  val apply_with_c_with: (num -> num -> num) -> num -> t -> unit

  val zero_vec: int -> t

  val is_zero_vec: t -> bool

  val nth: t -> int -> num

  val length: t -> int

  val map2: (num -> num -> num) -> t -> t -> t

  val map2_with: (num -> num -> num) -> t -> t -> unit

  val findi: (num -> bool) ->  t -> int

  val find_opt: (num -> bool) -> t -> t Option.t

  val map: (num -> num) -> t -> t

  val map_with: (num -> num) -> t -> unit

  val map: (num -> num) -> t -> t

  val compare_length_with: t -> int -> int

  val of_list: num list -> t

  val to_list: t -> num list

  val filteri: (int -> num -> bool) -> t -> t

  val append: t -> t -> t

  val exists: (num -> bool) -> t -> bool

  val rev: t -> t

  val rev_with: t -> unit

  val rev: t -> t

  val map2i: (int -> num -> num -> num) -> t -> t -> t

  val map2i_with: (int -> num -> num -> num) -> t -> t -> unit

  val mapi: (int -> num -> num) -> t -> t

  val mapi_with: (int -> num -> num) -> t -> unit

  val mapi: (int -> num -> num) -> t -> t

  val find2i: (num -> num -> bool) -> t -> t -> int

  val to_array: t -> num array

  val of_array: num array -> t

  val copy: t -> t

  val of_sparse_list: int -> (int * num) list -> t

  val to_sparse_list: t -> (int * num) list

end