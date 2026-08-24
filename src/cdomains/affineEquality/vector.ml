(** High-level abstraction of a vector. *)
module type Vector =
sig
  type num
  type t [@@deriving eq, ord, hash]

  val show: t -> string

  val copy: t -> t

  val of_list: num list -> t

  val of_array: num array -> t

  val of_sparse_list: int -> (int * num) list -> t

  val to_list: t -> num list

  val to_array: t -> num array

  val to_sparse_list: t -> (int * num) list

  val length: t -> int

  val compare_length_with: t -> int -> int

  val zero_vec: int -> t

  val is_const_vec: t -> bool

  val nth: t -> int -> num

  val set_nth: t -> int -> num ->  t

  val remove_nth: t -> int ->  t

  val keep_vals: t -> int ->  t

  val map2i: (int -> num -> num -> num) -> t -> t -> t

  val rev: t -> t
end

(*let timing_wrap = Timing.wrap*)
(* Disable timing of the vector and matrix implementations and AffineEqualityDomain as well as SharedFunctions.
   This is cleaner than a timing functor because the timed functions also call each other. *)
let timing_wrap _ f x = f x