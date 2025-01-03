(** High-level abstraction of a matrix. *)
module type Matrix =
sig
  type num
  type vec
  type t [@@deriving eq, ord, hash]

  val show: t -> string

  val copy: t -> t

  val empty: unit -> t (* TODO: needs unit? *)

  val is_empty: t -> bool

  val num_rows: t -> int

  val num_cols: t -> int

  val init_with_vec: vec -> t

  val append_row: t -> vec -> t

  val get_row: t -> int -> vec

  val remove_row: t -> int -> t

  val remove_zero_rows: t -> t

  val swap_rows: t -> int -> int -> t

  val map2: (vec -> num -> vec) -> t -> vec -> t

  val map2i: (int -> vec-> num -> vec) -> t -> vec -> t

  val add_empty_columns: t -> int array -> t

  val get_col: t -> int -> vec

  val set_col: t -> vec -> int -> t

  val del_col: t -> int -> t

  val del_cols: t -> int array -> t

  val find_opt: (vec -> bool) -> t -> vec option

  val append_matrices: t -> t -> t

  val reduce_col: t -> int -> t

  val normalize: t -> t Option.t (*Gauss-Jordan Elimination to get matrix in reduced row echelon form (rref) + deletion of zero rows. None matrix has no solution*)

  val rref_vec: t -> vec -> t Option.t

  val rref_matrix: t -> t -> t Option.t

  val is_covered_by: t -> t -> bool

end