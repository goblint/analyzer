(** String literals domain. *)

include Printable.S

val of_string: string -> t
(** Convert from string. *)

val to_string: t -> string option
(** Convert to string if possible. *)

(** C strings are different from OCaml strings as they are not processed after the first [NUL] byte, even though the OCaml string (and a C string literal) may be longer. *)

val to_c_string: t -> string option
(** Convert to C string if possible. *)

val to_n_c_string: int -> t -> string option
(** Convert to C string of given maximum length if possible. *)

val to_string_length: t -> int option
(** Find length of C string if possible. *)

val to_exp: t -> GoblintCil.exp
(** Convert to CIL expression. *)

val semantic_equal: t -> t -> bool option
(** Check semantic equality of two strings.

    @return [Some true] if definitely equal, [Some false] if definitely not equal, [None] if unknown. *)

(** Some {!Lattice.S} operations. *)

val leq: t -> t -> bool
val join: t -> t -> t
val meet: t -> t -> t

val repr : t -> t
(** Representative for address lattice. *)
