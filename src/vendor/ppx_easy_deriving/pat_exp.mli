(** Common representation of patterns and expressions. *)

open Ppxlib

type t =
  | Record of (longident * t) list
  | Tuple of t list
  | Unit
  | Base of string

val create_record: prefix:string -> longident list -> t
val create_tuple: prefix:string -> int -> t

val to_pat: loc:location -> t -> pattern
val to_exps: loc:location -> t -> expression list
val to_exp: loc:location -> t -> expression
