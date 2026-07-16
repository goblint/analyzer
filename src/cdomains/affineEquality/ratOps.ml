(** Abstracts the functions of the Mpqf module for rationals from Apron that implements multi-precision rationals.
    One could later exchange "Mpqf" with a different module that provides the functions specified by this interface. *)
module type RatOps =
sig
  type t [@@deriving eq, ord, hash]
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val neg : t -> t
  val abs : t -> t
  val to_string:  t -> string
  val of_int: int -> t
  val zero: t
  val one: t
  val get_den: t -> Z.t
  val get_num: t -> Z.t
end

(** It provides more readable infix operators for the functions of RatOps.
    It is designed to be included by modules that make use of RatOps's functions. *)
module ConvenienceOps (A: RatOps) =
struct
  let ( *: ) = A.mul
  let (+:) = A.add
  let (-:) = A.sub
  let (/:) = A.div
  let (=:) x y = A.equal x y
  let (<>:) x y = not (A.equal x y)
  let (<:) x y = A.compare x y < 0
  let (>:) x y = A.compare x y > 0
  let (<=:) x y = A.compare x y <= 0
  let (>=:) x y = A.compare x y >= 0
  let of_int x = A.of_int x
end