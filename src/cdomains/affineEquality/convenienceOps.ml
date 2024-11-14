open RatOps

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
