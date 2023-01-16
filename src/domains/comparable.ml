module type S =
sig
  type t
  val equal: t -> t -> bool
  val hash: t -> int
  val compare: t -> t -> int
end