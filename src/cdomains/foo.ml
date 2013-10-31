module type S =
sig
  type t
  type s
  val get_s: s
  val get_t: t
  val compare: t -> t -> int
end

module Value (Impl: sig
    type s
    val name: s
  end) : S with type s = Impl.s =
struct
  type s = Impl.s
  type t = int * int
  let get_t = 1,2
  let get_s = Impl.name
  let compare v1 v2 = 1
end

module Domain (V: S) =
struct
  include Set.Make (V)
end

(*  *)

module Val =
struct
  type s = string
  let name = "hallo"
end

module Dom =
struct
  (* module V : S with type s = string = Value (Val) *)
  module V = Value (Val)
  (* module P : (sig include S end) = struct include V end *)
  include Domain (V)
end

let x = Dom.singleton Dom.V.get_t
let s = Dom.V.get_s = "hallo"