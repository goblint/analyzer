open Cil

val single: name:string -> (unit -> varinfo)

module type S =
sig
  type t
  val map: name:(t -> string) -> ?size:int -> (t -> varinfo)
end

module Make:
  functor (X: Hashtbl.HashedType) ->
    S with type t = X.t

module Variables: S with type t = varinfo