open Cil

val single: name:string -> (unit -> varinfo)

module type S =
sig
  type t
  type marshal
  module type VarinfoMap =
  sig
    val to_varinfo : t -> varinfo
    val from_varinfo: varinfo -> t option
    val marshal: marshal
  end
  val map: ?marshal:(marshal option) -> ?size:int -> name:(t -> string) -> (module VarinfoMap)
end

module Make:
  functor (X: Hashtbl.HashedType) ->
    S with type t = X.t

module Variables: S with type t = varinfo
