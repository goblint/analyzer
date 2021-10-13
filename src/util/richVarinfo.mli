open Cil

val single: name:string -> (unit -> varinfo)

module type VarinfoMap =
sig
  type t
  type marshal
  val to_varinfo : t -> varinfo
  val from_varinfo: varinfo -> t option
  val is_contained_varinfo: varinfo -> bool
  val describe_varinfo: varinfo -> t -> string
  val marshal: marshal
end

module type S =
sig
  type t
  type marshal
  val map: ?marshal:(marshal option) -> ?size:int -> name:(t -> string) -> unit -> (module VarinfoMap with type t = t and type marshal = marshal)
end

module type T =
sig
  include Hashtbl.HashedType
  val describe_varinfo: varinfo -> t -> string
end

module EmptyVarinfoDescription:
  functor (X: Hashtbl.HashedType) ->
    T with type t = X.t

module Make:
  functor (X: T) ->
    S with type t = X.t

module Variables: S with type t = varinfo
