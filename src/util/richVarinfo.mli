open Cil

val single: name:string -> (unit -> varinfo)

module type VarinfoMap =
sig
  type t
  type marshal
  val to_varinfo : t -> varinfo
  val from_varinfo: varinfo -> t option
  val mem_varinfo: varinfo -> bool
  val describe_varinfo: varinfo -> t -> string
  val unmarshal: marshal -> unit
  val marshal: unit -> marshal
end

module VarinfoMapCollection:
sig
  val mappings : (module VarinfoMap) list ref
  val is_rich_varinfo : varinfo -> bool
  val describe_varinfo : varinfo -> string
  val register_mapping : (module VarinfoMap) -> unit
end

module type G =
sig
  include Hashtbl.HashedType
  val name_varinfo: t -> string
end

module type H =
sig
  include G
  val describe_varinfo: varinfo -> t -> string
end

module EmptyDescription:
  functor (Base: G) ->
    H with type t = Base.t

module Make:
  functor (X: H) ->
    VarinfoMap with type t = X.t
