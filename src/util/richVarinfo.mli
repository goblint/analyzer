open GoblintCil

val single: name:string -> (unit -> varinfo)

module type VarinfoMap =
sig
  type t
  type marshal
  val to_varinfo : t -> varinfo
  val unmarshal: marshal option -> unit
  val marshal: unit -> marshal
  val bindings: unit -> (t * varinfo) list
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

module Make:
  functor (X: G) ->
    VarinfoMap with type t = X.t

module BiVarinfoMap:
sig
  module type S =
  sig
    include VarinfoMap
    val from_varinfo: varinfo -> t option
    val mem_varinfo: varinfo -> bool
    val describe_varinfo: varinfo -> t -> string
  end

  module Collection:
  sig
    val mem_varinfo : varinfo -> bool
    val describe_varinfo : varinfo -> string
    val mappings: (module S) list ref
  end

  module Make:
    functor (X: H) ->
      S with type t = X.t
end
