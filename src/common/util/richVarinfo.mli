(** Custom {!GoblintCil.varinfo} management. *)

open GoblintCil

module type VarinfoMap =
sig
  type t
  type marshal
  val to_varinfo : t -> varinfo
  val unmarshal: marshal option -> unit
  val marshal: unit -> marshal
  val bindings: unit -> (t * varinfo) list
end

module VarinfoDescription:
sig
  type t = {
    vname_: string;
    vtype_: typ option;
    vattr_: attributes option;
    vstorage_: storage option;
    vglob_: bool option;
    vinline_: bool option;
    vdecl_: location option;
    vinit_: initinfo option;
    vaddrof_: bool option;
    vreferenced_: bool option;
  }
  val from_varinfo: varinfo -> t
  val empty: string -> t
  val update_varinfo: varinfo -> t -> varinfo
end

val single: name:string -> (unit -> varinfo)

module type G =
sig
  include Hashtbl.HashedType
  val varinfo_attributes: t -> VarinfoDescription.t
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
