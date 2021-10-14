open Cil

let create_var name = Goblintutil.create_var @@ makeGlobalVar name voidType

let single ~name =
  let vi = lazy (create_var name) in
  fun () ->
    Lazy.force vi

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

(* Collection of RichVarinfo mappings *)
module VarinfoMapCollection =
struct

  let mappings: (module VarinfoMap) list ref = ref []

  let mem_varinfo (v: varinfo) =
    List.exists (fun (module M: VarinfoMap) -> M.mem_varinfo v) !mappings

  (** Provides a description to be printed with the varinfo *)
  let describe_varinfo (v: varinfo) =
      match List.find_opt (fun (module M: VarinfoMap) -> M.mem_varinfo v) !mappings with
      | None -> failwith "Not a richt varinfo!"
      | Some m ->
        let module Map = (val m) in
        match Map.from_varinfo v with
        | Some x -> Map.describe_varinfo v x
        | None -> failwith "Element not found in mapping that claimed to contain it."

  let register_mapping (m) =
    mappings := m::!mappings
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

module EmptyDescription (Base: G) =
struct
  include Base
  let describe_varinfo _ _ = ""
end

(** This functor cannot register the module it creates in [VarinfoMapCollection].
    Thus this functor is private to this file, and should only be used through the [Make] defined below. *)
module PrivateMake (X: H) =
struct
  (* Mapping from X.t to varinfo *)
  module XH = Hashtbl.Make (X)
  (* Mapping from varinfo to X.t *)
  module VH = Hashtbl.Make (CilType.Varinfo)

  type t = X.t
  type marshal = varinfo XH.t * t VH.t

  let size = 113
  let xh = ref (XH.create size)
  let vh = ref (VH.create size)
  let to_varinfo x =
    try
      XH.find !xh x
    with Not_found ->
      let vi = create_var (X.name_varinfo x) in
      XH.replace !xh x vi;
      VH.replace !vh vi x;
      vi

  let from_varinfo vi =
    VH.find_opt !vh vi

  let mem_varinfo v =
    VH.mem !vh v

  let describe_varinfo v x =
    X.describe_varinfo v x

  let marshal () = !xh, !vh

  let unmarshal ((xh_loaded, vh_loaded): marshal) =
    xh := xh_loaded;
    vh := vh_loaded
end

module Make (X: H) =
struct
  module VarinfoMap = PrivateMake(X)
  include VarinfoMap
  let register =
    let m = (module VarinfoMap: VarinfoMap) in
    VarinfoMapCollection.register_mapping m;
end
