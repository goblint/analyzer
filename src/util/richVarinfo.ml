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
  val is_contained_varinfo: varinfo -> bool
  val describe_varinfo: varinfo -> t -> string
  val unmarshal: marshal -> unit
  val marshal: unit -> marshal
end
module type S =
sig
  type t
  type marshal
  val map: ?size:int -> ?describe_varinfo:(varinfo -> t -> string) -> name:(t -> string) -> unit -> (module VarinfoMap with type t = t and type marshal = marshal)
end

(* Collection of RichVarinfo mappings *)
module VarinfoMapCollection =
struct

  let mappings: (module VarinfoMap) list ref = ref []

  let is_rich_varinfo (v: varinfo) =
    List.exists (fun (module M: VarinfoMap) -> M.is_contained_varinfo v) !mappings

  (** Provides a description to be printed with the varinfo *)
  let describe_varinfo (v: varinfo) =
      match List.find_opt (fun (module M: VarinfoMap) -> M.is_contained_varinfo v) !mappings with
      | None -> failwith "Not a richt varinfo!"
      | Some m ->
        let module Map = (val m) in
        match Map.from_varinfo v with
        | Some x -> Map.describe_varinfo v x
        | None -> failwith "Element not found in mapping that claimed to contain it."

  let register_mapping (m) =
    mappings := m::!mappings
end

module Make (X: Hashtbl.HashedType) =
struct
  (* Mapping from X.t to varinfo *)
  module XH = Hashtbl.Make (X)
  (* Mapping from varinfo to X.t *)
  module VH = Hashtbl.Make (CilType.Varinfo)

  type t = X.t
  type marshal = varinfo XH.t * t VH.t

  (* Empty description is the default *)
  let describe _ _ = ""

  let map ?(size=113) ?(describe_varinfo=describe) ~name ()  =
    let m = (module struct
      let xh = ref (XH.create size)
      let vh = ref (VH.create size)

      type nonrec t = t
      type nonrec marshal = marshal

      let to_varinfo x =
        try
          XH.find !xh x
        with Not_found ->
          let vi = create_var (name x) in
          XH.replace !xh x vi;
          VH.replace !vh vi x;
          vi

      let from_varinfo vi =
        VH.find_opt !vh vi

      let is_contained_varinfo v =
        VH.mem !vh v

      let describe_varinfo v x =
        describe_varinfo v x

      let marshal () = !xh, !vh
      let unmarshal ((xh_loaded, vh_loaded): marshal) =
        xh := xh_loaded;
        vh := vh_loaded
    end
    : VarinfoMap with type t = t and type marshal = marshal) in
    VarinfoMapCollection.register_mapping (m :> (module VarinfoMap));
    m
end
