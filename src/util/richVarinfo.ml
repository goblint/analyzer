open GoblintCil

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

module Make (X: G) =
struct
  (* Mapping from X.t to varinfo *)
  module XH = Hashtbl.Make (X)

  type t = X.t
  type marshal = varinfo XH.t

  let size = 113
  let xh = ref (XH.create size)

  let store x vi =
    XH.replace !xh x vi

  let to_varinfo_helper store_f x =
    try
      XH.find !xh x
    with Not_found ->
      let vi = create_var (X.name_varinfo x) in
      store_f x vi;
      vi

  let to_varinfo x =
    to_varinfo_helper store x

  let marshal () = !xh

  let unmarshal = function
    | Some xh_loaded ->
      xh := xh_loaded
    | None -> ()

  let bindings () = List.of_seq (XH.to_seq !xh)
end

(* module to maintain bidirectional mappings between some type t and varinfo.
   Provides a way to add a description shown to the user for varinfos, based on the associated object of type t.
*)
module BiVarinfoMap =
struct

  module type S =
  sig
    include VarinfoMap
    val from_varinfo: varinfo -> t option
    val mem_varinfo: varinfo -> bool
    val describe_varinfo: varinfo -> t -> string
  end

  (* Collection of all BiVarinfo mappings.
     This collection is queried by functions that output varinfos that might occur in such a mapping. *)
  module Collection =
  struct

    let mappings: (module S) list ref = ref []

    let mem_varinfo (v: varinfo) =
      List.exists (fun (module M: S) -> M.mem_varinfo v) !mappings

    (** Provides a description to be printed with the varinfo *)
    let describe_varinfo (v: varinfo) =
      match List.find_opt (fun (module M: S) -> M.mem_varinfo v) !mappings with
      | None -> failwith "Not a rich varinfo!"
      | Some m ->
        let module Map = (val m) in
        match Map.from_varinfo v with
        | Some x -> Map.describe_varinfo v x
        | None -> failwith "Element not found in mapping that claimed to contain it."

    let register_mapping (m) =
      mappings := m::!mappings
  end

  (** For technical resaons, this functor cannot register the module it creates in [Collection] itself.
      Thus this functor is private to this file, and should only be used through the [Make] defined below. *)
  module PrivateMake (X: H) =
  struct
    module M = Make(X)
    (* Mapping from varinfo to X.t *)
    module VH = Hashtbl.Make (CilType.Varinfo)
    type t = M.t
    type marshal = M.marshal * t VH.t
    let vh = ref (VH.create 113)

    let to_varinfo x =
      let store_f x vi =
        M.store x vi;
        VH.replace !vh vi x
      in
      M.to_varinfo_helper store_f x

    let from_varinfo vi =
      VH.find_opt !vh vi

    let mem_varinfo v =
      VH.mem !vh v

    let describe_varinfo v x =
      X.describe_varinfo v x

    let marshal () =
      M.marshal (), !vh

    let unmarshal = function
      | Some (xh_loaded, vh_loaded) ->
        M.unmarshal (Some xh_loaded);
        vh := vh_loaded
      | None -> ()

    let bindings = M.bindings
  end

  (** Create a BiVarinfoMap and register it in the collection *)
  module Make (X: H) =
  struct
    module BiVarinfoMap = PrivateMake(X)
    include BiVarinfoMap
    let () =
      let m = (module BiVarinfoMap: S) in
      Collection.register_mapping m;
  end
end
