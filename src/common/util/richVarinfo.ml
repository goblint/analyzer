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

module VarinfoDescription = struct
  (**This type is equal to `varinfo`, but the fields are not mutable and they are optional.
     Only the name is not optional. *)
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

  let from_varinfo (v: varinfo) =
    {vname_=v.vname;
     vtype_=Some v.vtype;
     vattr_=Some v.vattr;
     vstorage_=Some v.vstorage;
     vglob_=Some v.vglob;
     vinline_=Some v.vinline;
     vdecl_=Some v.vdecl;
     vinit_=Some v.vinit;
     vaddrof_=Some v.vaddrof;
     vreferenced_=Some v.vreferenced}

  let empty name =
    {vname_=name;
     vtype_=None;
     vattr_=None;
     vstorage_=None;
     vglob_=None;
     vinline_=None;
     vdecl_=None;
     vinit_=None;
     vaddrof_=None;
     vreferenced_=None}

  let update_varinfo (v: varinfo) (update: t) =
    let open Batteries in
    {vname=update.vname_;
     vtype=Option.default v.vtype update.vtype_;
     vattr=Option.default v.vattr update.vattr_;
     vstorage=Option.default v.vstorage update.vstorage_;
     vglob=Option.default v.vglob update.vglob_;
     vinline=Option.default v.vinline update.vinline_;
     vdecl=Option.default v.vdecl update.vdecl_;
     vinit=Option.default v.vinit update.vinit_;
     vid=v.vid;
     vaddrof=Option.default v.vaddrof update.vaddrof_;
     vreferenced=Option.default v.vreferenced update.vreferenced_;
     vdescr=v.vdescr;
     vdescrpure=v.vdescrpure;
     vhasdeclinstruction=v.vhasdeclinstruction}
end

let create_var (vd: VarinfoDescription.t) =
  Cilfacade.create_var (
    VarinfoDescription.update_varinfo (makeGlobalVar vd.vname_ voidType) vd
  )

let single ~name =
  let vi = lazy (create_var (VarinfoDescription.empty name)) in
  fun () ->
    Lazy.force vi

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
      let vi = create_var (X.varinfo_attributes x) in
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
