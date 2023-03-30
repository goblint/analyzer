open GoblintCil

module TypeVarinfoMap : RichVarinfo.BiVarinfoMap.S with type t = CilType.Typ.t =
struct


  module Typ = struct
    include CilType.Typ
    let name_and_type_varinfo typ =
      let name = CilType.Typ.show typ in
      name, typ

    let describe_varinfo v ts = ""
  end

  module TypeMap = RichVarinfo.BiVarinfoMap.Make(Typ)
  include TypeMap
  let to_varinfo (t: t) =
    (* Invariant: Only sanitized types are inserted into TypeMap *)
    let sanitize_type (t: Cil.typ) =
      let t = unrollType t in
      (* TODO: Remove attributes from t *)
      t
    in
    let t = sanitize_type t in
    TypeMap.to_varinfo t

  let from_varinfo (v: Cil.varinfo) =
    if TypeMap.mem_varinfo v then
      let typ = v.vtype in
      Some typ
    else
      None

  let describe_varinfo (v: Cil.varinfo) (t: Cil.typ) =
    TypeMap.describe_varinfo v t

  let bindings () =
    failwith "TypeVarinfoMap.bindings: not implemented"
end

(* TODO: Do not expose module above through second module path *)
include TypeVarinfoMap