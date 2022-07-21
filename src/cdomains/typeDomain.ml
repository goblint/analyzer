module Type : MapDomain.Groupable with type t = Cil.typ =
struct
  type t = Cil.typ

  let compare x y = Stdlib.compare (Cil.typeSig x) (Cil.typeSig y)
  let equal x y = Cil.typeSig x = Cil.typeSig y
  let show x = Pretty.sprint ~width:80 (Cil.d_type () x)
  let to_yojson (x :t) = `String (show x)
  let hash = Hashtbl.hash

  let pretty = Cil.d_type
  let pretty_diff _ _ = failwith "unimplemented pretty_diff"
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (show x))
  let name () = "typeCasts"

  let invariant _ _ = None
  let tag _ = failwith "unimplemented tag"

  let arbitrary _ = failwith "unimplementmed arbitrary"

  let relift x = x


  type group = Trivial
  let show_group _ = "Trivial"
  let to_group x = Some Trivial
  let trace_enabled = false
end

module TypeSet = SetDomain.Make (Type)
module TypeSetTopped = SetDomain.LiftTop (TypeSet) (struct let topname = "All types" end)
module TypeCastMap = MapDomain.MapBot_LiftTop (Type) (TypeSet)
