module TypeSig : MapDomain.Groupable with type t = Cil.typsig =
struct
  type t = Cil.typsig

  let compare = Stdlib.compare
  let equal = (=)
  let show x = Pretty.sprint ~width:80 (Cil.d_typsig () x)
  let to_yojson (x :t) = `String (show x)
  let hash = Hashtbl.hash

  let pretty = Cil.d_typsig
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
  let  trace_enabled = false
end

module TypeSigSet = SetDomain.Make (TypeSig)
module TypeCastMap = MapDomain.MapBot (TypeSig) (TypeSigSet)
