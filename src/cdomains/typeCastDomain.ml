module TypeSig : MapDomain.Groupable with type t = Cil.typsig =
struct
  type t = Cil.typsig

  let compare = Stdlib.compare
  let equal = (=)
  let show x = Pretty.sprint ~width:80 (Cil.d_typsig () x)
  let to_yojson (x :t) = `String (show x)
  let hash = Hashtbl.hash

  let pretty = Cil.d_typsig
  let pretty_diff = failwith "unimplemented"
  (* These two lets us reuse the short function, and allows some overriding
   * possibilities. *)
  let printXml = failwith "uimplemented"
  (* This is for debugging *)
  let name () = "typeCasts"

  let invariant _ _ = None
  let tag = failwith "unimplemented"

  let arbitrary = failwith "unimplemented arbitrary"

  let relift x = x


  type group = Trivial
  let show_group _ = "Trivial"
  let to_group x = Some Trivial
  let  trace_enabled = false
end

module TypeSigSet = SetDomain.Make (TypeSig)
module TypeCastMap = MapDomain.MapBot (TypeSig) (TypeSigSet)
