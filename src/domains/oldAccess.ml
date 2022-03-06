module Ident : Printable.S with type t = string =
struct
  include Printable.Std (* for default invariant, tag, ... *)

  open Pretty
  type t = string [@@deriving eq, ord, hash, to_yojson]
  let show x = x
  let pretty () x = text (show x)
  let name () = "strings"
  let printXml f x =
    BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n"
      (XmlUtil.escape (show x))
end

module LabeledString =
struct
  include Printable.Prod (Ident) (Ident)
  let show (x,y) = x^":"^y
  let pretty () (x,y) =
    Pretty.text (show (x,y))
end
module LSSet = SetDomain.Make (LabeledString)
module LSSSet =
struct
  include SetDomain.Make (LSSet)
  (* TODO: is this actually some partition domain? *)
  let join po pd =
    let mult_po s = union (map (LSSet.union s) po) in
    fold mult_po pd (empty ())
  let bot () = singleton (LSSet.empty ())
  let is_bot x = cardinal x = 1 && LSSet.is_empty (choose x)
  (* top & is_top come from SetDomain.Make *)

  (* Since Queries.PartAccess and PartAccessResult are only used within MCP2,
     these operations are never really called. *)
  let leq _ _ = raise (Lattice.Unsupported "LSSSet.leq")
  (* meet (i.e. join in PartAccessResult) for PathSensitive query joining
     isn't needed, because accesses are handled only within MCP2. *)
  let meet _ _ = raise (Lattice.Unsupported "LSSSet.meet")
  let widen _ _ = raise (Lattice.Unsupported "LSSSet.widen")
  let narrow _ _ = raise (Lattice.Unsupported "LSSSet.narrow")
end

(* Reverse because MCP2.query [meet]s. *)
module PartAccessResult = Lattice.Reverse (Lattice.Prod (LSSSet) (LSSet))

module OldA =
struct
  include PartAccessResult
  let may_race (pp, lp) (pp2, lp2) =
    not (LSSSet.is_empty @@ LSSSet.inter pp pp2) &&
    LSSet.is_empty @@ LSSet.inter lp lp2
  let should_print _ = true
end
