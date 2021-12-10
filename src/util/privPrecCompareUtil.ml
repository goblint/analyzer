open Prelude
open PrecCompareUtil

module LV =
struct
  include Printable.Prod (CilType.Location) (Basetype.Variables)
  let name () = "location variables"
  type marshal = t
  let pretty () (l, v) = Pretty.dprintf "%a %a" CilType.Location.pretty l Basetype.Variables.pretty v
  let to_location = fst
end

module Util =
struct
  include Util (LV) (BaseDomain.VD)
  let init () =
    Cil.initCIL (); (* ValueDomain.Compound.leq depends on ptrdiffType initialization *)
end

include Util
