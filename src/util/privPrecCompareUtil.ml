open Prelude
open GoblintCil
open PrecCompareUtil

module LV =
struct
  include Printable.Prod (CilType.Location) (Basetype.Variables)
  let name () = "location variables"
  type marshal = t
  let pp ppf (l, v) = Fmt.pf ppf "%a %a" CilType.Location.pp l Basetype.Variables.pp v
  let to_location = fst
end

module Util =
struct
  include Util (LV) (BaseDomain.VD)
  let init () =
    Cil.initCIL (); (* ValueDomain.Compound.leq depends on ptrdiffType initialization *)
end

include Util
