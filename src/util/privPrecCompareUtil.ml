open Prelude

module LV =
struct
  include Printable.Prod (CilType.Location) (Basetype.Variables)
  let pretty () (l, v) = Pretty.dprintf "%a %a" CilType.Location.pretty l Basetype.Variables.pretty v
end
module LVH = Hashtbl.Make (LV)
module VD = BaseDomain.VD

type dump = {
  name: string;
  lvh: VD.t LVH.t;
}
