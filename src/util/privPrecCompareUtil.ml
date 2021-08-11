open Prelude

module LVH = Hashtbl.Make (Printable.Prod (CilType.Location) (Basetype.Variables))
module VD = BaseDomain.VD

type dump = {
  name: string;
  lvh: VD.t LVH.t;
}