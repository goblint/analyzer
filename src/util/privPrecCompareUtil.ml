open Prelude

module LVH = Hashtbl.Make (Printable.Prod (Basetype.ProgLines) (Basetype.Variables))
module VD = BaseDomain.VD

type dump = {
  name: string;
  lvh: VD.t LVH.t;
}