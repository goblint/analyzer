open Prelude

(* Currently serialization of Apron results only works for octagons. *)
module Man = ApronDomain.OctagonManager
module D2 = ApronDomain.D2 (Man)

let init () =
  Apron.Manager.set_deserialize Man.mgr

module LV =
struct
  include Node
  let pretty () n = Pretty.dprintf "%a" Node.pretty n
end
module LVH = Hashtbl.Make (LV)
module VD = BaseDomain.VD

type dump = {
  name: string;
  lvh: D2.t LVH.t;
}
