open GoblintCil

include ModularUtil0

module AD = ValueDomain.AD
module Addr = ValueDomain.Addr

let address_to_canonical a =
  let t = Addr.get_type a in
  type_to_varinfo t
(** From a set of [reachable_adresses], find all those represented by [canonical] varinfo.
    This is the basic h^{-1}. *)
let represented_by ~(canonical:varinfo) ~(reachable_addresses:AD.t) =
  AD.filter (fun a -> CilType.Varinfo.equal canonical (address_to_canonical a)) reachable_addresses
