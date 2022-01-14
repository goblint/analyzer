open Pretty

type myowntypeEntry = {addr : ValueDomain.Addr.t ; node : Node.t}

let pretty_node_loc () node = CilType.Location.pretty () (Node.location node)


module MyLock : Printable.S with type t = myowntypeEntry =
struct
  include Printable.Std (* for default invariant, tag, ... *)

  type t = myowntypeEntry
  module Ad = ValueDomain.Addr
  let name () = "address with location"
  let equal x y = Ad.equal x.addr y.addr (* ignores loc field *)
  let hash x = Ad.hash x.addr
  let compare x y = Ad.compare x.addr y.addr (* ignores loc field *)
  (* TODO: deadlock analysis output doesn't even use these, but manually outputs locations *)
  let show x = (Ad.show x.addr) ^ "@" ^ (CilType.Location.show (Node.location x.node))
  let pretty () x = Ad.pretty () x.addr ++ text "@" ++ pretty_node_loc () x.node
  let printXml c x = Ad.printXml c x.addr
  let to_yojson x = `String (show x)
end

module Lockset = SetDomain.ToppedSet (MyLock) (struct let topname = "all locks" end)
