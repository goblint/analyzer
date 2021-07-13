open Deriving.Cil
open Pretty

type myowntypeEntry = {addr : ValueDomain.Addr.t ; loc : location}


module MyLock : Printable.S with type t = myowntypeEntry =
struct
  include Printable.Std (* for default invariant, tag, ... *)

  type t = myowntypeEntry
  module Ad = ValueDomain.Addr
  let name () = "address with location"
  let equal x y = Ad.equal x.addr y.addr (* ignores loc field *)
  let hash x = Ad.hash x.addr
  let compare x y = Ad.compare x.addr y.addr (* ignores loc field *)
  let show x = (Ad.show x.addr) ^ "@" ^ (Basetype.ProgLines.show x.loc)
  let pretty () x = Ad.pretty () x.addr ++ text "@" ++ Basetype.ProgLines.pretty () x.loc
  let printXml c x = Ad.printXml c x.addr
  let pretty_diff () (x,y) = Ad.pretty_diff () (x.addr,y.addr)
  let to_yojson x = `String (show x)
end

module Lockset = SetDomain.ToppedSet (MyLock) (struct let topname = "all locks" end)
