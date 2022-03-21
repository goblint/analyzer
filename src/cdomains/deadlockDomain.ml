open Cil
open Pretty

type myowntypeEntry = {addr : ValueDomain.Addr.t ; loc : CilType.Location.t} [@@deriving eq, ord, hash]


module MyLock0 : Printable.S with type t = myowntypeEntry =
struct
  include Printable.Std (* for default invariant, tag, ... *)

  type t = myowntypeEntry [@@deriving eq, ord, hash]
  module Ad = ValueDomain.Addr
  let name () = "address with location"
  let show x = (Ad.show x.addr) ^ "@" ^ (CilType.Location.show x.loc)
  let pretty () x = Ad.pretty () x.addr ++ text "@" ++ CilType.Location.pretty () x.loc
  let printXml c x = Ad.printXml c x.addr
  let to_yojson x = `String (show x)
end

module MyLock : Printable.S with type t = myowntypeEntry =
struct
  include MyLock0
  module Ad = ValueDomain.Addr
  let equal x y = Ad.equal x.addr y.addr (* ignores loc field *)
  let hash x = Ad.hash x.addr
  let compare x y = Ad.compare x.addr y.addr (* ignores loc field *)
end

module Lockset = SetDomain.ToppedSet (MyLock) (struct let topname = "all locks" end)
