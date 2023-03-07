module BufferEntry = Printable.ProdSimple(Node)(IntDomain.Flattened)

module BufferEntryOrTop = struct
  include Printable.Std
  type t = AllTargets | Target of BufferEntry.t [@@deriving eq, ord, hash, to_yojson]
  let show = function AllTargets -> "All" | Target x -> BufferEntry.show x

  include Printable.SimpleShow (struct
      type nonrec t = t
      let show = show
    end)
end

module JmpBufSet =
struct
  include SetDomain.Make (BufferEntryOrTop)
  let top () = singleton BufferEntryOrTop.AllTargets
  let name () = "Jumpbuffers"

  let inter x y =
    if mem BufferEntryOrTop.AllTargets x || mem BufferEntryOrTop.AllTargets y then
      let fromx = if mem BufferEntryOrTop.AllTargets y then x else bot () in
      let fromy = if mem BufferEntryOrTop.AllTargets x then y else bot () in
      union fromx fromy
    else
      inter x y

  let meet = inter
end

module JmpBufSetTaint =
struct
  module Bufs = JmpBufSet
  include Lattice.Prod(JmpBufSet)(BoolDomain.MayBool)
  let buffers (a,_) = a
  let copied (_,b) = b
  let name () = "JumpbufferCopyTaint"
end


(* module JmpBufSet =
   struct
   include SetDomain.ToppedSet (BufferEntry) (struct let topname = "All jumpbufs" end)
   let name () = "Jumpbuffers"
   end *)

module NodeSet =
struct
  include SetDomain.ToppedSet (Node) (struct let topname = "All longjmp callers" end)
  let name () = "Longjumps"
end

module ActiveLongjmps =
struct
  include Lattice.ProdSimple(JmpBufSet)(NodeSet)
end

module LocallyModifiedMap =
struct
  module VarSet = SetDomain.ToppedSet(CilType.Varinfo) (struct let topname = "All vars" end)
  include MapDomain.MapBot_LiftTop (BufferEntry)(VarSet)

  let name () = "Locally modified variables since the corresponding setjmp"
end
