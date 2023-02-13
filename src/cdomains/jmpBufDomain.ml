module BufferEntry = Printable.ProdSimple(Node)(IntDomain.Flattened)

module JmpBufSet =
struct
  include SetDomain.ToppedSet (BufferEntry) (struct let topname = "All jumpbufs" end)
  let name () = "Jumpbuffers"
end

module NodeSet =
struct
  include SetDomain.ToppedSet (Node) (struct let topname = "All longjmp callers" end)
  let name () = "Longjumps"
end

module ActiveLongjmps =
struct
  include Lattice.ProdSimple(JmpBufSet)(NodeSet)
end
