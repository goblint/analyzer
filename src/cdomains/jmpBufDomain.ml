module BufferEntry = Printable.ProdSimple(Node)(IntDomain.Flattened)

module JmpBufSet =
struct
  include SetDomain.ToppedSet (BufferEntry) (struct let topname = "All jumpbufs" end)
  let name () = "Jumpbuffers"
end
