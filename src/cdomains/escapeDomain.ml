module EscapedVars  =
struct
  include SetDomain.ToppedSet (CilType.Varinfo) (struct let topname = "All Variables" end)
end
