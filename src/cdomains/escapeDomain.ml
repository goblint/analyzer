module EscapedVars  =
struct
  include SetDomain.ToppedSet (Basetype.Variables) (struct let topname = "All Variables" end)
end
