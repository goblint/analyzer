module Default = Goblint_timing.Make (struct let name = "Default" end)

module Program = Goblint_timing.Make (struct let name = "Program" end)

include Default
