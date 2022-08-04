module Locmap = BatHashtbl.Make (CilType.Location)

let dead_branches_then : bool Locmap.t = Locmap.create 10
let dead_branches_else : bool Locmap.t = Locmap.create 10
let dead_branches_cond : Cil.exp Locmap.t = Locmap.create 10
