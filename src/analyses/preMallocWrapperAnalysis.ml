open Cil

(* refs to reassign unmarshaled in init *)
module NH = Hashtbl.Make (Node)
module VH = Hashtbl.Make (CilType.Varinfo)

let heap_hash : varinfo NH.t ref = ref (NH.create 113)
let heap_vars : Node.t VH.t ref = ref (VH.create 113)

let is_heap_var x = VH.mem !heap_vars x
let get_node x = VH.find !heap_vars x
