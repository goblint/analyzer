open Cil

(* Keeps track of vars that are used in the *)

(* refs to reassign unmarshaled in init *)
module NH = Hashtbl.Make (Node)
module VH = Hashtbl.Make (CilType.Varinfo)

module PL = Lattice.Flat (Node) (struct
      let top_name = "Unknown node"
      let bot_name = "Unreachable node"
    end)

let heap_hash : varinfo NH.t ref = ref (NH.create 113)
let heap_vars : Node.t VH.t ref = ref (VH.create 113)

let is_heap_var x = VH.mem !heap_vars x
let get_node x = VH.find !heap_vars x
