(** domain of the base analysis *)

open Cil
module VD     = ValueDomain.Compound
module CPA    = MapDomain.MapBot_LiftTop (Basetype.Variables) (VD)
module Flag   = ConcDomain.SimpleThreadDomain


let heap_hash = Hashtbl.create 113 

let get_heap_var loc = 
  try Hashtbl.find heap_hash loc
  with Not_found ->
    let name = "(alloc@" ^ loc.file ^ ":" ^ string_of_int loc.line ^ ")" in
    let newvar = makeGlobalVar name voidType in
      Hashtbl.add heap_hash loc newvar;
      newvar
      
module Glob = 
struct
  module Var = Basetype.Variables
  module Val = VD
end

module Dom = Lattice.Prod(CPA)(Flag)
