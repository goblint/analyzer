(** domain of the base analysis *)

open Cil

module VD = ValueDomain.Compound

module CPA =
struct
  include MapDomain.MapBot_LiftTop (Basetype.Variables) (VD)
  let name () = "value domain"

  let invariant c (m:t) =
    fold (fun k v a ->
        if not (InvariantCil.var_is_tmp k) then
          let i = VD.invariant k.vname v in
          Invariant.(a && i)
        else
          a
      ) m Invariant.none
end

module Flag =
struct
  include ConcDomain.SimpleThreadDomain
  let name () = "flag domain"
end


let heap_hash = Hashtbl.create 113

let get_heap_var loc =
  try Hashtbl.find heap_hash loc
  with Not_found ->
    let name = "(alloc@" ^ loc.file ^ ":" ^ string_of_int loc.line ^ ")" in
    let newvar = Goblintutil.create_var (makeGlobalVar name voidType) in
    Hashtbl.add heap_hash loc newvar;
    newvar

module Glob =
struct
  module Var = Basetype.Variables
  module Val = VD
end

module Dom = Lattice.Prod(CPA)(Flag)
