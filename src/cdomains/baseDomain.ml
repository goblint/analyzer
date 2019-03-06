(** domain of the base analysis *)

open Cil

module VD = ValueDomain.Compound

module CPA =
struct
  include MapDomain.MapBot_LiftTop (Basetype.Variables) (VD)

  let arrays_should_join (x:t) (y:t) (x_eval_int: exp -> int64 option) (y_eval_int: exp -> int64 option) =
    let array_join_ok key (value:VD.t) =
      try 
        let other = find key y in
        VD.array_should_join value other x_eval_int y_eval_int
      with Not_found -> true
    in
    for_all array_join_ok x


  let name () = "value domain"
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
    let newvar = makeGlobalVar name voidType in
    Hashtbl.add heap_hash loc newvar;
    newvar

module Glob =
struct
  module Var = Basetype.Variables
  module Val = VD
end


module VarSet = SetDomain.Make(Basetype.Variables)
module VarMap = MapDomain.MapBot_LiftTop(Basetype.Variables)(VarSet)

module Dom = Lattice.Prod3(CPA)(Flag)(VarMap)
