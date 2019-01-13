(** domain of the base analysis *)

open Cil

module VD = ValueDomain.Compound

module CPA =
struct
  include MapDomain.MapBot_LiftTop (Basetype.Variables) (VD)
  let name () = "value domain"
end

module Flag =
struct
  include ConcDomain.SimpleThreadDomain
  let name () = "flag domain"
end


(* TODO: Do we need to make this dependant on the node? Probably yes, if we want
 to partition an array according to different things at different points *)
let affected_arrays = Hashtbl.create 113 (* TODO: Is this a good estimate? *)

let get_affected_arrays var =
  try Hashtbl.find affected_arrays var
  with Not_found -> []

let add_affected_array var arr =
  let current = get_affected_arrays var in
  if List.exists (fun x -> x == arr) current
  then ()
  else begin
    Hashtbl.replace affected_arrays var (arr::current); ()
  end

let add_all_affected_array arr vars =
  List.iter (fun x -> add_affected_array x arr) vars

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
