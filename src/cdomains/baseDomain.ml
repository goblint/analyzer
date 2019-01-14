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
let affected_arrays:(varinfo,varinfo list) Hashtbl.t = Hashtbl.create 113 (* TODO: Is this a good estimate? *)

let get_affected_arrays var =
  try Hashtbl.find affected_arrays var
  with Not_found -> []

let add_affected_array var arr =
  let current = get_affected_arrays var in
  if List.exists (fun x -> x == arr) current
  then ()
  else begin
    Printf.printf "Added %s affected by %s \n" arr.vname var.vname;
    Hashtbl.replace affected_arrays var (arr::current); ()
  end

let add_all_affected_array arr exp =
  let rec varsInExp exp = match exp with (* TODO: What if a variable occurs multiple times? *)
    | Const _
    | SizeOf _
    | SizeOfE _
    | AlignOfE _
    | AddrOfLabel _
    | SizeOfStr _
    | AlignOf _
    | Question _ (* TODO is this correct? *)
    | AddrOf _
    | StartOf _ -> []
    | UnOp (_, e, _ )
    | CastE (_, e) -> varsInExp e
    | BinOp (_, e1, e2, _) -> (varsInExp e1)@(varsInExp e2)
    | Lval (Var v, _) -> [v]
    | Lval (Mem _,_) -> [] in
  List.iter (fun x -> add_affected_array x arr) (varsInExp exp)

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
