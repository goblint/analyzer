open CilType
open GoblintCil
open Batteries
open GoblintCil

(** Variable Type used by the C-2PO  Analysis.
    It contains normal variables with a varinfo as well as auxiliary variables for
    assignment and return and duplicated variables for remembering the value of variables at the beginning of a function. *)
module VarType = struct
  (* the hash/compare values should not depend on the type.
     But they have to be defined even though they are not used, for some reason.*)
  let equal_typ _ _ = true
  let hash_typ _ = 0
  let compare_typ _ _ = 0

  type t = AssignAux of (typ[@compare.ignore][@eq.ignore][@hash.ignore])
         | ReturnAux of (typ[@compare.ignore][@eq.ignore][@hash.ignore])
         | NormalVar of Varinfo.t
         | DuplicVar of Varinfo.t [@@deriving eq,ord,hash]

  let from_varinfo normal duplicated =
    List.map (fun v -> NormalVar v) normal @ List.map (fun v -> DuplicVar v) duplicated

  let show v = match v with
    | AssignAux t -> "AuxAssign"
    | ReturnAux t -> "AuxReturn"
    | NormalVar v -> v.vname
    | DuplicVar v -> "c2po__" ^ v.vname ^ "'"

  let name_varinfo v = match v with
    | AssignAux t -> "AuxAssign"
    | ReturnAux t -> "AuxReturn"
    | NormalVar v -> string_of_int v.vid
    | DuplicVar v -> "c2po__" ^ string_of_int v.vid ^ "'"

  (* Description that gets appended to the varinfo-name in user output. *)
  let describe_varinfo (var: varinfo) v =
    show v
end

module VarVarinfoMap = RichVarinfo.BiVarinfoMap.Make(VarType)


module Var =
struct
  include VarType
  let dummy_varinfo typ: varinfo = VarVarinfoMap.to_varinfo (AssignAux typ)
  let return_varinfo typ = VarVarinfoMap.to_varinfo (ReturnAux typ)
  let to_varinfo v = let var = VarVarinfoMap.to_varinfo v in
    match v with
    | AssignAux t -> {var with vtype = t}
    | ReturnAux t -> {var with vtype = t}
    | NormalVar v -> v
    | DuplicVar v -> {v with vid = var.vid}

end
