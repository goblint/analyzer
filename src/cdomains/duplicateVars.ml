open CilType
open GoblintCil
open Batteries
open GoblintCil

module VarType = struct
  let equal_typ _ _ = true
  let hash_typ _ = 0
  let compare_typ _ _ = 0

  type t = AssignAux of (typ[@compare.ignore][@eq.ignore][@hash.ignore])
         | ReturnAux of (typ[@compare.ignore][@eq.ignore][@hash.ignore])
         | NormalVar of Varinfo.t
         | ShadowVar of Varinfo.t [@@deriving eq,ord,hash]

  let from_varinfo normal duplicated =
    List.map (fun v -> NormalVar v) normal @ List.map (fun v -> ShadowVar v) duplicated

  let show v = match v with
    | AssignAux t -> "AuxAssign"
    | ReturnAux t -> "AuxReturn"
    | NormalVar v -> v.vname
    | ShadowVar v -> "c2po__" ^ v.vname ^ "'"

  let name_varinfo v = match v with
    | AssignAux t -> "AuxAssign"
    | ReturnAux t -> "AuxReturn"
    | NormalVar v -> string_of_int v.vid
    | ShadowVar v -> "c2po__" ^ string_of_int v.vid ^ "'"


  (* Description that gets appended to the varinfo-name in user output. *)
  let describe_varinfo (var: varinfo) v =
    (* let loc = UpdateCil.getLoc node in
       CilType.Location.show loc *)
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
    | ShadowVar v -> {v with vid = var.vid}

end
