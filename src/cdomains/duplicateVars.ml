open CilType
open GoblintCil
open Batteries
open GoblintCil

module Var = struct
  let equal_typ _ _ = true
  let hash_typ _ = 0
  let compare_typ _ _ = 0

  type t = AssignAux of (typ[@compare.ignore][@eq.ignore][@hash.ignore])
         | ReturnAux of (typ[@compare.ignore][@eq.ignore][@hash.ignore])
         | NormalVar of Varinfo.t
         | ShadowVar of Varinfo.t [@@deriving eq,ord,hash]

  let dummy_varinfo typ: varinfo = {dummyFunDec.svar with vid=(-1);vtype=typ;vname="c2po__@dummy"}
  let return_varinfo typ = {dummyFunDec.svar with vtype=typ;vid=(-2);vname="c2po__@return"}

  let duplicated_variable var = { var with vid = - var.vid - 4; vname = "c2po__" ^ var.vname ^ "'" }
  let original_variable var = { var with vid = - (var.vid + 4); vname = String.lchop ~n:6 @@ String.rchop var.vname }

  let is_wrpointer_ghost_variable x = x.vid < 0 && String.starts_with x.vname "c2po__"
  let to_varinfo v = match v with
    | AssignAux t -> dummy_varinfo t
    | ReturnAux t -> return_varinfo t
    | NormalVar v -> v
    | ShadowVar v -> duplicated_variable v

  let from_varinfo normal duplicated =
    List.map (fun v -> NormalVar v) normal @ List.map (fun v -> ShadowVar v) duplicated

  let show v = match v with
    | AssignAux t -> "AuxAssign"
    | ReturnAux t -> "AuxReturn"
    | NormalVar v -> v.vname
    | ShadowVar v -> "c2po__" ^ v.vname ^ "'"
end
