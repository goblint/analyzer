(** Used by C2poDomain and StartStateAnalysis.
    Contains functions to duplicate variables in order to have shadow variables for each function parameter,
    that can be used to remeber the initial value of these parameters.
    It uses RichVarinfo to create the duplicated variables. *)
open CilType
open Batteries
open GoblintCil
module M = Messages

(** Variable Type used by the C-2PO  Analysis.
    It contains normal variables with a varinfo as well as auxiliary variables for
    assignment and return and duplicated variables for remembering the value of variables at the beginning of a function. *)
module VarType = struct
  let equal_typ a b =
    CilType.Typ.equal a b

  let hash_typ x =
    CilType.Typ.hash x

  let compare_typ a b =
    CilType.Typ.compare a b

  type t =
    | AssignAux of typ
    | ReturnAux of typ
    | NormalVar of Varinfo.t
    | DuplicVar of Varinfo.t [@@deriving eq,ord,hash]

  let from_varinfo normal duplicated =
    let to_normal v =
      NormalVar v
    in
    let to_duplicated v =
      DuplicVar v
    in

    let normal = Seq.of_list normal in
    let duplicated = Seq.of_list duplicated in

    let normal = Seq.map to_normal normal in
    let duplicated = Seq.map to_duplicated duplicated in
    let complete = Seq.append normal duplicated in

    List.of_seq complete

  (* Need to lookup attributes such as vaddrof in the original varinfo *)
  let vaddrof : t -> bool = function
    | AssignAux _
    | ReturnAux _ -> false
    | NormalVar v
    | DuplicVar v -> v.vaddrof

  let vglob : t -> bool = function
    | AssignAux _
    | ReturnAux _ -> false
    | NormalVar v
    | DuplicVar v -> v.vglob

  let duplic_var_prefix =
    "c2po__"
  let duplic_var_postfix =
    "'"

  let show v = match v with
    | AssignAux t ->
      "AuxAssign"
    | ReturnAux t ->
      "AuxReturn"
    | NormalVar v ->
      v.vname
    | DuplicVar v ->
      duplic_var_prefix ^ v.vname ^ duplic_var_postfix

  let pretty () v = Pretty.text (show v)

  let get_type v = match v with
    | AssignAux t
    | ReturnAux t -> t
    | NormalVar v
    | DuplicVar v -> v.vtype

  let is_assign_aux = function
    | AssignAux _ -> true
    | _ -> false

  let is_return_aux = function
    | ReturnAux _ -> true
    | _ -> false

  let name_varinfo v =
    match v with
    | AssignAux t ->
      "AuxAssign"
    | ReturnAux t ->
      "AuxReturn"
    | NormalVar v ->
      v.vname
    | DuplicVar v ->
      duplic_var_prefix ^ string_of_int v.vid ^ duplic_var_postfix

  let typ v =
    get_type v

  (* Description that gets appended to the varinfo-name in user output. *)
  let describe_varinfo (var: varinfo) v =
    show v
end

module VarVarinfoMap = RichVarinfo.BiVarinfoMap.Make(VarType)

module Var =
struct
  include VarType

  let dummy_varinfo typ: varinfo =
    VarVarinfoMap.to_varinfo (AssignAux typ)

  let return_varinfo typ =
    VarVarinfoMap.to_varinfo (ReturnAux typ)

  let to_varinfo v =
    let res = VarVarinfoMap.to_varinfo v in
    if M.tracing then M.trace "c2po-varinfo" "to_varinfo: %a -> %a" d_type (get_type v) d_type res.vtype;
    res

end
