(** Used by C2poDomain and StartStateAnalysis.
    Contains functions to duplicate variables in order to have shadow variables for each function parameter,
    that can be used to remeber the initial value of these parameters.
    It uses RichVarinfo to create the duplicated variables. *)
open CilType
open GoblintCil
open Batteries
open GoblintCil
module M = Messages

(** Variable Type used by the C-2PO  Analysis.
    It contains normal variables with a varinfo as well as auxiliary variables for
    assignment and return and duplicated variables for remembering the value of variables at the beginning of a function. *)
module VarType = struct
  let equal_typ a b =
    (* TODO: Structural equality on (possibly cyclic) data type typ may not terminate in incremental analysis. *)
    (* Using Stdlib.(=) instead of Stdlib.compare will not terminate even in the non-incremental case. *)
    Stdlib.compare a b = 0

  let hash_typ x =
    Hashtbl.hash x

  let compare_typ a b =
    (* TODO: Structural compare on (possibly cyclic) data type typ may not terminate in incremental analysis. *)
    Stdlib.compare a b

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

  let varinfo_attributes v =
    let open RichVarinfo.VarinfoDescription in
    match v with
    | AssignAux t ->
      ({(empty "AuxAssign") with vtype_=Some t})
    | ReturnAux t ->
      ({(empty "AuxReturn") with vtype_=Some t})
    | NormalVar v ->
      from_varinfo v
    | DuplicVar v ->
      let vname_ = duplic_var_prefix ^ string_of_int v.vid ^ duplic_var_postfix in
      from_varinfo {v with vname = vname_}

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
