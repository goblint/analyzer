open GoblintCil

module ExtVar =
struct
  type t =
    | AnyPrev of CilType.Varinfo.t
  [@@deriving eq, ord, hash]

  let name_varinfo = function
    | AnyPrev v -> "\\at(" ^ v.vname ^ ", AnyPrev)"

  let typ = function
    | AnyPrev v -> v.vtype

  let describe_varinfo _ = function
    | AnyPrev v -> ""
end

module ExtVarMap = RichVarinfo.BiVarinfoMap.Make (ExtVar)

let var_replace_with_original_visitor = object
  inherit nopCilVisitor

  method! vvrbl (vi: varinfo) =
    match ExtVarMap.from_varinfo vi with
    | None -> SkipChildren
    | Some (AnyPrev vi') -> ChangeTo vi'
end

let exp_is_suitable ?scope e =
  let open InvariantCil in
  let e = visitCilExpr var_replace_with_original_visitor e in (* replace extended vars with originals, so scope check passes (RichVarinfo-s are all global) *)
  not (exp_contains_tmp e || exp_contains_anon_comp_offset e) && GobOption.for_all (fun scope -> exp_is_in_scope scope e) scope

