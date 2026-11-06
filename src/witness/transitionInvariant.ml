
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
