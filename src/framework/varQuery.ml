type t =
  | Global of CilType.Varinfo.t
  (* TODO: add Node, Function *)
[@@deriving ord]
