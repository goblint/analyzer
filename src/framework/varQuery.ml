type t =
  | Global of CilType.Varinfo.t
  | Node of Node.t
  (* TODO: add Function *)
[@@deriving ord]

type 'v f = 'v -> unit
