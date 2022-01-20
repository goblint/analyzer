type t =
  | Global of CilType.Varinfo.t
  | Node of {node: Node.t; fundec: CilType.Fundec.t option}
  (* TODO: add Function *)
[@@deriving ord]

type 'v f = 'v -> unit
