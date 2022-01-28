type t =
  | Global of CilType.Varinfo.t
  | Node of {node: Node.t; fundec: CilType.Fundec.t option} (** Optional [fundec] override to allow querying old state in incremental. *)
[@@deriving ord]

type 'v f = 'v -> unit
