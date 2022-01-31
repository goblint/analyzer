type t =
    Global of Cil.varinfo
  | Node of { node : Node.t; fundec : Cil.fundec option; }
val compare : t -> t -> int
type 'v f = 'v -> unit
val varqueries_from_names : Cil.file -> string list -> t list * string list
