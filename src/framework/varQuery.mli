open GoblintCil

type t =
  | Global of Cil.varinfo
  | Node of {node: Node.t; fundec : Cil.fundec option} (** Optional [fundec] override to allow querying old state in incremental. *)
[@@deriving ord]

type 'v f = 'v -> unit

(** Takes a [Cil.file] and a list of names of globals.contents
    Returns a list of [VarQuery.t]s of globals whose [vname] is contained in the argument list,
    and the list of names for which no global with the name could be found. *)
val varqueries_from_names : Cil.file -> string list -> t list * string list
