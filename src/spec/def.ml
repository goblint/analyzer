exception Endl
exception Eof

type def = Node of (string * string) | Edge of (string * string * string)

let to_string = function
  | Node(n, m)    -> n^"\t\""^m^"\""
  | Edge(a, b, s) -> a^" -> "^b^"\t"^s