exception Endl
exception Eof

type def = Node of (string * string) | Edge of (string * string * string)

let to_string = function
  | Node(n, m)    -> n^"\t\""^m^"\""
  | Edge(a, b, s) -> a^" -> "^b^"\t"^s

let dot defs =
  let to_string = function
    | Node(n, m)    -> "  "^n^"\t[label=\""^m^"\"];"
    | Edge(a, b, s) -> "  "^a^" -> "^b^"\t[label=\""^s^"\"];"
  in
  let lines = "digraph file {"::(List.map to_string defs)@["}"] in
  (* List.iter print_endline lines *)
  String.concat "\n" lines