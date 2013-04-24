exception Endl
exception Eof

(* type value = String of string | Bool of bool | Int of int | Float of float *)
type var = Var_ | Vari of string | Ident of string
type fcall = {fname: string; args: exp list}
and exp =
  Fun of fcall |
  Exp_ |
  Var of var |
  String of string | Bool of bool | Int of int | Float of float
type stmt = {lval: var option; exp: exp}
type def = Node of (string * string) | Edge of (string * string * stmt)

let var_to_string = function
  | Var_ -> "$_"
  | Vari x -> "$"^x
  | Ident x -> x
let rec exp_to_string = function
  | Fun x -> x.fname^"("^String.concat ", " (List.map exp_to_string x.args)^")"
  | Exp_ -> "_"
  | Var x -> var_to_string x
  | String x -> "\""^x^"\""
  | Bool x -> string_of_bool x
  | Int x -> string_of_int x
  | Float x -> string_of_float x
let stmt_to_string stmt = match stmt.lval, stmt.exp with
  | Some var, exp -> var_to_string var^" = "^exp_to_string exp
  | None, exp -> exp_to_string exp
let to_string = function
  | Node(n, m)    -> n^"\t\""^m^"\""
  | Edge(a, b, s) -> a^" -> "^b^"\t"^stmt_to_string s

let dot defs =
  let to_string = function
    | Node(n, m)    -> "  "^n^"\t[label=\""^m^"\"];"
    | Edge(a, b, s) -> "  "^a^" -> "^b^"\t[label=\""^stmt_to_string s^"\"];"
  in
  let lines = "digraph file {"::(List.map to_string defs)@["}"] in
  (* List.iter print_endline lines *)
  String.concat "\n" lines