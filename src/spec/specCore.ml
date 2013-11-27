open Batteries

exception Endl
exception Eof

(* type value = String of string | Bool of bool | Int of int | Float of float *)
type lval = Ptr of lval | Var of string | Ident of string
type fcall = {fname: string; args: exp list}
and exp =
  Fun of fcall |
  Exp_ |
  Lval of lval |
  Regex of string |
  String of string | Bool of bool | Int of int | Float of float |
  Binop of string * exp * exp |
  Unop of string * exp
type stmt = {lval: lval option; exp: exp}
type def = Node of (string * string) (* node warning *)
         | Edge of (string * string list * bool * string * stmt) (* start-node, warning-nodes, forwarding, target-node, constraint *)

(* let stmts edges = List.map (fun (a,b,c) -> c) edges
let get_fun stmt = match stmt.exp with Fun x -> Some x | _ -> None
let fun_records edges = List.filter_map get_fun (stmts edges)
let fun_names edges = fun_records edges |> List.map (fun x -> x.fname)
let fun_by_fname fname edges = List.filter (fun x -> x.fname=fname) (fun_records edges) *)
let fname_is fname stmt =
  match stmt.exp with
  | Fun x -> x.fname=fname
  | _ -> false

let is_wildcard stmt = stmt.exp = Exp_

let branch_exp stmt =
  match stmt.exp with
  | Fun { fname="branch"; args=[exp; Bool tv] } -> Some (exp,tv)
  | _ -> None

let is_branch stmt = branch_exp stmt <> None

let startnode edges =
  (* The start node of the first transition is the start node of the automaton. *)
  let a,ws,fwd,b,c = List.hd edges in a

let warning state nodes =
  try
    Some (snd (List.find (fun x -> fst x = state) nodes)) (* find node for state and return its warning *)
  with
  | Not_found -> None (* no node for state *)

let get_key_variant stmt =
  let rec get_from_exp = function
    | Fun f -> get_from_args f.args (* TODO for special we only consider constraints where the root of the exp is Fun (see fname_is) *)
    | Lval (Var s) -> `Rval s
    | _ -> `None
  (* walks over arguments until it finds something or returns `None *)
  and get_from_argsi i = function
    | [] -> `None
    | x::xs ->
      match get_from_exp x with
      | `Rval s -> `Arg(s, i)
      | _       -> get_from_argsi (i+1) xs (* matches `None and `Arg -> `Arg of `Arg not supported *)
  and get_from_args args = get_from_argsi 0 args (* maybe better use List.findi *)
  in
  match stmt.lval with
  | Some (Var s) -> `Lval s
  | _ -> get_from_exp stmt.exp

let equal_form lval stmt =
  match lval, stmt.lval with
  | Some _, Some _
  | None, None -> true
  | _ -> false

(* get function arguments with tags corresponding to the type -> should only be called for functions, returns [] for everything else *)
let get_fun_args stmt =
  let rec get_arg = function
    | Regex x   -> `Regex x
    | String x  -> `String x
    | Bool x    -> `Bool x
    | Int x     -> `Int x
    | Float x   -> `Float x
    | Lval (Var x)  -> `Var x
    | Lval (Ident x) -> `Ident x
    | Fun x     -> `Error "Functions aren't allowed to have functions as an argument (put the function as a previous state instead)"
    | Exp_ -> `Free
    | Unop ("!", Bool x) -> `Bool (not x)
    | _         -> `Error "Unsupported operation inside function argument, use a simpler expression instead."
  in
  match stmt.exp with
  | Fun f -> List.map get_arg f.args
  | _ -> []

(* functions for output *)
let rec lval_to_string = function
  | Ptr x -> "*"^(lval_to_string x)
  | Var x -> "$"^x
  | Ident x -> x
let rec exp_to_string = function
  | Fun x -> x.fname^"("^String.concat ", " (List.map exp_to_string x.args)^")"
  | Exp_ -> "_"
  | Lval x -> lval_to_string x
  | Regex x -> "r\""^x^"\""
  | String x -> "\""^x^"\""
  | Bool x -> string_of_bool x
  | Int x -> string_of_int x
  | Float x -> string_of_float x
  | Binop (op, a, b) -> exp_to_string a ^ " " ^ op ^ " " ^ exp_to_string b
  | Unop  (op, a)    -> op ^ " " ^ exp_to_string a
let stmt_to_string stmt = match stmt.lval, stmt.exp with
  | Some lval, exp -> lval_to_string lval^" = "^exp_to_string exp
  | None, exp -> exp_to_string exp
let arrow_to_string ws fwd = (String.concat "," ws)^if fwd then ">" else ""
let def_to_string = function
  | Node(n, m)    -> n^"\t\""^m^"\""
  | Edge(a, ws, fwd, b, s) -> a^" -"^arrow_to_string ws fwd^"> "^b^"\t"^stmt_to_string s

let to_dot_graph defs =
  let no_warnings = true in
  let def_to_string = function
    | Node(n, m)    ->
      if no_warnings then ""
      else n^"\t[style=filled, fillcolor=orange, label=\""^n^": "^m^"\"];"
    | Edge(a, ws, fwd, b, s) ->
      let style = if fwd then "style=dotted, " else "" in
      let ws = if List.is_empty ws then "" else (String.concat "," ws)^" | " in
      a^" -> "^b^"\t["^style^"label=\""^ws^String.escaped (stmt_to_string s)^"\"];"
  in
  let ends,defs = List.partition (function Edge (a,ws,fwd,b,s) -> b="end" && s.exp=Exp_ | _ -> false) defs in
  let endstates = List.filter_map (function Edge (a,ws,fwd,b,s) -> Some a | _ -> None) ends in
  (* set the default style for nodes *)
  let defaultstyle = "node [shape=box, style=rounded];" in
  (* style end nodes and then reset *)
  let endstyle = if List.is_empty endstates then "" else "node [peripheries=2]; "^(String.concat " " endstates)^"; node [peripheries=1];" in
  let lines = "digraph file {"::defaultstyle::endstyle::(List.map def_to_string defs |> List.filter (fun s -> s<>"")) in
  (* List.iter print_endline lines *)
  String.concat "\n  " lines ^ "\n}"