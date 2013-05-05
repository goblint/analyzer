open Batteries

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

(* let stmts edges = List.map (fun (a,b,c) -> c) edges
let get_fun stmt = match stmt.exp with Fun x -> Some x | _ -> None
let fun_records edges = List.filter_map get_fun (stmts edges)
let fun_names edges = fun_records edges |> List.map (fun x -> x.fname)
let fun_by_fname fname edges = List.filter (fun x -> x.fname=fname) (fun_records edges) *)
let fname_is fname stmt =
  match stmt.exp with
  | Fun x -> x.fname=fname
  | _ -> false

let startnode edges =
  (* The start node of the first transition is the start node of the automaton. *)
  let a,b,c = List.hd edges in a

let warning state nodes =
  try
    Some (snd (List.find (fun x -> fst x = state) nodes)) (* find node for state and return its warning *)
  with
  | Not_found -> None (* no node for state *)

let get_key_variant stmt =
  let rec get_from_exp = function
    | Fun f -> get_from_args f.args (* TODO for special_fn we only consider constraints where the root of the exp is Fun (see fname_is) *)
    | Var (Vari s) -> `Rval s
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
  | Some (Vari s) -> `Lval s
  | _ -> get_from_exp stmt.exp

let equal_form lval stmt =
  match lval, stmt.lval with
  | Some _, Some _
  | None, None -> true
  | _ -> false

let is_forwarding stmt =
  match stmt.lval, stmt.exp with
  | None, Var Var_ -> true
  | _    -> false

(* get function arguments with tags corresponding to the type -> should only be called for functions, returns [] for everything else *)
let get_fun_args stmt =
  let get_arg = function
    | String x  -> `String x
    | Bool x    -> `Bool x
    | Int x     -> `Int x
    | Float x   -> `Float x
    | Var (Var_)    -> `Error "$_ should only appear alone and not as an argument"
    | Var (Vari x)  -> `Vari x
    | Var (Ident x) -> `Ident x
    | Fun x     -> `Error "Functions aren't allowed to have functions as an argument (put the function as a previous state instead)"
    | Exp_ -> `Free
  in
  match stmt.exp with
  | Fun f -> List.map get_arg f.args
  | _ -> []

(* functions for output *)
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
let def_to_string = function
  | Node(n, m)    -> n^"\t\""^m^"\""
  | Edge(a, b, s) -> a^" -> "^b^"\t"^stmt_to_string s

let to_dot_graph defs =
  let def_to_string = function
    | Node(n, m)    -> "  "^n^"\t[label=\""^m^"\"];"
    | Edge(a, b, s) -> "  "^a^" -> "^b^"\t[label=\""^stmt_to_string s^"\"];"
  in
  let lines = "digraph file {"::(List.map def_to_string defs)@["}"] in
  (* List.iter print_endline lines *)
  String.concat "\n" lines