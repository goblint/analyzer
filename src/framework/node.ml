open Cil
open Deriving.Cil
open Pretty

(** A node in the Control Flow Graph is either a statement or function. Think of
 * the function node as last node that all the returning nodes point to.  So
 * the result of the function call is contained in the function node. *)
type node =
  | Statement of stmt
  (** The statements as identified by CIL *)
  | FunctionEntry of varinfo
  (** *)
  | Function of varinfo
  (** The variable information associated with the function declaration. *)
[@@deriving to_yojson]

let write_cfgs : ((node -> bool) -> unit) ref = ref (fun _ -> ())

let pretty_node () = function
  | Statement s -> text "Statement " ++ dn_stmt () s
  | Function f -> text "Function " ++ text f.vname
  | FunctionEntry f -> text "FunctionEntry " ++ text f.vname


let pretty_short_node () = function
  | Statement s -> text "Statement @ " ++ d_loc () (get_stmtLoc s.skind)
  | Function f -> text "Function " ++ text f.vname
  | FunctionEntry f -> text "FunctionEntry " ++ text f.vname

let node_compare n1 n2 =
  match n1, n2 with
  | FunctionEntry f, FunctionEntry g -> compare f.vid g.vid
  | _                    , FunctionEntry g -> -1
  | FunctionEntry g, _                     -> 1
  | Statement _, Function _  -> -1
  | Function  _, Statement _ -> 1
  | Statement s, Statement l -> compare s.sid l.sid
  | Function  f, Function g  -> compare f.vid g.vid

let compare_node = node_compare

let equal_node x y =
  match x,y with
  | Statement s1, Statement s2 -> s1.sid = s2.sid
  | Function f1, Function f2 -> f1.vid = f2.vid
  | FunctionEntry f1, FunctionEntry f2 -> f1.vid = f2.vid
  | _ -> false

let print doc =
  print_string @@ Pretty.sprint max_int doc

let to_str doc =
  Pretty.sprint max_int doc
let print_b bool =
  print_endline (if bool then "true" else "false"); bool

module Node :
sig
  include Hashtbl.HashedType with type t = node
  include Set.OrderedType with type t := node
end =
struct
  type t = node [@@deriving eq, ord]
  let hash x =
    match x with
    | Statement s     -> s.sid * 17
    | Function f      -> f.vid
    | FunctionEntry f -> -f.vid
end
