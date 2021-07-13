open Cil
open Deriving.Cil
open Pretty

(** A node in the Control Flow Graph is either a statement or function. Think of
 * the function node as last node that all the returning nodes point to.  So
 * the result of the function call is contained in the function node. *)
type node =
  | Statement of CilType.Stmt.t
  (** The statements as identified by CIL *)
  | FunctionEntry of fundec
  (** *)
  | Function of fundec
  (** The variable information associated with the function declaration. *)
[@@deriving to_yojson]

let write_cfgs : ((node -> bool) -> unit) ref = ref (fun _ -> ())

let pretty_node () = function
  | Statement s -> text "Statement " ++ dn_stmt () s
  | Function f -> text "Function " ++ text f.svar.vname
  | FunctionEntry f -> text "FunctionEntry " ++ text f.svar.vname


let pretty_short_node () = function
  | Statement s -> text "Statement @ " ++ d_loc () (get_stmtLoc s.skind)
  | Function f -> text "Function " ++ text f.svar.vname
  | FunctionEntry f -> text "FunctionEntry " ++ text f.svar.vname

let node_compare n1 n2 =
  match n1, n2 with
  | FunctionEntry f, FunctionEntry g -> compare f.svar.vid g.svar.vid
  | _                    , FunctionEntry g -> -1
  | FunctionEntry g, _                     -> 1
  | Statement _, Function _  -> -1
  | Function  _, Statement _ -> 1
  | Statement s, Statement l -> compare s.sid l.sid
  | Function  f, Function g  -> compare f.svar.vid g.svar.vid

let compare_node = node_compare

let equal_node x y =
  match x,y with
  | Statement s1, Statement s2 -> CilType.Stmt.equal s1 s2
  | Function f1, Function f2 -> CilType.Fundec.equal f1 f2
  | FunctionEntry f1, FunctionEntry f2 -> CilType.Fundec.equal f1 f2
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
    | Function f      -> f.svar.vid
    | FunctionEntry f -> -f.svar.vid
end
