open Cil
open Pretty

(** A node in the Control Flow Graph is either a statement or function. Think of
 * the function node as last node that all the returning nodes point to.  So
 * the result of the function call is contained in the function node. *)
type t =
  | Statement of CilType.Stmt.t
  (** The statements as identified by CIL *)
  | FunctionEntry of CilType.Fundec.t
  (** *)
  | Function of CilType.Fundec.t
  (** The variable information associated with the function declaration. *)
[@@deriving eq, ord, to_yojson]

(* TODO: remove this? *)
let pretty_plain () = function
  | Statement s -> text "Statement " ++ dn_stmt () s
  | Function f -> text "Function " ++ text f.svar.vname
  | FunctionEntry f -> text "FunctionEntry " ++ text f.svar.vname

(* TODO: remove this? *)
let pretty_plain_short () = function
  | Statement s -> text "Statement @ " ++ d_loc () (get_stmtLoc s.skind)
  | Function f -> text "Function " ++ text f.svar.vname
  | FunctionEntry f -> text "FunctionEntry " ++ text f.svar.vname

let hash x =
  match x with
  | Statement s     -> s.sid * 17
  | Function f      -> f.svar.vid
  | FunctionEntry f -> -f.svar.vid

let location (node: t) =
  match node with
  | Statement stmt -> get_stmtLoc stmt.skind
  | Function fd -> fd.svar.vdecl
  | FunctionEntry fd -> fd.svar.vdecl

let find_fundec (node: t) =
  match node with
  | Statement stmt -> Cilfacade.find_stmt_fundec stmt
  | Function fd -> fd
  | FunctionEntry fd -> fd

let show_id = function
  | Statement stmt   -> string_of_int stmt.sid
  | Function fd      -> "ret" ^ string_of_int fd.svar.vid
  | FunctionEntry fd -> "fun" ^ string_of_int fd.svar.vid
