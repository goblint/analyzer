(** Node functions to avoid dependency cycles. *)

(** A node in the Control Flow Graph is either a statement or function. Think of
 * the function node as last node that all the returning nodes point to.  So
 * the result of the function call is contained in the function node. *)
type t =
  | Statement of CilType.Stmt.t
  (** The statements as identified by CIL *)
  (* The stmt in a Statement node is misleading because nodes are program points between transfer functions (edges), which actually correspond to statement execution. *)
  | FunctionEntry of CilType.Fundec.t
  (** *)
  | Function of CilType.Fundec.t
  (** The variable information associated with the function declaration. *)
[@@deriving eq, ord, to_yojson]

let hash = function
  | Statement   stmt -> Hashtbl.hash (CilType.Stmt.hash stmt, 0)
  | Function      fd -> Hashtbl.hash (CilType.Fundec.hash fd, 1)
  | FunctionEntry fd -> Hashtbl.hash (CilType.Fundec.hash fd, 2)

let location (node: t) =
  match node with
  | Statement stmt -> Cilfacade0.get_stmtLoc stmt
  | Function fd -> fd.svar.vdecl
  | FunctionEntry fd -> fd.svar.vdecl

let current_node: t option ref = ref None
