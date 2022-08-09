open GoblintCil
open Pretty

include Printable.Std

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
[@@deriving eq, ord, hash, to_yojson]

let name () = "node"

(* TODO: remove this? *)
(** Pretty node plainly with entire stmt. *)
let pretty_plain () = function
  | Statement s -> text "Statement " ++ dn_stmt () s
  | Function f -> text "Function " ++ text f.svar.vname
  | FunctionEntry f -> text "FunctionEntry " ++ text f.svar.vname

(* TODO: remove this? *)
(** Pretty node plainly with stmt location. *)
let pretty_plain_short () = function
  | Statement s -> text "Statement @ " ++ CilType.Location.pretty () (Cilfacade.get_stmtLoc s)
  | Function f -> text "Function " ++ text f.svar.vname
  | FunctionEntry f -> text "FunctionEntry " ++ text f.svar.vname

(** Pretty node for solver variable tracing with short stmt. *)
let pretty_trace () = function
  | Statement stmt   -> dprintf "node %d \"%a\"" stmt.sid Cilfacade.stmt_pretty_short stmt
  | Function      fd -> dprintf "call of %s" fd.svar.vname
  | FunctionEntry fd -> dprintf "entry state of %s" fd.svar.vname

(** Output functions for Printable interface *)
let pretty () x = pretty_trace () x
include Printable.SimplePretty (
  struct
    type nonrec t = t
    let pretty = pretty
  end
  )

(** Show node ID for CFG and results output. *)
let show_id = function
  | Statement stmt   -> string_of_int stmt.sid
  | Function fd      -> "ret" ^ string_of_int fd.svar.vid
  | FunctionEntry fd -> "fun" ^ string_of_int fd.svar.vid

(** Show node label for CFG. *)
let show_cfg = function
  | Statement stmt   -> string_of_int stmt.sid (* doesn't use this but defaults to no label and uses ID from show_id instead *)
  | Function fd      -> "return of " ^ fd.svar.vname ^ "()"
  | FunctionEntry fd -> fd.svar.vname ^ "()"


let location (node: t) =
  match node with
  | Statement stmt -> Cilfacade.get_stmtLoc stmt
  | Function fd -> fd.svar.vdecl
  | FunctionEntry fd -> fd.svar.vdecl

(** Find [fundec] which the node is in. In an incremental run this might yield old fundecs for pseudo-return nodes from the old file. *)
let find_fundec (node: t) =
  match node with
  | Statement stmt -> Cilfacade.find_stmt_fundec stmt
  | Function fd -> fd
  | FunctionEntry fd -> fd
