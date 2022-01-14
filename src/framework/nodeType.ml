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

open Cil
(* TODO: deduplicate with Cilfacade *)
let get_labelLoc = function
  | Label (_, loc, _) -> loc
  | Case (_, loc, _) -> loc
  | CaseRange (_, _, loc, _) -> loc
  | Default (loc, _) -> loc

let rec get_labelsLoc = function
  | [] -> Cil.locUnknown
  | label :: labels ->
    let loc = get_labelLoc label in
    if CilType.Location.equal loc Cil.locUnknown then
      get_labelsLoc labels (* maybe another label has known location *)
    else
      loc

let get_stmtkindLoc = Cil.get_stmtLoc (* CIL has a confusing name for this function *)

let get_stmtLoc stmt =
  match stmt.skind with
  (* Cil.get_stmtLoc returns Cil.locUnknown in these cases, so try labels instead *)
  | Instr []
  | Block {bstmts = []; _} ->
    get_labelsLoc stmt.labels
  | _ -> get_stmtkindLoc stmt.skind

let location (node: t) =
  match node with
  | Statement stmt -> get_stmtLoc stmt
  | Function fd -> fd.svar.vdecl
  | FunctionEntry fd -> fd.svar.vdecl

let current_node: t option ref = ref None
