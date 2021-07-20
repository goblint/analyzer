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

type node = t [@@deriving eq, ord, to_yojson] (* TODO: remove after properly transitioning to t *)

let write_cfgs : ((node -> bool) -> unit) ref = ref (fun _ -> ())

let pretty_node () = function
  | Statement s -> text "Statement " ++ dn_stmt () s
  | Function f -> text "Function " ++ text f.svar.vname
  | FunctionEntry f -> text "FunctionEntry " ++ text f.svar.vname


let pretty_short_node () = function
  | Statement s -> text "Statement @ " ++ d_loc () (get_stmtLoc s.skind)
  | Function f -> text "Function " ++ text f.svar.vname
  | FunctionEntry f -> text "FunctionEntry " ++ text f.svar.vname

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
