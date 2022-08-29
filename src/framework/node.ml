open GoblintCil

include Printable.Std

include Node0

let name () = "node"

(* TODO: remove this? *)
(** Pretty node plainly with entire stmt. *)
let pp_plain ppf = function
  | Statement s -> Fmt.pf ppf "Statement %a" dn_stmt s
  | Function f -> Fmt.pf ppf "Function %s" f.svar.vname
  | FunctionEntry f -> Fmt.pf ppf "FunctionEntry %s" f.svar.vname

(* TODO: remove this? *)
(** Pretty node plainly with stmt location. *)
let pp_plain_short ppf = function
  | Statement s -> Fmt.pf ppf "Statement @ %a" CilType.Location.pp (Cilfacade.get_stmtLoc s)
  | Function f -> Fmt.pf ppf "Function %s" f.svar.vname
  | FunctionEntry f -> Fmt.pf ppf "FunctionEntry %s" f.svar.vname

(** Pretty node for solver variable tracing with short stmt. *)
let pp_trace ppf = function
  | Statement stmt   -> Fmt.pf ppf "node %d \"%a\"" stmt.sid Cilfacade.stmt_pp_short stmt
  | Function      fd -> Fmt.pf ppf "call of %s (%d)" fd.svar.vname fd.svar.vid
  | FunctionEntry fd -> Fmt.pf ppf "entry state of %s (%d)" fd.svar.vname fd.svar.vid

(** Output functions for Printable interface *)
let pp ppf x = pp_trace ppf x
include Printable.SimplePretty (
  struct
    type nonrec t = t
    let pp = pp
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


(** Find [fundec] which the node is in. In an incremental run this might yield old fundecs for pseudo-return nodes from the old file. *)
let find_fundec (node: t) =
  match node with
  | Statement stmt -> Cilfacade.find_stmt_fundec stmt
  | Function fd -> fd
  | FunctionEntry fd -> fd
