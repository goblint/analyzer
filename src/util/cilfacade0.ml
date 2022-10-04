(** Cilfacade functions to avoid dependency cycles.*)
open GoblintCil

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

(** Following functions are similar to [Cil] versions, but return expression location instead of entire statement location, where possible. *)
(* Ideally we would have both copies of the functions available, but UpdateCil would have to be adapted per-stmtkind/instr to store and update either one or two locations. *)

(** Get expression location for [Cil.instr]. *)
let get_instrLoc = function
  | Set (_, _, _loc, eloc) -> eloc
  | Call (_, _, _, _loc, eloc) -> eloc
  | Asm (_, _, _, _, _, loc) -> loc
  | VarDecl (_, loc) -> loc

(** Get expression location for [Cil.stmt]. *)
(* confusingly CIL.get_stmtLoc works on stmtkind instead *)
let rec get_stmtLoc stmt =
  match stmt.skind with
  (* no stmtkind/instr location in these cases, so try labels instead *)
  | Instr []
  | Block {bstmts = []; _} ->
    get_labelsLoc stmt.labels

  | Instr (hd :: _) -> get_instrLoc hd
  | Return (_, loc) -> loc
  | Goto (_, loc) -> loc
  | ComputedGoto (_, loc) -> loc
  | Break loc -> loc
  | Continue loc -> loc
  | If (_, _, _, _loc, eloc) -> eloc
  | Switch (_, _, _, _loc, eloc) -> eloc
  | Loop (_, _loc, eloc, _, _) -> eloc
  | Block {bstmts = hd :: _; _} -> get_stmtLoc hd
