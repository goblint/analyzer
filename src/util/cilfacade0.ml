(** Cilfacade functions to avoid dependency cycles.*)
open Cil

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
