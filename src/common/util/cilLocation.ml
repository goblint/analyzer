open GoblintCil

type locs = {
  loc: location;
  eloc: location;
}

let get_labelLoc = function
  | Label (_, loc, _) -> {loc; eloc = locUnknown}
  | Case (_, loc, eloc) -> {loc; eloc}
  | CaseRange (_, _, loc, eloc) -> {loc; eloc}
  | Default (loc, eloc) -> {loc; eloc}

(* let rec get_labelsLoc = function
  | [] -> {loc = locUnknown; eloc = locUnknown}
  | label :: labels ->
    let loc = get_labelLoc label in
    if CilType.Location.equal loc Cil.locUnknown then
      get_labelsLoc labels (* maybe another label has known location *)
    else
      loc *)

(** Following functions are similar to [Cil] versions, but return expression location instead of entire statement location, where possible. *)
(* Ideally we would have both copies of the functions available, but UpdateCil would have to be adapted per-stmtkind/instr to store and update either one or two locations. *)

(** Get locations for {!Cil.instr}. *)
let get_instrLoc = function
  | Set (_, _, loc, eloc) -> {loc; eloc}
  | Call (_, _, _, loc, eloc) -> {loc; eloc}
  | Asm (_, _, _, _, _, loc) -> {loc; eloc = locUnknown}
  | VarDecl (_, loc) -> {loc; eloc = locUnknown}

(** Get locations for {!Cil.stmt}. *)
(* confusingly {!Cil.get_stmtLoc} works on stmtkind instead *)
let rec get_stmtLoc stmt: locs =
  match stmt.skind with
  (* no stmtkind/instr location in these cases, so try labels instead *)
  | Instr []
  | Block {bstmts = []; _} ->
    (* get_labelsLoc stmt.labels *)
    {loc = locUnknown; eloc = locUnknown}

  | Instr (hd :: _) -> get_instrLoc hd
  | Return (_, loc) -> {loc; eloc = locUnknown}
  | Goto (_, loc) -> {loc; eloc = locUnknown}
  | ComputedGoto (_, loc) -> {loc; eloc = locUnknown}
  | Break loc -> {loc; eloc = locUnknown}
  | Continue loc -> {loc; eloc = locUnknown}
  | If (_, _, _, loc, eloc) -> {loc; eloc}
  | Switch (_, _, _, loc, eloc) -> {loc; eloc}
  | Loop (_, loc, eloc, _, _) -> {loc; eloc}
  | Block {bstmts = hd :: _; _} -> get_stmtLoc hd
