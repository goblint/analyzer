open GobConfig
open GoblintCil

module M = Messages

(*loop unroll heuristics*)
(*used if AutoTune is activated*)

(*simple fixed loop iterations:
  - one single break with a comparison of a single integral local variable against a constant
  - before the loop the variable is assigned a constant (or never assigned)
  - only one assign to this variable inside the loop with a fixed difference to before
  - no pointer to this variable is used (to make the last two things checkable)

  We can't use succs/preds, because they get computed with the CFG, and the loop unrolling has to happen before
  TODO: gotos inside the loop could make the iteration not fixed, but are ignored for now
*)

(*exceptions to break out of visitors. two to make purpose clear*)
exception WrongOrMultiple
exception Found
class checkNoBreakVisitor = object
  inherit nopCilVisitor

  method! vstmt stmt =
    match stmt.skind with
    | Loop _
    | Switch _ -> SkipChildren (*Breaks in inner loops/switch are fine*)
    | Break _ -> raise WrongOrMultiple
    | _ -> DoChildren

end

let checkNoBreakStmt stmt =
  let visitor = new checkNoBreakVisitor in
  ignore @@ visitCilStmt visitor stmt

let checkNoBreakBlock block =
  let visitor = new checkNoBreakVisitor in
  ignore @@ visitCilBlock visitor block

class findBreakVisitor(compOption: exp option ref) = object
  inherit nopCilVisitor

  method! vstmt stmt =
    match stmt.skind with
    | Block _ -> DoChildren
    | Break _ -> raise WrongOrMultiple
    | If (cond, t, e, _, _) ->  (
        checkNoBreakBlock t;
        match e.bstmts with
        | [s] -> (
            match s.skind with
            | Break _ -> (
                match !compOption with
                | Some _ -> raise WrongOrMultiple (*more than one loop break*)
                | _ -> compOption := Some cond; SkipChildren
              )
            | _ -> checkNoBreakStmt stmt; SkipChildren
          )
        | _ -> checkNoBreakStmt stmt; SkipChildren
      )
    | _ ->  SkipChildren

end

class isPointedAtVisitor(var) = object
  inherit nopCilVisitor

  method! vexpr = function
    | AddrOf (Var info, NoOffset) when CilType.Varinfo.equal info var -> raise Found
    | _ -> DoChildren
end

class hasAssignmentVisitor(var) = object
  inherit nopCilVisitor

  method! vinst = function
    | Set ((Var info, NoOffset),_,_,_) when CilType.Varinfo.equal info var -> raise Found
    | _ -> SkipChildren
end

let hasAssignmentTo var block = try
    let visitor = new hasAssignmentVisitor(var) in
    ignore  @@ visitCilBlock visitor block;
    false
  with | Found -> true

class findAssignmentConstDiff((diff: Z.t option ref), var) = object
  inherit nopCilVisitor

  method! vstmt stmt = match stmt.skind with
    | Instr _
    | Block _ -> DoChildren
    | Loop (b,_,_,_,_) -> if hasAssignmentTo var b then raise WrongOrMultiple else SkipChildren
    | If (_, t, e, _, _) ->
      if hasAssignmentTo var t || hasAssignmentTo var t then
        raise WrongOrMultiple
      else SkipChildren
    | Switch (_,b,_,_,_) ->
      if hasAssignmentTo var b then
        raise WrongOrMultiple
      else SkipChildren
    | _ -> SkipChildren

  method! vinst = function
    | Set ((Var v, NoOffset), BinOp (PlusA, Const (CInt (cint,_,_)), Lval (Var v2, NoOffset), _ ),_,_)
    | Set ((Var v, NoOffset), BinOp (PlusA, Lval (Var v2, NoOffset), Const (CInt (cint,_,_)), _ ),_,_) when v.vid = var.vid && v2.vid = var.vid ->
      ( match !diff with
        | Some _ -> raise WrongOrMultiple
        | _ -> diff := Some cint; SkipChildren
      )
    | Set ((Var v, NoOffset), BinOp (MinusA, Lval (Var v2, NoOffset), Const (CInt (cint,_,_)), _ ),_,_) when v.vid = var.vid && v2.vid = var.vid ->
      ( match !diff with
        | Some _ -> raise WrongOrMultiple
        | _ -> diff := Some (Z.neg cint); SkipChildren
      )
    | Set ((Var v, NoOffset), _,_,_) when v.vid = var.vid  -> raise WrongOrMultiple
    | _ -> SkipChildren
end

class findStmtContainsInstructions = object
  inherit nopCilVisitor
  method! vinst = function
    | Set _
    | Call _ -> raise Found
    | _ -> DoChildren
end

let containsInstructions stmt =
  try
    ignore @@ visitCilStmt (new findStmtContainsInstructions) stmt; false
  with Found ->
    true

let isCompare = function
  | Lt | Gt |	Le | Ge | Ne -> true (*an loop that test for equality can not be of the type we look for*)
  | _ -> false

let loopBody loopStatement = match loopStatement.skind with
  | Loop (b,_,_,_,_) -> b
  | _ -> failwith "loopBody on non loop"
let loopLocation loopStatement = match loopStatement.skind with
  | Loop (_,l,_,_,_) -> l
  | _ -> failwith "loopLocation on non loop"

type assignment =
  | NoAssign
  | Const of Z.t
  | Other

let classifyInstruction var = function
  | Set (((Var info), NoOffset), Const(CInt (i,_,_)), _,_) when info.vid = var.vid -> Const i
  | Set (((Var info), NoOffset), _                       , _,_) when info.vid = var.vid -> Other
  | _ -> NoAssign

let lastAssignToVar var insList =
  let reverse = List.rev_map (classifyInstruction var) insList in
  let rec firstAssign = function
    | NoAssign::rest -> firstAssign rest
    | s::_ -> s
    | [] -> NoAssign
  in firstAssign reverse

(*find the last assignment to var before loop in f*)
(*return it if it is a constant and not inside a conditional branch*)
let constBefore var loop f =
  let targetLocation = loopLocation loop
  in let rec lastAssignmentToVarBeforeLoop (current: (Z.t option)) (statements: stmt list) = match statements with
      | st::stmts -> (
          let current' =
            (* If there exists labels that are not the ones inserted by loop unrolling, forget the found assigned constant value *)
            if List.exists (function
                | Label (s,_,_) -> not (String.starts_with ~prefix:"loop_continue" s || String.starts_with ~prefix:"loop_end" s)
                | _ -> true) st.labels
            then
              (Logs.debug "has Label"; (None))
            else
              current
          in
          match st.skind with
          | Instr list -> (
              match lastAssignToVar var list with
              | NoAssign -> lastAssignmentToVarBeforeLoop current' stmts
              | Other -> lastAssignmentToVarBeforeLoop (None) stmts
              | Const i -> lastAssignmentToVarBeforeLoop (Some i) stmts
            )
          | If (_, t, e, _, _) -> (
              match lastAssignmentToVarBeforeLoop current' t.bstmts with
              | (_, false) -> (
                  match lastAssignmentToVarBeforeLoop current' e.bstmts with
                  | (_, false) -> (*neither the then nor the else part contain loop*)
                    if hasAssignmentTo var t || hasAssignmentTo var e then
                      lastAssignmentToVarBeforeLoop (None) stmts (*because we do not know which path has been taken, invalidate previous assignment*)
                    else
                      lastAssignmentToVarBeforeLoop (current') stmts
                  | c -> c
                )
              | c -> c
            )
          | Loop (block, loc,_,_,_) -> (
              if CilType.Location.equal loc targetLocation then ( (*sid is not initialised at this point-> use location to identify loop*)
                (current', true) (*Stop iteration at the searched for loop*)
              ) else
                match lastAssignmentToVarBeforeLoop current' block.bstmts with
                | (_, false) -> (
                    if hasAssignmentTo var block then
                      lastAssignmentToVarBeforeLoop (None) stmts
                    else
                      lastAssignmentToVarBeforeLoop (current') stmts
                  )
                | c -> c
            )
          | Block block ->
            let (l, f) = lastAssignmentToVarBeforeLoop current' block.bstmts in
            if f then
              (l,f)
            else
              lastAssignmentToVarBeforeLoop l stmts
          | Switch (_, block, _,_,_) -> (
              match lastAssignmentToVarBeforeLoop current' block.bstmts with
              | (_, false) -> (
                  if hasAssignmentTo var block then
                    lastAssignmentToVarBeforeLoop (None) stmts
                  else
                    lastAssignmentToVarBeforeLoop (current') stmts
                )
              | c -> c
            )
          | _-> lastAssignmentToVarBeforeLoop (None) stmts (*the control flow could only go further if a goto jumps to this*)
        )
      | [] -> (current, false) (*we did not have the loop inside these statements*)
  in
  fst @@ lastAssignmentToVarBeforeLoop (Some Z.zero) f.sbody.bstmts (*the top level call should never return false*)

(*find a single break in the else branch of a toplevel if*)
let findBreakComparison loopStatement =
  try
    let compOption = ref None in
    let visitor = new findBreakVisitor (compOption) in
    ignore @@ visitCilBlock visitor (loopBody loopStatement);
    !compOption
  with WrongOrMultiple ->
    None

let findLoopVarAndGoal loopStatement func (op, exp1, exp2) =
  match Cil.stripCasts exp1, Cil.stripCasts exp2 with
  | Const (CInt (goal, _, _)), Lval (Var varinfo, NoOffset) when not varinfo.vglob ->
    (* TODO: define isCompare and flip in cilfacade and refactor to use instead of the many separately defined similar functions *)
    let flip = function | Lt -> Gt | Gt -> Lt | Ge -> Le | Le -> Ge | s -> s in
    Some (flip op, varinfo, goal)
  | Lval (Var varinfo, NoOffset), Const (CInt (goal, _, _)) when not varinfo.vglob ->
    Some (op, varinfo, goal)
  | Lval (Var varinfo, NoOffset), Lval (Var varinfo2, NoOffset) when not varinfo.vglob && not varinfo2.vglob ->
    (* When loop condition has a comparison between variables, we assume that the left one is the counter and right one is the bound.
       TODO: can we do something more meaningful instead of this assumption? *)
    begin match constBefore varinfo2 loopStatement func with
      | Some goal -> Logs.debug "const: %a %a" CilType.Varinfo.pretty varinfo2 GobZ.pretty goal; Some (op, varinfo, goal)
      | None -> None
    end;
  | _ -> None

let getLoopVar loopStatement func = function
  | BinOp (op, exp1, exp2, TInt _) when isCompare op -> findLoopVarAndGoal loopStatement func (op, exp1, exp2)
  | _ -> None

let getsPointedAt var func =
  try
    let visitor = new isPointedAtVisitor (var) in
    ignore  @@ visitCilFunction visitor func;
    false
  with Found ->
    true

let assignmentDifference loop var =
  try
    let diff = ref None in
    let visitor = new findAssignmentConstDiff (diff, var) in
    ignore @@ visitCilBlock visitor loop;
    !diff
  with WrongOrMultiple ->
    None

let adjustGoal diff goal op =
  match op with
  | Lt -> if Z.lt diff Z.zero then None else Some goal
  | Gt -> if Z.gt diff Z.zero then None else Some goal
  | Le -> if Z.lt diff Z.zero then None else Some (Z.succ goal)
  | Ge -> if Z.gt diff Z.zero then None else Some (Z.pred goal)
  | Ne -> Some goal
  | _ -> failwith "unexpected comparison in loopIterations"

let loopIterations start diff goal shouldBeExact =
  let range = Z.sub goal start in
  if Z.equal diff Z.zero || Z.equal range Z.zero || (Z.gt diff Z.zero && Z.lt range Z.zero) ||  (Z.lt diff Z.zero && Z.gt range Z.zero) then
    None (*unfitting parameters*)
  else (
    let roundedDown = Z.div range diff in
    let isExact = Z.equal (Z.mul roundedDown diff) range in
    if isExact then
      Some roundedDown
    else if shouldBeExact then
      None
    else
      Some (Z.succ roundedDown)
  )

let fixedLoopSize loopStatement func =
  let open GobOption.Syntax in
  let* comparison = findBreakComparison loopStatement in
  let* op, var, goal = getLoopVar loopStatement func comparison in
  if getsPointedAt var func then
    None
  else
    let diff = Option.value (assignmentDifference (loopBody loopStatement) var) ~default:Z.one in
    (* Assume var start value if there was no constant assignment to the var before loop;
       Assume it to be 0, if loop is increasing and 11 (TODO: can we do better than just 11?) if loop is decreasing *)
    let start = Option.value (constBefore var loopStatement func) ~default:(if diff < Z.zero then Z.of_int 11 else Z.zero) in
    let* goal = adjustGoal diff goal op in
    let iterations = loopIterations start diff goal (op=Ne) in
    Logs.debug "comparison: %a" CilType.Exp.pretty comparison;
    Logs.debug "variable: %s" var.vname;
    Logs.debug "start: %a" GobZ.pretty start;
    Logs.debug "diff: %a" GobZ.pretty diff;
    match iterations with
    | None -> Logs.debug "iterations failed"; None
    | Some s ->
      try
        let s' = Z.to_int s in
        Logs.debug "iterations: %d" s';
        Some s'
      with Z.Overflow ->
        Logs.debug "iterations too big for integer";
        None


class arrayVisitor = object
  inherit nopCilVisitor

  method! vvrbl info =
    if Cil.isArrayType info.vtype && not @@ (hasAttribute "goblint_array_domain" info.vattr || AutoTune0.is_large_array info.vtype) then
      info.vattr <- addAttribute (Attr ("goblint_array_domain", [AStr "unroll"]) ) info.vattr;
    DoChildren
end
let annotateArrays loopBody = ignore @@ visitCilBlock (new arrayVisitor) loopBody

let max_default_unrolls_per_spec (spec: Svcomp.Specification.t) =
  match spec with
  | NoDataRace -> 0
  | NoOverflow -> 2
  | _ -> 4

let loop_unrolling_factor loopStatement func totalLoops =
  let configFactor = get_int "exp.unrolling-factor" in
  if containsInstructions loopStatement then
    if AutoTune0.isActivated "loopUnrollHeuristic" then
      match fixedLoopSize loopStatement func with
      | Some i when i <= 20 -> Logs.debug "fixed loop size %d" i; i
      | _ ->
        match Svcomp.Specification.of_option () with
        | [] -> 4
        | specs -> BatList.max @@ List.map max_default_unrolls_per_spec specs
    else
      configFactor
  else (* Don't unroll empty (= while(1){}) loops*)
    0

(*actual loop unrolling*)

(* Hashtable used to patch gotos later *)
module StatementHashTable = Hashtbl.Make(struct
    type t = stmt
    (* Identity by physical equality. *)
    let equal = (==)
    let hash = Hashtbl.hash
  end)

(* replaces goto s with goto s' when newtarget s = Some s' IN PLACE *)
class patchLabelsGotosVisitor(newtarget) = object
  inherit nopCilVisitor

  method! vstmt s =
    match s.skind with
    | Goto (target,loc) ->
      (match newtarget !target with
       | None -> SkipChildren
       | Some nt -> s.skind <- Goto (ref nt, loc); DoChildren)
    | _ -> DoChildren
end

include LoopUnrolling0

(*
  Makes a copy, replacing top-level breaks with goto loopEnd and top-level continues with
  goto currentIterationEnd
  Also assigns fresh names to all labels and patches gotos for labels appearing in the current
  fragment to their new name
*)
class copyandPatchLabelsVisitor(loopEnd, currentIterationEnd, gotos) = object
  inherit nopCilVisitor

  val mutable loopNestingDepth = 0

  method! vstmt s =
    let after x = x in
    let rename_labels sn =
      let new_labels = List.map (function Label(str,loc,b) -> Label (Cil.freshLabel str,loc,b) | x -> x) sn.labels in
      (* this makes new physical copy*)
      let new_s = {sn with labels = new_labels} in
      CopyOfHashTable.replace copyof new_s s;
      if M.tracing then M.trace "cfg" "Marking %a as copy of %a" CilType.Stmt.pretty new_s CilType.Stmt.pretty s;
      if new_s.labels <> [] then
        (* Use original s, ns might be temporay e.g. if the type of statement changed *)
        (* record that goto s; appearing in the current fragment should later be patched to goto new_s *)
        StatementHashTable.add gotos s new_s;
      new_s
    in
    match s.skind with
    | Continue loc ->
      if loopNestingDepth = 0 then
        (* turn top-level continues into gotos to end of current unrolling *)
        ChangeDoChildrenPost(rename_labels {s with skind = Goto (ref currentIterationEnd, loc)}, after)
      else
        ChangeDoChildrenPost(rename_labels s, after)
    | Break loc ->
      if loopNestingDepth = 0 then
        (* turn top-level breaks into gotos to end of current unrolling *)
        ChangeDoChildrenPost(rename_labels {s with skind = Goto (ref loopEnd,loc)}, after)
      else
        ChangeDoChildrenPost(rename_labels s, after)
    | Loop _ -> loopNestingDepth <- loopNestingDepth+1;
      ChangeDoChildrenPost(rename_labels s, fun x -> loopNestingDepth <- loopNestingDepth-1; after x)
    | _ -> ChangeDoChildrenPost(rename_labels s, after)
end

let copy_and_patch_labels break_target current_continue_target stmts =
  let gotos = StatementHashTable.create 20 in
  let patcher = new copyandPatchLabelsVisitor (break_target, current_continue_target, gotos) in
  let stmts' = List.map (visitCilStmt patcher) stmts in
  (* the labels can only be patched once the entire part of the AST we want has been transformed, and *)
  (* we know all lables appear in the hash table *)
  let patchLabelsVisitor = new patchLabelsGotosVisitor(StatementHashTable.find_opt gotos) in
  List.map (visitCilStmt patchLabelsVisitor) stmts'

class loopUnrollingVisitor (func, totalLoops) = object
  (* Labels are simply handled by giving them a fresh name. Jumps coming from outside will still always go to the original label! *)
  inherit nopCilVisitor

  val mutable nests = 0

  method! vstmt stmt =
    let duplicate_and_rem_labels stmt =
      match stmt.skind with
      | Loop (b, loc, loc2, break, continue) ->
        nests <- nests - 1; Logs.debug "nests: %i" nests;
        let factor = loop_unrolling_factor stmt func totalLoops in
        if factor > 0 then (
          Logs.info "unrolling loop at %a with factor %d" CilType.Location.pretty loc factor;
          annotateArrays b;
          (* top-level breaks should immediately go to the end of the loop, and not just break out of the current iteration *)
          let break_target = { (Cil.mkEmptyStmt ()) with labels = [Label (Cil.freshLabel "loop_end",loc, false)]} in
          let copies = List.init factor (fun i ->
              (* continues should go to the next unrolling *)
              let current_continue_target = { (Cil.mkEmptyStmt ()) with labels = [Label (Cil.freshLabel ("loop_continue_" ^ (string_of_int i)),loc, false)]} in
              let one_copy_stmts = copy_and_patch_labels break_target current_continue_target b.bstmts in
              one_copy_stmts @ [current_continue_target]
            )
          in
          mkStmt (Block (mkBlock (List.flatten copies @ [stmt; break_target])))
        ) else stmt (*no change*)
      | _ -> stmt
    in
    match stmt.skind with
    | Loop _ when nests + 1 < 4 -> nests <- nests + 1; ChangeDoChildrenPost(stmt, duplicate_and_rem_labels)
    | Loop _ -> SkipChildren
    | _ -> ChangeDoChildrenPost(stmt, duplicate_and_rem_labels)
end

let unroll_loops fd totalLoops =
  if not (Cil.hasAttribute "goblint_stub" fd.svar.vattr) then (
    Cil.populateLabelAlphaTable fd;
    let thisVisitor = new loopUnrollingVisitor(fd, totalLoops) in
    ignore (visitCilFunction thisVisitor fd)
  )
