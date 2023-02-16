open GobConfig
open GoblintCil

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
    | AddrOf (Var info, NoOffset) when info.vid == var.vid -> raise Found
    | _ -> DoChildren
end

class hasAssignmentVisitor(var) = object
  inherit nopCilVisitor

  method! vinst = function
    | Set ((Var info, NoOffset),_,_,_) when info.vid == var.vid -> raise Found
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
        | _ -> diff := Some (Cilint.big_int_of_cilint cint); SkipChildren
      )
    | Set ((Var v, NoOffset), BinOp (MinusA, Lval (Var v2, NoOffset), Const (CInt (cint,_,_)), _ ),_,_) when v.vid = var.vid && v2.vid = var.vid ->
      ( match !diff with
        | Some _ -> raise WrongOrMultiple
        | _ -> diff := Some (Z.neg (Cilint.big_int_of_cilint cint)); SkipChildren
      )
    | Set ((Var v, NoOffset), _,_,_) when v.vid = var.vid  -> raise WrongOrMultiple
    | _ -> SkipChildren
end

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
  | Set (((Var info), NoOffset), Const(CInt (i,_,_)), _,_) when info.vid = var.vid -> Const (Cilint.big_int_of_cilint i)
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
          let current' = if st.labels <> [] then (print_endline "has Label"; (None)) else current in
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

let rec loopIterations start diff comp =
  let flip = function
    | Lt -> Gt
    | Gt -> Lt
    | Ge -> Le
    | Le -> Ge
    | s -> s
  in let loopIterations' goal shouldBeExact =
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
  in
  match comp with
  | BinOp (op, (Const _ as c), var, t) -> loopIterations start diff (BinOp (flip op, var, c, t))
  | BinOp (Lt, _, (Const (CInt (cint,_,_) )), _) -> if Z.lt diff Z.zero then None else loopIterations' (Cilint.big_int_of_cilint cint) false
  | BinOp (Gt, _, (Const (CInt (cint,_,_) )), _) -> if Z.gt diff Z.zero then None else loopIterations' (Cilint.big_int_of_cilint cint) false
  | BinOp (Le, _, (Const (CInt (cint,_,_) )), _) -> if Z.lt diff Z.zero then None else loopIterations' (Z.succ @@ Cilint.big_int_of_cilint cint) false
  | BinOp (Ge, _, (Const (CInt (cint,_,_) )), _) -> if Z.gt diff Z.zero then None else loopIterations' (Z.pred @@ Cilint.big_int_of_cilint cint ) false
  | BinOp (Ne, _, (Const (CInt (cint,_,_) )), _) -> loopIterations' (Cilint.big_int_of_cilint cint) true
  | _ -> failwith "unexpected comparison in loopIterations"

let ( >>= ) = Option.bind
let fixedLoopSize loopStatement func =
  let findBreakComparison = try (*find a single break in the else branch of a toplevel if*)
      let compOption = ref None in
      let visitor = new findBreakVisitor(compOption) in
      ignore @@ visitCilBlock visitor (loopBody loopStatement);
      !compOption
    with | WrongOrMultiple ->  None
  in let getLoopVar = function
      | BinOp (op, (Const (CInt _ )), Lval ((Var info), NoOffset), (TInt _))
      | BinOp (op, Lval ((Var info), NoOffset), (Const (CInt _ )), (TInt _)) when isCompare op && not info.vglob->
        Some info
      | _ -> None
  in let getsPointedAt var = try
         let visitor = new isPointedAtVisitor(var) in
         ignore  @@ visitCilFunction visitor func;
         false
       with | Found -> true
  in let assignmentDifference loop var = try
         let diff = ref None in
         let visitor = new findAssignmentConstDiff(diff, var) in
         ignore @@ visitCilStmt visitor loop;
         !diff
       with | WrongOrMultiple ->  None
  in

  findBreakComparison >>= fun comparison ->
  getLoopVar comparison >>= fun var ->
  if getsPointedAt var then
    None
  else
    constBefore var loopStatement func >>= fun start ->
    assignmentDifference loopStatement var >>= fun diff ->
    print_endline "comparison: ";
    Pretty.fprint stdout (dn_exp () comparison) ~width:50;
    print_endline "";
    print_endline "variable: ";
    print_endline var.vname;
    print_endline "start:";
    print_endline @@ Z.to_string start;
    print_endline "diff:";
    print_endline @@ Z.to_string diff;
    let iterations = loopIterations start diff comparison in
    match iterations with
    | None -> print_endline "iterations failed"; None
    | Some s ->
      try
        let s' = Z.to_int s in
        print_endline "iterations:";
        print_endline @@ string_of_int s';
        Some s'
      with  _ -> print_endline "iterations too big for integer"; None


class arrayVisitor = object
  inherit nopCilVisitor

  method! vvrbl info =
    if not @@ (hasAttribute "goblint_array_domain" info.vattr || AutoTune0.is_large_array info.vtype) then
      info.vattr <- addAttribute (Attr ("goblint_array_domain", [AStr "unroll"]) ) info.vattr;
    DoChildren
end
let annotateArrays loopBody = ignore @@ visitCilBlock (new arrayVisitor) loopBody

(*unroll loops that handle locks, threads and mallocs, asserts and reach_error*)
class loopUnrollingCallVisitor = object
  inherit nopCilVisitor

  method! vinst = function
    | Call (_,Lval ((Var info), NoOffset),args,_,_) when LibraryFunctions.is_special info -> (
        let desc = LibraryFunctions.find info in
        match desc.special args with
        | Malloc _
        | Calloc _
        | Realloc _
        | Lock _
        | Unlock _
        | ThreadCreate _
        | Assert _
        | ThreadJoin _ ->
          raise Found;
        | _ ->
          if List.mem "specification" @@ get_string_list "ana.autotune.activated" && get_string "ana.specification" <> "" then (
            match SvcompSpec.of_option () with
            | UnreachCall s -> if info.vname = s then raise Found
            | _ -> ()
          );
          DoChildren
      )
    | _ -> DoChildren

end

let loop_unrolling_factor loopStatement func totalLoops =
  let configFactor = get_int "exp.unrolling-factor" in
  if AutoTune0.isActivated "loopUnrollHeuristic" then
    let unrollFunctionCalled = try
        let thisVisitor = new loopUnrollingCallVisitor in
        ignore (visitCilStmt thisVisitor loopStatement);
        false;
      with
        Found -> true
    in
    (*unroll up to near an instruction count, higher if the loop uses malloc/lock/threads *)
    let targetInstructions = if unrollFunctionCalled then 50 else 25 in
    let loopStats = AutoTune0.collectFactors visitCilStmt loopStatement in
    if loopStats.instructions > 0 then
      let fixedLoop = fixedLoopSize loopStatement func in
      (* Unroll at least 10 times if there are only few (17?) loops *)
      let unroll_min = if totalLoops < 17 && AutoTune0.isActivated "forceLoopUnrollForFewLoops" then 10 else 0 in
      match fixedLoop with
      | Some i -> if i * loopStats.instructions < 100 then (print_endline "fixed loop size"; i) else max unroll_min (100 / loopStats.instructions)
      | _ -> max unroll_min (targetInstructions / loopStats.instructions)
    else
      (* Don't unroll empty (= while(1){}) loops*)
      0
  else
    configFactor

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

(*
  Makes a copy, replacing top-level breaks with goto loopEnd and top-level continues with
  goto currentIterationEnd
  Also assigns fresh names to all labels and patches gotos for labels appearing in the current
  fragment to their new name
*)
class copyandPatchLabelsVisitor(loopEnd,currentIterationEnd) = object
  inherit nopCilVisitor

  val mutable depth = 0
  val mutable loopNestingDepth = 0

  val gotos = StatementHashTable.create 20

  method! vstmt s =
    let after x =
      depth <- depth-1;
      if depth = 0 then
        (* the labels can only be patched once the entire part of the AST we want has been transformed, and *)
        (* we know all lables appear in the hash table *)
        let patchLabelsVisitor = new patchLabelsGotosVisitor(StatementHashTable.find_opt gotos) in
        let x  = visitCilStmt patchLabelsVisitor x in
        StatementHashTable.clear gotos;
        x
      else
        x
    in
    let rename_labels sn =
      let new_labels = List.map (function Label(str,loc,b) -> Label (Cil.freshLabel str,loc,b) | x -> x) sn.labels in
      (* this makes new physical copy*)
      let new_s = {sn with labels = new_labels} in
      if new_s.labels <> [] then
        (* Use original s, ns might be temporay e.g. if the type of statement changed *)
        (* record that goto s; appearing in the current fragment should later be patched to goto new_s *)
        StatementHashTable.add gotos s new_s;
      new_s
    in
    depth <- depth+1;
    match s.skind with
    | Continue loc ->
      if loopNestingDepth = 0 then
        (* turn top-level continues into gotos to end of current unrolling *)
        ChangeDoChildrenPost(rename_labels {s with skind = Goto (!currentIterationEnd, loc)}, after)
      else
        ChangeDoChildrenPost(rename_labels s, after)
    | Break loc ->
      if loopNestingDepth = 0 then
        (* turn top-level breaks into gotos to end of current unrolling *)
        ChangeDoChildrenPost(rename_labels {s with skind = Goto (loopEnd,loc)}, after)
      else
        ChangeDoChildrenPost(rename_labels s, after)
    | Loop _ -> loopNestingDepth <- loopNestingDepth+1;
      ChangeDoChildrenPost(rename_labels s, fun x -> loopNestingDepth <- loopNestingDepth-1; after x)
    | _ -> ChangeDoChildrenPost(rename_labels s, after)
end

class loopUnrollingVisitor(func, totalLoops) = object
  (* Labels are simply handled by giving them a fresh name. Jumps coming from outside will still always go to the original label! *)
  inherit nopCilVisitor

  method! vstmt s =
    match s.skind with
    | Loop (b,loc, loc2, break , continue) ->
      let duplicate_and_rem_labels s =
        let factor = loop_unrolling_factor s func totalLoops in
        if(factor > 0) then (
          print_endline @@ "unrolling loop at " ^ CilType.Location.show loc ^" with factor " ^ string_of_int factor;
          annotateArrays b;
          match s.skind with
          | Loop (b,loc, loc2, break , continue) ->
            (* top-level breaks should immediately go to the end of the loop, and not just break out of the current iteration *)
            let break_target = { (Cil.mkEmptyStmt ()) with labels = [Label (Cil.freshLabel "loop_end",loc, true)]} in
            (* continues should go to the next unrolling *)
            let continue_target i = { (Cil.mkEmptyStmt ()) with labels = [Label (Cil.freshLabel ("loop_continue_" ^ (string_of_int i)),loc, true)]} in
            (* passed as a reference so we can reuse the patcher for all unrollings of the current loop *)
            let current_continue_target = ref dummyStmt in
            let patcher = new copyandPatchLabelsVisitor (ref break_target, ref current_continue_target) in
            let one_copy () = visitCilStmt patcher (mkStmt (Block (mkBlock b.bstmts))) in
            let copies = List.init (factor) (fun i ->
                current_continue_target := continue_target i;
                mkStmt (Block (mkBlock [one_copy (); !current_continue_target])))
            in
            mkStmt (Block (mkBlock (copies@[s]@[break_target])))
          | _ -> failwith "invariant broken"
        ) else s (*no change*)
      in ChangeDoChildrenPost(s, duplicate_and_rem_labels);
    | _ -> DoChildren
end

let unroll_loops fd totalLoops =
  Cil.populateLabelAlphaTable fd;
  let thisVisitor = new loopUnrollingVisitor(fd, totalLoops) in
  ignore (visitCilFunction thisVisitor fd)
