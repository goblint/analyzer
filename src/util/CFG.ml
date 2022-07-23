open Cil
open GobConfig

class allBBVisitor = object (* puts every instruction into its own basic block *)
  inherit nopCilVisitor
  method! vstmt s =
    match s.skind with
    | Instr(il) ->
      let list_of_stmts =
        List.map (fun one_inst -> mkStmtOneInstr one_inst) il in
      let block = mkBlock list_of_stmts in
      ChangeDoChildrenPost(s, (fun _ -> s.skind <- Block(block); s))
    | _ -> DoChildren

  method! vvdec _ = SkipChildren
  method! vexpr _ = SkipChildren
  method! vlval _ = SkipChildren
  method! vtype _ = SkipChildren
end

let end_basic_blocks f =
  let thisVisitor = new allBBVisitor in
  visitCilFileSameGlobals thisVisitor f

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

(* Hashtable used to patch gotos later *)
module StatementHashTable = Hashtbl.Make(struct
    type t = stmt
    (* Identity by physical equality. *)
    let equal = (==)
    let hash = Hashtbl.hash
  end)

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

class findAssignmentConstDiff((diff: int option ref), var) = object
  inherit nopCilVisitor

  method! vstmt stmt = match stmt.skind with 
    | Loop _ 
    | Instr _ 
    | Block _ -> DoChildren
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
      | _ -> diff := Some (cilint_to_int cint); SkipChildren
     )
    | Set ((Var v, NoOffset), BinOp (MinusA, Lval (Var v2, NoOffset), Const (CInt (cint,_,_)), _ ),_,_) when v.vid = var.vid && v2.vid = var.vid ->
     ( match !diff with 
      | Some _ -> raise WrongOrMultiple 
      | _ -> diff := Some (- (cilint_to_int cint)); SkipChildren
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
  | Const of int
  | Other

let classifyInstruction var = function
  | Set (((Var info), NoOffset), Const(CInt (i,_,_)), _,_) when info.vid = var.vid -> Const (cilint_to_int i)
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
  in let rec lastAssignmentToVarBeforeLoop (current: (int option)) (statements: stmt list) = match statements with
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
  fst @@ lastAssignmentToVarBeforeLoop (Some 0) f.sbody.bstmts (*the top level call should never return false*)

let rec loopIterations start diff comp = 
  let flip = function
    | Lt -> Gt
    | Gt -> Lt
    | Ge -> Le
    | Le -> Ge
    | s -> s
  in let loopIterations' goal shouldBeExact =
    let range = goal - start in 
    if diff = 0 || range = 0 || (diff > 0 && range < 0) ||  (diff < 0 && range > 0) then 
      None (*unfitting parameters*)
    else (  
      let roundedDown = range / diff in
      let isExact = roundedDown * diff = range in
      if isExact then
        Some roundedDown
      else if shouldBeExact then 
        None
      else
        Some (roundedDown +1)
    )
  in 
  match comp with 
    | BinOp (op, (Const _ as c), var, t) -> loopIterations start diff (BinOp (flip op, var, c, t))
    | BinOp (Lt, _, (Const (CInt (cint,_,_) )), _) -> if diff < 0 then None else loopIterations' (cilint_to_int cint) false
    | BinOp (Gt, _, (Const (CInt (cint,_,_) )), _) -> if diff > 0 then None else loopIterations' (cilint_to_int cint) false
    | BinOp (Le, _, (Const (CInt (cint,_,_) )), _) -> if diff < 0 then None else loopIterations' (cilint_to_int cint + 1) false
    | BinOp (Ge, _, (Const (CInt (cint,_,_) )), _) -> if diff > 0 then None else loopIterations' (cilint_to_int cint - 1) false
    | BinOp (Ne, _, (Const (CInt (cint,_,_) )), _) -> loopIterations' (cilint_to_int cint) true
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
  in let ( >>= ) = Option.bind
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
  print_endline @@ string_of_int start;
  print_endline "diff:";
  print_endline @@ string_of_int diff;
  let iterations = loopIterations start diff comparison in
  (match iterations with 
    |None -> print_endline "iterations failed";
    | Some s ->  
      print_endline "iterations:";
      print_endline @@ string_of_int s
  );
  iterations

(*unroll loops that handle locks, threads and mallocs*)
class loopUnrollingCallVisitor = object
  inherit nopCilVisitor

  method! vinst = function
    | Call (_,Lval ((Var info), NoOffset),args,_,_) -> (
      let desc = LibraryFunctions.find info in
      match desc.special args with
        | Malloc _
        | Calloc _
        | Realloc _
        | Lock _
        | Unlock _
        | ThreadCreate _
        | ThreadJoin _ -> 
          raise Found;
        | _ -> ();
      DoChildren;)
    | _ -> DoChildren

end

let loop_unrolling_factor loopStatement func = 0 (*
  let configFactor = get_int "exp.unrolling-factor" in
  let unrollFunctionCalled = try
      let thisVisitor = new loopUnrollingCallVisitor in
      ignore (visitCilStmt thisVisitor loopStatement);
      false;
    with
    Found -> true 
  in
  (*unroll up to near an instruction count, higher if the loop uses malloc/lock/threads *)
  let targetInstructions = if unrollFunctionCalled then 50 else 25 in
  let loopStats = AutoTune.collectFactors visitCilStmt loopStatement in
  let targetFactor = if loopStats.instructions > 0 then targetInstructions / loopStats.instructions else 0 in (* Don't unroll empty (= while(1){}) loops*)
  let fixedLoop = fixedLoopSize loopStatement func in
  if not @@ get_bool "ana.autotune.enabled" then 
    configFactor 
  else 
    match fixedLoop with 
      | Some i -> if i * loopStats.instructions < 100 then (print_endline "fixed loop size"; i) else targetFactor
      | _ -> targetFactor
  *)

class loopUnrollingVisitor(func) = object
  (* Labels are simply handled by giving them a fresh name. Jumps coming from outside will still always go to the original label! *)
  inherit nopCilVisitor

  method! vstmt s =
    match s.skind with
    | Loop (b,loc, loc2, break , continue) ->
      let duplicate_and_rem_labels s =
        let factor = loop_unrolling_factor s func in
        if(factor > 0) then (
          print_endline @@ "unrolling loop at " ^ CilType.Location.show loc ^" with factor " ^ string_of_int factor;
          match s.skind with
          | Loop (b,loc, loc2, break , continue) ->
            (* We copy the statement to later be able to modify it without worrying about invariants *)
            let s = { s with sid = s.sid } in
            
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
      in ChangeDoChildrenPost({s with sid = s.sid},duplicate_and_rem_labels);
    | _ -> DoChildren
end

let loop_unrolling fd =
  let thisVisitor = new loopUnrollingVisitor(fd) in
  ignore (visitCilFunction thisVisitor fd)


let visitors = ref []
let register_preprocess name visitor_fun =
  visitors := !visitors @ [name, visitor_fun]

let do_preprocess ast =
  let f fd (name, visitor_fun) =
    (* this has to be done here, since the settings aren't available when register_preprocess is called *)
    if List.mem name (get_string_list "ana.activated") then
      ignore @@ visitCilFunction (visitor_fun fd) fd
  in
  iterGlobals ast (function GFun (fd,_) -> List.iter (f fd) !visitors | _ -> ())



let createCFG (fileAST: file) =
  (* The analyzer keeps values only for blocks. So if you want a value for every program point, each instruction      *)
  (* needs to be in its own block. end_basic_blocks does that.                                                        *)
  (* After adding support for VLAs, there are new VarDecl instructions at the point where a variable was declared and *)
  (* its declaration is no longer printed at the beginning of the function. Putting these VarDecl into their own      *)
  (* BB causes the output CIL file to no longer compile.                                                              *)
  (* Since we want the output of justcil to compile, we do not run allBB visitor if justcil is enable, regardless of  *)
  (* exp.basic-blocks. This does not matter, as we will not run any analysis anyway, when justcil is enabled.         *)
  if not (get_bool "exp.basic-blocks") && not (get_bool "justcil") then end_basic_blocks fileAST;

  (* We used to renumber vids but CIL already generates them fresh, so no need.
   * Renumbering is problematic for using [Cabs2cil.environment], e.g. in witness invariant generation to use original variable names.
   * See https://github.com/goblint/cil/issues/31#issuecomment-824939793. *)

  iterGlobals fileAST (fun glob ->
      match glob with
      | GFun(fd,_) ->
        (* before prepareCfg so continues still appear as such *)
        if (get_int "exp.unrolling-factor")>0 || get_bool "ana.autotune.enabled" then loop_unrolling fd;
        prepareCFG fd;
        computeCFGInfo fd true
      | _ -> ()
    );
  do_preprocess fileAST