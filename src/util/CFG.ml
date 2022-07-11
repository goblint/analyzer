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


(*unroll loops that handle locks, threads and mallocs*)
(*used if autoselect is activated*)
exception UnrollReasonFound
class loopUnrollingFactorVisitor = object
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
          raise UnrollReasonFound;
        | _ -> ();
      DoChildren;)
    | _ -> DoChildren

end

let loop_unrolling_factor loopStatement = 
  let configFactor = get_int "exp.unrolling-factor" in
  if get_bool "ana.autoselect" then (
    let thisVisitor = new loopUnrollingFactorVisitor in
    try 
      ignore (visitCilStmt thisVisitor loopStatement);
      configFactor; (*fallback to explicit setting, 0 by default*)
    with
      UnrollReasonFound -> Int.max configFactor 10 (*unroll loops further, but not less than "exp.unrolling-factor"*)
  ) 
  else configFactor

class loopUnrollingVisitor = object
  (* Labels are simply handled by giving them a fresh name. Jumps coming from outside will still always go to the original label! *)
  inherit nopCilVisitor

  method! vstmt s =
    match s.skind with
    | Loop (b,loc, loc2, break , continue) ->
      let factor = loop_unrolling_factor s in
      let duplicate_and_rem_labels s =
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
      in
      if(factor > 0) then
        ChangeDoChildrenPost({s with sid = s.sid},duplicate_and_rem_labels)
      else
        DoChildren
    | _ -> DoChildren
end

let loop_unrolling fd =
  let thisVisitor = new loopUnrollingVisitor in
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
        if (get_int "exp.unrolling-factor")>0 || get_bool "ana.autoselect" then loop_unrolling fd;
        prepareCFG fd;
        computeCFGInfo fd true
      | _ -> ()
    );
  do_preprocess fileAST