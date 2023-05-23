open GobConfig
open GoblintCil

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


class countLoopsVisitor(count) = object
  inherit nopCilVisitor

  method! vstmt stmt = match stmt.skind with
    | Loop _ -> count := !count + 1; DoChildren
    | _ -> DoChildren

end 

let loopCount file = 
  let count = ref 0 in
  let visitor = new countLoopsVisitor(count) in
  ignore (visitCilFileSameGlobals visitor file);
  !count


let createCFG (fileAST: file) =
  
  (* The analyzer keeps values only for blocks. So if you want a value for every program point, each instruction      *)
  (* needs to be in its own block. end_basic_blocks does that.                                                        *)
  (* After adding support for VLAs, there are new VarDecl instructions at the point where a variable was declared and *)
  (* its declaration is no longer printed at the beginning of the function. Putting these VarDecl into their own      *)
  (* BB causes the output CIL file to no longer compile.                                                              *)
  (* Since we want the output of justcil to compile, we do not run allBB visitor if justcil is enable, regardless of  *)
  (* exp.basic-blocks. This does not matter, as we will not run any analysis anyway, when justcil is enabled.         *)
  
  Cilfacade.do_preprocess_cil fileAST;
  if not (get_bool "exp.basic-blocks") && not (get_bool "justcil") then end_basic_blocks fileAST;

  (* We used to renumber vids but CIL already generates them fresh, so no need.
   * Renumbering is problematic for using [Cabs2cil.environment], e.g. in witness invariant generation to use original variable names.
   * See https://github.com/goblint/cil/issues/31#issuecomment-824939793. *)

  let loops = loopCount fileAST in
  
  iterGlobals fileAST (fun glob ->
      match glob with
      | GFun(fd,_) ->
        (* before prepareCfg so continues still appear as such *)
        if (get_int "exp.unrolling-factor")>0 || AutoTune0.isActivated "loopUnrollHeuristic" then LoopUnrolling.unroll_loops fd loops;
        prepareCFG fd;
        computeCFGInfo fd true
      | _ -> ()
    );
  
  if get_bool "dbg.run_cil_check" then assert (Check.checkFile [] fileAST);  
  Cilfacade.do_preprocess fileAST
