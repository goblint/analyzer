(** Creation of CIL CFGs. *)

open GobConfig
open GoblintCil

include CilCfg0


class countLoopsVisitor(count) = object
  inherit nopCilVisitor

  method! vstmt stmt = match stmt.skind with
    | Loop _ -> incr count; DoChildren
    | _ -> DoChildren

end

let loopCount file =
  let count = ref 0 in
  let visitor = new countLoopsVisitor(count) in
  ignore (visitCilFileSameGlobals visitor file);
  !count


let createCFG (fileAST: file) =
  Cilfacade.do_preprocess fileAST;
  (* The analyzer keeps values only for blocks. So if you want a value for every program point, each instruction      *)
  (* needs to be in its own block. end_basic_blocks does that.                                                        *)
  (* After adding support for VLAs, there are new VarDecl instructions at the point where a variable was declared and *)
  (* its declaration is no longer printed at the beginning of the function. Putting these VarDecl into their own      *)
  (* BB causes the output CIL file to no longer compile.                                                              *)
  (* Since we want the output of justcil to compile, we do not run allBB visitor if justcil is enable, regardless of  *)
  (* exp.basic-blocks. This does not matter, as we will not run any analysis anyway, when justcil is enabled.         *)
  (* the preprocessing must be done here, to add the changes of CIL to the CFG*)
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