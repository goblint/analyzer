open Cil
module E = Errormsg
module GU = Goblintutil

let init () =
  initCIL ();
  lineDirectiveStyle := None;
  Rmtmps.keepUnused := true;
  print_CIL_Input := true

let ugglyImperativeHack = ref dummyFile
let showtemps = ref false
    
let parse fileName = 
  Frontc.parse fileName ()

let print (fileAST: file) = 
  dumpFile defaultCilPrinter stdout "stdout" fileAST
    
let printDebug fileAST = 
  dumpFile Printer.debugCilPrinter stdout "stdout" fileAST

let rmTemps fileAST = 
  Rmtmps.removeUnusedTemps fileAST

class allBBVisitor = object
  inherit nopCilVisitor 
  method vstmt s =
    match s.skind with
      | Instr(il) ->
          let list_of_stmts = 
            List.map (fun one_inst -> mkStmtOneInstr one_inst) il in
          let block = mkBlock list_of_stmts in
            ChangeDoChildrenPost(s, (fun _ -> s.skind <- Block(block); s))
      | _ -> DoChildren

  method vvdec _ = SkipChildren
  method vexpr _ = SkipChildren
  method vlval _ = SkipChildren
  method vtype _ = SkipChildren
end 

let end_basic_blocks f =
  let thisVisitor = new allBBVisitor in
  visitCilFileSameGlobals thisVisitor f  

let createCFG (fileAST: file) =
  end_basic_blocks fileAST; 
  (* Partial.calls_end_basic_blocks fileAST; *)
  Partial.globally_unique_vids fileAST; 
  iterGlobals fileAST (fun glob -> 
    match glob with
      | GFun(fd,_) -> 
          prepareCFG fd; 
          computeCFGInfo fd true
      | _ -> ()
  )

let partial fileAST =
  Partial.partial fileAST "main" []

let simplify fileAST =
  iterGlobals fileAST Simplify.doGlobal

let oneret fileAST =
  iterGlobals fileAST (fun glob -> 
    match glob with
      | GFun(fd,_) -> Oneret.oneret fd; 
      | _ -> ()
  )

let getAST fileName = 
  let fileAST = parse fileName in
    (*  rmTemps fileAST; *)
    (*  oneret fileAST;*)
    (*  simplify fileAST;*)
    fileAST

let getMergedAST fileASTs = 
  let merged = Mergecil.merge fileASTs "stdout" in
    if !E.hadErrors then
      E.s (E.error "There were errors during merging\n");
    merged


exception Found of fundec
let getMain fileAST = 
  let main = !GU.mainfun in
  try 
    iterGlobals fileAST (fun glob ->
      match glob with 
        | GFun({svar={vname=vn}} as def,_) when vn = main -> raise (Found def)
        | _ -> ()
    );
    failwith ("No "^ main ^ " method!")
  with
    | Found def -> GU.has_main := true; def

let getFuns fileAST  : fundec list =
  let mainname = !GU.mainfun in
  let main = ref dummyFunDec in
  let found = ref false in
  let f rest glob =
    match glob with 
      | GFun({svar={vname=mn}} as def,_) when mn = mainname-> 
          found := true; GU.has_main := true; main := def; rest
      | GFun({svar={vname=mn}} as def,_) when List.mem mn !GU.exitfun -> 
          def :: rest
      | GFun ({svar={vstorage=NoStorage}} as def, _) -> def :: rest
      | GFun (def, _) when not (!GU.nonstatic) -> def :: rest
      | GFun (def, _) when (!GU.oil && (String.length def.svar.vname >= 12 && String.sub def.svar.vname 0 12 = "function_of_")) -> def :: rest
      | _ -> rest 
  in
  let others = foldGlobals fileAST f [] in
    if !found then !main :: others else others

let getdec fv = 
  try 
    iterGlobals !ugglyImperativeHack (fun glob ->
      match glob with 
        | GFun({svar={vid=vid}} as def,_) when vid = fv.vid -> raise (Found def)
        | _ -> ()
    );
    raise Not_found
  with
    | Found def -> def

let getFirstStmt fd = List.hd fd.sbody.bstmts

let pstmt stmt = dumpStmt defaultCilPrinter stdout 0 stmt; print_newline ()

let p_expr exp = Pretty.printf "%a\n" (printExp defaultCilPrinter) exp
let d_expr exp = Pretty.printf "%a\n" (printExp plainCilPrinter) exp
