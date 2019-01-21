
let toFunctionName (glob: Cil.global) =
  match glob with 
  | GFun (fundec,_location)-> Some fundec.Cil.svar.Cil.vname
  | _ -> None


module StringMap = Map.Make(String)


let eqB (a: Cil.block) (b: Cil.block) =
 a.Cil.battrs == b.Cil.battrs && a.bstmts = b.bstmts

let eqS (a: Cil.stmt) (b: Cil.stmt) =
  a.Cil.skind == b.Cil.skind

let eqF (a: Cil.fundec) (b: Cil.fundec) =
       a.svar == b.svar (*&& a.sformals == b.sformals &&
       a.slocals == b.slocals && eqB a.sbody b.sbody && 
       List.for_all (fun (a,b) -> eqS a b) (List.combine a.sallstmts b.sallstmts)*)

(* Do a (recrusive) equal comparision ignoring location information *)
let eq (a:Cil.global) (b: Cil.global) = match a, b with
  (GFun (x1,x2)), (GFun (y1,y2)) -> eqF x1 y1
  | _, _ -> true 
(* Beware: the sallstmts is only available when getCFG is called in control.ml *)
(*	After you call Cil.computeCFGInfo this field is set to contain all statements in the function*)



let compareCilFiles (oldAST: Cil.file) (newAST: Cil.file) =
  let oldMap = StringMap.empty in
  let addGlobal map global  = 
    match toFunctionName global with
      Some funName -> StringMap.add funName global map |
      None -> map 
  in
  let checkExists map global = 
    match toFunctionName global with
      Some funName -> let oldFunction =  StringMap.find funName map in
                      (* Do a (recrusive) equal comparision ignoring location information *)
                      Prelude.print_bool @@ eq oldFunction global
       |
      None -> ()
  in
  (* Store a map from functionNames in the old file to the function definition*)
  let oldMap = Cil.foldGlobals oldAST addGlobal oldMap in
  (*  For each function in the new file, check whether a function with the same name 
      already existed in the old version, and whether it is the same function.
   *)
  Cil.iterGlobals newAST (checkExists oldMap) 


