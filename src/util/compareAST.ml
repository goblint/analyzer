open Cil

let toFunctionName (glob: Cil.global) =
  match glob with 
  | GFun (fundec,_location)-> Some fundec.Cil.svar.Cil.vname
  | _ -> None


module StringMap = Map.Make(String)

let eq_list eq xs ys = 
    try
        List.for_all (fun (a,b) -> eq a b) (List.combine xs ys)
    with Invalid_argument _ -> false

let eqB (a: Cil.block) (b: Cil.block) =
    a.Cil.battrs = b.Cil.battrs && a.bstmts = b.bstmts

let eqS (a: Cil.stmt) (b: Cil.stmt) =
  a.Cil.skind = b.Cil.skind

let eq_lval (a: lval) (b: lval) = false

let eq_exp (a: exp) (b: exp) = false

let eq_instr (a: instr) (b: instr) = match a, b with
    | Set (lv1, exp1, _l1), Set (lv2, exp2, _l2) -> eq_lval lv1 lv2 && eq_exp exp1 exp2
    | Call (Some lv1, f1, args1, _l1), Call (Some lv2, f2, args2, _l2) -> eq_lval lv1 lv2 && eq_exp f1 f2 && eq_list eq_exp args1 args2
    | Call (None, f1, args1, _l1), Call (None, f2, args2, _l2) -> eq_exp f1 f2 && eq_list eq_exp args1 args2
    | Asm _, Asm _ -> false (* We don't handle inline assembler yet *)
    | _, _ -> false
let eq_varinfo (a: varinfo) (b: varinfo) = 
    a.vname = b.vname && a.vtype = b.vtype && a.vattr = b.vattr &&
    a.vstorage = b.vstorage && a.vglob = b.vglob && a.vinline = b.vinline


let eq_label (a: label) (b: label) = match a, b with
    Label (lb1, _l1, s1), Label (lb2, _l2, s2) -> lb1 = lb2 && s1 = s2 
|   Case (exp1, _l1), Case (exp2, _l2) -> exp1 = exp2 
| Default _l1, Default l2 -> true
| _, _ -> false

(* This is needed for checking whether a goto does go to the same semantic location/statement*)
let eq_stmt_with_location (a: stmt) (b: stmt) = eq_list eq_label a.labels b.labels && a.sid = b.sid

let rec eq_stmtkind (a: stmtkind) (b: stmtkind) = match a, b with
    | Instr is1, Instr is2 -> eq_list eq_instr is1 is2
    | Return (Some exp1, _l1), Return (Some exp2, _l2) -> eq_exp exp1 exp2
    | Return (None, _l1), Return (None, _l2) -> true
    | Return _, Return _ -> false
    | Goto (st1, _l1), Goto (st2, _l2) -> eq_stmt_with_location !st1 !st2
    | Break _, Break _ -> true
    | Continue _, Continue _ -> true
    | If (exp1, then1, else1, _l1), If (exp2, then2, else2, _l2) -> eq_exp exp1 exp2 && eq_block then1 then2 && eq_block else1 else2
    | Switch (exp1, block1, stmts1, _l1), Switch (exp2, block2, stmts2, _l2) -> eq_exp exp1 exp2 && eq_block block1 block2 && eq_list eq_stmt stmts1 stmts2
    | Loop (block1, _l1, _con1, _br1), Loop (block2, _l2, _con2, _br2) -> eq_block block1 block2 
    | Block block1, Block block2 -> eq_block block1 block2
    | TryFinally (tryBlock1, finallyBlock1, _l1), TryFinally (tryBlock2, finallyBlock2, _l2) -> eq_block tryBlock1 tryBlock2 && eq_block finallyBlock1 finallyBlock2
    | TryExcept (tryBlock1, exn1, exceptBlock1, _l1), TryExcept (tryBlock2, exn2, exceptBlock2, _l2) -> eq_block tryBlock1 tryBlock2 && eq_block exceptBlock1 exceptBlock2
    | _, _ -> false

and eq_stmt (a: Cil.stmt) (b: Cil.stmt) = 
    List.for_all (fun (x,y) -> eq_label x y) (List.combine a.labels b.labels) &&
    eq_stmtkind a.skind b.skind

and eq_block (a: Cil.block) (b: Cil.block) =
    a.battrs = b.battrs && List.for_all (fun (x,y) -> eq_stmt x y) (List.combine a.bstmts b.bstmts)

let eqF (a: Cil.fundec) (b: Cil.fundec) =
    try
       eq_varinfo a.svar b.svar && 
       List.for_all (fun (x, y) -> eq_varinfo x y) (List.combine a.sformals b.sformals) && 
       List.for_all (fun (x, y) -> eq_varinfo x y) (List.combine a.slocals b.slocals) &&
       eq_block a.sbody b.sbody
    with Invalid_argument _ -> (* One of the combines failed because the lists have differend length *)
                            false
       (*&& a.sformals == b.sformals &&
       a.slocals == b.slocals && eqB a.sbody b.sbody && 
       List.for_all (fun (a,b) -> eqS a b) (List.combine a.sallstmts b.sallstmts)*)

(* Do a (recrusive) equal comparision ignoring location information *)
let eq (a:Cil.global) (b: Cil.global) = match a, b with
  (GFun (x1,x2)), (GFun (y1,y2)) -> eqF x1 y1
  | _, _ -> assert false (* We should only compare two GFuns with this function, otherwise fail *) 
(* Beware: the sallstmts is only available when getCFG is called in control.ml *)
(*	After you call Cil.computeCFGInfo this field is set to contain all statements in the function*)


type status = Unchanged | NotFound | Changed

let compareCilFiles (oldAST: Cil.file) (newAST: Cil.file) =
  let oldMap = StringMap.empty in
  let addGlobal map global  = 
    match toFunctionName global with
      Some funName -> StringMap.add funName global map |
      None -> map 
  in
  let checkUnchanged map global = 
    match toFunctionName global with
      Some funName -> (try
                        let oldFunction =  StringMap.find funName map in
                        (* Do a (recursive) equal comparision ignoring location information *)
                        let identical = eq oldFunction global in
                        Prelude.print_string @@ funName ^ " ";
                        Prelude.print_bool identical;
                        Prelude.print_newline (); 
                        Some identical
                        with Not_found -> Prelude.print_string (funName ^ "Not found\n"); Some false)
      | None -> None
  in
  (* Store a map from functionNames in the old file to the function definition*)
  let oldMap = Cil.foldGlobals oldAST addGlobal oldMap in
  (*  For each function in the new file, check whether a function with the same name 
      already existed in the old version, and whether it is the same function.
   *)
  Cil.iterGlobals newAST (fun a-> ignore @@ checkUnchanged oldMap a)


