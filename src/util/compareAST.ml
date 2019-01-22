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

let print (a: Pretty.doc)  =
    print_endline @@ Pretty.sprint 100 a

let eq_enuminfo (a: enuminfo) (b: enuminfo) = true

let eq_args (a: string * typ * attributes) (b: string * typ * attributes) = true

let rec eq_constant (a: constant) (b: constant) = match a, b with
    CInt64 (val1, kind1, str1), CInt64 (val2, kind2, str2) -> val1 = val2 && kind1 = kind2 (* Ignore string representation, i.e. 0x2 == 2 *)
    | CEnum (exp1, str1, enuminfo1), CEnum (exp2, str2, enuminfo2) -> eq_exp exp1 exp2 (* Ignore name and enuminfo  *)
    | a, b -> a = b

and eq_exp (a: exp) (b: exp) = match a,b with
    | Const c1, Const c2 -> eq_constant c1 c2
    | Lval lv1, Lval lv2 -> eq_lval lv1 lv2
    | SizeOf typ1, SizeOf typ2 -> eq_typ typ1 typ2
    | SizeOfE exp1, SizeOfE exp2 -> eq_exp exp1 exp2
    | SizeOfStr str1, SizeOfStr str2 -> str1 = str2 (* possibly, having the same length would suffice *)
    | AlignOf typ1, AlignOf typ2 -> eq_typ typ1 typ2 
    | AlignOfE exp1, AlignOfE exp2 -> eq_exp exp1 exp2
    | UnOp (op1, exp1, typ1), UnOp (op2, exp2, typ2) -> op1 == op2 && eq_exp exp1 exp2 && eq_typ typ1 typ2
    | BinOp (op1, left1, right1, typ1), BinOp (op2, left2, right2, typ2) ->  op1 = op2 && eq_exp left1 left2 && eq_exp right1 right2 && eq_typ typ1 typ2
    | CastE (typ1, exp1), CastE (typ2, exp2) -> (* eq_typ typ1 typ2 && *) eq_exp exp1 exp2 
    | AddrOf lv1, AddrOf lv2 -> eq_lval lv1 lv2 
    | StartOf lv1, StartOf lv2 -> eq_lval lv1 lv2
    | _, _ -> false

and eq_lhost (a: lhost) (b: lhost) = match a, b with 
    Var v1, Var v2 -> true (* eq_varinfo v1 v2 *) 
    | Mem exp1, Mem exp2 -> eq_exp exp1 exp2 
    | _, _ -> false

and eq_typinfo (a: typeinfo) (b: typeinfo) = a.tname = b.tname && eq_typ a.ttype b.ttype (* Ignore the treferenced field *)

and eq_typ (a: typ) (b: typ) = match a, b with
    | TPtr (typ1, attr1), TPtr (typ2, attr2) -> eq_typ typ1 typ2 && attr1 = attr2
    | TArray (typ1, (Some lenExp1), attr1), TArray (typ2, (Some lenExp2), attr2) -> eq_typ typ1 typ2 && eq_exp lenExp1 lenExp2 && attr1 = attr2
    | TArray (typ1, None, attr1), TArray (typ2, None, attr2) -> eq_typ typ1 typ2 && attr1 = attr2
    | TFun (typ1, (Some list1), varArg1, attr1), TFun (typ2, (Some list2), varArg2, attr2) 
                ->  eq_typ typ1 typ2 && eq_list eq_args list1 list2 && varArg1 = varArg2 &&
                    attr1 = attr2
    | TFun (typ1, None, varArg1, attr1), TFun (typ2, None, varArg2, attr2) 
                ->  eq_typ typ1 typ2 && varArg1 = varArg2 &&
                    attr1 = attr2
    | TNamed (typinfo1, attr1), TNamed (typeinfo2, attr2) -> eq_typinfo typinfo1 typeinfo2 && attr1 = attr2
    | TComp (compinfo1, attr1), TComp (compinfo2, attr2) -> eq_compinfo compinfo1 compinfo2 && attr1 = attr2 
    | TEnum (enuminfo1, attr1), TEnum (enuminfo2, attr2) -> eq_enuminfo enuminfo1 enuminfo2 && attr1 = attr2
    | TBuiltin_va_list attr1, TBuiltin_va_list attr2 -> attr1 = attr2
    | _, _ -> a = b (* The remaining cases can be checked by the generic equality operator *)

and eq_attrparam (a: attrparam) (b: attrparam) = true

and eq_attribute (a: attribute) (b: attribute) = match a, b with
  Attr (name1, params1), Attr (name2, params2) -> name1 = name2 && eq_list eq_attrparam params1 params2

and eq_varinfo (a: varinfo) (b: varinfo) = a.vname = b.vname && eq_typ a.vtype b.vtype && eq_list eq_attribute a.vattr b.vattr &&
  a.vstorage = b.vstorage && a.vglob = b.vglob && a.vinline = b.vinline && a.vaddrof = b.vaddrof
  (* Ignore the location, vid, vreferenced, vdescr, vdescrpure *)

and eq_compinfo (a: compinfo) (b: compinfo) = a.cstruct = b.cstruct && a.cname = b.cname && eq_list eq_fieldinfo a.cfields b.cfields
  && eq_list eq_attribute a.cattr b.cattr && a.cdefined = b.cdefined (* Ignore ckey, and ignore creferenced *)

and eq_fieldinfo (a: fieldinfo) (b: fieldinfo) =
    a.fname = b.fname && eq_typ a.ftype b.ftype && a.fbitfield = b.fbitfield &&  eq_list (=) a.fattr b.fattr

and eq_offset (a: offset) (b: offset) = match a, b with
    NoOffset, NoOffset -> true 
    | Field (info1, offset1), Field (info2, offset2) -> eq_fieldinfo info1 info2 && eq_offset offset1 offset2 
    | Index (exp1, offset1), Index (exp2, offset2) -> eq_exp exp1 exp2 && eq_offset offset1 offset2
    | _, _ -> false 

and eq_lval (a: lval) (b: lval) = match a, b with
    (host1, off1), (host2, off2) ->  eq_lhost host1 host2 && eq_offset off1 off2  

let eq_instr (a: instr) (b: instr) = match a, b with
    | Set (lv1, exp1, _l1), Set (lv2, exp2, _l2) -> eq_lval lv1 lv2 && eq_exp exp1 exp2
    | Call (Some lv1, f1, args1, _l1), Call (Some lv2, f2, args2, _l2) -> eq_lval lv1 lv2 && eq_exp f1 f2 && eq_list eq_exp args1 args2
    | Call (None, f1, args1, _l1), Call (None, f2, args2, _l2) -> eq_exp f1 f2 && eq_list eq_exp args1 args2
    | Asm _, Asm _ -> false (* We don't handle inline assembler yet *)
    | _, _ -> false

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


