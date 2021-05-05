(** Helpful functions for dealing with [Cil]. *)

open GobConfig
open Json
open Cil
module E = Errormsg
module GU = Goblintutil



let init () =
  initCIL ();
  lowerConstants := GobConfig.get_bool "exp.lower-constants";
  Mergecil.ignore_merge_conflicts := true;
  (* lineDirectiveStyle := None; *)
  Rmtmps.keepUnused := true;
  print_CIL_Input := true

let current_statement = ref dummyStmt
let current_file = ref dummyFile
let showtemps = ref false

let parse fileName =
  Frontc.parse fileName ()

let print_to_file (fileName: string) (fileAST: file) =
  let oc = Stdlib.open_out fileName in
  dumpFile defaultCilPrinter oc fileName fileAST

let print (fileAST: file) =
  dumpFile defaultCilPrinter stdout "stdout" fileAST

let printDebug fileAST =
  dumpFile Printer.debugCilPrinter stdout "stdout" fileAST

let rmTemps fileAST =
  Rmtmps.removeUnusedTemps fileAST

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

let visitors = ref []
let register_preprocess name visitor_fun =
  visitors := !visitors @ [name, visitor_fun]

let do_preprocess ast =
  let f fd (name, visitor_fun) =
    (* this has to be done here, since the settings aren't available when register_preprocess is called *)
    if List.mem name (List.map Json.string @@ get_list "ana.activated") then
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
  (* Partial.calls_end_basic_blocks fileAST; *)
  Partial.globally_unique_vids fileAST;
  iterGlobals fileAST (fun glob ->
      match glob with
      | GFun(fd,_) ->
        prepareCFG fd;
        computeCFGInfo fd true
      | _ -> ()
    );
  do_preprocess fileAST

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

(* a visitor that puts calls to constructors at the starting points to main *)
class addConstructors cons = object
  inherit nopCilVisitor
  val mutable cons1 = cons
  method! vfunc fd =
    if List.mem fd.svar.vname (List.map string (get_list "mainfun")) then begin
      if get_bool "dbg.verbose" then ignore (Pretty.printf "Adding constructors to: %s\n" fd.svar.vname);
      let loc = try get_stmtLoc (List.hd fd.sbody.bstmts).skind with Failure _ -> locUnknown in
      let f fd = mkStmt (Instr [Call (None,Lval (Var fd.svar, NoOffset),[],loc)]) in
      let call_cons = List.map f cons1 in
      let body = mkBlock (call_cons @ fd.sbody.bstmts) in
      fd.sbody <- body;
      ChangeTo fd
    end else SkipChildren

  method! vstmt _ = SkipChildren
  method! vvdec _ = SkipChildren
  method! vexpr _ = SkipChildren
  method! vlval _ = SkipChildren
  method! vtype _ = SkipChildren
end

let getMergedAST fileASTs =
  let merged = Mergecil.merge fileASTs "stdout" in
  if !E.hadErrors then
    E.s (E.error "There were errors during merging\n");
  merged

(* call constructors at start of main functions *)
let callConstructors ast =
  let constructors =
    let cons = ref [] in
    iterGlobals ast (fun glob ->
        match glob with
        | GFun({svar={vattr=attr; _}; _} as def, _) when hasAttribute "constructor" attr ->
          cons := def::!cons
        | _ -> ()
      );
    !cons
  in
  let d_fundec () fd = Pretty.text fd.svar.vname in
  if get_bool "dbg.verbose" then ignore (Pretty.printf "Constructors: %a\n" (Pretty.d_list ", " d_fundec) constructors);
  visitCilFileSameGlobals (new addConstructors constructors) ast;
  ast

exception Found of fundec
let getFun fun_name =
  try
    iterGlobals !current_file (fun glob ->
        match glob with
        | GFun({svar={vname=vn; _}; _} as def,_) when vn = fun_name -> raise (Found def)
        | _ -> ()
      );
    failwith ("Function "^ fun_name ^ " not found!")
  with
  | Found def -> def

let in_section check attr_list =
  let f attr = match attr with
    | Attr ("section", [AStr str]) -> check str
    | _ -> false
  in List.exists f attr_list

let is_init = in_section (fun s -> s = ".init.text")
let is_initptr = in_section (fun s -> s = ".initcall6.init")
let is_exit = in_section (fun s -> s = ".exit.text")

let rec get_varinfo exp: varinfo =
  (* ignore (Pretty.printf "expression: %a\n" (printExp plainCilPrinter) exp); *)
  match exp with
  | AddrOf (Var v, _) -> v
  | CastE (_,e) -> get_varinfo e
  | _ -> failwith "Unimplemented: searching for variable in more complicated expression"

exception MyException of varinfo
let find_module_init funs fileAST =
  try iterGlobals fileAST (
      function
      | GVar ({vattr=attr; _}, {init=Some (SingleInit exp) }, _) when is_initptr attr ->
        raise (MyException (get_varinfo exp))
      | _ -> ()
    );
    (funs, [])
  with MyException var ->
    let f (s:fundec) = s.svar.vname = var.vname in
    List.partition f funs

type startfuns = fundec list * fundec list * fundec list

let getFuns fileAST : startfuns =
  let add_main f (m,e,o) = (f::m,e,o) in
  let add_exit f (m,e,o) = (m,f::e,o) in
  let add_other f (m,e,o) = (m,e,f::o) in
  let f acc glob =
    match glob with
    | GFun({svar={vname=mn; _}; _} as def,_) when List.mem mn (List.map string (get_list "mainfun")) -> add_main def acc
    | GFun({svar={vname=mn; _}; _} as def,_) when mn="StartupHook" && !OilUtil.startuphook -> add_main def acc
    | GFun({svar={vname=mn; _}; _} as def,_) when List.mem mn (List.map string (get_list "exitfun")) -> add_exit def acc
    | GFun({svar={vname=mn; _}; _} as def,_) when List.mem mn (List.map string (get_list "otherfun")) -> add_other def acc
    | GFun({svar={vname=mn; vattr=attr; _}; _} as def, _) when get_bool "kernel" && is_init attr ->
      Printf.printf "Start function: %s\n" mn; set_string "mainfun[+]" mn; add_main def acc
    | GFun({svar={vname=mn; vattr=attr; _}; _} as def, _) when get_bool "kernel" && is_exit attr ->
      Printf.printf "Cleanup function: %s\n" mn; set_string "exitfun[+]" mn; add_exit def acc
    | GFun ({svar={vstorage=NoStorage; _}; _} as def, _) when (get_bool "nonstatic") -> add_other def acc
    | GFun (def, _) when ((get_bool "allfuns")) ->  add_other def  acc
    | GFun (def, _) when get_string "ana.osek.oil" <> "" && OilUtil.is_starting def.svar.vname -> add_other def acc
    | _ -> acc
  in
  foldGlobals fileAST f ([],[],[])

let dec_table_ok = ref false
let dec_table = Hashtbl.create 111
let dec_make () : unit =
  dec_table_ok := true ;
  Hashtbl.clear dec_table;
  iterGlobals !current_file (fun glob ->
      match glob with
      | GFun({svar={vid=vid; _}; _} as def,_) -> Hashtbl.add dec_table vid def
      | _ -> ()
    )

let rec getdec fv =
  if !dec_table_ok then
    Hashtbl.find dec_table fv.vid
  else begin
    dec_make ();
    getdec fv
  end

let getFirstStmt fd = List.hd fd.sbody.bstmts

let pstmt stmt = dumpStmt defaultCilPrinter stdout 0 stmt; print_newline ()

let p_expr exp = Pretty.printf "%a\n" (printExp defaultCilPrinter) exp
let d_expr exp = Pretty.printf "%a\n" (printExp plainCilPrinter) exp

(* Returns the ikind of a TInt(_) and TEnum(_). Unrolls typedefs. Warns if a a different type is put in and return IInt *)
let rec get_ikind t =
  (* important to unroll the type here, otherwise problems with typedefs *)
  match Cil.unrollType t with
  | TInt (ik,_)
  | TEnum ({ekind = ik; _},_) -> ik
  | TPtr _ -> get_ikind !Cil.upointType
  | _ ->
    Messages.warn_each "Something that we expected to be an integer type has a different type, assuming it is an IInt";
    Cil.IInt

let ptrdiff_ikind () = get_ikind !ptrdiffType

let rec typeOf (e: exp) : typ =
  match e with
  | Const(CInt64 (_, ik, _)) -> TInt(ik, [])

  (* Character constants have type int.  ISO/IEC 9899:1999 (E),
   * section 6.4.4.4 [Character constants], paragraph 10, if you
   * don't believe me. *)
  | Const(CChr _) -> intType

  (* The type of a string is a pointer to characters ! The only case when
   * you would want it to be an array is as an argument to sizeof, but we
   * have SizeOfStr for that *)
  | Const(CStr s) -> charPtrType

  | Const(CWStr s) -> TPtr(!wcharType,[])

  | Const(CReal (_, fk, _)) -> TFloat(fk, [])

  | Const(CEnum(tag, _, ei)) -> typeOf tag

  | Lval(lv) -> typeOfLval lv
  | SizeOf _ | SizeOfE _ | SizeOfStr _ -> !typeOfSizeOf
  | AlignOf _ | AlignOfE _ -> !typeOfSizeOf
  | UnOp (_, _, t) -> t
  | BinOp (_, _, _, t) -> t
  | CastE (t, _) -> t
  | AddrOf (lv) -> TPtr(typeOfLval lv, [])
  | StartOf (lv) -> begin
      match unrollType (typeOfLval lv) with
        TArray (t,_, a) -> TPtr(t, a)
      | _ -> raise Not_found
    end
  | Question _ -> failwith "Logical operations should be compiled away by CIL."
  | _ -> failwith "Unmatched pattern."

and typeOfInit (i: init) : typ =
  match i with
    SingleInit e -> typeOf e
  | CompoundInit (t, _) -> t

and typeOfLval = function
    Var vi, off -> typeOffset vi.vtype off
  | Mem addr, off -> begin
      match unrollType (typeOf addr) with
        TPtr (t, _) -> typeOffset t off
      | _ -> raise Not_found
    end

and typeOffset basetyp =
  let blendAttributes baseAttrs =
    let (_, _, contageous) =
      partitionAttributes ~default:(AttrName false) baseAttrs in
    typeAddAttributes contageous
  in
  function
    NoOffset -> basetyp
  | Index (_, o) -> begin
      match unrollType basetyp with
        TArray (t, _, baseAttrs) ->
        let elementType = typeOffset t o in
        blendAttributes baseAttrs elementType
      | t -> raise Not_found
    end
  | Field (fi, o) ->
    match unrollType basetyp with
      TComp (_, baseAttrs) ->
      let fieldType = typeOffset fi.ftype o in
      blendAttributes baseAttrs fieldType
    | _ -> raise Not_found

(** HashSet of line numbers *)
let locs = Hashtbl.create 200

(** Visitor to count locs appearing inside a fundec. *)
class countFnVisitor = object
    inherit nopCilVisitor
    method! vstmt s =
      match s.skind with
      | Return (_, loc)
      | Goto (_, loc)
      | ComputedGoto (_, loc)
      | Break loc
      | Continue loc
      | If (_,_,_,loc)
      | Switch (_,_,_,loc)
      | Loop (_,loc,_,_)
      | TryFinally (_,_,loc)
      | TryExcept (_,_,_,loc)
        -> Hashtbl.replace locs loc.line (); DoChildren
      | _ ->
        DoChildren

    method! vinst = function
      | Set (_,_,loc)
      | Call (_,_,_,loc)
      | Asm (_,_,_,_,_,loc)
        -> Hashtbl.replace locs loc.line (); SkipChildren
      | _ -> SkipChildren

    method! vvdec _ = SkipChildren
    method! vexpr _ = SkipChildren
    method! vlval _ = SkipChildren
    method! vtype _ = SkipChildren
end

let fnvis = new countFnVisitor

(** Count the number of unique locations appearing in fundec [fn].
    Uses {!Cilfacade.locs} hashtable for intermediate computations
*)
let countLoc fn =
  let _ = visitCilFunction fnvis fn in
  let res = Hashtbl.length locs in
  Hashtbl.clear locs;
  res
