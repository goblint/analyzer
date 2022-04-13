open Cil

(* global_type and global_t are implicitly used by GlobalMap to keep GVarDecl apart from GVar and GFun, so do not remove! *)
type global_type = Fun | Decl | Var

and global_identifier = {name: string ; global_t: global_type} [@@deriving ord]

let identifier_of_global glob =
  match glob with
  | GFun (fundec, l) -> {name = fundec.svar.vname; global_t = Fun}
  | GVar (var, init, l) -> {name = var.vname; global_t = Var}
  | GVarDecl (var, l) -> {name = var.vname; global_t = Decl}
  | _ -> raise Not_found

module GlobalMap = Map.Make(struct
    type t = global_identifier [@@deriving ord]
  end)


(* hack: CIL generates new type names for anonymous types - we want to ignore these *)
let compare_name a b =
  let anon_struct = "__anonstruct_" in
  let anon_union = "__anonunion_" in
  if a = b then true else BatString.(starts_with a anon_struct && starts_with b anon_struct || starts_with a anon_union && starts_with b anon_union)

let rec eq_constant (a: constant) (b: constant) = match a, b with
  | CInt (val1, kind1, str1), CInt (val2, kind2, str2) -> Cilint.compare_cilint val1 val2 = 0 && kind1 = kind2 (* Ignore string representation, i.e. 0x2 == 2 *)
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
  | CastE (typ1, exp1), CastE (typ2, exp2) -> eq_typ typ1 typ2 &&  eq_exp exp1 exp2
  | AddrOf lv1, AddrOf lv2 -> eq_lval lv1 lv2
  | StartOf lv1, StartOf lv2 -> eq_lval lv1 lv2
  | _, _ -> false

and eq_lhost (a: lhost) (b: lhost) = match a, b with
    Var v1, Var v2 -> eq_varinfo v1 v2
  | Mem exp1, Mem exp2 -> eq_exp exp1 exp2
  | _, _ -> false

and global_typ_acc: (typ * typ) list ref = ref [] (* TODO: optimize with physical Hashtbl? *)

and mem_typ_acc (a: typ) (b: typ) acc = List.exists (fun p -> match p with (x, y) -> a == x && b == y) acc (* TODO: seems slightly more efficient to not use "fun (x, y) ->" directly to avoid caml_tuplify2 *)

and pretty_length () l = Pretty.num (List.length l)

and eq_typ_acc (a: typ) (b: typ) (acc: (typ * typ) list) =
  if Messages.tracing then Messages.tracei "compareast" "eq_typ_acc %a vs %a (%a, %a)\n" d_type a d_type b pretty_length acc pretty_length !global_typ_acc; (* %a makes List.length calls lazy if compareast isn't being traced *)
  let r = match a, b with
    | TPtr (typ1, attr1), TPtr (typ2, attr2) -> eq_typ_acc typ1 typ2 acc && GobList.equal eq_attribute attr1 attr2
    | TArray (typ1, (Some lenExp1), attr1), TArray (typ2, (Some lenExp2), attr2) -> eq_typ_acc typ1 typ2 acc && eq_exp lenExp1 lenExp2 &&  GobList.equal eq_attribute attr1 attr2
    | TArray (typ1, None, attr1), TArray (typ2, None, attr2) -> eq_typ_acc typ1 typ2 acc && GobList.equal eq_attribute attr1 attr2
    | TFun (typ1, (Some list1), varArg1, attr1), TFun (typ2, (Some list2), varArg2, attr2)
      ->  eq_typ_acc typ1 typ2 acc && GobList.equal (eq_args acc) list1 list2 && varArg1 = varArg2 &&
          GobList.equal eq_attribute attr1 attr2
    | TFun (typ1, None, varArg1, attr1), TFun (typ2, None, varArg2, attr2)
      ->  eq_typ_acc typ1 typ2 acc && varArg1 = varArg2 &&
          GobList.equal eq_attribute attr1 attr2
    | TNamed (typinfo1, attr1), TNamed (typeinfo2, attr2) -> eq_typ_acc typinfo1.ttype typeinfo2.ttype acc && GobList.equal eq_attribute attr1 attr2 (* Ignore tname, treferenced *)
    | TNamed (tinf, attr), b -> eq_typ_acc tinf.ttype b acc (* Ignore tname, treferenced. TODO: dismiss attributes, or not? *)
    | a, TNamed (tinf, attr) -> eq_typ_acc a tinf.ttype acc (* Ignore tname, treferenced . TODO: dismiss attributes, or not? *)
    (* The following two lines are a hack to ensure that anonymous types get the same name and thus, the same typsig *)
    | TComp (compinfo1, attr1), TComp (compinfo2, attr2) ->
      if mem_typ_acc a b acc || mem_typ_acc a b !global_typ_acc then (
        if Messages.tracing then Messages.trace "compareast" "in acc\n";
        true
      )
      else (
        let acc = (a, b) :: acc in
        let res = eq_compinfo compinfo1 compinfo2 acc && GobList.equal eq_attribute attr1 attr2 in
        if res && compinfo1.cname <> compinfo2.cname then
          compinfo2.cname <- compinfo1.cname;
        if res then
          global_typ_acc := (a, b) :: !global_typ_acc;
        res
      )
    | TEnum (enuminfo1, attr1), TEnum (enuminfo2, attr2) -> let res = eq_enuminfo enuminfo1 enuminfo2 && GobList.equal eq_attribute attr1 attr2 in (if res && enuminfo1.ename <> enuminfo2.ename then enuminfo2.ename <- enuminfo1.ename); res
    | TBuiltin_va_list attr1, TBuiltin_va_list attr2 -> GobList.equal eq_attribute attr1 attr2
    | TVoid attr1, TVoid attr2 -> GobList.equal eq_attribute attr1 attr2
    | TInt (ik1, attr1), TInt (ik2, attr2) -> ik1 = ik2 && GobList.equal eq_attribute attr1 attr2
    | TFloat (fk1, attr1), TFloat (fk2, attr2) -> fk1 = fk2 && GobList.equal eq_attribute attr1 attr2
    | _, _ -> false
  in
  if Messages.tracing then Messages.traceu "compareast" "eq_typ_acc %a vs %a\n" d_type a d_type b;
  r

and eq_typ (a: typ) (b: typ) = eq_typ_acc a b []

and eq_eitems (a: string * exp * location) (b: string * exp * location) = match a, b with
    (name1, exp1, _l1), (name2, exp2, _l2) -> name1 = name2 && eq_exp exp1 exp2
(* Ignore location *)

and eq_enuminfo (a: enuminfo) (b: enuminfo) =
  compare_name a.ename b.ename &&
  GobList.equal eq_attribute a.eattr b.eattr &&
  GobList.equal eq_eitems a.eitems b.eitems
(* Ignore ereferenced *)

and eq_args (acc: (typ * typ) list) (a: string * typ * attributes) (b: string * typ * attributes) = match a, b with
    (name1, typ1, attr1), (name2, typ2, attr2) -> name1 = name2 && eq_typ_acc typ1 typ2 acc && GobList.equal eq_attribute attr1 attr2

and eq_attrparam (a: attrparam) (b: attrparam) = match a, b with
  | ACons (str1, attrparams1), ACons (str2, attrparams2) -> str1 = str2 && GobList.equal eq_attrparam attrparams1 attrparams2
  | ASizeOf typ1, ASizeOf typ2 -> eq_typ typ1 typ2
  | ASizeOfE attrparam1, ASizeOfE attrparam2 -> eq_attrparam attrparam1 attrparam2
  | ASizeOfS typsig1, ASizeOfS typsig2 -> typsig1 = typsig2
  | AAlignOf typ1, AAlignOf typ2 -> eq_typ typ1 typ2
  | AAlignOfE attrparam1, AAlignOfE attrparam2 -> eq_attrparam attrparam1 attrparam2
  | AAlignOfS typsig1, AAlignOfS typsig2 -> typsig1 = typsig2
  | AUnOp (op1, attrparam1), AUnOp (op2, attrparam2) -> op1 = op2 && eq_attrparam attrparam1 attrparam2
  | ABinOp (op1, left1, right1), ABinOp (op2, left2, right2) -> op1 = op2 && eq_attrparam left1 left2 && eq_attrparam right1 right2
  | ADot (attrparam1, str1), ADot (attrparam2, str2) -> eq_attrparam attrparam1 attrparam2 && str1 = str2
  | AStar attrparam1, AStar attrparam2 -> eq_attrparam attrparam1 attrparam2
  | AAddrOf attrparam1, AAddrOf attrparam2 -> eq_attrparam attrparam1 attrparam2
  | AIndex (left1, right1), AIndex (left2, right2) -> eq_attrparam left1 left2 && eq_attrparam right1 right2
  | AQuestion (left1, middle1, right1), AQuestion (left2, middle2, right2) -> eq_attrparam left1 left2 && eq_attrparam middle1 middle2 && eq_attrparam right1 right2
  | a, b -> a = b

and eq_attribute (a: attribute) (b: attribute) = match a, b with
    Attr (name1, params1), Attr (name2, params2) -> name1 = name2 && GobList.equal eq_attrparam params1 params2

and eq_varinfo (a: varinfo) (b: varinfo) = a.vname = b.vname && eq_typ a.vtype b.vtype && GobList.equal eq_attribute a.vattr b.vattr &&
                                           a.vstorage = b.vstorage && a.vglob = b.vglob && a.vaddrof = b.vaddrof
(* Ignore the location, vid, vreferenced, vdescr, vdescrpure, vinline *)

(* Accumulator is needed because of recursive types: we have to assume that two types we already encountered in a previous step of the recursion are equivalent *)
and eq_compinfo (a: compinfo) (b: compinfo) (acc: (typ * typ) list) =
  a.cstruct = b.cstruct &&
  compare_name a.cname b.cname &&
  GobList.equal (fun a b-> eq_fieldinfo a b acc) a.cfields b.cfields &&
  GobList.equal eq_attribute a.cattr b.cattr &&
  a.cdefined = b.cdefined (* Ignore ckey, and ignore creferenced *)

and eq_fieldinfo (a: fieldinfo) (b: fieldinfo) (acc: (typ * typ) list)=
  if Messages.tracing then Messages.tracei "compareast" "fieldinfo %s vs %s\n" a.fname b.fname;
  let r = a.fname = b.fname && eq_typ_acc a.ftype b.ftype acc && a.fbitfield = b.fbitfield &&  GobList.equal eq_attribute a.fattr b.fattr in
  if Messages.tracing then Messages.traceu "compareast" "fieldinfo %s vs %s\n" a.fname b.fname;
  r

and eq_offset (a: offset) (b: offset) = match a, b with
    NoOffset, NoOffset -> true
  | Field (info1, offset1), Field (info2, offset2) -> eq_fieldinfo info1 info2 [] && eq_offset offset1 offset2
  | Index (exp1, offset1), Index (exp2, offset2) -> eq_exp exp1 exp2 && eq_offset offset1 offset2
  | _, _ -> false

and eq_lval (a: lval) (b: lval) = match a, b with
    (host1, off1), (host2, off2) -> eq_lhost host1 host2 && eq_offset off1 off2

let eq_instr (a: instr) (b: instr) = match a, b with
  | Set (lv1, exp1, _l1, _el1), Set (lv2, exp2, _l2, _el2) -> eq_lval lv1 lv2 && eq_exp exp1 exp2
  | Call (Some lv1, f1, args1, _l1, _el1), Call (Some lv2, f2, args2, _l2, _el2) -> eq_lval lv1 lv2 && eq_exp f1 f2 && GobList.equal eq_exp args1 args2
  | Call (None, f1, args1, _l1, _el1), Call (None, f2, args2, _l2, _el2) -> eq_exp f1 f2 && GobList.equal eq_exp args1 args2
  | Asm (attr1, tmp1, ci1, dj1, rk1, l1), Asm (attr2, tmp2, ci2, dj2, rk2, l2) -> GobList.equal String.equal tmp1 tmp2 && GobList.equal(fun (x1,y1,z1) (x2,y2,z2)-> x1 = x2 && y1 = y2 && eq_lval z1 z2) ci1 ci2 && GobList.equal(fun (x1,y1,z1) (x2,y2,z2)-> x1 = x2 && y1 = y2 && eq_exp z1 z2) dj1 dj2 && GobList.equal String.equal rk1 rk2(* ignore attributes and locations *)
  | VarDecl (v1, _l1), VarDecl (v2, _l2) -> eq_varinfo v1 v2
  | _, _ -> false

let eq_label (a: label) (b: label) = match a, b with
    Label (lb1, _l1, s1), Label (lb2, _l2, s2) -> lb1 = lb2 && s1 = s2
  |   Case (exp1, _l1, _el1), Case (exp2, _l2, el_2) -> exp1 = exp2
  | Default (_l1, _el1), Default (_l2, _el2) -> true
  | _, _ -> false

(* This is needed for checking whether a goto does go to the same semantic location/statement*)
let eq_stmt_with_location ((a, af): stmt * fundec) ((b, bf): stmt * fundec) =
  let offsetA = a.sid - (List.hd af.sallstmts).sid in
  let offsetB = b.sid - (List.hd bf.sallstmts).sid in
  GobList.equal eq_label a.labels b.labels && offsetA = offsetB

(* cfg_comp: blocks need only be compared in the AST comparison. For cfg comparison of functions one instead walks
   through the cfg and only compares the currently visited node (The cil blocks inside an if statement should not be
   compared together with its condition to avoid a to early and not precise detection of a changed node inside).
   Switch, break and continue statements are removed during cfg preparation and therefore need not to be handeled *)
let rec eq_stmtkind ?(cfg_comp = false) ((a, af): stmtkind * fundec) ((b, bf): stmtkind * fundec) =
  let eq_block' = fun x y -> if cfg_comp then true else eq_block (x, af) (y, bf) in
  match a, b with
  | Instr is1, Instr is2 -> GobList.equal eq_instr is1 is2
  | Return (Some exp1, _l1), Return (Some exp2, _l2) -> eq_exp exp1 exp2
  | Return (None, _l1), Return (None, _l2) -> true
  | Return _, Return _ -> false
  | Goto (st1, _l1), Goto (st2, _l2) -> eq_stmt_with_location (!st1, af) (!st2, bf)
  | Break _, Break _ -> if cfg_comp then failwith "CompareCFG: Invalid stmtkind in CFG" else true
  | Continue _, Continue _ -> if cfg_comp then failwith "CompareCFG: Invalid stmtkind in CFG" else true
  | If (exp1, then1, else1, _l1, _el1), If (exp2, then2, else2, _l2, _el2) -> eq_exp exp1 exp2 && eq_block' then1 then2 && eq_block' else1 else2
  | Switch (exp1, block1, stmts1, _l1, _el1), Switch (exp2, block2, stmts2, _l2, _el2) -> if cfg_comp then failwith "CompareCFG: Invalid stmtkind in CFG" else eq_exp exp1 exp2 && eq_block' block1 block2 && GobList.equal (fun a b -> eq_stmt (a,af) (b,bf)) stmts1 stmts2
  | Loop (block1, _l1, _el1, _con1, _br1), Loop (block2, _l2, _el2, _con2, _br2) -> eq_block' block1 block2
  | Block block1, Block block2 -> eq_block' block1 block2
  | _, _ -> false

and eq_stmt ?(cfg_comp = false) ((a, af): stmt * fundec) ((b, bf): stmt * fundec) =
  GobList.equal eq_label a.labels b.labels &&
  eq_stmtkind ~cfg_comp (a.skind, af) (b.skind, bf)

and eq_block ((a, af): Cil.block * fundec) ((b, bf): Cil.block * fundec) =
  a.battrs = b.battrs && GobList.equal (fun x y -> eq_stmt (x, af) (y, bf)) a.bstmts b.bstmts

let rec eq_init (a: init) (b: init) = match a, b with
  | SingleInit e1, SingleInit e2 -> eq_exp e1 e2
  | CompoundInit (t1, l1), CompoundInit (t2, l2) -> eq_typ t1 t2 && GobList.equal (fun (o1, i1) (o2, i2) -> eq_offset o1 o2 && eq_init i1 i2) l1 l2
  | _, _ -> false

let eq_initinfo (a: initinfo) (b: initinfo) = match a.init, b.init with
  | (Some init_a), (Some init_b) -> eq_init init_a init_b
  | None, None -> true
  | _, _ -> false
