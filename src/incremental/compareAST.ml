open GoblintCil
open CilMaps

(* global_type and global_t are implicitly used by GlobalMap to keep GVarDecl apart from GVar and GFun, so do not remove! *)
type global_type = Fun | Decl | Var

and global_identifier = {name: string ; global_t: global_type} [@@deriving ord]

module StringMap = Map.Make(String)

type method_rename_assumption = {original_method_name: string; new_method_name: string; parameter_renames: string StringMap.t}
type method_rename_assumptions = method_rename_assumption VarinfoMap.t

(*rename_mapping is carried through the stack when comparing the AST. Holds a list of rename assumptions.*)
type rename_mapping = (string StringMap.t) * (method_rename_assumptions)

(*Compares two names, being aware of the rename_mapping. Returns true iff:
  1. there is a rename for name1 -> name2 = rename(name1)
  2. there is no rename for name1 -> name1 = name2*)
let rename_mapping_aware_name_comparison (name1: string) (name2: string) (rename_mapping: rename_mapping) =
  let (local_c, method_c) = rename_mapping in
  let existingAssumption: string option = StringMap.find_opt name1 local_c in

  match existingAssumption with
  | Some now ->
    (*Printf.printf "Assumption is: %s -> %s\n" original now;*)
    now = name2
  | None ->
    (*Printf.printf "No assumption when %s, %s, %b\n" name1 name2 (name1 = name2);*)
    name1 = name2 (*Var names differ, but there is no assumption, so this can't be good*)

let rename_mapping_to_string (rename_mapping: rename_mapping) =
  let (local, methods) = rename_mapping in
  let local_string = [%show: (string * string) list] (List.of_seq (StringMap.to_seq local)) in
  let methods_string: string = List.of_seq (VarinfoMap.to_seq methods |> Seq.map snd) |>
                               List.map (fun x -> match x with {original_method_name; new_method_name; parameter_renames} ->
                                   "(methodName: " ^ original_method_name ^ " -> " ^ new_method_name ^
                                   "; renamed_params=" ^ [%show: (string * string) list] (List.of_seq (StringMap.to_seq parameter_renames)) ^ ")") |>
                               String.concat ", " in
  "(local=" ^ local_string ^ "; methods=[" ^ methods_string ^ "])"

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
let compare_name (a: string) (b: string) =
  let anon_struct = "__anonstruct_" in
  let anon_union = "__anonunion_" in
  if a = b then true else BatString.(starts_with a anon_struct && starts_with b anon_struct || starts_with a anon_union && starts_with b anon_union)

let rec eq_constant ~(rename_mapping: rename_mapping) ~(acc: (typ * typ) list) (a: constant) (b: constant) =
  match a, b with
  | CInt (val1, kind1, str1), CInt (val2, kind2, str2) -> Cilint.compare_cilint val1 val2 = 0 && kind1 = kind2 (* Ignore string representation, i.e. 0x2 == 2 *)
  | CEnum (exp1, str1, enuminfo1), CEnum (exp2, str2, enuminfo2) -> eq_exp exp1 exp2 ~rename_mapping ~acc (* Ignore name and enuminfo  *)
  | a, b -> a = b

and eq_exp ?(no_const_vals = false) (a: exp) (b: exp) ~(rename_mapping: rename_mapping) ~(acc: (typ * typ) list) =
  match a, b with
  | Const c1, Const c2 -> eq_constant c1 c2 ~rename_mapping ~acc
  | Lval lv1, Lval lv2 -> eq_lval lv1 lv2 ~rename_mapping ~acc
  | SizeOf typ1, SizeOf typ2 -> eq_typ_acc typ1 typ2 ~rename_mapping ~acc
  | SizeOfE exp1, SizeOfE exp2 -> eq_exp exp1 exp2 ~rename_mapping ~acc
  | SizeOfStr str1, SizeOfStr str2 -> str1 = str2 (* possibly, having the same length would suffice *)
  | AlignOf typ1, AlignOf typ2 -> eq_typ_acc typ1 typ2 ~rename_mapping ~acc
  | AlignOfE exp1, AlignOfE exp2 -> eq_exp exp1 exp2 ~rename_mapping ~acc
  | UnOp (op1, exp1, typ1), UnOp (op2, exp2, typ2) -> op1 == op2 && eq_exp exp1 exp2 ~rename_mapping ~acc && eq_typ_acc typ1 typ2 ~rename_mapping ~acc
  | BinOp (op1, left1, right1, typ1), BinOp (op2, left2, right2, typ2) ->  op1 = op2 && eq_exp left1 left2 ~rename_mapping ~acc && eq_exp right1 right2 ~rename_mapping ~acc && eq_typ_acc typ1 typ2 ~rename_mapping ~acc
  | CastE (typ1, exp1), CastE (typ2, exp2) -> eq_typ_acc typ1 typ2 ~rename_mapping ~acc  && eq_exp exp1 exp2 ~rename_mapping ~acc
  | AddrOf lv1, AddrOf lv2 -> eq_lval lv1 lv2 ~rename_mapping ~acc
  | StartOf lv1, StartOf lv2 -> eq_lval lv1 lv2 ~rename_mapping ~acc
  | Real exp1, Real exp2 -> eq_exp exp1 exp2 ~rename_mapping ~acc
  | Imag exp1, Imag exp2 -> eq_exp exp1 exp2 ~rename_mapping ~acc
  | Question (b1, t1, f1, typ1), Question (b2, t2, f2, typ2) -> eq_exp b1 b2 ~rename_mapping ~acc && eq_exp t1 t2 ~rename_mapping ~acc && eq_exp f1 f2 ~rename_mapping ~acc && eq_typ_acc typ1 typ2 ~rename_mapping ~acc
  | AddrOfLabel _, AddrOfLabel _ -> false (* TODO: what to do? *)
  | _, _ -> false

and eq_lhost (a: lhost) (b: lhost) ~(rename_mapping: rename_mapping) ~(acc: (typ * typ) list) = match a, b with
    Var v1, Var v2 -> eq_varinfo v1 v2 ~rename_mapping ~acc
  | Mem exp1, Mem exp2 -> eq_exp exp1 exp2 ~rename_mapping ~acc
  | _, _ -> false

and global_typ_acc: (typ * typ) list ref = ref [] (* TODO: optimize with physical Hashtbl? *)

and mem_typ_acc (a: typ) (b: typ) acc = List.exists (fun p -> match p with (x, y) -> a == x && b == y) acc (* TODO: seems slightly more efficient to not use "fun (x, y) ->" directly to avoid caml_tuplify2 *)

and pretty_length () l = Pretty.num (List.length l)

and eq_typ_acc (a: typ) (b: typ) ~(acc: (typ * typ) list) ~(rename_mapping: rename_mapping) =
  if Messages.tracing then Messages.tracei "compareast" "eq_typ_acc %a vs %a (%a, %a)\n" d_type a d_type b pretty_length acc pretty_length !global_typ_acc; (* %a makes List.length calls lazy if compareast isn't being traced *)
  let r = match a, b with
    | TPtr (typ1, attr1), TPtr (typ2, attr2) -> eq_typ_acc typ1 typ2 ~rename_mapping ~acc  && GobList.equal (eq_attribute ~rename_mapping ~acc) attr1 attr2
    | TArray (typ1, (Some lenExp1), attr1), TArray (typ2, (Some lenExp2), attr2) -> eq_typ_acc typ1 typ2 ~rename_mapping ~acc  && eq_exp lenExp1 lenExp2 ~rename_mapping ~acc &&  GobList.equal (eq_attribute ~rename_mapping ~acc) attr1 attr2
    | TArray (typ1, None, attr1), TArray (typ2, None, attr2) -> eq_typ_acc typ1 typ2 ~rename_mapping ~acc  && GobList.equal (eq_attribute ~rename_mapping ~acc) attr1 attr2
    | TFun (typ1, (Some list1), varArg1, attr1), TFun (typ2, (Some list2), varArg2, attr2)
      -> eq_typ_acc typ1 typ2 ~rename_mapping ~acc  && GobList.equal (eq_args rename_mapping acc) list1 list2 && varArg1 = varArg2 &&
         GobList.equal (eq_attribute ~rename_mapping ~acc) attr1 attr2
    | TFun (typ1, None, varArg1, attr1), TFun (typ2, None, varArg2, attr2)
      ->  eq_typ_acc typ1 typ2 ~rename_mapping ~acc  && varArg1 = varArg2 &&
          GobList.equal (eq_attribute ~rename_mapping ~acc) attr1 attr2
    | TNamed (typinfo1, attr1), TNamed (typinfo2, attr2) -> eq_typ_acc typinfo1.ttype typinfo2.ttype ~rename_mapping ~acc  && GobList.equal (eq_attribute ~rename_mapping ~acc) attr1 attr2 (* Ignore tname, treferenced *)
    | TNamed (tinf, attr), b -> eq_typ_acc tinf.ttype b ~rename_mapping ~acc  (* Ignore tname, treferenced. TODO: dismiss attributes, or not? *)
    | a, TNamed (tinf, attr) -> eq_typ_acc a tinf.ttype ~rename_mapping ~acc  (* Ignore tname, treferenced . TODO: dismiss attributes, or not? *)
    (* The following two lines are a hack to ensure that anonymous types get the same name and thus, the same typsig *)
    | TComp (compinfo1, attr1), TComp (compinfo2, attr2) ->
      if mem_typ_acc a b acc || mem_typ_acc a b !global_typ_acc then (
        if Messages.tracing then Messages.trace "compareast" "in acc\n";
        true
      )
      else (
        let acc = (a, b) :: acc in
        let res = eq_compinfo compinfo1 compinfo2 acc rename_mapping && GobList.equal (eq_attribute ~rename_mapping ~acc) attr1 attr2 in
        if res && compinfo1.cname <> compinfo2.cname then
          compinfo2.cname <- compinfo1.cname;
        if res then
          global_typ_acc := (a, b) :: !global_typ_acc;
        res
      )
    | TEnum (enuminfo1, attr1), TEnum (enuminfo2, attr2) -> let res = eq_enuminfo enuminfo1 enuminfo2 ~rename_mapping ~acc && GobList.equal (eq_attribute ~rename_mapping ~acc) attr1 attr2 in (if res && enuminfo1.ename <> enuminfo2.ename then enuminfo2.ename <- enuminfo1.ename); res
    | TBuiltin_va_list attr1, TBuiltin_va_list attr2 -> GobList.equal (eq_attribute ~rename_mapping ~acc) attr1 attr2
    | TVoid attr1, TVoid attr2 -> GobList.equal (eq_attribute ~rename_mapping ~acc) attr1 attr2
    | TInt (ik1, attr1), TInt (ik2, attr2) -> ik1 = ik2 && GobList.equal (eq_attribute ~rename_mapping ~acc) attr1 attr2
    | TFloat (fk1, attr1), TFloat (fk2, attr2) -> fk1 = fk2 && GobList.equal (eq_attribute ~rename_mapping ~acc) attr1 attr2
    | _, _ -> false
  in
  if Messages.tracing then Messages.traceu "compareast" "eq_typ_acc %a vs %a\n" d_type a d_type b;
  r


and eq_eitems ~(rename_mapping: rename_mapping) ~(acc: (typ * typ) list) (a: string * exp * location) (b: string * exp * location) = match a, b with
    (name1, exp1, _l1), (name2, exp2, _l2) -> name1 = name2 && eq_exp exp1 exp2 ~rename_mapping ~acc
(* Ignore location *)

and eq_enuminfo (a: enuminfo) (b: enuminfo) ~(rename_mapping: rename_mapping) ~(acc: (typ * typ) list) =
  compare_name a.ename b.ename &&
  GobList.equal (eq_attribute ~rename_mapping ~acc) a.eattr b.eattr &&
  GobList.equal (eq_eitems ~rename_mapping ~acc) a.eitems b.eitems
(* Ignore ereferenced *)

and eq_args (rename_mapping: rename_mapping) (acc: (typ * typ) list) (a: string * typ * attributes) (b: string * typ * attributes) = match a, b with
    (name1, typ1, attr1), (name2, typ2, attr2) ->
    rename_mapping_aware_name_comparison name1 name2 rename_mapping && eq_typ_acc typ1 typ2 ~rename_mapping ~acc && GobList.equal (eq_attribute ~rename_mapping ~acc) attr1 attr2

and eq_attrparam ~(acc: (typ * typ) list) ~(rename_mapping: rename_mapping) (a: attrparam) (b: attrparam) = match a, b with
  | ACons (str1, attrparams1), ACons (str2, attrparams2) -> str1 = str2 && GobList.equal (eq_attrparam ~rename_mapping ~acc) attrparams1 attrparams2
  | ASizeOf typ1, ASizeOf typ2 -> eq_typ_acc typ1 typ2 ~rename_mapping ~acc
  | ASizeOfE attrparam1, ASizeOfE attrparam2 -> eq_attrparam attrparam1 attrparam2 ~rename_mapping ~acc
  | ASizeOfS typsig1, ASizeOfS typsig2 -> typsig1 = typsig2
  | AAlignOf typ1, AAlignOf typ2 -> eq_typ_acc typ1 typ2 ~rename_mapping ~acc
  | AAlignOfE attrparam1, AAlignOfE attrparam2 -> eq_attrparam attrparam1 attrparam2 ~rename_mapping ~acc
  | AAlignOfS typsig1, AAlignOfS typsig2 -> typsig1 = typsig2
  | AUnOp (op1, attrparam1), AUnOp (op2, attrparam2) -> op1 = op2 && eq_attrparam attrparam1 attrparam2 ~rename_mapping ~acc
  | ABinOp (op1, left1, right1), ABinOp (op2, left2, right2) -> op1 = op2 && eq_attrparam left1 left2 ~rename_mapping ~acc && eq_attrparam right1 right2 ~rename_mapping ~acc
  | ADot (attrparam1, str1), ADot (attrparam2, str2) -> eq_attrparam attrparam1 attrparam2 ~rename_mapping ~acc && str1 = str2
  | AStar attrparam1, AStar attrparam2 -> eq_attrparam attrparam1 attrparam2 ~rename_mapping ~acc
  | AAddrOf attrparam1, AAddrOf attrparam2 -> eq_attrparam attrparam1 attrparam2 ~rename_mapping ~acc
  | AIndex (left1, right1), AIndex (left2, right2) -> eq_attrparam left1 left2 ~rename_mapping ~acc && eq_attrparam right1 right2 ~rename_mapping ~acc
  | AQuestion (left1, middle1, right1), AQuestion (left2, middle2, right2) -> eq_attrparam left1 left2 ~rename_mapping ~acc && eq_attrparam middle1 middle2 ~rename_mapping ~acc && eq_attrparam right1 right2 ~rename_mapping ~acc
  | a, b -> a = b

and eq_attribute  ~(acc: (typ * typ) list) ~(rename_mapping: rename_mapping) (a: attribute) (b: attribute) = match a, b with
  | Attr (name1, params1), Attr (name2, params2) -> name1 = name2 && GobList.equal (eq_attrparam ~rename_mapping ~acc ) params1 params2

and eq_varinfo  (a: varinfo) (b: varinfo) ~(acc: (typ * typ) list) ~(rename_mapping: rename_mapping) =
  (*Printf.printf "Comp %s with %s\n" a.vname b.vname;*)

  let (_, method_rename_mappings) = rename_mapping in

  (*When we compare function names, we can directly compare the naming from the rename_mapping if it exists.*)
  let isNamingOk = match b.vtype with
    | TFun(_, _, _, _) -> (
        let specific_method_rename_mapping = VarinfoMap.find_opt a method_rename_mappings in
        match specific_method_rename_mapping with
        | Some method_rename_mapping -> method_rename_mapping.original_method_name = a.vname && method_rename_mapping.new_method_name = b.vname
        | None -> a.vname = b.vname
      )
    | _ -> rename_mapping_aware_name_comparison a.vname b.vname rename_mapping
  in

  (*If the following is a method call, we need to check if we have a mapping for that method call. *)
  let typ_rename_mapping = match b.vtype with
    | TFun(_, _, _, _) -> (
        let new_locals = VarinfoMap.find_opt a method_rename_mappings in

        match new_locals with
        | Some locals ->
          (locals.parameter_renames, method_rename_mappings)
        | None -> (StringMap.empty, method_rename_mappings)
      )
    | _ -> rename_mapping
  in

  let typeCheck = eq_typ_acc a.vtype b.vtype ~rename_mapping:typ_rename_mapping ~acc  in
  let attrCheck = GobList.equal (eq_attribute ~rename_mapping ~acc ) a.vattr b.vattr in

  let result = isNamingOk && typeCheck && attrCheck &&
               a.vstorage = b.vstorage && a.vglob = b.vglob && a.vaddrof = b.vaddrof in

  result
(* Ignore the location, vid, vreferenced, vdescr, vdescrpure, vinline *)

(* Accumulator is needed because of recursive types: we have to assume that two types we already encountered in a previous step of the recursion are equivalent *)
and eq_compinfo (a: compinfo) (b: compinfo) (acc: (typ * typ) list) (rename_mapping: rename_mapping) =
  a.cstruct = b.cstruct &&
  compare_name a.cname b.cname &&
  GobList.equal (fun a b-> eq_fieldinfo a b ~rename_mapping ~acc ) a.cfields b.cfields &&
  GobList.equal (eq_attribute ~rename_mapping ~acc ) a.cattr b.cattr &&
  a.cdefined = b.cdefined (* Ignore ckey, and ignore creferenced *)

and eq_fieldinfo (a: fieldinfo) (b: fieldinfo) ~(acc: (typ * typ) list) ~(rename_mapping: rename_mapping) =
  if Messages.tracing then Messages.tracei "compareast" "fieldinfo %s vs %s\n" a.fname b.fname;
  let r = a.fname = b.fname && eq_typ_acc a.ftype b.ftype ~rename_mapping ~acc  && a.fbitfield = b.fbitfield &&  GobList.equal (eq_attribute ~rename_mapping ~acc) a.fattr b.fattr in
  if Messages.tracing then Messages.traceu "compareast" "fieldinfo %s vs %s\n" a.fname b.fname;
  r

and eq_offset (a: offset) (b: offset) ~(rename_mapping: rename_mapping) ~(acc: (typ * typ) list) = match a, b with
    NoOffset, NoOffset -> true
  | Field (info1, offset1), Field (info2, offset2) -> eq_fieldinfo info1 info2 ~rename_mapping ~acc && eq_offset offset1 offset2 ~rename_mapping ~acc
  | Index (exp1, offset1), Index (exp2, offset2) -> eq_exp exp1 exp2 ~rename_mapping ~acc && eq_offset offset1 offset2 ~rename_mapping ~acc
  | _, _ -> false

and eq_lval (a: lval) (b: lval) ~(rename_mapping: rename_mapping) ~(acc: (typ * typ) list) = match a, b with
    (host1, off1), (host2, off2) -> eq_lhost host1 host2 ~rename_mapping ~acc && eq_offset off1 off2 ~rename_mapping ~acc

let eq_typ =
  eq_typ_acc ~acc:[]

let eq_exp =
  eq_exp ~acc:[]

let eq_varinfo =
  eq_varinfo ~acc:[]

let eq_lval =
  eq_lval ~acc:[]

let eq_offset =
  eq_offset ~acc:[]

let eq_instr (a: instr) (b: instr) ~(rename_mapping: rename_mapping) = match a, b with
  | Set (lv1, exp1, _l1, _el1), Set (lv2, exp2, _l2, _el2) -> eq_lval lv1 lv2 ~rename_mapping && eq_exp exp1 exp2 ~rename_mapping
  | Call (Some lv1, f1, args1, _l1, _el1), Call (Some lv2, f2, args2, _l2, _el2) ->
    eq_lval lv1 lv2 ~rename_mapping && eq_exp f1 f2 ~rename_mapping && GobList.equal (eq_exp ~rename_mapping) args1 args2
  | Call (None, f1, args1, _l1, _el1), Call (None, f2, args2, _l2, _el2) ->
    eq_exp f1 f2 ~rename_mapping && GobList.equal (eq_exp ~rename_mapping) args1 args2
  | Asm (attr1, tmp1, ci1, dj1, rk1, l1), Asm (attr2, tmp2, ci2, dj2, rk2, l2) -> GobList.equal String.equal tmp1 tmp2 && GobList.equal(fun (x1,y1,z1) (x2,y2,z2)-> x1 = x2 && y1 = y2 && eq_lval z1 z2 ~rename_mapping) ci1 ci2 && GobList.equal(fun (x1,y1,z1) (x2,y2,z2)-> x1 = x2 && y1 = y2 && eq_exp z1 z2 ~rename_mapping) dj1 dj2 && GobList.equal String.equal rk1 rk2(* ignore attributes and locations *)
  | VarDecl (v1, _l1), VarDecl (v2, _l2) -> eq_varinfo v1 v2 ~rename_mapping
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
let rec eq_stmtkind ?(cfg_comp = false) ((a, af): stmtkind * fundec) ((b, bf): stmtkind * fundec) ~(rename_mapping: rename_mapping) =
  let eq_block' = fun x y -> if cfg_comp then true else eq_block (x, af) (y, bf) ~rename_mapping in
  match a, b with
  | Instr is1, Instr is2 -> GobList.equal (eq_instr ~rename_mapping) is1 is2
  | Return (Some exp1, _l1), Return (Some exp2, _l2) -> eq_exp exp1 exp2 ~rename_mapping
  | Return (None, _l1), Return (None, _l2) -> true
  | Return _, Return _ -> false
  | Goto (st1, _l1), Goto (st2, _l2) -> eq_stmt_with_location (!st1, af) (!st2, bf)
  | Break _, Break _ -> if cfg_comp then failwith "CompareCFG: Invalid stmtkind in CFG" else true
  | Continue _, Continue _ -> if cfg_comp then failwith "CompareCFG: Invalid stmtkind in CFG" else true
  | If (exp1, then1, else1, _l1, _el1), If (exp2, then2, else2, _l2, _el2) -> eq_exp exp1 exp2 ~rename_mapping && eq_block' then1 then2 && eq_block' else1 else2
  | Switch (exp1, block1, stmts1, _l1, _el1), Switch (exp2, block2, stmts2, _l2, _el2) -> if cfg_comp then failwith "CompareCFG: Invalid stmtkind in CFG" else eq_exp exp1 exp2 ~rename_mapping && eq_block' block1 block2 && GobList.equal (fun a b -> eq_stmt (a,af) (b,bf) ~rename_mapping) stmts1 stmts2
  | Loop (block1, _l1, _el1, _con1, _br1), Loop (block2, _l2, _el2, _con2, _br2) -> eq_block' block1 block2
  | Block block1, Block block2 -> eq_block' block1 block2
  | _, _ -> false

and eq_stmt ?cfg_comp ((a, af): stmt * fundec) ((b, bf): stmt * fundec) ~(rename_mapping: rename_mapping) =
  GobList.equal eq_label a.labels b.labels &&
  eq_stmtkind ?cfg_comp (a.skind, af) (b.skind, bf) ~rename_mapping

and eq_block ((a, af): Cil.block * fundec) ((b, bf): Cil.block * fundec) ~(rename_mapping: rename_mapping) =
  a.battrs = b.battrs && GobList.equal (fun x y -> eq_stmt (x, af) (y, bf) ~rename_mapping) a.bstmts b.bstmts

let rec eq_init (a: init) (b: init) ~(rename_mapping: rename_mapping) = match a, b with
  | SingleInit e1, SingleInit e2 -> eq_exp e1 e2 ~rename_mapping
  | CompoundInit (t1, l1), CompoundInit (t2, l2) -> eq_typ t1 t2 ~rename_mapping && GobList.equal (fun (o1, i1) (o2, i2) -> eq_offset o1 o2 ~rename_mapping && eq_init i1 i2 ~rename_mapping) l1 l2
  | _, _ -> false

let eq_initinfo (a: initinfo) (b: initinfo) (rename_mapping: rename_mapping) = match a.init, b.init with
  | (Some init_a), (Some init_b) -> eq_init init_a init_b ~rename_mapping
  | None, None -> true
  | _, _ -> false
