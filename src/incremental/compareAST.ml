open Cil
open CilMaps

(* global_type and global_t are implicitly used by GlobalMap to keep GVarDecl apart from GVar and GFun, so do not remove! *)
type global_type = Fun | Decl | Var

and global_identifier = {name: string ; global_t: global_type} [@@deriving ord]

module StringMap = Map.Make(String)

type method_rename_assumption = {original_method_name: string; new_method_name: string}
type method_rename_assumptions = method_rename_assumption VarinfoMap.t
type glob_var_rename_assumptions = string VarinfoMap.t

(*On a successful match, these compinfo and enuminfo names have to be set to the snd element of the tuple. *)
type renamesOnSuccess = (compinfo * compinfo) list * (enuminfo * enuminfo) list

(*rename_mapping is carried through the stack when comparing the AST. Holds a list of rename assumptions.*)
type rename_mapping = (string StringMap.t) * (method_rename_assumptions) * glob_var_rename_assumptions * renamesOnSuccess

let emptyRenameMapping: rename_mapping = (StringMap.empty, VarinfoMap.empty, VarinfoMap.empty, ([], []))

(*Compares two names, being aware of the rename_mapping. Returns true iff:
  1. there is a rename for name1 -> name2 = rename(name1)
  2. there is no rename for name1 -> name1 = name2*)
let rename_mapping_aware_name_comparison (name1: string) (name2: string) (rename_mapping: rename_mapping) =
  let (local_c, method_c, _, _) = rename_mapping in
  let existingAssumption: string option = StringMap.find_opt name1 local_c in

  match existingAssumption with
  | Some now ->
    (*Printf.printf "Assumption is: %s -> %s\n" original now;*)
    now = name2
  | None ->
    (*Printf.printf "No assumption when %s, %s, %b\n" name1 name2 (name1 = name2);*)
    name1 = name2 (*Var names differ, but there is no assumption, so this can't be good*)

(*Creates the mapping of local renames. If the locals do not match in size, an empty mapping is returned.*)
let create_locals_rename_mapping (originalLocalNames: string list) (updatedLocalNames: string list): string StringMap.t =
  if (List.length originalLocalNames) = (List.length updatedLocalNames) then
    List.combine originalLocalNames updatedLocalNames |>
    List.filter (fun (original, now) -> not (original = now)) |>
    List.map (fun (original, now) -> (original, now)) |>
    (fun list ->
       List.fold_left (fun map mapping -> StringMap.add (fst mapping) (snd mapping) map) StringMap.empty list
    )
  else StringMap.empty

let string_tuple_to_string (tuple: (string * string) list) = "[" ^ (tuple |>
                                                                    List.map (fun x -> match x with (first, second) -> "(" ^ first ^ " -> " ^ second ^ ")") |>
                                                                    String.concat ", ") ^ "]"

let rename_mapping_to_string (rename_mapping: rename_mapping) =
  let (local, methods, glob_vars, _) = rename_mapping in
  let local_string = [%show: (string * string) list] (List.of_seq (StringMap.to_seq local)) in
  let methods_string: string = List.of_seq (VarinfoMap.to_seq methods |> Seq.map snd) |>
                               List.map (fun x -> match x with {original_method_name; new_method_name} ->
                                   "(methodName: " ^ original_method_name ^ " -> " ^ new_method_name ^ ")") |>
                               String.concat ", " in

  let global_var_string: string = string_tuple_to_string (List.of_seq (VarinfoMap.to_seq glob_vars) |>
                                                          List.map (fun (v, nowName) -> v.vname, nowName)) in

  "(local=" ^ local_string ^ "; methods=[" ^ methods_string ^ "]; glob_vars=" ^ global_var_string ^ ")"

let identifier_of_global glob =
  match glob with
  | GFun (fundec, l) -> {name = fundec.svar.vname; global_t = Fun}
  | GVar (var, init, l) -> {name = var.vname; global_t = Var}
  | GVarDecl (var, l) -> {name = var.vname; global_t = Decl}
  | _ -> raise Not_found

module GlobalMap = Map.Make(struct
    type t = global_identifier [@@deriving ord]
  end)

(*rename mapping forward propagation, takes the result from a call and propagates the rename mapping to the next call.
   the second call is only executed if the previous call returned true*)
let (&&>>) (prev_result: bool * rename_mapping) f : bool * rename_mapping =
  let (prev_equal, updated_rename_mapping) = prev_result in
  if prev_equal then f updated_rename_mapping else false, updated_rename_mapping

(*Same as && but propagates the rename mapping*)
let (&&>) (prev_result: bool * rename_mapping) (b: bool) : bool * rename_mapping =
  let (prev_equal, rename_mapping) = prev_result in
  (prev_equal && b, rename_mapping)

(*Same as Goblist.eq but propagates the rename_mapping*)
let forward_list_equal f l1 l2 (prev_result: rename_mapping) : bool * rename_mapping =
  if ((List.compare_lengths l1 l2) = 0) then
    List.fold_left2 (fun (b, r) x y -> if b then f x y r else (b, r)) (true, prev_result) l1 l2
  else false, prev_result

(* hack: CIL generates new type names for anonymous types - we want to ignore these *)
let compare_name (a: string) (b: string) =
  let anon_struct = "__anonstruct_" in
  let anon_union = "__anonunion_" in
  if a = b then true else BatString.(starts_with a anon_struct && starts_with b anon_struct || starts_with a anon_union && starts_with b anon_union)

let rec eq_constant (rename_mapping: rename_mapping) (a: constant) (b: constant) : bool * rename_mapping =
  match a, b with
  | CInt (val1, kind1, str1), CInt (val2, kind2, str2) -> Cilint.compare_cilint val1 val2 = 0 && kind1 = kind2, rename_mapping (* Ignore string representation, i.e. 0x2 == 2 *)
  | CEnum (exp1, str1, enuminfo1), CEnum (exp2, str2, enuminfo2) -> eq_exp exp1 exp2 rename_mapping (* Ignore name and enuminfo  *)
  | a, b -> a = b, rename_mapping

and eq_exp2 (rename_mapping: rename_mapping) (a: exp) (b: exp) = eq_exp a b rename_mapping

and eq_exp (a: exp) (b: exp) (rename_mapping: rename_mapping) : bool * rename_mapping =
  match a, b with
  | Const c1, Const c2 -> eq_constant rename_mapping c1 c2
  | Lval lv1, Lval lv2 -> eq_lval lv1 lv2 rename_mapping
  | SizeOf typ1, SizeOf typ2 -> eq_typ typ1 typ2 rename_mapping
  | SizeOfE exp1, SizeOfE exp2 -> eq_exp exp1 exp2 rename_mapping
  | SizeOfStr str1, SizeOfStr str2 -> str1 = str2, rename_mapping (* possibly, having the same length would suffice *)
  | AlignOf typ1, AlignOf typ2 -> eq_typ typ1 typ2 rename_mapping
  | AlignOfE exp1, AlignOfE exp2 -> eq_exp exp1 exp2 rename_mapping
  | UnOp (op1, exp1, typ1), UnOp (op2, exp2, typ2) ->
    (op1 == op2, rename_mapping) &&>> eq_exp exp1 exp2 &&>> eq_typ typ1 typ2
  | BinOp (op1, left1, right1, typ1), BinOp (op2, left2, right2, typ2) ->  (op1 = op2, rename_mapping) &&>> eq_exp left1 left2 &&>> eq_exp right1 right2 &&>> eq_typ typ1 typ2
  | CastE (typ1, exp1), CastE (typ2, exp2) -> eq_typ typ1 typ2 rename_mapping &&>> eq_exp exp1 exp2
  | AddrOf lv1, AddrOf lv2 -> eq_lval lv1 lv2 rename_mapping
  | StartOf lv1, StartOf lv2 -> eq_lval lv1 lv2 rename_mapping
  | Real exp1, Real exp2 -> eq_exp exp1 exp2 rename_mapping
  | Imag exp1, Imag exp2 -> eq_exp exp1 exp2 rename_mapping
  | Question (b1, t1, f1, typ1), Question (b2, t2, f2, typ2) -> eq_exp b1 b2 rename_mapping &&>> eq_exp t1 t2 &&>> eq_exp f1 f2 &&>> eq_typ typ1 typ2
  | AddrOfLabel _, AddrOfLabel _ -> false, rename_mapping (* TODO: what to do? *)
  | _, _ -> false, rename_mapping

and eq_lhost (a: lhost) (b: lhost) (rename_mapping: rename_mapping) = match a, b with
    Var v1, Var v2 -> eq_varinfo v1 v2 rename_mapping
  | Mem exp1, Mem exp2 -> eq_exp exp1 exp2 rename_mapping
  | _, _ -> false, rename_mapping

and global_typ_acc: (typ * typ) list ref = ref [] (* TODO: optimize with physical Hashtbl? *)

and mem_typ_acc (a: typ) (b: typ) acc = List.exists (fun p -> match p with (x, y) -> a == x && b == y) acc (* TODO: seems slightly more efficient to not use "fun (x, y) ->" directly to avoid caml_tuplify2 *)

and pretty_length () l = Pretty.num (List.length l)

and eq_typ_acc (a: typ) (b: typ) (acc: (typ * typ) list) ?(fun_parameter_name_comparison_enabled: bool = true) (rename_mapping: rename_mapping) : bool * rename_mapping =
  (* Registers a compinfo rename or a enum rename*)
  let register_rename_on_success = fun rename_mapping compinfo_option enum_option ->
    let maybeAddTuple = fun list option ->
      Option.value ~default:list (Option.bind option (fun elem -> Some(elem :: list)))
    in

    let (a, b, c, renames_on_success) = rename_mapping in
    let (compinfoRenames, enumRenames) = renames_on_success in

    let updatedCompinfoRenames = maybeAddTuple compinfoRenames compinfo_option in
    let updatedEnumRenames = maybeAddTuple enumRenames enum_option in

    a, b, c, (updatedCompinfoRenames, updatedEnumRenames)
  in

  if Messages.tracing then Messages.tracei "compareast" "eq_typ_acc %a vs %a (%a, %a)\n" d_type a d_type b pretty_length acc pretty_length !global_typ_acc; (* %a makes List.length calls lazy if compareast isn't being traced *)
  let r, updated_rename_mapping = match a, b with
    | TPtr (typ1, attr1), TPtr (typ2, attr2) ->
      eq_typ_acc typ1 typ2 acc rename_mapping &&>> forward_list_equal eq_attribute attr1 attr2
    | TArray (typ1, (Some lenExp1), attr1), TArray (typ2, (Some lenExp2), attr2) -> eq_typ_acc typ1 typ2 acc rename_mapping &&>> eq_exp lenExp1 lenExp2 &&>>  forward_list_equal (eq_attribute) attr1 attr2
    | TArray (typ1, None, attr1), TArray (typ2, None, attr2) -> eq_typ_acc typ1 typ2 acc rename_mapping &&>> forward_list_equal eq_attribute attr1 attr2
    | TFun (typ1, (Some list1), varArg1, attr1), TFun (typ2, (Some list2), varArg2, attr2) ->
      eq_typ_acc typ1 typ2 acc rename_mapping &&>>
      forward_list_equal (eq_args acc ~fun_parameter_name_comparison_enabled:fun_parameter_name_comparison_enabled) list1 list2 &&>
      (varArg1 = varArg2) &&>>
      forward_list_equal eq_attribute attr1 attr2
    | TFun (typ1, None, varArg1, attr1), TFun (typ2, None, varArg2, attr2) ->
      eq_typ_acc typ1 typ2 acc rename_mapping &&>
      (varArg1 = varArg2) &&>>
      forward_list_equal eq_attribute attr1 attr2
    | TNamed (typinfo1, attr1), TNamed (typeinfo2, attr2) ->
      eq_typ_acc typinfo1.ttype typeinfo2.ttype acc rename_mapping &&>> forward_list_equal eq_attribute attr1 attr2 (* Ignore tname, treferenced *)
    | TNamed (tinf, attr), b -> eq_typ_acc tinf.ttype b acc rename_mapping (* Ignore tname, treferenced. TODO: dismiss attributes, or not? *)
    | a, TNamed (tinf, attr) -> eq_typ_acc a tinf.ttype acc rename_mapping (* Ignore tname, treferenced . TODO: dismiss attributes, or not? *)
    (* The following two lines are a hack to ensure that anonymous types get the same name and thus, the same typsig *)
    | TComp (compinfo1, attr1), TComp (compinfo2, attr2) ->
      if mem_typ_acc a b acc || mem_typ_acc a b !global_typ_acc then (
        if Messages.tracing then Messages.trace "compareast" "in acc\n";
        true, rename_mapping
      )
      else (
        let acc = (a, b) :: acc in
        let (res, rm) = eq_compinfo compinfo1 compinfo2 acc rename_mapping &&>> forward_list_equal eq_attribute attr1 attr2 in
        let updated_rm: rename_mapping = if res && compinfo1.cname <> compinfo2.cname then
            (* This renaming now only takes place when the comparison was successful.*)
            (*compinfo2.cname <- compinfo1.cname;*)

            register_rename_on_success rm (Some((compinfo2, compinfo1))) None
          else rm
        in
        if res then
          global_typ_acc := (a, b) :: !global_typ_acc;
        res, updated_rm
      )
    | TEnum (enuminfo1, attr1), TEnum (enuminfo2, attr2) ->
      let (res, rm) = eq_enuminfo enuminfo1 enuminfo2 rename_mapping &&>> forward_list_equal eq_attribute attr1 attr2 in
      if res && enuminfo1.ename <> enuminfo2.ename then
        res, register_rename_on_success rm None (Some((enuminfo2, enuminfo1)))
      else res, rm
    | TBuiltin_va_list attr1, TBuiltin_va_list attr2 -> forward_list_equal eq_attribute attr1 attr2 rename_mapping
    | TVoid attr1, TVoid attr2 -> forward_list_equal eq_attribute attr1 attr2 rename_mapping
    | TInt (ik1, attr1), TInt (ik2, attr2) -> (ik1 = ik2, rename_mapping) &&>> forward_list_equal eq_attribute attr1 attr2
    | TFloat (fk1, attr1), TFloat (fk2, attr2) -> (fk1 = fk2, rename_mapping) &&>> forward_list_equal eq_attribute attr1 attr2
    | _, _ -> false, rename_mapping
  in
  if Messages.tracing then Messages.traceu "compareast" "eq_typ_acc %a vs %a\n" d_type a d_type b;
  (r, updated_rename_mapping)

and eq_typ (a: typ) (b: typ) ?(fun_parameter_name_comparison_enabled: bool = true) (rename_mapping: rename_mapping) : bool * rename_mapping = eq_typ_acc a b [] ~fun_parameter_name_comparison_enabled:fun_parameter_name_comparison_enabled rename_mapping

and eq_eitems (a: string * exp * location) (b: string * exp * location) (rename_mapping: rename_mapping) = match a, b with
    (name1, exp1, _l1), (name2, exp2, _l2) -> (name1 = name2, rename_mapping) &&>> eq_exp exp1 exp2
(* Ignore location *)

and eq_enuminfo (a: enuminfo) (b: enuminfo) (rename_mapping: rename_mapping) =
  (compare_name a.ename b.ename, rename_mapping) &&>>
  forward_list_equal eq_attribute a.eattr b.eattr &&>>
  forward_list_equal eq_eitems a.eitems b.eitems
(* Ignore ereferenced *)

(*param: fun_parameter_name_comparison_enabled when set to false, skips the comparison of the names*)
and eq_args (acc: (typ * typ) list) (a: string * typ * attributes) (b: string * typ * attributes) ?(fun_parameter_name_comparison_enabled: bool = true) (rename_mapping: rename_mapping) : bool * rename_mapping = match a, b with
    (name1, typ1, attr1), (name2, typ2, attr2) ->
    ((not fun_parameter_name_comparison_enabled) || rename_mapping_aware_name_comparison name1 name2 rename_mapping, rename_mapping) &&>>
    eq_typ_acc typ1 typ2 acc &&>>
    forward_list_equal eq_attribute attr1 attr2

and eq_attrparam (a: attrparam) (b: attrparam) (rename_mapping: rename_mapping) : bool * rename_mapping = match a, b with
  | ACons (str1, attrparams1), ACons (str2, attrparams2) -> (str1 = str2, rename_mapping) &&>> forward_list_equal eq_attrparam attrparams1 attrparams2
  | ASizeOf typ1, ASizeOf typ2 -> eq_typ typ1 typ2 rename_mapping
  | ASizeOfE attrparam1, ASizeOfE attrparam2 -> eq_attrparam attrparam1 attrparam2 rename_mapping
  | ASizeOfS typsig1, ASizeOfS typsig2 -> typsig1 = typsig2, rename_mapping
  | AAlignOf typ1, AAlignOf typ2 -> eq_typ typ1 typ2 rename_mapping
  | AAlignOfE attrparam1, AAlignOfE attrparam2 -> eq_attrparam attrparam1 attrparam2 rename_mapping
  | AAlignOfS typsig1, AAlignOfS typsig2 -> typsig1 = typsig2, rename_mapping
  | AUnOp (op1, attrparam1), AUnOp (op2, attrparam2) -> (op1 = op2, rename_mapping) &&>> eq_attrparam attrparam1 attrparam2
  | ABinOp (op1, left1, right1), ABinOp (op2, left2, right2) -> (op1 = op2, rename_mapping) &&>> eq_attrparam left1 left2 &&>> eq_attrparam right1 right2
  | ADot (attrparam1, str1), ADot (attrparam2, str2) -> eq_attrparam attrparam1 attrparam2 rename_mapping &&> (str1 = str2)
  | AStar attrparam1, AStar attrparam2 -> eq_attrparam attrparam1 attrparam2 rename_mapping
  | AAddrOf attrparam1, AAddrOf attrparam2 -> eq_attrparam attrparam1 attrparam2 rename_mapping
  | AIndex (left1, right1), AIndex (left2, right2) -> eq_attrparam left1 left2 rename_mapping &&>> eq_attrparam right1 right2
  | AQuestion (left1, middle1, right1), AQuestion (left2, middle2, right2) ->
    eq_attrparam left1 left2 rename_mapping &&>>
    eq_attrparam middle1 middle2 &&>>
    eq_attrparam right1 right2
  | a, b -> a = b, rename_mapping

and eq_attribute (a: attribute) (b: attribute) (rename_mapping: rename_mapping) : bool * rename_mapping = match a, b with
  | Attr (name1, params1), Attr (name2, params2) -> (name1 = name2, rename_mapping) &&>> forward_list_equal eq_attrparam params1 params2

and eq_varinfo2 (rename_mapping: rename_mapping) (a: varinfo) (b: varinfo) = eq_varinfo a b rename_mapping

and eq_varinfo (a: varinfo) (b: varinfo) (rename_mapping: rename_mapping) : bool * rename_mapping =
  (*Printf.printf "Comp %s with %s\n" a.vname b.vname;*)

  let (locals_renames, method_rename_mappings, glob_vars, renames_on_success) = rename_mapping in

  let compare_local_and_global_var =
    if a.vglob then
      let present_mapping = VarinfoMap.find_opt a glob_vars in

      match present_mapping with
      | Some (knownNowName) ->
        b.vname = knownNowName, method_rename_mappings, glob_vars
      | None -> (
          let update_glob_vars = VarinfoMap.add a b.vname glob_vars in
          true, method_rename_mappings, update_glob_vars
        )
    else rename_mapping_aware_name_comparison a.vname b.vname rename_mapping, method_rename_mappings, glob_vars
  in

  (*When we compare function names, we can directly compare the naming from the rename_mapping if it exists.*)
  let isNamingOk, updated_method_rename_mappings, updatedGlobVarMapping = match a.vtype, b.vtype with
    | TFun(_, aParamSpec, _, _), TFun(_, bParamSpec, _, _) -> (
        let specific_method_rename_mapping = VarinfoMap.find_opt a method_rename_mappings in
        match specific_method_rename_mapping with
        | Some method_rename_mapping ->
          let is_naming_ok = method_rename_mapping.original_method_name = a.vname && method_rename_mapping.new_method_name = b.vname in
          is_naming_ok, method_rename_mappings, glob_vars
        | None ->
          if a.vname <> b.vname then
            let assumption =
              {original_method_name = a.vname; new_method_name = b.vname} in

            true, VarinfoMap.add a assumption method_rename_mappings, glob_vars
          else true, method_rename_mappings, glob_vars
      )
    | TInt (_, _), TInt (_, _) -> compare_local_and_global_var
    | TFloat (_, _), TFloat (_, _) -> compare_local_and_global_var
    | TPtr (_, _), TPtr(_, _) -> compare_local_and_global_var
    | _, _ -> rename_mapping_aware_name_comparison a.vname b.vname rename_mapping, method_rename_mappings, glob_vars
  in

  (*If the following is a method call, we need to check if we have a mapping for that method call. *)
  let fun_parameter_name_comparison_enabled = match b.vtype with
    | TFun(_, _, _, _) -> false
    (*| GVar (_, _, _) -> (
        let new_local = VarinfoMap.find_opt a glob_vars in

        match new_local with
        | Some now_name -> (StringMap.add a.vname now_name StringMap.empty, updated_method_rename_mappings, updatedGlobVarMapping)
        | None -> (StringMap.empty, updated_method_rename_mappings, updatedGlobVarMapping)
      )*)
    | _ -> true
  in

  (*Ignore rename mapping for type check, as it doesn't change anyway. We only need the renames_on_success*)
  let (typeCheck, (_, _, _, updated_renames_on_success)) = eq_typ a.vtype b.vtype ~fun_parameter_name_comparison_enabled:fun_parameter_name_comparison_enabled (StringMap.empty, VarinfoMap.empty, VarinfoMap.empty, renames_on_success) in

  (isNamingOk && typeCheck, (locals_renames, updated_method_rename_mappings, updatedGlobVarMapping, updated_renames_on_success)) &&>>
  forward_list_equal eq_attribute a.vattr b.vattr &&>
  (a.vstorage = b.vstorage) &&> (a.vglob = b.vglob) &&> (a.vaddrof = b.vaddrof)
(* Ignore the location, vid, vreferenced, vdescr, vdescrpure, vinline *)

(* Accumulator is needed because of recursive types: we have to assume that two types we already encountered in a previous step of the recursion are equivalent *)
and eq_compinfo (a: compinfo) (b: compinfo) (acc: (typ * typ) list) (rename_mapping: rename_mapping) : bool * rename_mapping =
  (a.cstruct = b.cstruct, rename_mapping) &&>
  compare_name a.cname b.cname &&>>
  forward_list_equal (fun a b -> eq_fieldinfo a b acc) a.cfields b.cfields &&>>
  forward_list_equal eq_attribute a.cattr b.cattr &&>
  (a.cdefined = b.cdefined) (* Ignore ckey, and ignore creferenced *)

and eq_fieldinfo (a: fieldinfo) (b: fieldinfo) (acc: (typ * typ) list) (rename_mapping: rename_mapping) =
  if Messages.tracing then Messages.tracei "compareast" "fieldinfo %s vs %s\n" a.fname b.fname;
  let (r, rm) = (a.fname = b.fname, rename_mapping) &&>>
                eq_typ_acc a.ftype b.ftype acc &&> (a.fbitfield = b.fbitfield) &&>>
                forward_list_equal eq_attribute a.fattr b.fattr in
  if Messages.tracing then Messages.traceu "compareast" "fieldinfo %s vs %s\n" a.fname b.fname;
  (r, rm)

and eq_offset (a: offset) (b: offset) (rename_mapping: rename_mapping) : bool * rename_mapping = match a, b with
    NoOffset, NoOffset -> true, rename_mapping
  | Field (info1, offset1), Field (info2, offset2) -> eq_fieldinfo info1 info2 [] rename_mapping &&>> eq_offset offset1 offset2
  | Index (exp1, offset1), Index (exp2, offset2) -> eq_exp exp1 exp2 rename_mapping &&>> eq_offset offset1 offset2
  | _, _ -> false, rename_mapping

and eq_lval (a: lval) (b: lval) (rename_mapping: rename_mapping) : bool * rename_mapping = match a, b with
    (host1, off1), (host2, off2) -> eq_lhost host1 host2 rename_mapping &&>> eq_offset off1 off2

let eq_instr (a: instr) (b: instr) (rename_mapping: rename_mapping) = match a, b with
  | Set (lv1, exp1, _l1, _el1), Set (lv2, exp2, _l2, _el2) -> eq_lval lv1 lv2 rename_mapping &&>> eq_exp exp1 exp2
  | Call (Some lv1, f1, args1, _l1, _el1), Call (Some lv2, f2, args2, _l2, _el2) ->
    eq_lval lv1 lv2 rename_mapping &&>> eq_exp f1 f2 &&>> forward_list_equal eq_exp args1 args2
  | Call (None, f1, args1, _l1, _el1), Call (None, f2, args2, _l2, _el2) ->
    eq_exp f1 f2 rename_mapping &&>> forward_list_equal eq_exp args1 args2
  | Asm (attr1, tmp1, ci1, dj1, rk1, l1), Asm (attr2, tmp2, ci2, dj2, rk2, l2) ->
    (GobList.equal String.equal tmp1 tmp2, rename_mapping) &&>>
    forward_list_equal (fun (x1,y1,z1) (x2,y2,z2) x-> (x1 = x2, x) &&> (y1 = y2) &&>> eq_lval z1 z2) ci1 ci2 &&>>
    forward_list_equal (fun (x1,y1,z1) (x2,y2,z2) x-> (x1 = x2, x) &&> (y1 = y2) &&>> eq_exp z1 z2) dj1 dj2 &&>
    GobList.equal String.equal rk1 rk2(* ignore attributes and locations *)
  | VarDecl (v1, _l1), VarDecl (v2, _l2) -> eq_varinfo v1 v2 rename_mapping
  | _, _ -> false, rename_mapping

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
let rec eq_stmtkind ?(cfg_comp = false) ((a, af): stmtkind * fundec) ((b, bf): stmtkind * fundec) (rename_mapping: rename_mapping) =
  let eq_block' = fun x y rm -> if cfg_comp then true, rm else eq_block (x, af) (y, bf) rm in
  match a, b with
  | Instr is1, Instr is2 -> forward_list_equal eq_instr is1 is2 rename_mapping
  | Return (Some exp1, _l1), Return (Some exp2, _l2) -> eq_exp exp1 exp2 rename_mapping
  | Return (None, _l1), Return (None, _l2) -> true, rename_mapping
  | Return _, Return _ -> false, rename_mapping
  | Goto (st1, _l1), Goto (st2, _l2) -> eq_stmt_with_location (!st1, af) (!st2, bf), rename_mapping
  | Break _, Break _ -> if cfg_comp then failwith "CompareCFG: Invalid stmtkind in CFG" else true, rename_mapping
  | Continue _, Continue _ -> if cfg_comp then failwith "CompareCFG: Invalid stmtkind in CFG" else true, rename_mapping
  | If (exp1, then1, else1, _l1, _el1), If (exp2, then2, else2, _l2, _el2) -> eq_exp exp1 exp2 rename_mapping &&>>
                                                                              eq_block' then1 then2 &&>>
                                                                              eq_block' else1 else2
  | Switch (exp1, block1, stmts1, _l1, _el1), Switch (exp2, block2, stmts2, _l2, _el2) -> if cfg_comp then failwith "CompareCFG: Invalid stmtkind in CFG" else eq_exp exp1 exp2 rename_mapping &&>> eq_block' block1 block2 &&>> forward_list_equal (fun a b -> eq_stmt (a,af) (b,bf)) stmts1 stmts2
  | Loop (block1, _l1, _el1, _con1, _br1), Loop (block2, _l2, _el2, _con2, _br2) -> eq_block' block1 block2 rename_mapping
  | Block block1, Block block2 -> eq_block' block1 block2 rename_mapping
  | _, _ -> false, rename_mapping

and eq_stmt ?cfg_comp ((a, af): stmt * fundec) ((b, bf): stmt * fundec) (rename_mapping: rename_mapping) =
  (GobList.equal eq_label a.labels b.labels, rename_mapping) &&>>
  eq_stmtkind ?cfg_comp (a.skind, af) (b.skind, bf)

and eq_block ((a, af): Cil.block * fundec) ((b, bf): Cil.block * fundec) (rename_mapping: rename_mapping) : bool * rename_mapping =
  (a.battrs = b.battrs, rename_mapping) &&>> forward_list_equal (fun x y -> eq_stmt (x, af) (y, bf)) a.bstmts b.bstmts

let rec eq_init (a: init) (b: init) (rename_mapping: rename_mapping) = match a, b with
  | SingleInit e1, SingleInit e2 -> eq_exp e1 e2 rename_mapping
  | CompoundInit (t1, l1), CompoundInit (t2, l2) ->
    eq_typ t1 t2 rename_mapping &&>>
    forward_list_equal (fun (o1, i1) (o2, i2) x -> eq_offset o1 o2 x &&>> eq_init i1 i2) l1 l2
  | _, _ -> false, rename_mapping

let eq_initinfo (a: initinfo) (b: initinfo) (rename_mapping: rename_mapping) = match a.init, b.init with
  | (Some init_a), (Some init_b) -> eq_init init_a init_b rename_mapping
  | None, None -> true, rename_mapping
  | _, _ -> false, rename_mapping
