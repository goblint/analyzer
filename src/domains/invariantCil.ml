open Cil

module VM = Map.Make (Basetype.Variables)

let var_original_names: string VM.t Lazy.t =
  (* only invert environment map when necessary (e.g. witnesses) *)
  lazy (
    Hashtbl.fold (fun original_name (envdata, _) acc ->
      match envdata with
      | Cabs2cil.EnvVar vi when vi.vname <> "" -> (* TODO: fix temporary variables with empty names being in here *)
        VM.add vi original_name acc
      | _ -> acc
    ) Cabs2cil.environment VM.empty
  )

let var_find_original_name vi = VM.find_opt vi (Lazy.force var_original_names)
let var_replace_original_name vi =
  match var_find_original_name vi with
  | Some original_name when original_name <> vi.vname -> {vi with vname = original_name}
  | _ -> vi

class exp_replace_original_name_visitor = object
  inherit nopCilVisitor
  method! vvrbl (vi: varinfo) =
    ChangeTo (var_replace_original_name vi)
end
let exp_replace_original_name e =
  let visitor = new exp_replace_original_name_visitor in
  visitCilExpr visitor e


let var_fundecs: fundec option VM.t Lazy.t =
  lazy (
    foldGlobals !Cilfacade.current_file (fun acc global ->
        match global with
        | GFun (fd, _) ->
          let acc = VM.add fd.svar None acc in (* function itself can be used as a variable (function pointer) *)
          let acc = List.fold_left (fun acc vi -> VM.add vi (Some fd) acc) acc fd.sformals in
          let acc = List.fold_left (fun acc vi -> VM.add vi (Some fd) acc) acc fd.slocals in
          acc
        | GVar (vi, _, _)
        | GVarDecl (vi, _) ->
          VM.add vi None acc
        | _ -> acc
      ) VM.empty
  )

let var_find_fundec vi = VM.find vi (Lazy.force var_fundecs)
let var_is_in_scope scope vi =
  match var_find_fundec vi with
  | None -> true
  | Some fd -> CilType.Fundec.equal fd scope

class exp_is_in_scope_visitor (scope: fundec) (acc: bool ref) = object
  inherit nopCilVisitor
  method! vvrbl (vi: varinfo) =
    acc := !acc && var_is_in_scope scope vi;
    SkipChildren
end
let exp_is_in_scope scope e =
  let acc = ref true in
  let visitor = new exp_is_in_scope_visitor scope acc in
  ignore (visitCilExpr visitor e);
  !acc

(* TODO: detect temporaries created by Cil? *)
(* let var_is_tmp {vdescrpure} = not vdescrpure (* doesn't exclude tmp___0 *) *)
(* TODO: instead check if vdescr is nonempty? (doesn't cover all cases, e.g. ternary temporary) *)
let tmp_var_regexp = Str.regexp "^\\(tmp\\(___[0-9]+\\)?\\|cond\\|RETURN\\)$"
(* let var_is_tmp {vname; _} = Str.string_match tmp_var_regexp vname 0 *)
let var_is_tmp vi = Option.is_none (var_find_original_name vi)
(* TODO: use visitor for this *)
let rec exp_contains_tmp = function
  | Lval (Var vi, _) -> var_is_tmp vi
  | UnOp (_, e, _) -> exp_contains_tmp e
  | BinOp (_, e1, e2, _) -> exp_contains_tmp e1 || exp_contains_tmp e2
  | CastE (_, e) -> exp_contains_tmp e
  | exp -> exp = MyCFG.unknown_exp

(* TODO: synchronize magic constant with BaseDomain *)
let var_is_heap {vname; _} = BatString.starts_with vname "(alloc@"
