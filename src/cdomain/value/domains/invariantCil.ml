(** Invariant manipulation related to CIL transformations. *)

open GoblintCil


let var_replace_original_name vi =
  match Cilfacade.find_original_name vi with
  | Some original_name when original_name <> vi.vname -> {vi with vname = original_name}
  | _ -> vi

class exp_replace_original_name_visitor = object
  inherit nopCilVisitor
  method! vvrbl (vi: varinfo) =
    ChangeTo (var_replace_original_name vi)
end
let exp_replace_original_name =
  let visitor = new exp_replace_original_name_visitor in
  visitCilExpr visitor

let fieldinfo_is_anon (fi: fieldinfo) =
  String.starts_with ~prefix:"__annonCompField" fi.fname (* TODO: what if CIL-ed program explicitly has this? *)

let rec offset_remove_anon_comp_offset = function
  | NoOffset -> NoOffset
  | Index (e, offs') -> Index (e, offset_remove_anon_comp_offset offs')
  | Field (fi, offs') ->
    let offs'' = offset_remove_anon_comp_offset offs' in
    if fieldinfo_is_anon fi then (
      match offs'' with
      | Field _ -> offs''
      | NoOffset
      | Index _ -> failwith "offset_remove_anon_comp_offset: anon comp field not followed by field"
    )
    else
      Field (fi, offs'')

class exp_remove_anon_comp_offset_visitor = object
  inherit nopCilVisitor
  method! voffs (offs: offset) =
    ChangeTo (offset_remove_anon_comp_offset offs)
end
let exp_remove_anon_comp_offset =
  let visitor = new exp_remove_anon_comp_offset_visitor in
  visitCilExpr visitor

class exp_deep_unroll_types_visitor = object
  inherit nopCilVisitor
  method! vtype (t: typ) =
    ChangeTo (unrollTypeDeep t)
end
let exp_deep_unroll_types =
  let visitor = new exp_deep_unroll_types_visitor in
  visitCilExpr visitor

let var_may_be_shadowed scope vi =
  let vi_original_name = Cilfacade.find_original_name vi in
  let local_may_shadow local =
    not (CilType.Varinfo.equal vi local) && (* exclude self-equality by vid because the original names would always equal *)
    vi_original_name = Cilfacade.find_original_name local
  in
  List.exists local_may_shadow scope.sformals || List.exists local_may_shadow scope.slocals

let var_is_in_scope scope vi =
  match Cilfacade.find_scope_fundec vi with
  | None ->
    vi.vstorage <> Static && (* CIL pulls static locals into globals, but they aren't syntactically in global scope *)
    not (var_may_be_shadowed scope vi)
  | Some fd ->
    CilType.Fundec.equal fd scope &&
    (GobConfig.get_bool "witness.invariant.all-locals" || (not @@ hasAttribute "goblint_cil_nested" vi.vattr)) &&
    not (var_may_be_shadowed scope vi) (* TODO: could distinguish non-nested and nested? *)

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

let exclude_vars_regexp = ResettableLazy.from_fun (fun () ->
    GobConfig.get_string_list "witness.invariant.exclude-vars"
    |> String.concat "\\|"
    |> Printf.sprintf "^\\(%s\\)$"
    |> Str.regexp
  )

(* TODO: detect temporaries created by Cil? *)
(* let var_is_tmp {vdescrpure} = not vdescrpure (* doesn't exclude tmp___0 *) *)
(* TODO: instead check if vdescr is nonempty? (doesn't cover all cases, e.g. ternary temporary) *)
let varname_is_tmp vname = Str.string_match (ResettableLazy.force exclude_vars_regexp) vname 0
let var_is_tmp vi = BatOption.map_default varname_is_tmp true (Cilfacade.find_original_name vi)

class exp_contains_tmp_visitor (acc: bool ref) = object
  inherit nopCilVisitor as super
  method! vvrbl (vi: varinfo) =
    if var_is_tmp vi then
      acc := true;
    SkipChildren

  method! vexpr (e: exp) =
    if e = MyCFG.unknown_exp then (
      acc := true;
      SkipChildren
    )
    else
      super#vexpr e
end
let exp_contains_tmp e =
  let acc = ref false in
  let visitor = new exp_contains_tmp_visitor acc in
  ignore (visitCilExpr visitor e);
  !acc

class exp_contains_anon_type_visitor = object
  inherit nopCilVisitor
  method! vtype (t: typ) =
    match t with
    | TComp ({cname; _}, _) when String.starts_with ~prefix:"__anon" cname ->
      raise Stdlib.Exit
    | _ ->
      (* CIL visitor does not go into TNamed, but we don't need to anyway: direct occurrences of __anon struct names in invariants are the problem. *)
      DoChildren
end
let exp_contains_anon_type =
  let visitor = new exp_contains_anon_type_visitor in
  fun e ->
    match visitCilExpr visitor e with
    | _ -> false
    | exception Stdlib.Exit -> true


(* TODO: synchronize magic constant with BaseDomain *)
let var_is_heap {vname; _} = String.starts_with vname ~prefix:"(alloc@"

let reset_lazy () =
  ResettableLazy.reset exclude_vars_regexp
