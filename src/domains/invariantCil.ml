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
let exp_replace_original_name e =
  let visitor = new exp_replace_original_name_visitor in
  visitCilExpr visitor e


let var_is_in_scope scope vi =
  match Cilfacade.find_scope_fundec vi with
  | None -> vi.vstorage <> Static (* CIL pulls static locals into globals, but they aren't syntactically in global scope *)
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
let var_is_tmp vi =
  match Cilfacade.find_original_name vi with
  | None -> true
  | Some vname -> varname_is_tmp vname
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

(* TODO: synchronize magic constant with BaseDomain *)
let var_is_heap {vname; _} = BatString.starts_with vname "(alloc@"

let reset_lazy () =
  ResettableLazy.reset exclude_vars_regexp
