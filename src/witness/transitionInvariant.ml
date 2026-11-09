open GoblintCil

module ExtVar =
struct
  type t =
    | AnyPrev of CilType.Varinfo.t
  [@@deriving eq, ord, hash]

  let name_varinfo = function
    | AnyPrev v -> "\\at(" ^ v.vname ^ ", AnyPrev)"

  let typ = function
    | AnyPrev v -> v.vtype

  let describe_varinfo _ = function
    | AnyPrev v -> ""
end

module ExtVarMap = RichVarinfo.BiVarinfoMap.Make (ExtVar)

class exp_has_transition_visitor has_normal_var has_anyprev_var = object
  inherit nopCilVisitor

  method! vvrbl (vi: varinfo) =
    begin match ExtVarMap.from_varinfo vi with
      | None -> has_normal_var := true
      | Some (AnyPrev vi') -> has_anyprev_var := true
    end;
    SkipChildren
end

let exp_has_transition e =
  let has_normal_var = ref false in
  let has_anyprev_var = ref false in
  let visitor = new exp_has_transition_visitor has_normal_var has_anyprev_var in
  ignore (visitCilExpr visitor e);
  !has_normal_var && !has_anyprev_var

let var_replace_with_original_visitor = object
  inherit nopCilVisitor

  method! vvrbl (vi: varinfo) =
    match ExtVarMap.from_varinfo vi with
    | None -> SkipChildren
    | Some (AnyPrev vi') -> ChangeTo vi'
end

let exp_is_suitable ?scope e =
  let open InvariantCil in
  let e = visitCilExpr var_replace_with_original_visitor e in (* replace extended vars with originals, so scope check passes (RichVarinfo-s are all global) *)
  not (exp_contains_tmp e || exp_contains_anon_comp_offset e) && GobOption.for_all (fun scope -> exp_is_in_scope scope e) scope


let extvar_localize_visitor = object
  inherit nopCilVisitor

  method! vvrbl (vi: varinfo) =
    match ExtVarMap.from_varinfo vi with
    | None -> SkipChildren
    | Some (AnyPrev vi') -> ChangeTo {vi with vglob = vi'.vglob} (* still ExtVar but with original locality *)
end


module InvariantParser (Base: module type of WitnessUtil.InvariantParser) =
struct
  open GobResult.Syntax

  let anyprev_regexp = Str.regexp {|\\at( *\([A-Za-z_][A-Za-z0-9_]*\) *, *AnyPrev *)|}
  let anyprev_vname var = Printf.sprintf "__at_%s_AnyPrev__" var

  module StringSet = Set.Make (String)

  let parse_cabs inv =
    let anyprev_vars = ref StringSet.empty in (* collect (replaced) AnyPrev variable names from invariant *)
    (* regex replace AnyPrev syntax with a corresponding valid C variable name *)
    let inv' = Str.global_substitute anyprev_regexp (fun m ->
        let var = Str.matched_group 1 m in
        anyprev_vars := StringSet.add var !anyprev_vars;
        anyprev_vname var
      ) inv
    in
    let+ inv_cabs = Base.parse_cabs inv' in
    (inv_cabs, !anyprev_vars)

  let extvar_fix_visitor = object
    inherit nopCilVisitor

    method! vvrbl (vi: varinfo) =
      match ExtVarMap.from_varinfo vi with
      | None -> SkipChildren
      | Some v -> ChangeTo (ExtVarMap.to_varinfo v) (* replace ExtVar vi with the original version of itself (after modifying vname below) *)
  end

  let parse_cil inv_parser ?check ~fundec ~loc (inv_cabs, anyprev_vars) =
    (* TODO: support AnyPrev on locals and globals *)
    let locals' =
      StringSet.fold (fun var acc ->
          match List.find (fun local -> local.vname = var) fundec.slocals with (* TODO: List.find isn't the most efficient *)
          | local -> {(ExtVarMap.to_varinfo (AnyPrev local)) with vname = anyprev_vname var} :: acc (* override vname so Base.parse_cil would use the regex replaced name *)
          | exception Not_found -> failwith "AnyPrev variable isn't local"
        ) anyprev_vars fundec.slocals
    in
    let fundec' = {fundec with slocals = locals'} in
    let+ e = Base.parse_cil inv_parser ?check ~fundec:fundec' ~loc inv_cabs in
    visitCilExpr extvar_fix_visitor e (* replace ExtVar varinfos with overridden vname with original unrenamed ExtVars varinfos *)
end
