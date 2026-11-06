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


let var_replace_with_local_visitor = object
  inherit nopCilVisitor

  method! vvrbl (vi: varinfo) =
    match ExtVarMap.from_varinfo vi with
    | None -> SkipChildren
    | Some (AnyPrev vi') -> ChangeTo {vi with vglob = vi'.vglob}
end


module InvariantParser (Base: module type of WitnessUtil.InvariantParser) =
struct
  open GobResult.Syntax

  let parse_cabs inv =
    let regexp = Str.regexp {|\\at(\([a-z]+\), *AnyPrev)|} in
    let vars' = ref [] in
    let inv' = Str.global_substitute regexp (fun m ->
        let m = Str.matched_group 1 m in
        vars' := m :: !vars';
        Printf.sprintf "__at_%s_AnyPrev__" m
      ) inv
    in
    let+ inv_cabs = Base.parse_cabs inv' in
    (inv_cabs, vars')

  let var_fix_visitor = object
    inherit nopCilVisitor

    method! vvrbl (vi: varinfo) =
      match ExtVarMap.from_varinfo vi with
      | None -> SkipChildren
      | Some v -> ChangeTo (ExtVarMap.to_varinfo v)
  end

  let parse_cil inv_parser ?check ~fundec ~loc (inv_cabs, vars') =
    let locals' =
      List.map (fun var' ->
          match List.find (fun local -> local.vname = var') fundec.slocals with
          | local -> {(ExtVarMap.to_varinfo (AnyPrev local)) with vname = Printf.sprintf "__at_%s_AnyPrev__" var'}
          | exception Not_found -> failwith "TODO"
        ) !vars'
    in
    let fundec' = {
        fundec with slocals = locals' @ fundec.slocals
      }
    in
    let+ e = Base.parse_cil inv_parser ?check ~fundec:fundec' ~loc inv_cabs in
    visitCilExpr var_fix_visitor e (* replace varinfos with renamed vname with unrenamed ExtVars varinfos *)
end
