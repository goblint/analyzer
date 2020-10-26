open Cil

let var_find_original_name (vi: varinfo): string option =
  (* TODO: optimize this *)
  Hashtbl.fold (fun original_name (envdata, _) acc ->
      match envdata with
      | Cabs2cil.EnvVar vi' when vi' = vi -> Some original_name
      | _ -> acc
    ) Cabs2cil.environment None

(* TODO: detect temporaries created by Cil? *)
(* let var_is_tmp {vdescrpure} = not vdescrpure (* doesn't exclude tmp___0 *) *)
(* TODO: instead check if vdescr is nonempty? (doesn't cover all cases, e.g. ternary temporary) *)
let tmp_var_regexp = Str.regexp "^\\(tmp\\(___[0-9]+\\)?\\|cond\\|RETURN\\)$"
let var_is_tmp {vname; _} = Str.string_match tmp_var_regexp vname 0
let rec exp_contains_tmp = function
  | Lval (Var vi, _) -> var_is_tmp vi
  | UnOp (_, e, _) -> exp_contains_tmp e
  | BinOp (_, e1, e2, _) -> exp_contains_tmp e1 || exp_contains_tmp e2
  | exp -> exp = MyCFG.unknown_exp

(* TODO: synchronize magic constant with BaseDomain *)
let var_is_heap {vname; _} = BatString.starts_with vname "(alloc@"
