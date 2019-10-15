open Cil

(* TODO: detect temporaries created by Cil? *)
(* let var_is_tmp {vdescrpure} = not vdescrpure (* doesn't exclude tmp___0 *) *)
(* TODO: instead check if vdescr is nonempty? (doesn't cover all cases, e.g. ternary temporary) *)
let tmp_var_regexp = Str.regexp "^\\(tmp\\(___[0-9]+\\)?\\|cond\\|RETURN\\)$"
let var_is_tmp {vname} = Str.string_match tmp_var_regexp vname 0
let rec exp_contains_tmp = function
  | Lval (Var vi, _) -> var_is_tmp vi
  | UnOp (_, e, _) -> exp_contains_tmp e
  | BinOp (_, e1, e2, _) -> exp_contains_tmp e1 || exp_contains_tmp e2
  | exp -> exp = MyCFG.unknown_exp