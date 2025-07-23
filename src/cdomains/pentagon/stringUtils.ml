open GobApron
open ZExt

module StringUtils =
struct

  (* Symbol for top.  *)
  let top_str = "⊤";;

  (* Symbol for bot. *)
  let bot_str = "⊥";;

  (* Symbol for a 32 bit integer. *)
  let int32_str = "I32";;

  let string_of_dim_change (dim_change:Apron.Dim.change) = 
    let dim = dim_change.dim in
    let real_dim = dim_change.realdim in
    let int_dim = dim_change.intdim in
    Printf.sprintf "{ real_dim=%i; int_dim=%i; dim=[|%s|]}" real_dim int_dim (String.concat ", " (Array.to_list (Array.map Int.to_string (dim))))

  let string_of_var (var: Var.t) =
    let s = (Var.to_string var) in
    match String.index_opt s '#' with
    | Some i -> 
      (* Environment might contain variable names like #ret *)
      if i = 0 then 
        let len = String.length s in
        String.sub s 1 (len-1)
      else
        String.sub s 0 i
    | None -> s

  let string_of_texpr1 (texpr: Texpr1.expr) =
    let rec aux texpr = 
      match texpr with
      | Texpr1.Cst (Interval inv) -> "Cst(Interval inv)"
      | Cst (Scalar s) -> Scalar.to_string s
      | Var x -> string_of_var x
      | Unop  (Neg,  e, _, _) -> "(-" ^ aux e ^ ")"
      | Unop  (Cast, e, _, _) -> "((cast)" ^ aux e ^ ")"
      | Unop  (Sqrt, e, _, _) -> "(sqrt(" ^ aux e ^ "))"
      | Binop (Add, e1, e2, _, _) -> "(" ^ aux e1 ^ " + " ^ aux e2 ^ ")"
      | Binop (Sub, e1, e2, _, _) -> "(" ^ aux e1 ^ " - " ^ aux e2 ^ ")"
      | Binop (Mul, e1, e2, _, _) -> "(" ^ aux e1 ^ " * " ^ aux e2 ^ ")"
      | Binop (Div, e1, e2, _, _) -> "(" ^ aux e1 ^ " / " ^ aux e2 ^ ")"
      | Binop (Mod, e1, e2, _, _) -> "(" ^ aux e1 ^ " % " ^ aux e2 ^ ")"
      | Binop (Pow, e1, e2, _, _) -> "(" ^ aux e1 ^ " ^ " ^ aux e2 ^ ")"
    in
    aux texpr

  let string_of_tcons1 tcons1 =
    (string_of_texpr1 @@ Texpr1.to_expr @@ Tcons1.get_texpr1 @@ tcons1) ^ " " ^  Tcons1.string_of_typ (Tcons1.get_typ tcons1) ^ " 0";;

  (** Returns "-I32" or "+I32" if z is bound, else just the string of z. *)
  let int32_bound_str z =
    let (=*) = ZExtOps.(=*) in
    let min_int = Int32.to_int Int32.min_int in
    let max_int = Int32.to_int Int32.max_int in
    if z =* (ZExt.of_int min_int) then 
      "-" ^ int32_str 
    else if z =* (ZExt.of_int max_int) then 
      "+" ^ int32_str
    else
      ZExt.to_string z

  let string_of_monoms env (sum_of_terms, (constant,divisor)) =
    if divisor <> Z.one then
      failwith "DIVISOR WAS NOT ONE"
    else
      let term_str_list = BatList.map (fun (coefficient, x', divisor) -> 
          if divisor <> Z.one then
            failwith "DIVISOR WAS NOT ONE"
          else
            let var_str = Var.to_string @@ Environment.var_of_dim env x' in 
            let output_str = (Z.to_string coefficient) ^ "*" ^ var_str in
            if Z.sign coefficient < 0 then 
              "("^output_str^")" 
            else
              output_str
        ) sum_of_terms
      in
      String.concat " + " term_str_list ^ " + " ^ (Z.to_string constant) 
end