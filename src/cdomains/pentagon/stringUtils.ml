open GobApron
open ZExt

module StringUtils =
struct
  let string_of_dim_change (dim_change:Apron.Dim.change) = 
    let dim = dim_change.dim in
    let real_dim = dim_change.realdim in
    let int_dim = dim_change.intdim in
    Printf.sprintf "{ real_dim=%i; int_dim=%i; dim=[|%s|]}" real_dim int_dim (String.concat ", " (Array.to_list (Array.map Int.to_string (dim))))

  let to_subscript i =
    let transl = [|"₀";"₁";"₂";"₃";"₄";"₅";"₆";"₇";"₈";"₉"|] in
    let rec subscr i =
      if i = 0 then ""
      else (subscr (i/10)) ^ transl.(i mod 10) in
    subscr i

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
    if z =* (ZExt.of_int (-2147483648)) then 
      "-I32" 
    else if z =* (ZExt.of_int (2147483647)) then 
      "+I32"
    else
      ZExt.to_string z
end