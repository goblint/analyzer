open Prelude
open Pretty
open Cil
module M = Messages
open Apron

let ( *: ) = Mpqf.mul
let (+:) = Mpqf.add
let (-:) = Mpqf.sub
let (/:) = Mpqf.div
let (=:) = Mpqf.equal
let (<:) x y = Mpqf.cmp x y < 0
let (>:) x y = Mpqf.cmp x y > 0
let (<=:) x y = Mpqf.cmp x y <= 0
let (>=:) x y = Mpqf.cmp x y >= 0

let to_rt x = Mpqf.of_int x

module Vector =
struct
  include List
  type t = Mpqf.t list

  let show t =
    let rec list_str l =
      match l with
      | [] -> "]"
      | x :: xs -> (Mpqf.to_string x) ^" "^(list_str xs)
    in
    "["^list_str t^"\n"

  let equal v1 v2 =
    equal Mpqf.equal v1 v2

  let keep_vals (v:t) n :t =
    filteri (function i -> function x -> i < n) v

  let remove_val n v =
    filteri (function i -> function x -> i <> n) v

  let set_val v n new_val :t =
    mapi (function i -> function x -> if i = n then new_val else x) v

  let rec insert_val n vl (v:t) =
    match v with
    | [] -> if n = 0 then [vl] else failwith "Entry does not exist"
    | x :: xs -> if n > 0 then x :: insert_val (n-1) vl xs else vl :: x :: xs

  let apply_with_c op c =
    map (function x -> op x c)

  let zero_vec size = init size (function i -> to_rt 0)

end

module Matrix =
struct
  type t = Vector.t list
  let empty () = ([] : t)

  let show (x:t) =
    List.fold_left (^) "" (List.map (Vector.show) x)

  let add_empty_column (m : t) pos : t =
    List.map (function y -> Vector.insert_val pos (to_rt 0) y) m

  let append_row (m : t) row : t =
    List.append m [row]

  let get_row (m : t) row_n =
    List.nth m row_n

  let rec remove_row (m : t) row_n : t =
    List.filteri (function i -> function x -> row_n <> i) m

  let get_col (a:t) n : Vector.t =
    List.map (function x -> Vector.nth x n) a

  let append_matrices (a:t) (b:t) : t =
    List.append a b

  let num_rows (m : t) =
    List.length m

  let num_cols (m : t) =
    match m with
    | [] -> 0
    | x :: xs -> Vector.length x

  let rec equal (m1: t) (m2: t) =
    List.equal Vector.equal m1 m2

  let subtract_rows_c row1 row2 c =
    Vector.map2 (function x -> function y -> x -: (y *: c)) row1 row2

  let reduce_row_to_zero (m :t) row_n col_n =
    let red_row = List.nth m row_n in
    let c = Vector.nth red_row col_n in
    List.map (function x -> subtract_rows_c x red_row ((Vector.nth x col_n) /: c)) m

  let remove_col (m: t) col_n =
    let del_col m = List.map (Vector.remove_val col_n) m in
    match List.findi (function i -> function x -> Vector.nth x col_n <> to_rt 0)  (List.rev m) with
    | exception Not_found -> del_col m
    | (i, _) -> let len = List.length m - (i + 1)
      in del_col (remove_row (reduce_row_to_zero m len col_n) len)

  (*Gauss-Jordan Elimination to get matrix in reduced row echelon form (rref) + deletion of zero rows. None matrix has no solution*)

  let normalize t =
    let exception NoSolution in
    let rec create_rref new_t curr_row =
      if curr_row >= num_rows new_t then new_t else
        let row = List.nth new_t curr_row in
        match Vector.findi (function i -> function x -> x <> to_rt 0) row with
        | exception Not_found -> create_rref (remove_row new_t curr_row) curr_row
        | (i, p) -> if Vector.compare_length_with row (i + 1) <= 0 then raise NoSolution
                    else let p = Vector.map (function x -> x /: p) row in
                    let col = get_col new_t i in
                    let res = List.map2i (function row_i -> function y -> function z -> if row_i <> curr_row then subtract_rows_c y p z else p) new_t col
                    in create_rref res (curr_row + 1)
    in
    match create_rref t 0 with
    | c -> let sort v1 v2 =
             let f = Vector.findi (function i -> function x -> x =: to_rt 1) in
             let (i1, _), (i2, _) = (f v1), (f v2) in Int.compare i1 i2
      in Some (List.sort sort c)
    | exception NoSolution -> None
end

module VarManagement =
struct
  include EnvDomain.EnvOps

  type var = EnvDomain.Var.t
  type t = {
    d :  Matrix.t Option.t;
    env : Environment.t
  }

  let dim_add (ch: Apron.Dim.change) m =
    let to_add = Array.to_list ch.dim in
    List.fold_left (function m -> function x -> Matrix.add_empty_column m x) m to_add

  let dim_remove (ch: Apron.Dim.change) m =
    let to_remove = Array.to_list ch.dim in
    List.fold_left (function y -> function x -> Matrix.remove_col y x) m to_remove

  let change_d t new_env add =
    let dim_change = if add then Environment.dimchange t.env new_env
      else Environment.dimchange new_env t.env
    in let d' = match t.d with
        | None -> Some []
        | Some m -> Some (if add then dim_add dim_change m else dim_remove dim_change m)
    in {d = d'; env = new_env}

  let add_vars t vars =
    let vs' = get_filtered_vars_add (t.env) vars in
    let env' = Environment.add t.env vs' [||] in
    change_d t env' true

  let remove_vars t vars =
    let vs' = get_filtered_vars_remove (t.env) vars in
    let env' = Environment.remove t.env vs' in
    change_d t env' false

  let remove_filter t f =
    let env' = remove_filter_with t.env f in
    change_d t env' false

  let keep_filter t f =
    let env' = keep_filter_with t.env f in
    change_d t env' false

  let keep_vars t vs =
    let env' = keep_vars_with t.env vs in
    change_d t env' false

  let vars t = vars t.env

  let type_tracked typ =
    isIntegralType typ

  let varinfo_tracked vi =
    (* no vglob check here, because globals are allowed in apron, but just have to be handled separately *)
    type_tracked vi.vtype && not vi.vaddrof
end

open Apron.Texpr1

let calc_const (t:Matrix.t) env texp =
  let exception NoConst in
  let rec parse_replace m exp =
    if t = [] then raise NoConst else
      match exp with
      | Cst x -> to_rt (EnvDomain.int_of_cst x)
      | Var x -> let n = Environment.dim_of_var env x in
        let row = List.find_opt (function x -> Vector.nth x n = to_rt 1) m in
        let is_const_row row = Vector.compare_length_with (Vector.filteri (function i -> function x ->
            Vector.compare_length_with row (i + 1) > 0 && x <> to_rt 0) row) 1 = 0
        in
        begin match row with
          | Some y when is_const_row y -> Vector.nth y (Vector.length y - 1)
          | _ -> raise NoConst
        end
      | Unop (u, e, _, _) ->
        begin match u with
          | Neg -> to_rt (-1) *: parse_replace m e
          | Cast -> parse_replace m e (*Ignore*)
          | Sqrt -> raise NoConst(*fails*)
        end
      | Binop (b, e1, e2, _, _) ->
        begin match b with
          | Add -> parse_replace m e1 +: parse_replace m e2
          | Sub -> parse_replace m e1 -: parse_replace m e2
          | Mul -> parse_replace m e1 *: parse_replace m e2
          | Div -> parse_replace m e1 /: parse_replace m e2
          | Mod -> raise NoConst
          | Pow -> raise NoConst
        end
  in
  match parse_replace t texp with
  | c -> Some c
  | exception NoConst -> None

module ExpressionBounds: EnvDomain.ConvBounds with type d = VarManagement.t =
struct

  type d = VarManagement.t

  let bound_texpr (t :d) texpr =
    match t.d with
    | Some m -> let texpr = Texpr1.to_expr texpr in
      begin match calc_const m t.env texpr  with
        | Some c when Mpqf.get_den c = Mpzf.of_int 1 ->
          let int_val = IntOps.BigIntOps.of_string (Mpzf.to_string (Mpqf.get_num c))
          in Some int_val, Some int_val
        | _ -> None, None
      end
    | _ -> None, None

end

module Convert = EnvDomain.Convert (ExpressionBounds)

module D2: RelationDomain.RelD2 with type var = EnvDomain.Var.t =
struct

  include VarManagement
  let tag t = failwith "No tag"
  let show t =
    match t.d with
    | None -> Format.asprintf "⟂ (env: %a)" (Environment.print:Format.formatter -> Environment.t -> unit) t.env
    | Some [] -> Format.asprintf "⊤ (env: %a)" (Environment.print:Format.formatter -> Environment.t -> unit) t.env
    | Some m -> Format.asprintf "%s env %a" (Matrix.show m) (Environment.print:Format.formatter -> Environment.t -> unit) t.env

  let pretty () (x:t) = text (show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>\nmatrix\n</key>\n<value>\n%s</value>\n<key>\nenv\n</key>\n<value>\n%s</value>\n</map>\n</value>\n" (XmlUtil.escape (Format.asprintf "%s" (show x) )) (XmlUtil.escape (Format.asprintf "%a" (Environment.print: Format.formatter -> Environment.t -> unit) (x.env)))
  let equal t1 t2 =
    match t1.d, t2.d with
    | None, None -> true
    | Some x1, Some x2 -> Matrix.equal x1 x2
    | _ -> false

  let equal t1 t2 =
    let res = equal t1 t2 in
    if M.tracing then M.tracel "eq" "equal a: %s b: %s -> %b \n" (show t1) (show t2) res ;
    res

  let hash t =
    Hashtbl.hash t
  let compare t1 t2 = Stdlib.compare t1 t2

  let compare t1 t2 =
    let res = compare t1 t2 in
    if M.tracing then M.tracel "cmp" "compare a: %s b: %s -> %i \n" (show t1) (show t2) res ;
    res

  let name () = "affeq"

  let to_yojson t = failwith "ToDo Implement in future"

  let invariant a b = Invariant.none

  let arbitrary () = failwith "no arbitrary"

  let bot () =
    {d = None; env = Environment.make [||] [||]}
  let is_bot t = t.d = None
  let is_bot_env t =
    match t.d with
    | None -> true
    | _ -> false
  let top () =
    {d = Some (Matrix.empty ()); env = Environment.make [||] [||] }
  let is_top t = t.d = Some (Matrix.empty ())

  let meet t1 t2 =
    let sup_env = Environment.lce t1.env t2.env in
    match (t1.d, t2.d) with
    | Some [], Some y -> {d = Some (dim_add (Environment.dimchange t2.env sup_env) y); env = sup_env}
    | Some x, Some [] -> {d = Some (dim_add (Environment.dimchange t1.env sup_env) x); env = sup_env}
    | Some x, Some y -> let mod_x = dim_add (Environment.dimchange t1.env sup_env) x in
                        let mod_y = dim_add (Environment.dimchange t2.env sup_env) y in
                        let rref_matr = Matrix.normalize (Matrix.append_matrices mod_x mod_y) in
                        {d = rref_matr; env = sup_env}
    | _, _ -> {d = None; env = sup_env}

  let meet t1 t2 =
    let res = meet t1 t2 in
    if M.tracing then M.tracel "meet" "meet \n a: %s \n b: %s -> %s \n" (show t1) (show t2) (show res) ;
    res

  let leq t1 t2 =
    if is_bot t1 || is_top t2 then true else
    if is_bot t2 || is_top t1 then false else (
      Environment.equal t1.env t2.env &&
      match t1.d, t2.d with
      | Some x, Some y -> begin match Matrix.normalize @@ Matrix.append_matrices x y with
                          | Some m -> Matrix.equal m x
                          | None -> false
                          end
      | _ -> false )

  let leq t1 t2 =
    let res = leq t1 t2 in
    if M.tracing then M.tracel "leq" "leq a: %s b: %s -> %b \n" (show t1) (show t2) res ;
    res

  let join a b =
    let rec lin_disjunc r s a b =
      if s >= Matrix.num_cols a then a else
      let case_two a r col_b =
        let col_b = let a_length, b_length = List.length a, Vector.length col_b in
                    match Int.compare a_length b_length with
                    | -1 -> Vector.keep_vals col_b a_length
                    |  1 -> Vector.append col_b @@ Vector.zero_vec (a_length - b_length)
                    | _ -> col_b
                    in
        let a_r = Matrix.get_row a r in
        let mapping = List.map2i (function i -> function x -> function y -> if i < r then
                      Vector.map2 (+:) x (Vector.apply_with_c ( *:) y a_r) else x ) a col_b
        in Matrix.remove_row mapping r
      in
      let case_three a b col_a col_b max =
        let col_a, col_b = Vector.keep_vals col_a max, Vector.keep_vals col_b max in
        if Vector.equal col_a col_b then (a, b, max) else
          let vcomb = Vector.combine (Vector.rev col_a) (Vector.rev col_b) in
          let i, (x, y) = Vector.findi (function i -> function x,y -> x <> y) vcomb in
          let r, diff = Vector.length vcomb - (i + 1), x -: y  in
          let a_r, b_r = Matrix.get_row a r, Matrix.get_row b r in
          let rec multiply_by_t m col_v t i = match m, col_v with
                                              | x :: xs, c :: cs->  if i > 0 then let beta = c /: diff in
                                                                    let mul_t = Vector.apply_with_c ( *:) beta t in
                                                                    Vector.map2 (-:) x mul_t :: multiply_by_t xs cs t (i - 1)
                                                                    else xs
                                              | _, _ -> []
          in
          let sub_col = Vector.map2 (function x -> function y -> x -: y) col_a col_b in
          Matrix.remove_row (multiply_by_t a sub_col a_r max) r, Matrix.remove_row (multiply_by_t b sub_col b_r max) r, (max - 1)
        in
        let col_a, col_b = Matrix.get_col a s, Matrix.get_col b s in
        match Int.compare (Matrix.num_rows a) r, Int.compare (Matrix.num_rows b) r with
        | 1 , 1 ->  let a_rs, b_rs = Mpqf.to_float (Vector.nth col_a r), Mpqf.to_float (Vector.nth col_b r) in
                    begin match  a_rs, b_rs with
                    | 1., 1. -> lin_disjunc (r + 1) (s + 1) a b
                    | 1., 0. -> lin_disjunc r (s + 1) (case_two a r col_b) b
                    | 0., 1. -> lin_disjunc r (s + 1) a (case_two b r col_a)
                    | 0., 0. ->  let new_a, new_b, new_r = case_three a b col_a col_b r in
                                lin_disjunc new_r (s + 1) new_a new_b
                    | _      -> failwith "Matrix not normalized" end
        | 1 , _  -> let a_rs = Vector.nth col_a r in
                    if a_rs = (to_rt 1) then lin_disjunc r (s + 1) (case_two a r col_b) b
                    else
                    let new_a, new_b, new_r = case_three a b col_a col_b r in
                    lin_disjunc new_r (s + 1) new_a new_b
        | _ , 1  -> let b_rs = Vector.nth col_b r in
                    if b_rs = (to_rt 1) then lin_disjunc r (s + 1) a (case_two b r col_a)
                    else
                    let new_a, new_b, new_r = case_three a b col_a col_b r in
                     lin_disjunc new_r (s + 1) new_a new_b
        | _      -> let new_a, new_b, new_r = case_three a b col_a col_b r in
                    lin_disjunc new_r (s + 1) new_a new_b
    in
    match a.d, b.d with
    | None, m -> b
    | m, None -> a
    | Some x, Some y when x = [] || y = [] -> {d = Some ([]); env = Environment.lce a.env b.env}
    | Some x, Some y when (Environment.compare a.env b.env <> 0) ->
         let sup_env = Environment.lce a.env b.env in
         let mod_x = dim_add (Environment.dimchange a.env sup_env) x in
         let mod_y = dim_add (Environment.dimchange b.env sup_env) y in
         {d = Some (lin_disjunc 0 0 mod_x mod_y); env = sup_env}
    | Some x, Some y  -> {d = Some(lin_disjunc 0 0 x y); env = a.env}

  let join a b =
    let res = join a b in
    if M.tracing then M.tracel "join" "join a: %s b: %s -> %s \n" (show a) (show b) (show res) ;
    res
  let widen a b = join a b
  let narrow a b = meet a b
  let pretty_diff () (x, y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  (*Parses a Texpr to obtain a coefficient + const (last entry) vector to repr. an affine relation.*)
  let get_coeff_vec env texp =
    let exception NotLinear in
    let zero_vec = Vector.zero_vec @@ Environment.size env + 1 in
    let neg = Vector.map (function x -> (to_rt (-1)) *: x) in
    let rec convert_texpr env texp =
      begin match texp with
       | Cst x ->  Vector.set_val zero_vec ((Vector.length zero_vec) - 1) (to_rt (EnvDomain.int_of_cst x))
       | Var x ->  Vector.set_val zero_vec (Environment.dim_of_var env x) (to_rt 1)
       | Unop (u, e, _, _) ->
           begin match u with
           | Neg -> neg @@ convert_texpr env e (* Multiply by -1*)
           | Cast -> convert_texpr env e (*Ignore*)
           | Sqrt -> raise NotLinear end (*Maybe works if only constant?*)
       | Binop (b, e1, e2, _, _) ->
           begin match b with
           | Add ->  Vector.map2 (+:)(convert_texpr env e1) (convert_texpr env e2) (*Simply add vectors*)
           | Sub -> Vector.map2 (+:) (convert_texpr env  e1) (neg @@ convert_texpr env e2) (*Subtract them*)
           | Mul -> let only_c v  = Vector.compare_lengths
                                    (Vector.filteri (function i -> function x -> Vector.compare_length_with v (i + 1) <> 0 && x <> to_rt 0) v) [] = 0
                    in let x1, x2 = convert_texpr env  e1, convert_texpr env e2 in
                    if only_c x1 || only_c x2
                    then Vector.map2 ( *: ) x1 x2
                    else raise NotLinear (*Var * Var is invalid!*)
           | _ -> raise NotLinear (*rest is not valid!*) end
       end
    in match convert_texpr env texp with
    | exception NotLinear -> None
    | x -> Some(x)

  let assign_invertible_rels x var b env =
    let j0 = Environment.dim_of_var env var in
    let a_j0 = Matrix.get_col x j0  in (*Corresponds to Axj0*)
    let b0 = Vector.nth b j0 in
    let reduced_a = Vector.apply_with_c (/:) b0 a_j0 in  (*Corresponds to Axj0/Bj0*)
    let recalc_entries m rd_a = List.map2 (function x -> function y -> Vector.map2i (function j -> function z -> function d ->
                                                                       if j = j0 then y
                                                                       else if j < (Vector.length b) -1 then z -: y *: d
                                                                       else z +: y *: d) x b) m rd_a
    in {d = Some (recalc_entries x reduced_a); env = env}

  let assign_uninvertible_rel x var b env =
    let neg_vec = List.mapi (function i -> function z -> if i < Vector.length b - 1 then to_rt (-1) *: z else z) b
    in let var_vec = Vector.set_val neg_vec (Environment.dim_of_var env var) (to_rt 1)
    in {d = Some (Option.get @@ Matrix.normalize @@ Matrix.append_row x var_vec); env = env}

  let remove_rels_with_var x var env =
    let j0 = Environment.dim_of_var env var
    in match Vector.findi (function i -> function x -> x <> to_rt 0) (Vector.rev @@ Matrix.get_col x j0) with
            | exception Not_found -> x
            | r, _ -> Matrix.reduce_row_to_zero x (Matrix.num_rows x - r - 1) j0

  let rec forget_vars a vars =
    match a.d with
    | None -> a
    | Some m -> begin match vars with
        | [] -> a
        | x :: xs -> forget_vars {d = Some (Option.get @@ Matrix.normalize @@ remove_rels_with_var m x a.env); env = a.env} xs end

  let assign_texpr t var texp =
    let is_invertible v = Vector.nth v @@ Environment.dim_of_var t.env var <> to_rt 0
    in let affineEq_vec = get_coeff_vec t.env texp
    in match t.d, affineEq_vec with
    | Some [], Some v
    | None, Some v -> if is_invertible v then t (*top/bottom and inv. assign = top*)
                      else assign_uninvertible_rel [] var v t.env
    | Some x, Some v -> if is_invertible v then assign_invertible_rels x var v t.env
                        else let new_x = remove_rels_with_var x var t.env
                             in assign_uninvertible_rel new_x var v t.env
    | _, _ -> t

  let assign_exp t var exp (no_ov: bool) =
    match Convert.texpr1_expr_of_cil_exp t t.env no_ov exp with
    | exp -> assign_texpr t var exp
    | exception Convert.Unsupported_CilExp -> match t.d with
                                              | None -> t
                                              | Some x -> forget_vars t [var]

  let assign_var t v v' =
    let texpr1 = Texpr1.of_expr (t.env) (Var v') in
    assign_texpr t v (Apron.Texpr1.to_expr texpr1)

  let assign_var t v v' =
    let res = assign_var t v v' in
    if M.tracing then M.tracel "ops" "assign_var\n";
    res

  let assign_var_parallel t vv's =
    let new_t's = List.map (function (v,v') -> assign_var t v v') vv's in
    List.fold_left join t new_t's

  let assign_var_parallel t vv's =
    let res = assign_var_parallel t vv's in
    if M.tracing then M.tracel "ops" "assign_var parallel\n";
    res

  let assign_var_parallel' t vs1 vs2 =
    let vv's = List.combine vs1 vs2 in
    assign_var_parallel t vv's

  let assign_var_parallel' t vv's =
    let res = assign_var_parallel' t vv's in
    if M.tracing then M.tracel "ops" "assign_var parallel'\n";
    res

  let substitute_exp t var exp ov =
    let b = get_coeff_vec t.env (Convert.texpr1_expr_of_cil_exp t t.env ov exp) in
    match b, t.d with
    | Some x, Some m -> let dim_var = Environment.dim_of_var t.env var in
                        if Vector.nth x dim_var = (to_rt 0) then
                        meet {d = Some (remove_rels_with_var m var t.env); env = t.env} {d = Matrix.normalize [x]; env = t.env}
                        else assign_invertible_rels m var x t.env
    | None, Some m -> {d = Some (Option.get @@ Matrix.normalize (remove_rels_with_var m var t.env)); env = t.env}
    | _, _ -> t

  let substitute_exp t var exp ov =
    let res = substitute_exp t var exp ov
    in if M.tracing then M.tracel "ops" "Substitute_expr t: \n %s var: %s \n exp: %s \n -> \n %s\n" (show t) (Var.to_string var) (Pretty.sprint 1 (Cil.printExp Cil.defaultCilPrinter () exp)) (show res); res

  let exp_is_cons = function
    (* constraint *)
    | BinOp ((Lt | Gt | Le | Ge | Eq | Ne), _, _, _) -> true
    (* expression *)
    | _ -> false


  (** Assert a constraint expression. *)
  let rec meet_with_tcons d tcons original_expr =
    let meet_with_const cmp = begin match d.d with
      | Some m -> begin match calc_const m d.env @@ Texpr1.to_expr (Tcons1.get_texpr1 tcons) with
                        | Some c -> if cmp c (to_rt 0) then {d = None; env = d.env} else d
                        | None -> d end
      | _ -> d end
    in
    let meet_with_vec e =
      (*Flip the sign of the const. val in coeff vec*)
      let flip_e = List.mapi (function i -> function x -> if i = (Vector.length e) -1 then (to_rt (-1)) *: x else x) e
      in meet d {d = Matrix.normalize [flip_e]; env = d.env}
    in
    match Tcons1.get_typ tcons, get_coeff_vec d.env (Texpr1.to_expr @@ Tcons1.get_texpr1 tcons) with
    | DISEQ, Some e | SUP, Some e -> if equal (meet_with_vec e) d then {d = None; env = d.env} else d
    | SUPEQ, _ -> meet_with_const (<:)
    | SUP, None  -> meet_with_const (<=:)
    | EQ, Some e -> meet_with_vec e
    | _ -> d

  let rec assert_cons d e negate no_ov =
    begin match Convert.tcons1_of_cil_exp d (d.env) e negate no_ov with
      | tcons1 -> meet_with_tcons d tcons1 e
      | exception Convert.Unsupported_CilExp ->
        d
    end

  (** Assert any expression. *)
  let assert_inv d e negate no_ov =
    let e' =
      if exp_is_cons e then
        e
      else
        (* convert non-constraint expression, such that we assert(e != 0) *)
        BinOp (Ne, e, zero, intType)
    in
    assert_cons d e' negate no_ov


  let assert_inv d e negate no_ov =
    let res = assert_inv d e negate no_ov in
    if M.tracing then M.tracel "assert" "assert_inv: d: %s expression: %s negate: %b -> %s\n" (show d) (Pretty.sprint 1 (Cil.printExp Cil.defaultCilPrinter () e)) negate (show res);
    res


  let check_asserts d e =
    if is_bot_env (assert_inv d e false false) then
      `False
    else if is_bot_env (assert_inv d e true false) then
      `True
    else
      `Top

  let check_assert d e =
    let res = check_asserts d e in
    if M.tracing then M.tracel "assert" "check_assert: %s \n" (show d);
    res


  (** Evaluate non-constraint expression as interval. *)
  let eval_interval_expr d e =
    match Convert.texpr1_of_cil_exp d (d.env) e false with
    | texpr1 -> ExpressionBounds.bound_texpr d texpr1
    | exception Convert.Unsupported_CilExp -> (None, None)


  (** Evaluate constraint or non-constraint expression as integer. *)
  let eval_int d e =
    let module ID = Queries.ID in
    let ik = Cilfacade.get_ikind_exp e in
    if exp_is_cons e then
      match check_asserts d e with
      | `True -> ID.of_bool ik true
      | `False -> ID.of_bool ik false
      | `Top -> ID.top ()
    else
      match eval_interval_expr d e with
      | (Some min, Some max) -> ID.of_interval ik (min, max)
      | (Some min, None) -> ID.starting ik min
      | (None, Some max) -> ID.ending ik max
      | (None, None) -> ID.top ()

  let eval_int d e =
    let res = eval_int d e  in
    if M.tracing then M.tracel "assert" "Eval Int: matrix: %s expr: %s -> %s \n" (show d) (Pretty.sprint 1 (Cil.printExp Cil.defaultCilPrinter () e)) (Pretty.sprint 1 (Queries.ID.pretty () res)) ;
    res

  let unify a b =
    let new_env, a_change, b_change  = Environment.lce_change a.env b.env in
    let compute_change change t =
      match change with
      | None -> t
      | Some change -> begin match t.d with
          | None -> {d = None; env = new_env}
          | Some d -> {d = Some (dim_add change d); env = new_env}
        end
    in
    let reduced_a, reduced_b = compute_change a_change a, compute_change b_change b
    in meet reduced_a reduced_b

  let unify a b =
    let res = unify a b in
    if M.tracing then M.tracel "ops" "unify\n";
    res

  type marshal = t

  let marshal t = t

  let unmarshal t = t

  let relift t = t

end