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
  type t = Mpqf.t list

  let length = List.length

  let create_zero_vec n : t =
    let rec zero_vec t n =
      if n > 0 then zero_vec (Mpqf.of_int 0 :: t) (n-1)
      else t
    in
    zero_vec [] n

  let show t =
    let rec list_str l =
      match l with
      | [] -> "]"
      | x :: xs -> (Mpqf.to_string x) ^","^(list_str xs)
    in
    "["^list_str t^"\n"

  let map_vec f v =
    List.map f v

  let equal v1 v2 =
    List.equal Mpqf.equal v1 v2

  let neg v =
    List.map (function x -> (to_rt (-1)) *: x) v

  let sub_vecs t1 t2 =
    List.map2 (function x -> function y -> x -: y) t1 t2

  let mul_vecs t1 t2 =
    List.map2 (function x -> function y -> x *: y) t1 t2

  let add_vecs t1 t2 =
    List.map2 (function x -> function y -> x +: y) t1 t2

  (*Searches for the last entry where v1 and v2 are different. Returns diff + position*)
  let find_last_diff v1 v2 =
    let rec l_d v1 v2 entry diff i =
      match v1, v2 with
      | x1 :: xs1, x2 :: xs2 -> if x1 <> x2 then l_d xs1 xs2 i (x1 -: x2) (i + 1)
        else l_d xs1 xs2 entry diff (i + 1)
      | _, _ -> (entry, diff)
    in l_d v1 v2 0 (to_rt 0) 0

  let is_only_zero v =
    match List.find_opt (function x -> x <> (to_rt 0)) v with
    | None -> true
    | _ -> false

  let rec keep_rows (v:t) n =
    match v with
    | [] -> if n = 0 then [] else failwith "Number exceeds list"
    | x :: xs -> if n > 0 then x :: keep_rows xs (n-1) else []

  let rec remove_val n (v:t) =
    match v with
    | [] -> failwith "Entry does not exist"
    | x :: xs -> if n > 0 then x :: remove_val (n-1) xs else xs

  let rec set_val t n new_val :t =
    match t with
    | [] -> failwith "Entry does not exist"
    | x :: xs -> if n > 0 then x :: set_val xs (n-1) new_val else  new_val :: xs

  let rec add_val n vl (v:t) =
    match v with
    | [] -> if n = 0 then [vl] else failwith "Entry does not exist"
    | x :: xs -> if n > 0 then add_val (n-1) vl xs else x :: vl :: xs

  let mul_by_constant c v =
    List.map (function x -> x *: c) v

  let divide_by_const v c =
    List.map (function x -> x /: c) v

  let rec is_constant v =
    match v with
    | [] -> true
    | x :: [] -> true
    | x :: xs -> if x <> (to_rt 0) then false else is_constant xs

  let get_val n (v: t) =
    List.nth v n

  (*Extended List.opt with index*)
  let find_opt_with_index f v =
    let rec find_opt_i v i =
      match v with
      | [] -> None
      | x :: xs -> if f x then Some ((i, x)) else find_opt_i xs (i + 1)
    in find_opt_i v 0


  let get_rownum_not_zero t =
    let rev_list = List.rev t in
    match find_opt_with_index (function x -> x <> (to_rt 0)) rev_list with
    | Some (i, c) -> Some (List.length t - (i + 1), c)
    | x -> x

end

module Matrix =
struct
  type t = Mpqf.t list list

  exception NoSolution
  let empty () = ([] : t)

  let show (x:t) =
    List.fold_left (^) "" (List.map (Vector.show) x)

  let rec add_row (m : t) (row: Vector.t) pos : t =
    match m with
    | [] -> if pos > 0 then failwith "Position exceeds size" else [row]
    | x :: xs ->
      if pos > 0 then (x :: (add_row xs row (pos - 1)))
      else row :: m

  let add_column (m : t) (col: Vector.t) pos : t =
    match m with
    | [] -> List.map (function x -> [x]) col
    | x -> List.map2 (function y -> function z -> Vector.add_val pos z y) x col


  let append_row (m : t) row =
    List.append m [row]

  let get_row (m : t) row_n =
    List.nth m row_n

  let rec remove_row (m : t) row_n =
    match m with
    | [] -> failwith "Entry does not exist"
    | x :: xs -> if row_n > 0 then x :: remove_row xs (row_n - 1) else xs

  let get_col (a:t) n : Vector.t =
    List.map (function x -> List.nth x n) a
  let remove_column (m : t) pos =
    List.map (Vector.remove_val pos) m

  (*Extended removal of columns. Removes rows that had an entry <> 0 at the col pos as well*)
  let remove_column_ex (m : t) pos =
    let filtered_rows = List.filter (function x -> if (List.nth x pos) = (to_rt 0) then true else false) m
    in remove_column filtered_rows pos

  let append_matrices (a:t) (b:t) : t =
    List.append a b

  let num_rows (m : t) =
    List.length m

  let num_cols (m : t) =
    match m with
    | [] -> 0
    | x :: xs -> List.length x
  let create_zero_col (m : t)  =
    let rec create_zero_list l n =
      if n > 0 then create_zero_list (List.append [to_rt 0] l) (n-1)
      else l
    in
    create_zero_list [] (num_rows m)

  let rec equal (a: t) (b: t) =
    match (a, b) with
    | [], [] -> true
    | x1 :: xs1, x2 :: xs2 -> if List.equal Mpqf.equal x1 x2 then equal xs1 xs2 else false
    | _ -> false

  let add_rows row1 row2 =
    List.map2 (function x -> function y -> x +: y) row1 row2

  let subtract_rows_c row1 row2 col_val =
    let subtr_vec = Vector.mul_by_constant col_val row2
    in Vector.sub_vecs row1 subtr_vec


  let reduce_row_to_zero (t :t) row_n col_n =
    let red_row = List.nth t row_n in
    let c = List.nth red_row col_n in
    List.map (function x -> subtract_rows_c x red_row ((Vector.get_val col_n x) /: c)) t

  let obtain_pivot row =
    let non_zero = Vector.find_opt_with_index (function x -> x <> (to_rt 0)) row in
    match non_zero with
    | None -> None
    | Some (i, p) -> if List.compare_length_with row (i + 1) <= 0 then raise NoSolution else
        Some (i, List.map (function x -> x /: p) row)

  let rec remove_zero_rows t =
    List.filter (function x -> not (Vector.is_only_zero x)) t

  let switch_rows t =
    let sort v1 v2 =
      let f = Vector.find_opt_with_index (function x -> x =: to_rt 1) in
      let res_v1, res_v2 = f v1, f v2 in
      match res_v1, res_v2 with
      | Some (i1, _), Some (i2, _) -> Int.compare i1 i2
      | _, _ -> failwith "Matrix has not been reduced yet"
    in List.sort sort t

  (*Gauss-Jordan Elimination to get matrix in reduced row echelon form (rref) + deletion of zero rows. None matrix has no solution*)
  let normalize_opt t =
    let rec create_rref new_t curr_row =
      if curr_row >= num_rows new_t then new_t else
        let pivot_row = (match obtain_pivot (List.nth new_t curr_row) with
            | c -> c
            | exception NoSolution -> raise NoSolution) (*Perhaps not the most elegant way to handle this*)
        in
        match pivot_row with
        | None -> create_rref (remove_row new_t curr_row) (curr_row)
        | Some (i, p) -> let col = get_col new_t i in
          let res = List.map2i (function row_i -> function y -> function z ->
              if row_i <> curr_row then subtract_rows_c y p z else p) new_t col in
          create_rref res (curr_row + 1)
    in match create_rref t 0 with
    | c -> Some (switch_rows c)
    | exception NoSolution -> None


  (*Normalize and fail if there is no solution*)
  let normalize t =
    match normalize_opt t with
    | Some (x) -> x
    | None -> failwith "Matrix does not have a solution"

  let normalize t =
    let res = normalize t in
    if M.tracing then M.tracel "norm" "normalize \n %s -> %s" (show t) (show res); res

  (*Checks if the rows of t2 are also present in t1*)
  let rec is_contained_in t1 t2 =
    match t1, t2 with
    | [], _ -> true
    | x1 :: xs1, x2 ::xs2 -> Vector.equal x1 x2 && is_contained_in xs1 xs2
    | _, _ -> false
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
    List.fold_left (function m -> function x -> Matrix.add_column m (Matrix.create_zero_col m) x) m to_add

  let dim_remove (ch: Apron.Dim.change) m =
    let to_remove = Array.to_list ch.dim in
    List.fold_left (function y -> function x -> Matrix.normalize (Matrix.remove_column_ex y x)) m to_remove

  let add_vars a vars =
    let vs' = get_filtered_vars_add (a.env) vars in
    let env' = Environment.add a.env vs' [||] in
    let d' = (match a.d with
        | None -> Some ([])
        | Some (m) -> Some (dim_add (Environment.dimchange a.env env') m))
    in {d = d'; env = env'}

  let remove_vars a vars =
    let vs' = get_filtered_vars_remove (a.env) vars in
    let env' = Environment.remove a.env vs' in
    let d' = (match a.d with
        | None -> None
        | Some (m) -> Some (dim_remove (Environment.dimchange env' a.env) m))
    in {d = d'; env = env'}

  let remove_filter a f =
    let env' = remove_filter_with a.env f in
    let d' = (match a.d with
        | None -> None
        | Some (m) -> Some (dim_remove (Environment.dimchange a.env env') m))
    in {d = d'; env = env'}

  let keep_filter a f =
    let env' = keep_filter_with a.env f in
    let d' = (match a.d with
        | None -> None
        | Some (m) -> Some (dim_remove (Environment.dimchange a.env env') m))
    in {d = d'; env = env'}

  let keep_vars a vs =
    let env' = keep_vars_with a.env vs in
    let d' = (match a.d with
        | None -> None
        | Some (m) -> Some (dim_remove (Environment.dimchange a.env env') m))
    in {d = d'; env = env'}

  let forget_vars a l = (*ToDo Mem_var shouldn't be called*)
    remove_vars a l

  let vars a = vars a.env

  let show_vars env =
    let all_vars = vars env in
    let rec vars_to_string l =
      match l with
      | [] -> "]"
      | x :: xs -> (Var.to_string x) ^ vars_to_string xs
    in
    "[" ^ vars_to_string all_vars

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
    if t = [] then raise (NoConst) else
      match exp with
      | Cst x -> to_rt (EnvDomain.int_of_cst x)
      | Var x -> let n = Environment.dim_of_var env x in
        let row = List.find_opt (function x -> List.nth x n = (to_rt 1)) m in
        begin match row with
          | None -> raise (NoConst)
          | Some (y) -> if Vector.is_constant y then Vector.get_val ((List.length y) - 1) y
            else raise (NoConst)  end
      | Unop (u, e, _, _) ->(
          match u with
          | Neg -> (to_rt (-1)) *: parse_replace m e
          | Cast -> parse_replace m e (*Ignore*)
          | Sqrt -> raise (NoConst) )(*fails*)
      | Binop (b, e1, e2, _, _) ->
        begin match b with
          | Add -> (parse_replace m e1) +: (parse_replace m e2)
          | Sub -> (parse_replace m e1) -: (parse_replace m e2)
          | Mul -> (parse_replace m e1) *: (parse_replace m e2)
          | Div -> (parse_replace m e1) /: (parse_replace m e2)
          | Mod -> raise (NoConst) (*ToDO calculate mod*)
          | Pow -> raise (NoConst) (*ToDo calculate pow*)
        end
  in
  match parse_replace t texp with
  | c -> Some (c)
  | exception NoConst -> None

module ExpressionBounds: EnvDomain.ConvBounds with type d = VarManagement.t = (* ToDo Implement proper bounds calculation*)
struct

  type d = VarManagement.t

  let bound_texpr (t :d) texpr ik =
    let min,max = IntDomain.Size.range_big_int ik in
    let texpr = Texpr1.to_expr texpr
    in
    match t.d with
    | None -> Some(min), Some(max)
    | Some (m) -> ( match calc_const m t.env texpr  with
        | None -> Some (min), Some (max)
        | Some (c) -> if (Mpqf.get_den c) = (Mpzf.of_int 1) then
            let int_val = IntOps.BigIntOps.of_string (Mpzf.to_string (Mpqf.get_num c)) in
            Some(int_val), Some (int_val)
          else (None, None))

end

module Convert = EnvDomain.Convert (ExpressionBounds)

module D2: RelationDomain.RelD2 with type var = EnvDomain.Var.t =
struct

  include VarManagement
  let tag t = failwith "No tag"
  let show a =
    match a.d with
    | None -> Format.asprintf "⟂ (env: %a)" (Environment.print:Format.formatter -> Environment.t -> unit) a.env
    | Some ([]) -> Format.asprintf "⊤ (env: %a)" (Environment.print:Format.formatter -> Environment.t -> unit) a.env
    | Some (m) -> Format.asprintf "%s env %a" (Matrix.show m) (Environment.print:Format.formatter -> Environment.t -> unit) a.env

  let pretty () (x:t) = text (show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>\nmatrix\n</key>\n<value>\n%s</value>\n<key>\nenv\n</key>\n<value>\n%s</value>\n</map>\n</value>\n" (XmlUtil.escape (Format.asprintf "%s" (show x) )) (XmlUtil.escape (Format.asprintf "%a" (Environment.print: Format.formatter -> Environment.t -> unit) (x.env)))
  let equal a b =
    match a.d, b.d with
    | None, None -> true
    | Some (x1), Some(x2) -> Matrix.equal x1 x2
    | _ -> false

  let equal a b =
    let res = equal a b in
    if M.tracing then M.tracel "eq" "equal a: %s b: %s -> %b \n" (show a) (show b) res ;
    res

  let hash t =
    Hashtbl.hash t
  let compare a b = Stdlib.compare a b

  let compare a b =
    let res = compare a b in
    if M.tracing then M.tracel "cmp" "compare a: %s b: %s -> %i \n" (show a) (show b) res ;
    res

  let name () = "affeq"
  let to_yojson a = failwith "ToDo Implement in future"
  let invariant a b = Invariant.none
  let arbitrary () = failwith "no arbitrary"
  let leq a b =
    match a.d, b.d with
    | None, _ ->  true
    | _ , Some ([]) -> true
    | Some (x), Some (y) -> Matrix.is_contained_in y x (*Not sufficient! There could be rows inbetween*)
    | _ -> false
  let leq a b =
    let res = leq a b in
    if M.tracing then M.tracel "leq" "leq a: %s b: %s -> %b \n" (show a) (show b) res ;
    res

  let case_two a r col_b =
    let rec append_zeros v r = if List.compare_length_with v r = (-1) then append_zeros (List.append v [to_rt 0]) r else v in
    let a_r = Matrix.get_row a r in
    let mapping = List.map2i (function i -> function x -> function y -> if i < r then
          Vector.add_vecs x (Vector.mul_by_constant y a_r) else x ) a (append_zeros col_b (List.length a))
    in Matrix.remove_row mapping r

  let case_three a b col_a col_b max =
    let col_a, col_b = Vector.keep_rows col_a max, Vector.keep_rows col_b max in
    if Vector.equal col_a col_b then (a, b, max) else
      let r, diff = Vector.find_last_diff col_a col_b  in
      let a_t, b_t = Matrix.get_row a r, Matrix.get_row b r in
      let rec multiply_by_t m col_v t i =
        match m, col_v with
        | x :: xs, c :: cs->  if i > 0 then let beta = c /: diff in
            let mul_t = Vector.mul_by_constant beta t in
            Vector.sub_vecs x mul_t :: multiply_by_t xs cs t (i - 1)
          else xs
        | _, _ -> []
      in
      let sub_col = List.map2 (function x -> function y -> x -: y) col_a col_b in
      Matrix.remove_row (multiply_by_t a sub_col a_t max) r, Matrix.remove_row (multiply_by_t b sub_col b_t max) r, (max - 1)

  let rec lin_disjunc r s a b =
    if s >= Matrix.num_cols a then a
    else
      let col_a, col_b = Matrix.get_col a s, Matrix.get_col b s in
      match Int.compare (Matrix.num_rows a) r, Int.compare (Matrix.num_rows b) r with
      | 1 , 1 -> let a_rs, b_rs = Mpqf.to_float (Vector.get_val r col_a), Mpqf.to_float (Vector.get_val r col_b) in
        (match  a_rs, b_rs with
         |  1., 1. -> lin_disjunc (r + 1) (s + 1) a b
         | 1., 0. -> lin_disjunc r (s + 1) (case_two a r col_b) b
         | 0., 1. -> lin_disjunc r (s + 1) a (case_two b r col_a)
         | 0., 0. ->  let new_a, new_b, new_r = case_three a b col_a col_b r in
           lin_disjunc new_r (s + 1) new_a new_b
         | _, _ -> failwith "Matrix not normalized")
      | 1 , _  -> let a_rs = Vector.get_val r col_a in
        if a_rs = (to_rt 1) then lin_disjunc r (s + 1) (case_two a r col_b) b
        else
          let new_a, new_b, new_r = case_three a b col_a col_b r in
          lin_disjunc new_r (s + 1) new_a new_b
      | _ , 1  -> let b_rs = Vector.get_val r col_b in
        if b_rs = (to_rt 1) then lin_disjunc r (s + 1) a (case_two b r col_a)
        else
          let new_a, new_b, new_r = case_three a b col_a col_b r in
          lin_disjunc new_r (s + 1) new_a new_b
      | _, _ -> let new_a, new_b, new_r = case_three a b col_a col_b r in
        lin_disjunc new_r (s + 1) new_a new_b

  let join a b =
    match a.d, b.d with
    | None, m -> b
    | m, None -> a
    | Some (x), Some (y) when x = [] || y = [] -> {d = Some ([]); env = a.env}
    | Some (x), Some(y) -> {d = Some(lin_disjunc 0 0 x y); env = a.env}

  let join a b =
    let res = join a b in
    if M.tracing then M.tracel "join" "join a: %s b: %s -> %s \n" (show a) (show b) (show res) ;
    res

  let meet a b =
    match (a.d, b.d) with
    | Some([]), y -> {d = y; env = a.env}
    | x , Some([])  -> {d = x; env = a.env}
    | Some(x), Some(y) ->
      let rref_matr = Matrix.normalize_opt (Matrix.append_matrices x y) in
      {d = rref_matr; env = a.env}
    | _, _ -> {d = None; env = a.env}
  let meet a b =
    let res = meet a b in
    if M.tracing then M.tracel "meet" "meet \n a: %s \n b: %s -> %s \n" (show a) (show b) (show res) ;
    res
  let widen a b = join a b
  let narrow a b = meet a b
  let pretty_diff () (x, y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let bot () =
    {d = None; env = Environment.make [||] [||]}
  let is_bot a = a.d = None
  let is_bot_env a =
    match a.d with
    | None -> true
    | _ -> false
  let top () =
    {d = Some (Matrix.empty ()); env = Environment.make [||] [||] }
  let is_top a = a.d = Some (Matrix.empty ())

  (*Parses a Texpr to obtain a coefficient + const (last entry) vector to repr. an affine relation.*)
  let get_coeff_vec env texp =
    let exception NotLinear in
    let zero_vec = Vector.create_zero_vec ((Environment.size env) + 1) in
    let rec convert_texpr env texp =
      (match texp with
       | Cst x ->  Vector.set_val zero_vec ((Vector.length zero_vec) - 1) (to_rt (EnvDomain.int_of_cst x))
       | Var x ->  Vector.set_val zero_vec (Environment.dim_of_var env x) (to_rt 1)
       | Unop (u, e, _, _) -> (
           match u with
           | Neg -> Vector.neg (convert_texpr env e) (* Multiply by -1*)
           | Cast -> convert_texpr env e (*Ignore*)
           | Sqrt -> raise NotLinear )(*Maybe works if only constant?*)
       | Binop (b, e1, e2, _, _) -> (
           match b with
           | Add ->  Vector.add_vecs (convert_texpr env e1) (convert_texpr env e2) (*Simply add vectors*)
           | Sub -> Vector.add_vecs (convert_texpr env  e1) (Vector.neg (convert_texpr env e2)) (*Subtract them*)
           | Mul -> let x1, x2 = convert_texpr env  e1, convert_texpr env e2 in
             if Vector.is_constant x1 || Vector.is_constant x2
             then Vector.mul_vecs x1 x2
             else raise NotLinear (*Var * Var is invalid!*)
           | _ -> raise NotLinear (*rest is not valid!*)) )
    in match convert_texpr env texp with
    | exception NotLinear -> None
    | x -> Some(x)

  let assign_invertible_rels x var b env =
    let j0 = Environment.dim_of_var env var in
    let a_j0 = Matrix.get_col x j0  in (*Corresponds to Axj0*)
    let b0 = (Vector.get_val j0 b) in
    let reduced_a = Vector.divide_by_const a_j0 b0 in  (*Corresponds to Axj0/Bj0*)
    let rec recalc_entries m rd_a =
      match m, rd_a with
      | [], [] -> []
      | x::xs, y::ys -> List.map2i (function j -> function z -> function d ->
          if j = j0 then y
          else if j < ((Vector.length b) -1) then z -: y *: d
          else z +: y *: d) x b :: recalc_entries xs ys
      | _, _ -> failwith "Unequal sizes"
    in {d = Some (recalc_entries x reduced_a); env = env}

  let assign_uninvertible_rel x var b env =
    let neg_vec = List.mapi (function i -> function z -> if i < (Vector.length b) - 1 then to_rt (-1) *: z else z) b
    in let var_vec = Vector.set_val neg_vec (Environment.dim_of_var env var) (to_rt 1)
    in {d = Some (Matrix.normalize (Matrix.append_row x var_vec)); env = env}

  let remove_rels_with_var x var env =
    let j0 = Environment.dim_of_var env var
    in let n = Vector.get_rownum_not_zero (Matrix.get_col x j0)
    in
    match n with
    | None -> x
    | Some (r, _) -> Matrix.reduce_row_to_zero x r j0

  let assign_texpr t var texp =
    let is_invertible v = Vector.get_val (Environment.dim_of_var t.env var) v <> (to_rt 0)
    in
    let affineEq_vec = get_coeff_vec t.env texp in
    match t.d, affineEq_vec with
    | Some ([]), Some(v)
    | None, Some(v) -> if is_invertible v then t (*top/bottom and inv. assign = top*)
      else assign_uninvertible_rel [] var v t.env
    | Some (x), Some (v) -> if is_invertible v then assign_invertible_rels x var v t.env
      else let new_x = remove_rels_with_var x var t.env in
        assign_uninvertible_rel new_x var v t.env
    | _, _ -> t

  let assign_exp t var exp =
    match Convert.texpr1_expr_of_cil_exp t t.env exp with
    | exp -> assign_texpr t var exp
    | exception Convert.Unsupported_CilExp -> match t.d with
      | None -> t
      | Some(x) -> forget_vars t [var]

  let assign_exp t var exp =
    let res = assign_exp t var exp in
    res

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

  let substitute_exp t var exp =
    let b = get_coeff_vec t.env (Convert.texpr1_expr_of_cil_exp t t.env exp) in
    match b, t.d with
    | Some (x), Some(m) -> let dim_var = Environment.dim_of_var t.env var in
      if Vector.get_val dim_var x = (to_rt 0) then
        meet {d = Some (remove_rels_with_var m var t.env); env = t.env} {d = Some [x]; env = t.env}
      else assign_invertible_rels m var x t.env
    | None, Some(m) -> {d = Some (Matrix.normalize (remove_rels_with_var m var t.env)); env = t.env}
    | _, _ -> t

  let substitute_exp t var exp =
    let res = substitute_exp t var exp
    in if M.tracing then M.tracel "ops" "Substitute_expr\n"; res

  let exp_is_cons = function
    (* constraint *)
    | BinOp ((Lt | Gt | Le | Ge | Eq | Ne), _, _, _) -> true
    (* expression *)
    | _ -> false


  (** Assert a constraint expression. *)
  let rec meet_with_tcons d tcons original_expr negate =
    match Tcons1.get_typ tcons with
    | EQ ->
      if M.tracing then M.tracel "asserts" "EQ meet_with_tcons %s\n" (Format.asprintf "%a" (Tcons1.print: Format.formatter -> Tcons1.t -> unit) tcons);
      begin match get_coeff_vec d.env (Texpr1.to_expr (Tcons1.get_texpr1 tcons)) with
        | None -> d
        | Some (e) ->  meet d {d = Some ([e]); env = d.env}
      end
    | DISEQ ->
      if M.tracing then M.tracel "asserts" "DISEQ meet_with_tcons %s\n" (Format.asprintf "%a" (Tcons1.print: Format.formatter -> Tcons1.t -> unit) tcons);
      begin match get_coeff_vec d.env (Texpr1.to_expr (Tcons1.get_texpr1 tcons)) with
        | None -> d
        | Some (e) -> if equal (meet d {d = Some ([e]); env = d.env}) d then {d = None; env = d.env} else d
      end
    | SUP -> if M.tracing then M.tracel "asserts" "SUP meet_with_tcons: %s \n" (Format.asprintf "%a" (Texpr1.print: Format.formatter -> Texpr1.t -> unit) (Tcons1.get_texpr1 tcons));
      let coeff_vec = get_coeff_vec d.env (Texpr1.to_expr (Tcons1.get_texpr1 tcons)) in
      begin match coeff_vec, d.d with
        | Some (vec), Some (m) when meet {d = Some [vec]; env = d.env} d = d -> {d = None; env = d.env} (*e.g: x + y > 0 = None if x + y = 0*)
        | None, Some(m) -> begin match calc_const m d.env (Texpr1.to_expr (Tcons1.get_texpr1 tcons)) with
            | None -> d
            | Some (c) -> if c <=: (to_rt 0) then {d = None; env = d.env} else d end
        | _ -> d
      end
    | SUPEQ -> if M.tracing then M.tracel "asserts" "SUPEQ meet_with_tcons: %s \n" (Format.asprintf "%a" (Tcons1.print: Format.formatter -> Tcons1.t -> unit) tcons);
      let coeff_vec = get_coeff_vec d.env (Texpr1.to_expr (Tcons1.get_texpr1 tcons)) in
      begin match coeff_vec, d.d with
        | None, Some(m) -> begin match calc_const m d.env (Texpr1.to_expr (Tcons1.get_texpr1 tcons)) with
            | None -> d
            | Some (c) -> if c <: (to_rt 0) then {d = None; env = d.env} else d end
        | _ -> d
      end
    | _ -> d

  let rec assert_cons d e negate =
    begin match Convert.tcons1_of_cil_exp d (d.env) e negate with
      | tcons1 -> meet_with_tcons d tcons1 e negate
      | exception Convert.Unsupported_CilExp ->
        d
    end

  (** Assert any expression. *)
  let assert_inv d e negate =
    let e' =
      if exp_is_cons e then
        e
      else
        (* convert non-constraint expression, such that we assert(e != 0) *)
        BinOp (Ne, e, zero, intType)
    in
    assert_cons d e' negate


  let assert_inv d e negate =
    let res = assert_inv d e negate in
    if M.tracing then M.tracel "assert" "assert_inv: d: %s expression: %s negate: %b -> %s\n" (show d) (Pretty.sprint 1 (Cil.printExp Cil.defaultCilPrinter () e)) negate (show res);
    res


  let check_asserts d e =
    if is_bot_env (assert_inv d e false) then
      `False
    else if is_bot_env (assert_inv d e true) then
      `True
    else
      `Top

  let check_assert d e =
    let res = check_asserts d e in
    if M.tracing then M.tracel "assert" "check_assert: %s \n" (show d);
    res


  (** Evaluate non-constraint expression as interval. *)
  let eval_interval_expr d e =
    match Convert.texpr1_of_cil_exp d (d.env) e with
    | texpr1 -> ExpressionBounds.bound_texpr d texpr1 (Cilfacade.get_ikind_exp e)
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
    let res = eval_int d e in
    if M.tracing then M.tracel "assert" "Eval Int: matrix: %s expr: %s -> %s \n" (show d) (Pretty.sprint 1 (Cil.printExp Cil.defaultCilPrinter () e)) (IntDomain.IntDomTuple.show res) ;
    res

  let unify a b =
    let new_env, a_change, b_change  = Environment.lce_change a.env b.env in
    let reduced_a = match a_change with
      | None -> a
      | Some (change) -> begin match a.d with
          | None -> {d = None; env = new_env}
          | Some (d) -> {d = Some (dim_remove change d); env = new_env}
        end
    in
    let reduced_b = match b_change with
      | None -> b
      | Some (change) -> begin match b.d with
          | None -> {d = None; env = new_env}
          | Some (d) -> {d = Some (dim_remove change d); env = new_env}
        end
    in
    meet reduced_a reduced_b

  let unify a b =
    let res = unify a b in
    if M.tracing then M.tracel "ops" "unify\n";
    res

  type marshal = t

  let marshal t = t

  let unmarshal t = t

  let relift t = t

end