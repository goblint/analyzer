open Prelude
open Pretty
module M = Messages
open Apron
open VectorMatrix

module Mpqf = struct
  include Mpqf

  let get_den x = Z_mlgmpidl.z_of_mpzf @@ Mpqf.get_den x

  let get_num x = Z_mlgmpidl.z_of_mpzf @@ Mpqf.get_num x
end

module Var = SharedFunctions.Var
module V = RelationDomain.V(Var)

module VarManagement (Vec: AbstractVector) (Mx: AbstractMatrix)=
struct
  include SharedFunctions.EnvOps
  module Vector = Vec (Mpqf)
  module Matrix = Mx(Mpqf) (Vec)

  type t = {
    d :  Matrix.t Option.t;
    env : Environment.t
  }

  let empty_env = Environment.make [||] [||]

  let bot () =
    {d = Some (Matrix.empty ()); env = empty_env}

  let bot_env = {d = None; env = empty_env}

  let is_bot_env t = t.d = None

  let copy_pt t = t

  let dim_add (ch: Apron.Dim.change) m =
    Array.iteri (fun i x -> ch.dim.(i) <- x + i) ch.dim;
    Matrix.add_empty_columns m ch.dim

  let dim_add ch m = Stats.time "dim add" (dim_add ch) m

  let dim_remove (ch: Apron.Dim.change) m del =
    if Array.length ch.dim = 0 || Matrix.is_empty m then m else (
      Array.iteri (fun i x-> ch.dim.(i) <- x + i) ch.dim;
      let m' = if not del then let m = Matrix.copy_pt m in Array.fold_left (fun y x -> Matrix.reduce_col_pt_with y x) m ch.dim else m in
      Matrix.remove_zero_rows @@ Matrix.del_cols m' ch.dim)

  let dim_remove ch m del = Stats.time "dim remove" (dim_remove ch m) del

  let change_d t new_env add del =
    if Environment.equal t.env new_env then t else
      let dim_change = if add then Environment.dimchange t.env new_env
        else Environment.dimchange new_env t.env
      in match t.d with
      | None -> bot_env
      | Some m -> {d = Some (if add then dim_add dim_change m else dim_remove dim_change m del); env = new_env}

  let change_d t new_env add del = Stats.time "dimension change" (change_d t new_env add) del

  let add_vars t vars =
    let vs' =
      vars
      |> List.enum
      |> Enum.filter (fun v -> not (Environment.mem_var t.env v))
      |> Array.of_enum in
    let env' = Environment.add t.env vs' [||] in
    change_d t env' true false

  let add_vars t vars = Stats.time "add_vars" (add_vars t) vars

  let drop_vars t vars del =
    let vs' =
      vars
      |> List.enum
      |> Enum.filter (fun v -> Environment.mem_var t.env v)
      |> Array.of_enum
    in
    let env' = Environment.remove t.env vs' in
    change_d t env' false del

  let drop_vars t vars = Stats.time "drop_vars" (drop_vars t) vars

  let remove_vars t vars = drop_vars t vars false

  let remove_vars t vars = Stats.time "remove_vars" (remove_vars t) vars

  let remove_vars_pt_with t vars =
    remove_vars t vars

  let remove_filter t f =
    let env' = remove_filter t.env f in
    change_d t env' false false

  let remove_filter t f = Stats.time "remove_filter" (remove_filter t) f

  let remove_filter_pt_with t f =
    remove_filter t f

  let keep_filter t f =
    let env' = keep_filter t.env f in
    change_d t env' false false

  let keep_filter t f = Stats.time "keep_filter" (keep_filter t) f

  let keep_vars t vs =
    let env' = keep_vars t.env vs in
    change_d t env' false false

  let keep_vars t vs = Stats.time "keep_vars" (keep_vars t) vs

  let vars t = vars t.env

  let mem_var t var = Environment.mem_var t.env var

  include ConvenienceOps(Mpqf)

  let get_c v = match Vector.findi (fun x -> x <> (of_int 0)) v with
    | exception Not_found -> Some (of_int 0)
    | i when Vector.compare_length_with v (i + 1) = 0 -> Some (Vector.nth v i)
    | _ -> None

  let get_coeff_vec (t: t) texp =
    (*Parses a Texpr to obtain a coefficient + const (last entry) vector to repr. an affine relation.
      Returns None if the expression is not affine linear*)
    let open Apron.Texpr1 in
    let exception NotLinear in
    let zero_vec = Vector.zero_vec @@ Environment.size t.env + 1 in
    let neg = Vector.map_pt_with (fun x -> (of_int (-1)) *: x) in
    let is_const_vec v = Vector.compare_length_with (Vector.filteri (fun i x -> (*Inefficient*)
        Vector.compare_length_with v (i + 1) > 0 && x <> of_int 0) v) 1 = 0
    in
    let rec convert_texpr texp =
      begin match texp with
        | Cst x -> let of_union union =
                     let open Coeff in
                     match union with
                     | Interval _ -> failwith "Not a constant"
                     | Scalar x -> (match x with
                         | Float x -> Mpqf.of_float x
                         | Mpqf x -> x
                         | Mpfrf x -> Mpfr.to_mpq x) in Vector.set_val zero_vec ((Vector.length zero_vec) - 1) (of_union x)
        | Var x ->
          (*If x is a constant, replace it with its const. val. immediately*)
          let zero_vec_cp = Vector.copy_pt_with zero_vec in
          let entry_only v = Vector.set_val_pt_with v(Environment.dim_of_var t.env x) (of_int 1) in
          begin match t.d with
            | Some m -> let row = Matrix.find_opt (fun r -> Vector.nth r (Environment.dim_of_var t.env x) = of_int 1) m in
              begin match row with
                | Some v when is_const_vec v ->
                  Vector.set_val_pt_with zero_vec_cp ((Vector.length zero_vec) - 1) (Vector.nth v (Vector.length v - 1))
                | _ -> entry_only zero_vec_cp end
            | None -> entry_only zero_vec_cp end
        | Unop (u, e, _, _) ->
          begin match u with
            | Neg -> neg @@ convert_texpr e
            | Cast -> convert_texpr e (*Ignore*)
            | Sqrt -> raise NotLinear end
        | Binop (b, e1, e2, _, _) ->
          begin match b with
            | Add ->  Vector.map2_pt_with (+:)(convert_texpr e1) (convert_texpr e2)
            | Sub -> Vector.map2_pt_with (+:) (convert_texpr  e1) (neg @@ convert_texpr e2)
            | Mul ->
              let x1, x2 = convert_texpr e1, convert_texpr e2 in
              begin match get_c x1, get_c x2 with
                | _, Some c -> Vector.apply_with_c_pt_with ( *:) c x1
                | Some c, _ -> Vector.apply_with_c_pt_with ( *:) c x2
                | _, _ -> raise NotLinear end
            | _ -> raise NotLinear end
      end
    in match convert_texpr texp with
    | exception NotLinear -> None
    | x -> Some(x)

  let get_coeff_vec t texp = Stats.time "coeff_vec" (get_coeff_vec t) texp
end

module ExpressionBounds (Vc: AbstractVector) (Mx: AbstractMatrix): (SharedFunctions.ConvBounds with type t = VarManagement(Vc) (Mx).t) =
struct
  include VarManagement (Vc) (Mx)

  let bound_texpr t texpr =
    let texpr = Texpr1.to_expr texpr in
    match get_coeff_vec t texpr  with
    | Some v -> begin match get_c v with
        | Some c when Mpqf.get_den c = IntOps.BigIntOps.of_int 1 ->
          let int_val = Mpqf.get_num c
          in Some int_val, Some int_val
        | _ -> None, None end
    | _ -> None, None


  let bound_texpr d texpr1 =
    let res = bound_texpr d texpr1 in
    match res with
    | Some min, Some max ->  if M.tracing then M.tracel "bounds" "min: %s max: %s" (IntOps.BigIntOps.to_string min) (IntOps.BigIntOps.to_string max); res
    | _ -> res

  let bound_texpr d texpr1 = Stats.time "bounds calculation" (bound_texpr d) texpr1
end

module D2(Vc: AbstractVector) (Mx: AbstractMatrix): (SharedFunctions.AssertionRelS with type var = V.t)=
struct
  include ConvenienceOps (Mpqf)
  include VarManagement (Vc) (Mx)

  module Bounds = ExpressionBounds (Vc) (Mx)

  module Convert = SharedFunctions.Convert (V) (Bounds) (struct let allow_global = true end) (RelationDomain.Tracked)

  type var = V.t

  let tag t = failwith "No tag"

  let show t =
    let conv_to_ints row =
      let module BI = IntOps.BigIntOps in
      let row = Array.copy @@ Vector.to_array row
      in
      for i = 0 to Array.length row -1 do
        let val_i = Mpqf.of_mpz @@ Z_mlgmpidl.mpzf_of_z @@ Mpqf.get_den row.(i)
        in Array.iteri(fun j x -> row.(j) <- val_i *: x)  row
      done;
      let int_arr = Array.init (Array.length row) (fun i -> Mpqf.get_num row.(i))
      in let div = Mpqf.of_mpz @@ Z_mlgmpidl.mpzf_of_z @@ Array.fold_left BI.gcd int_arr.(0) int_arr
      in Array.iteri (fun i x -> row.(i) <- x /: div) row;
      Vector.of_array @@ row
    in
    let vec_to_constraint vec env = 
      let vars, _ = Environment.vars env
      in let dim_to_str var = 
           let vl =  Vector.nth vec (Environment.dim_of_var env var)
           in let var_str = Var.to_string var
           in if vl = of_int 1 then "+" ^ var_str 
           else if vl = of_int (-1) then "-" ^ var_str
           else if vl <: of_int (-1) then Mpqf.to_string vl ^ var_str
           else if vl >: of_int 1 then Format.asprintf "+%s" (Mpqf.to_string vl) ^ var_str
           else ""
      in
      let c_to_str vl =
        if vl >: of_int 0 then "-" ^ Mpqf.to_string vl
        else if vl <: of_int 0 then "+" ^ Mpqf.to_string vl
        else ""
      in
      let res = (String.concat "" @@ Array.to_list @@ Array.map dim_to_str vars)
                ^ (c_to_str @@ Vector.nth vec (Vector.length vec - 1)) ^ "=0"
      in if String.starts_with res "+" then String.sub res 1 (String.length res - 1) else res
    in
    match t.d with
    | None -> "Bottom Env"
    | Some m when Matrix.is_empty m -> "⊤"
    | Some m -> 
      let constraint_list = List.init (Matrix.num_rows m) (fun i -> vec_to_constraint (conv_to_ints @@ Matrix.get_row m i) t.env)
      in Format.asprintf "%s" ("[|"^ (String.concat "; " constraint_list) ^"|]") 

  let pretty () (x:t) = text (show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>\nmatrix\n</key>\n<value>\n%s</value>\n<key>\nenv\n</key>\n<value>\n%s</value>\n</map>\n</value>\n" (XmlUtil.escape (Format.asprintf "%s" (show x) )) (XmlUtil.escape (Format.asprintf "%a" (Environment.print: Format.formatter -> Environment.t -> unit) (x.env)))
  let equal t1 t2 =
    Environment.equal t1.env t2.env &&
    match t1.d, t2.d with
    | None, None -> true
    | Some x1, Some x2 -> Matrix.equal x1 x2
    | _ -> false

  let hash t =
    Hashtbl.hash t

  let compare t1 t2 = Stdlib.compare t1 t2

  let name () = "affeq"

  let to_yojson t = failwith "ToDo Implement in future"

  let arbitrary () = failwith "no arbitrary"


  let is_bot t = equal t (bot ())

  let bot_env = {d = None; env = Environment.make [||] [||]}

  let is_bot_env t = t.d = None

  let top () = failwith "D2.top ()"

  let is_top t = false

  let is_top_env t = (not @@ Environment.equal empty_env t.env) && Option.is_some t.d && Matrix.is_empty (Option.get t.d)

  let meet t1 t2 =
    let sup_env = Environment.lce t1.env t2.env in
    let t1, t2 = change_d t1 sup_env true false, change_d t2 sup_env true false
    in if is_bot t1 || is_bot t2 then bot() else
      let m1, m2 = Option.get t1.d, Option.get t2.d in
      match m1, m2 with
      | x, y when is_top_env t1-> {d = Some (dim_add (Environment.dimchange t2.env sup_env) y); env = sup_env}
      | x, y when is_top_env t2 -> {d = Some (dim_add (Environment.dimchange t1.env sup_env) x); env = sup_env}
      | x, y ->
        let rref_matr = Matrix.rref_matrix_with (Matrix.copy_pt x) (Matrix.copy_pt y) in
        if Option.is_none rref_matr then bot () else
          {d = rref_matr; env = sup_env}


  let meet t1 t2 =
    let res = meet t1 t2 in
    if M.tracing then M.tracel "meet" "meet a: %s b: %s -> %s \n" (show t1) (show t2) (show res) ;
    res

  let meet t1 t2 = Stats.time "meet" (meet t1) t2

  let leq t1 t2 =
    let env_comp = Environment.compare t1.env t2.env in
    if env_comp = -2 || env_comp > 0 then false else
    if is_bot t1 || is_top_env t2 then true else
    if is_bot t2 || is_top_env t1 then false else (
      let m1, m2 = Option.get t1.d, Option.get t2.d in
      let m1' = if env_comp = 0 then m1 else dim_add (Environment.dimchange t1.env t2.env) m1 in
      Matrix.is_covered_by m2 m1')

  let leq a b = Stats.time "leq" (leq a) b

  let leq t1 t2 =
    let res = leq t1 t2 in
    if M.tracing then M.tracel "leq" "leq a: %s b: %s -> %b \n" (show t1) (show t2) res ;
    res

  let join a b =
    let rec lin_disjunc r s a b =
      if s >= Matrix.num_cols a then a else
        let case_two a r col_b =
          let a_r = Matrix.get_row a r in
          let mapping = Matrix.map2i_pt_with (fun i x y -> if i < r then
                                                 Vector.map2_pt_with (fun u j -> u +: y *: j) x a_r else x) a col_b
          in Matrix.remove_row mapping r
        in
        let case_three a b col_a col_b max =
          let col_a, col_b = Vector.copy_pt_with col_a, Vector.copy_pt_with col_b in
          let col_a, col_b = Vector.keep_vals col_a max, Vector.keep_vals col_b max in
          if Vector.equal col_a col_b then (a, b, max) else
            let a_rev, b_rev = Vector.rev_pt_with col_a,  Vector.rev_pt_with col_b in
            let i = Vector.find2i (fun x y -> x <> y) a_rev b_rev in
            let (x, y) = Vector.nth a_rev i, Vector.nth b_rev i in
            let r, diff = Vector.length a_rev - (i + 1), x -: y  in
            let a_r, b_r = Matrix.get_row a r, Matrix.get_row b r in
            let sub_col = Vector.rev_pt_with @@ Vector.map2_pt_with (fun x y -> x -: y) a_rev b_rev in
            let multiply_by_t m t =
              Matrix.map2i_pt_with (fun i' x c -> if i' <= max then let beta = c /: diff in
                                       Vector.map2_pt_with (fun u j -> u -: (beta *: j)) x t else x) m sub_col
            in
            Matrix.remove_row (multiply_by_t a a_r) r, Matrix.remove_row (multiply_by_t b b_r) r, (max - 1)
        in
        let col_a, col_b = Matrix.get_col a s, Matrix.get_col b s in
        let nth_zero v i =  match Vector.nth v i with
          | exception Invalid_argument _ -> of_int 0
          | x -> x
        in
        let a_rs, b_rs = nth_zero col_a r, nth_zero col_b r in
        if Mpqf.get_den a_rs <> (IntOps.BigIntOps.of_int 1) || Mpqf.get_den b_rs <> (IntOps.BigIntOps.of_int 1) then failwith "Matrix not in rref form" else
          begin match Int.of_float @@ Mpqf.to_float @@ a_rs, Int.of_float @@ Mpqf.to_float @@ b_rs with
            | 1, 1 -> lin_disjunc (r + 1) (s + 1) a b
            | 1, 0 -> lin_disjunc r (s + 1) (case_two a r col_b) b
            | 0, 1 -> lin_disjunc r (s + 1) a (case_two b r col_a)
            | 0, 0 ->  let new_a, new_b, new_r = case_three a b col_a col_b r in
              lin_disjunc new_r (s + 1) new_a new_b
            | _      -> failwith "Matrix not in rref form" end
    in
    if is_bot a then b else if is_bot b then a else
      match Option.get a.d, Option.get b.d with
      | x, y when is_top_env a || is_top_env b -> {d = Some (Matrix.empty ()); env = Environment.lce a.env b.env}
      | x, y when (Environment.compare a.env b.env <> 0) ->
        let sup_env = Environment.lce a.env b.env in
        let mod_x = dim_add (Environment.dimchange a.env sup_env) x in
        let mod_y = dim_add (Environment.dimchange b.env sup_env) y in
        {d = Some (lin_disjunc 0 0 (Matrix.copy_pt mod_x) (Matrix.copy_pt mod_y)); env = sup_env}
      | x, y when Matrix.equal x y -> {d = Some x; env = a.env}
      | x, y  -> {d = Some(lin_disjunc 0 0 (Matrix.copy_pt x) (Matrix.copy_pt y)); env = a.env}

  let join a b = Stats.time "join" (join a) b

  let join a b =
    let res = join a b in
    if M.tracing then M.tracel "join" "join a: %s b: %s -> %s \n" (show a) (show b) (show res) ;
    res
  let widen a b =
    let a_env = a.env in
    let b_env = b.env in
    if Environment.equal a_env b_env  then
      join a b
    else b

  let narrow a b = a
  let pretty_diff () (x, y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  let remove_rels_with_var x var env imp =
    let j0 = Environment.dim_of_var env var in
    if imp then Matrix.reduce_col_pt_with x j0 else Matrix.reduce_col x j0

  let remove_rels_with_var x var env imp = Stats.time "remove_rels_with_var" (remove_rels_with_var x var env) imp

  let forget_vars t vars =
    if is_bot t || is_top_env t then t
    else
      let m = Option.get t.d in
      if List.is_empty vars then t else
        let rec rem_vars m vars' =
          begin match vars' with
            |            [] -> m
            | x :: xs -> rem_vars (remove_rels_with_var m x t.env true) xs end
        in {d = Some (Matrix.remove_zero_rows @@ rem_vars (Matrix.copy_pt m) vars); env = t.env}

  let forget_vars t vars =
    let res = forget_vars t vars in
    if M.tracing then M.tracel "ops" "forget_vars %s -> %s\n" (show t) (show res);
    res

  let forget_vars t vars = Stats.time "forget_vars" (forget_vars t) vars

  let assign_texpr (t: VarManagement(Vc)(Mx).t) var texp =
    let assign_invertible_rels x var b env =
      let j0 = Environment.dim_of_var env var in
      let a_j0 = Matrix.get_col x j0  in (*Corresponds to Axj0*)
      let b0 = Vector.nth b j0 in
      let reduced_a = Vector.apply_with_c_pt_with (/:) b0 a_j0 in  (*Corresponds to Axj0/Bj0*)
      let recalc_entries m rd_a = Matrix.map2_pt_with (fun x y -> Vector.map2i_pt_with (fun j z d ->
          if j = j0 then y
          else if Vector.compare_length_with b (j + 1) > 0 then z -: y *: d
          else z +: y *: d) x b) m rd_a
      in
      let opt_m = Matrix.normalize_pt_with @@ recalc_entries x reduced_a
      in if Option.is_none opt_m then bot () else {d = opt_m; env = env}
    in
    let assign_invertible_rels x var b env = Stats.time "assign_invertible" (assign_invertible_rels x var b) env in
    let assign_uninvertible_rel x var b env =
      let b_length = Vector.length b in
      let neg_vec = Vector.mapi_pt_with (fun i z -> if i < b_length - 1 then of_int (-1) *: z else z) b
      in let var_vec = Vector.set_val_pt_with neg_vec (Environment.dim_of_var env var) (of_int 1)
      in let opt_m = Matrix.rref_vec_with x var_vec in
      if Option.is_none opt_m then bot () else
        {d = opt_m; env = env}
    in
    (* let assign_uninvertible_rel x var b env = Stats.time "assign_uninvertible" (assign_uninvertible_rel x var b) env in *)
    let is_invertible v = Vector.nth v @@ Environment.dim_of_var t.env var <> of_int 0
    in let affineEq_vec = get_coeff_vec t texp
    in if is_bot t then t else let m = Option.get t.d in
      match affineEq_vec with
      | Some v when is_top_env t -> if is_invertible v then t else assign_uninvertible_rel m var v t.env
      | Some v -> if is_invertible v then let t' = assign_invertible_rels (Matrix.copy_pt m) var v t.env in {d = t'.d; env = t'.env}
        else let new_m = Matrix.remove_zero_rows @@ remove_rels_with_var m var t.env false
          in assign_uninvertible_rel new_m var v t.env
      | None -> {d = Some (Matrix.remove_zero_rows @@ remove_rels_with_var m var t.env false); env = t.env}

  let assign_texpr t var texp = Stats.time "assign_texpr" (assign_texpr t var) texp

  let assign_exp (t: VarManagement(Vc)(Mx).t) var exp (no_ov: bool Lazy.t) =
    let t = if not @@ Environment.mem_var t.env var then add_vars t [var] else t in
    match Convert.texpr1_expr_of_cil_exp t t.env exp (Lazy.force no_ov) with
    | exp -> assign_texpr t var exp
    | exception Convert.Unsupported_CilExp _ ->
      if is_bot t then t else forget_vars t [var]

  let assign_exp t var exp no_ov =
    let res = assign_exp t var exp no_ov in
    if M.tracing then M.tracel "ops" "assign_exp t:\n %s \n var: %s \n exp: %s\n no_ov: %b -> \n %s\n"
        (show t) (Var.to_string var) (Pretty.sprint ~width:1 (Cil.printExp Cil.defaultCilPrinter () exp)) (Lazy.force no_ov) (show res) ;
    res
  let assign_var (t: VarManagement(Vc)(Mx).t) v v' =
    let t = add_vars t [v; v'] in
    let texpr1 = Texpr1.of_expr (t.env) (Var v') in
    assign_texpr t v (Apron.Texpr1.to_expr texpr1)

  let assign_var t v v' =
    let res = assign_var t v v' in
    if M.tracing then M.tracel "ops" "assign_var t:\n %s \n v: %s \n v': %s\n -> %s\n" (show t) (Var.to_string v) (Var.to_string v') (show res) ;
    res

  let assign_var_parallel t vv's =
    let assigned_vars = List.map (function (v, _) -> v) vv's in
    let t = add_vars t assigned_vars in
    let primed_vars = List.init (List.length assigned_vars) (fun i -> Var.of_string (Int.to_string i  ^"'")) in
    let t_primed = add_vars t primed_vars in
    let multi_t = List.fold_left2 (fun t' v_prime (_,v') -> assign_var t' v_prime v') t_primed primed_vars vv's in
    match multi_t.d with
    | Some m when not @@ is_top_env multi_t -> let replace_col m x y = let dim_x, dim_y = Environment.dim_of_var multi_t.env x, Environment.dim_of_var multi_t.env y in
                                                 let col_x = Matrix.get_col m dim_x in
                                                 Matrix.set_col_with m col_x dim_y in
      let m_cp = Matrix.copy_pt m in
      let switched_m = List.fold_left2 (fun m' x y -> replace_col m' x y) m_cp primed_vars assigned_vars in
      let res = drop_vars {d = Some switched_m; env = multi_t.env} primed_vars true in
      let opt_m = Matrix.normalize_pt_with (Option.get res.d) in
      if Option.is_none opt_m then bot () else
        {d = opt_m; env = res.env}
    | _ -> t

  let assign_var_parallel t vv's =
    let res = assign_var_parallel t vv's in
    if M.tracing then M.tracel "ops" "assign_var parallel: %s -> %s \n" (show t) (show res);
    res

  let assign_var_parallel t vv's = Stats.time "var_parallel" (assign_var_parallel t) vv's

  let assign_var_parallel_pt_with t vv's =
    assign_var_parallel t vv's

  let assign_var_parallel_pt_with t vv's =
    let res = assign_var_parallel_pt_with t vv's in
    if M.tracing then M.tracel "var_parallel" "assign_var parallel'\n";
    res

  let assign_var_parallel' t vs1 vs2 =
    let vv's = List.combine vs1 vs2 in
    assign_var_parallel t vv's

  let assign_var_parallel' t vv's =
    let res = assign_var_parallel' t vv's in
    if M.tracing then M.tracel "ops" "assign_var parallel'\n";
    res

  let substitute_exp t var exp no_ov =
    let t = if not @@ Environment.mem_var t.env var then add_vars t [var] else t in
    let res = assign_exp t var exp no_ov in
    forget_vars res [var]

  let substitute_exp t var exp ov =
    let res = substitute_exp t var exp ov
    in if M.tracing then M.tracel "ops" "Substitute_expr t: \n %s \n var: %s \n exp: %s \n -> \n %s\n" (show t) (Var.to_string var) (Pretty.sprint ~width:1 (Cil.printExp Cil.defaultCilPrinter () exp)) (show res);
    res

  let substitute_exp t var exp ov = Stats.time "substitution" (substitute_exp t var exp) ov

  (** Assert a constraint expression. *)
  let meet_tcons t tcons expr =
    let check_const cmp c = if cmp c (of_int 0) then bot_env else t
    in
    let exception NotRefinable in
    let meet_vec e =
      (*Flip the sign of the const. val in coeff vec*)
      let flip_e = Vector.mapi_pt_with (fun i x -> if Vector.compare_length_with e (i + 1) = 0 then (of_int (-1)) *: x else x) e in
      let res = if is_bot t then bot () else
          let opt_m = Matrix.rref_vec_with (Matrix.copy_pt @@ Option.get t.d) flip_e
          in if Option.is_none opt_m then bot () else {d = opt_m; env = t.env} in
      let overflow_res res = if IntDomain.should_ignore_overflow (Cilfacade.get_ikind_exp expr) then res else raise NotRefinable in
      match Convert.determine_bounds_one_var expr with
      | None -> overflow_res res
      | Some (ev, min, max) ->
        begin match Bounds.bound_texpr res (Convert.texpr1_of_cil_exp res res.env ev true) with
          | exception Convert.Unsupported_CilExp _ -> overflow_res res
          | Some b_min, Some b_max ->  let module BI = IntOps.BigIntOps in
            if min = BI.of_int 0 && b_min = b_max then raise NotRefinable
            else if (b_min < min && b_max < min) || (b_max > max && b_min > max) then
              (if GobConfig.get_string "sem.int.signed_overflow" = "assume_none" then bot () else raise NotRefinable)
            else res
          | _, _ -> overflow_res res end
    in
    match get_coeff_vec t (Texpr1.to_expr @@ Tcons1.get_texpr1 tcons) with
    | Some v -> begin match get_c v, Tcons1.get_typ tcons with
        | Some c, DISEQ -> check_const (=:) c
        | Some c, SUP -> check_const (<=:) c
        | Some c, EQ -> check_const (<>) c
        | Some c, SUPEQ -> check_const (<:) c
        | None, DISEQ | None, SUP ->
          begin match meet_vec v with
            | exception NotRefinable -> t
            | res -> if equal res t then bot_env else t end
        | None, EQ -> begin match meet_vec v with
            | exception NotRefinable -> t
            | res -> if is_bot res then bot_env else res end
        | _, _ -> t end
    | None -> t

  let meet_tcons t tcons expr = Stats.time "meet_tcons" (meet_tcons t tcons) expr

  let unify a b =
    meet a b

  let unify a b =
    let res = unify a b  in
    if M.tracing then M.tracel "ops" "unify: %s %s -> %s\n" (show a) (show b) (show res);
    res

  let assert_cons d e negate no_ov =
    let no_ov = Lazy.force no_ov in
    if M.tracing then M.tracel "assert_cons" "assert_cons with expr: %s \n %b" (Pretty.sprint ~width:1 (Cil.printExp Cil.defaultCilPrinter () e)) no_ov;
    begin match Convert.tcons1_of_cil_exp d d.env e negate no_ov with
      | tcons1 -> meet_tcons d tcons1 e
      | exception Convert.Unsupported_CilExp _ ->
        d
    end

  let assert_cons d e negate no_ov = Stats.time "assert_cons" (assert_cons d e negate) no_ov

  let relift t = t

  let env (t: Bounds.t) = t.env

  type marshal = Bounds.t

  let marshal t = t

  let unmarshal t = t

end
