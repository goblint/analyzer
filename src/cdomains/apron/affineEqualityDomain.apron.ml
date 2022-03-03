open Prelude
open Pretty
open Cil
module M = Messages
open Apron


module type Arith =
sig
  type t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val neg : t -> t
  val abs : t -> t
  val cmp : t -> t -> int
  val to_string:  t -> string
  val of_int: int -> t
end

module ConvenienceOps (A: Arith) =
struct
  let ( *: ) = A.mul
  let (+:) = A.add
  let (-:) = A.sub
  let (/:) = A.div
  let (=:) x y = A.cmp x y = 0
  let (<:) x y = A.cmp x y < 0
  let (>:) x y = A.cmp x y > 0
  let (<=:) x y = A.cmp x y <= 0
  let (>=:) x y = A.cmp x y >= 0
  let of_int x = A.of_int x
end

module type Vector =
sig
  type num
  type t

  val show: t -> string

  val equal:  t ->  t -> bool

  val keep_vals: t -> int ->  t

  val remove_val: t -> int ->  t

  val set_val: t -> int -> num ->  t

  val insert_val: int -> num ->  t ->  t

  val apply_with_c: (num -> num -> num) -> num ->  t ->  t

  val zero_vec: int -> t

  val nth: t -> int -> num

  val length: t -> int

  val map2: (num -> num -> num) -> t -> t -> t

  val findi: (int -> num -> bool) ->  t -> int * num

  val map: (num -> num) -> t -> t

  val compare_length_with: t -> int -> int

  val of_list: num list -> t

  val to_list: t -> num list

  val filteri: (int -> num -> bool) -> t -> t

  val append: t -> t -> t

  val rev: t -> t

  val map2i: (int -> num -> num -> num) -> t -> t -> t

  val mapi: (int -> num -> num) -> t -> t

  val find2i: (int -> num -> num -> bool) -> t -> t -> int * (num * num)
end

module type AbstractVector =
  functor (A: Arith) ->
  sig
    include Vector with type num:= A.t
  end

module ListVector: AbstractVector =
  functor (A: Arith) ->
  struct
    include List
    include ConvenienceOps (A)
    type t = A.t List.t

    let show t =
      let rec list_str l =
        match l with
        | [] -> "]"
        | x :: xs -> (A.to_string x) ^" "^(list_str xs)
      in
      "["^list_str t^"\n"

    let equal v1 v2 =
      equal (=:) v1 v2

    let keep_vals v n =
      filteri (fun i x -> i < n) v

    let remove_val v n =
      if n < 0 || n >= length v then failwith "Entry does not exist"
      else filteri (fun i x -> i <> n) v

    let set_val (v: A.t List.t) n (new_val: A.t) =
      if n < 0 || n >= length v then failwith "Entry does not exist"
      else mapi (fun i x -> if i = n then new_val else x) v

    let rec insert_val n vl v =
      match v with
      | [] -> if n = 0 then [vl] else failwith "Entry does not exist"
      | x :: xs -> if n > 0 then x :: insert_val (n-1) vl xs else vl :: x :: xs

    let apply_with_c op c v =
      map (fun x -> op x c) v

    let zero_vec size = init size (fun i -> of_int 0)

    let of_list l = l

    let to_list v = v

    let find2i f r s =
      let rec inditer r' s' i =
        match r', s' with
        | x1 :: xs1, x2 :: xs2 -> if f i x1 x2 then i, (x1, x2) else inditer xs1 xs2 (i + 1)
        | _, _ -> failwith "Lists have unequal sizes"
      in inditer r s 0
  end

module type Matrix =
sig
  type num
  type vec
  type t

  val empty: unit -> t

  val show: t -> string

  val add_empty_column: t -> int -> t

  val append_row: t -> vec -> t

  val get_row: t -> int -> vec

  val remove_row: t -> int -> t

  val get_col: t -> int -> vec

  val append_matrices: t -> t -> t

  val num_rows: t -> int

  val num_cols: t -> int

  val equal: t -> t -> bool

  val subtract_rows_c: vec -> vec -> num -> vec

  val reduce_row_to_zero: t -> int -> int -> t

  val remove_col: t -> int -> t

  val normalize: t -> t Option.t

  val find_opt: (vec -> bool) -> t -> vec option

  val map2: (vec -> num -> vec) -> t -> vec -> t

  val map2i: (int -> vec-> num -> vec) -> t -> vec -> t

  val of_list: vec list -> t

  val to_list: t -> vec list

end

module type AbstractMatrix =
  functor (A: Arith) (V: AbstractVector) ->
  sig
    include Matrix with type vec := V(A).t and type num := A.t
  end

module ListMatrix : AbstractMatrix =
  functor (A: Arith) (V: AbstractVector) ->
  struct
    include ConvenienceOps(A)
    module V = V(A)
    type t = V.t list

    let empty () = []

    let show x =
      List.fold_left (^) "" (List.map (V.show) x)

    let add_empty_column m pos =
      List.map (fun y -> V.insert_val pos (of_int 0) y) m

    let append_row m row  =
      List.append m [row]

    let get_row m n =
      List.nth m n

    let num_rows m =
      List.length m

    let remove_row m n =
      if n < 0 || n >= num_rows m then failwith "Entry does not exist"
      else List.filteri (fun i x -> n <> i) m

    let get_col m n =
      V.of_list (List.map (fun x -> V.nth x n) m)

    let append_matrices m1 m2  =
      List.append m1 m2

    let num_cols m =
      match m with
      | [] -> 0
      | x :: xs -> V.length x

    let equal m1 m2 =
      List.equal V.equal m1 m2

    let subtract_rows_c row1 row2 c =
      V.map2 (fun x y -> x -: (y *: c)) row1 row2

    let reduce_row_to_zero m row_n col_n =
      let red_row = get_row m row_n in
      let c = V.nth red_row col_n in
      List.map (fun x -> subtract_rows_c x red_row ((V.nth x col_n) /: c)) m

    let remove_col m col_n =
      let del_col = List.map (fun x -> V.remove_val x col_n) in
      match List.findi (fun i x -> V.nth x col_n <> of_int 0)  (List.rev m) with
      | exception Not_found -> del_col m
      | (i, _) -> let len_i = List.length m - (i + 1)
        in del_col (remove_row (reduce_row_to_zero m len_i col_n) len_i)

    (*Gauss-Jordan Elimination to get matrix in reduced row echelon form (rref) + deletion of zero rows. None matrix has no solution*)

    let map2i f m v = List.map2i f m (V.to_list v)

    let normalize t =
      let exception NoSolution in
      let rec create_rref new_t curr_row =
        if curr_row >= num_rows new_t then new_t else
          let row = List.nth new_t curr_row in
          match V.findi (fun i -> fun x -> x <> of_int 0) row with
          | exception Not_found -> create_rref (remove_row new_t curr_row) curr_row
          | (i, p) -> if V.compare_length_with row (i + 1) <= 0 then raise NoSolution
            else let p = V.map (fun x -> x /: p) row in
              let col = get_col new_t i in
              let res = map2i (fun row_i y z -> if row_i <> curr_row then subtract_rows_c y p z else p) new_t col
              in create_rref res (curr_row + 1)
      in
      match create_rref t 0 with
      | c -> let sort v1 v2 =
               let f = V.findi (fun i x -> x =: of_int 1) in
               let (i1, _), (i2, _) = (f v1), (f v2) in Int.compare i1 i2
        in Some (List.sort sort c)
      | exception NoSolution -> None

    let find_opt = List.find_opt

    let map2 f m v = List.map2 f m (V.to_list v)

    let of_list l = l

    let to_list m = m
  end

module VarManagement (Vec: AbstractVector) (Mx: AbstractMatrix)=
struct
  include SharedFunctions.EnvOps
  module Vector = Vec (Mpqf)
  module Matrix = Mx(Mpqf) (Vec)

  type t = {
    d :  Matrix.t Option.t;
    env : Environment.t
  }

  let dim_add (ch: Apron.Dim.change) m =
    let to_add = Array.to_list ch.dim in
    List.fold_lefti (fun m' i  x -> Matrix.add_empty_column m' (x + i)) m to_add

  let dim_remove (ch: Apron.Dim.change) m =
    let to_remove = Array.to_list ch.dim in
    List.fold_left (fun y x -> Matrix.remove_col y x) m to_remove

  let change_d t new_env add =
    let dim_change = if add then Environment.dimchange t.env new_env
      else Environment.dimchange new_env t.env
    in let d' = match t.d with
        | None -> Some (Matrix.empty ())
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

  include ConvenienceOps(Mpqf)

  let rec get_c v = match Vector.findi (fun i x -> x <> (of_int 0)) v with
    | exception Not_found -> Some (of_int 0)
    | (i, p) when Vector.compare_length_with v (i + 1) = 0 -> Some (p)
    | _ -> None

  (*Parses a Texpr to obtain a coefficient + const (last entry) vector to repr. an affine relation.
    Returns None if the expression is not affine linear*)
  let get_coeff_vec (t: t) texp =
    let open Apron.Texpr1 in
    let exception NotLinear in
    let zero_vec = Vector.zero_vec @@ Environment.size t.env + 1 in
    let neg = Vector.map (fun x -> (of_int (-1)) *: x) in
    let is_const_vec v = Vector.compare_length_with (Vector.filteri (fun i x ->
      Vector.compare_length_with v (i + 1) > 0 && x <> of_int 0) v) 1 = 0 in
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
          let entry_only = Vector.set_val zero_vec (Environment.dim_of_var t.env x) (of_int 1) in
          begin match t.d with
            | Some m -> let row = Matrix.find_opt (fun r -> Vector.nth r (Environment.dim_of_var t.env x) = of_int 1) m in
              begin match row with
                | Some v when is_const_vec v ->
                  Vector.set_val zero_vec ((Vector.length zero_vec) - 1) (Vector.nth v (Vector.length v - 1))
                | _ -> entry_only end
            | None -> entry_only end
        | Unop (u, e, _, _) ->
          begin match u with
            | Neg -> neg @@ convert_texpr e
            | Cast -> convert_texpr e (*Ignore*)
            | Sqrt -> raise NotLinear end
        | Binop (b, e1, e2, _, _) ->
          begin match b with
            | Add ->  Vector.map2 (+:)(convert_texpr e1) (convert_texpr e2)
            | Sub -> Vector.map2 (+:) (convert_texpr  e1) (neg @@ convert_texpr e2)
            | Mul ->
              let x1, x2 = convert_texpr e1, convert_texpr e2 in
              begin match get_c x1, get_c x2 with
                | _, Some c -> Vector.apply_with_c ( *:) c x1
                | Some c, _ -> Vector.apply_with_c ( *:) c x2
                | _, _ -> raise NotLinear end
            | _ -> raise NotLinear end
      end
    in match convert_texpr texp with
    | exception NotLinear -> None
    | x -> Some(x)
end

module ExpressionBounds (V: AbstractVector) (Mx: AbstractMatrix): (SharedFunctions.ConvBounds with type t = VarManagement(V) (Mx).t) =
struct
  include VarManagement (V) (Mx)

  let bound_texpr t texpr =
    let texpr = Texpr1.to_expr texpr in
    match get_coeff_vec t texpr  with
    | Some v -> begin match get_c v with
        | Some c when Mpqf.get_den c = Mpzf.of_int 1 ->
          let int_val = IntOps.BigIntOps.of_string (Mpzf.to_string (Mpqf.get_num c))
          in Some int_val, Some int_val
        | _ -> None, None end
    | _ -> None, None

end

module D2(V: AbstractVector) (Mx: AbstractMatrix): SharedFunctions.AssertionRelD2 with type var = SharedFunctions.Var.t =
struct
  include ConvenienceOps (Mpqf)
  include VarManagement (V) (Mx)

  module Bounds = ExpressionBounds (V) (Mx)
  module Convert = SharedFunctions.Convert (Bounds)

  type var = SharedFunctions.Var.t

  let tag t = failwith "No tag"
  let show t =
    match t.d with
    | None -> Format.asprintf "⟂ (env: %a)" (Environment.print:Format.formatter -> Environment.t -> unit) t.env
    | Some m when m = Matrix.empty () -> Format.asprintf "⊤ (env: %a)" (Environment.print:Format.formatter -> Environment.t -> unit) t.env
    | Some m -> Format.asprintf "%s env %a" (Matrix.show m) (Environment.print:Format.formatter -> Environment.t -> unit) t.env

  let pretty () (x:t) = text (show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>\nmatrix\n</key>\n<value>\n%s</value>\n<key>\nenv\n</key>\n<value>\n%s</value>\n</map>\n</value>\n" (XmlUtil.escape (Format.asprintf "%s" (show x) )) (XmlUtil.escape (Format.asprintf "%a" (Environment.print: Format.formatter -> Environment.t -> unit) (x.env)))
  let equal t1 t2 =
    match t1.d, t2.d with
    | None, None -> true
    | Some x1, Some x2 -> Matrix.equal x1 x2
    | _ -> false

  let hash t =
    Hashtbl.hash t

  let compare t1 t2 = Stdlib.compare t1 t2

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
    | Some x, Some y when x = Matrix.empty () -> {d = Some (dim_add (Environment.dimchange t2.env sup_env) y); env = sup_env}
    | Some x, Some y when y = Matrix.empty () -> {d = Some (dim_add (Environment.dimchange t1.env sup_env) x); env = sup_env}
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
          let col_b = let a_length, b_length = Matrix.num_rows a, Vector.length col_b in
            match Int.compare a_length b_length with
            | -1 -> Vector.keep_vals col_b a_length
            |  1 -> Vector.append col_b @@ Vector.zero_vec (a_length - b_length)
            | _ -> col_b
          in
          let a_r = Matrix.get_row a r in
          let mapping = Matrix.map2i (fun i x y -> if i < r then
                                         Vector.map2 (+:) x (Vector.apply_with_c ( *:) y a_r) else x ) a col_b
          in Matrix.remove_row mapping r
        in
        let case_three a b col_a col_b max =
          let col_a, col_b = Vector.keep_vals col_a max, Vector.keep_vals col_b max in
          if Vector.equal col_a col_b then (a, b, max) else
            let a_rev, b_rev = Vector.rev col_a,  Vector.rev col_b in
            let i, (x, y) = Vector.find2i (fun i x y -> x <> y) a_rev b_rev in
            let r, diff = Vector.length a_rev - (i + 1), x -: y  in
            let a_r, b_r = Matrix.get_row a r, Matrix.get_row b r in
            let multiply_by_t' m col_v t i =
              let zero_vec = Vector.of_list @@ List.init (Matrix.num_rows m - Vector.length col_v) (fun x -> of_int 0) in
              let cs = Vector.append col_v zero_vec in
              Matrix.map2i (fun i' x c -> if i' <= i then let beta = c /: diff in
                               let mul_t = Vector.apply_with_c ( *:) beta t in Vector.map2 (-:) x mul_t else x) m cs
            in
            let sub_col = Vector.map2 (fun x y -> x -: y) col_a col_b in
            Matrix.remove_row (multiply_by_t' a sub_col a_r max) r, Matrix.remove_row (multiply_by_t' b sub_col b_r max) r, (max - 1)
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
          if a_rs = (of_int 1) then lin_disjunc r (s + 1) (case_two a r col_b) b
          else
            let new_a, new_b, new_r = case_three a b col_a col_b r in
            lin_disjunc new_r (s + 1) new_a new_b
        | _ , 1  -> let b_rs = Vector.nth col_b r in
          if b_rs = (of_int 1) then lin_disjunc r (s + 1) a (case_two b r col_a)
          else
            let new_a, new_b, new_r = case_three a b col_a col_b r in
            lin_disjunc new_r (s + 1) new_a new_b
        | _      -> let new_a, new_b, new_r = case_three a b col_a col_b r in
          lin_disjunc new_r (s + 1) new_a new_b
    in
    match a.d, b.d with
    | None, m -> b
    | m, None -> a
    | Some x, Some y when x = Matrix.empty () || y = Matrix.empty () -> {d = Some (Matrix.empty ()); env = Environment.lce a.env b.env}
    | Some x, Some y when (Environment.compare a.env b.env <> 0) ->
      let sup_env = Environment.lce a.env b.env in
      let mod_x = dim_add (Environment.dimchange a.env sup_env) x in
      let mod_y = dim_add (Environment.dimchange b.env sup_env) y in
      {d = Some (lin_disjunc 0 0 mod_x mod_y); env = sup_env}
    | Some x, Some y  -> {d = Some(lin_disjunc 0 0 x y); env = a.env}

  let join a b =
    let res = join a b in
    if M.tracing then M.tracel "ops" "join a: %s b: %s -> %s \n" (show a) (show b) (show res) ;
    res
  let widen a b = join a b
  let narrow a b = meet a b
  let pretty_diff () (x, y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  let assign_invertible_rels x var b env =
    let j0 = Environment.dim_of_var env var in
    let a_j0 = Matrix.get_col x j0  in (*Corresponds to Axj0*)
    let b0 = Vector.nth b j0 in
    let reduced_a = Vector.apply_with_c (/:) b0 a_j0 in  (*Corresponds to Axj0/Bj0*)
    let recalc_entries m rd_a = Matrix.map2 (fun x y -> Vector.map2i (fun j z d ->
        if j = j0 then y
        else if Vector.compare_length_with b (j + 1) > 0 then z -: y *: d
        else z +: y *: d) x b) m rd_a
    in {d = Matrix.normalize @@ recalc_entries x reduced_a; env = env}

  let assign_uninvertible_rel x var b env =
    let neg_vec = Vector.mapi (fun i z -> if Vector.compare_length_with b (i + 1) > 0 then of_int (-1) *: z else z) b
    in let var_vec = Vector.set_val neg_vec (Environment.dim_of_var env var) (of_int 1)
    in {d = Matrix.normalize @@ Matrix.append_row x var_vec; env = env}

  let remove_rels_with_var x var env =
    let j0 = Environment.dim_of_var env var
    in match Vector.findi (fun i x -> x <> of_int 0) (Vector.rev @@ Matrix.get_col x j0) with
    | exception Not_found -> x
    | r, _ -> Matrix.reduce_row_to_zero x (Matrix.num_rows x - r - 1) j0

  let rec forget_vars t vars =
    match t.d with
    | None -> t
    | Some m -> begin match vars with
        | [] -> t
        | x :: xs -> forget_vars {d = Some (Option.get @@ Matrix.normalize @@ remove_rels_with_var m x t.env); env = t.env} xs end


  let assign_texpr (t: VarManagement(V)(Mx).t) var texp =
    let is_invertible v = Vector.nth v @@ Environment.dim_of_var t.env var <> of_int 0
    in let affineEq_vec = get_coeff_vec t texp
    in match t.d, affineEq_vec with
    | None, _ -> failwith "Can not assign to bottom state!"
    | Some m, Some v when m = Matrix.empty ()-> if is_invertible v then t else assign_uninvertible_rel (Matrix.empty ()) var v t.env
    | Some x, Some v -> if is_invertible v then assign_invertible_rels x var v t.env
      else let new_x = remove_rels_with_var x var t.env
        in assign_uninvertible_rel new_x var v t.env
    | Some x, None -> {d = Matrix.normalize @@ remove_rels_with_var x var t.env; env = t.env}

  let assign_exp (t: VarManagement(V)(Mx).t) var exp (no_ov: bool) =
    match Convert.texpr1_expr_of_cil_exp t t.env no_ov exp with
    | exp -> assign_texpr t var exp
    | exception Convert.Unsupported_CilExp -> match t.d with
      | None -> t
      | Some x -> forget_vars t [var]

  let assign_exp t var exp no_ov =
    let res = assign_exp t var exp no_ov in
    if M.tracing then M.tracel "assign" "assign_exp t:\n %s \n var: %s \n exp: %s\n no_ov: %b -> \n %s\n"
        (show t) (Var.to_string var) (Pretty.sprint ~width:1 (Cil.printExp Cil.defaultCilPrinter () exp)) no_ov (show res) ;
    res
  let assign_var (t: VarManagement(V)(Mx).t) v v' =
    let texpr1 = Texpr1.of_expr (t.env) (Var v') in
    assign_texpr t v (Apron.Texpr1.to_expr texpr1)

  let assign_var t v v' =
    let res = assign_var t v v' in
    if M.tracing then M.tracel "ops" "assign_var t:\n %s \n v: %s \n v': %s\n -> %s\n" (show t) (Var.to_string v) (Var.to_string v') (show res) ;
    res

  let assign_var_parallel t vv's =
    let new_t's = List.map (fun (v,v') -> assign_var t v v') vv's in
    List.fold_left join t new_t's

  let assign_var_parallel' t vs1 vs2 =
    let vv's = List.combine vs1 vs2 in
    assign_var_parallel t vv's

  let assign_var_parallel' t vv's =
    let res = assign_var_parallel' t vv's in
    if M.tracing then M.tracel "ops" "assign_var parallel'\n";
    res

  let substitute_exp t var exp no_ov =
    assign_exp t var exp no_ov

  let substitute_exp t var exp ov =
    let res = substitute_exp t var exp ov
    in if M.tracing then M.tracel "sub" "Substitute_expr t: \n %s \n var: %s \n exp: %s \n -> \n %s\n" (show t) (Var.to_string var) (Pretty.sprint ~width:1 (Cil.printExp Cil.defaultCilPrinter () exp)) (show res); res

  (** Assert a constraint expression. *)
  let meet_with_tcons t tcons expr =
    let module ID = Queries.ID in
    let check_const cmp c = if cmp c (of_int 0) then {d = None; env = t.env} else t
    in
    let exception UnsignedVar in
    let meet_with_vec e =
      (*Flip the sign of the const. val in coeff vec*)
      let flip_e = Vector.mapi (fun i x -> if Vector.compare_length_with e (i + 1) = 0 then (of_int (-1)) *: x else x) e in
      let res = meet t {d = Matrix.normalize @@ Matrix.of_list [flip_e]; env = t.env} in
      match Convert.determine_bounds_one_var expr with
      | None -> res
      | Some (ev, min, max) ->
        begin match Bounds.bound_texpr res (Convert.texpr1_of_cil_exp res res.env ev true) with
              | Some b_min, Some b_max ->  let module BI = IntOps.BigIntOps in
                                            if min = BI.of_int 0 && b_min = b_max then raise UnsignedVar
                                            else if (b_min < min && b_max < min) || (b_max > max && b_min > max) then
                                                    (if GobConfig.get_string "sem.int.signed_overflow" = "assume_none" then {d = None; env = t.env} else t)
                                                    else res
              | _, _ -> res end
    in
    (* let refine_by x y = if BI.equal (BI.rem v (BI.of_int 2)) BI.zero then *)
    match get_coeff_vec t (Texpr1.to_expr @@ Tcons1.get_texpr1 tcons) with
    | Some v -> begin match get_c v, Tcons1.get_typ tcons with
        | Some c, DISEQ -> check_const (=:) c
        | Some c, SUP -> check_const (<=:) c
        | Some c, EQ -> check_const (<>) c
        | Some c, SUPEQ -> check_const (<:) c
        | None, DISEQ | None, SUP ->
         begin match meet_with_vec v with
          | exception UnsignedVar -> t
          | res -> if equal res t then {d = None; env = t.env} else t end
        | None, EQ -> begin match meet_with_vec v with
                            | exception UnsignedVar -> t
                            | res -> res end
        | _, _ -> t end
    | None -> t

  let unify (a: Bounds.t) (b: Bounds.t) =
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

  let assert_cons d e negate no_ov =
    if M.tracing then M.tracel "assert_cons" "assert_cons with expr: %s \n %b" (Pretty.sprint ~width:1 (Cil.printExp Cil.defaultCilPrinter () e)) no_ov;
    begin match Convert.tcons1_of_cil_exp d d.env e negate no_ov with
      | tcons1 -> meet_with_tcons d tcons1 e
      | exception Convert.Unsupported_CilExp ->
        d
    end

  let relift t = t

  let env (t: Bounds.t) = t.env

  type marshal = Bounds.t

  let marshal t = t

  let unmarshal t = t

end

module ListD2 = D2(ListVector) (ListMatrix)

module AD2 = SharedFunctions.AssertionModule (ListD2)
