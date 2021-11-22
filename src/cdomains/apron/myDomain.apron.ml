open Prelude
open Pretty
open Cil
module M = Messages
open Apron

  module Vector =
  struct
  type t = int list

  let create_zero_vec n : t =
    let rec zero_vec t n =
     if n > 0 then zero_vec (0 :: t) (n-1)
     else t
   in
   zero_vec [] n

  let map_vec f v =
    List.map f v

  let neg v =
    List.map (function x -> (-1) * x) v

  let rec sub_vecs t1 t2 =
    List.map2 (function x -> function y -> x - y) t1 t2

  let rec mul_vecs t1 t2 =
    List.map2 (function x -> function y -> x * y) t1 t2

  let rec add_vecs t1 t2 =
    List.map2 (function x -> function y -> x + y) t1 t2

  let rec remove_val n (v:t) =
    match v with
    | [] -> failwith "Entry does not exist"
    | x :: xs -> if n > 0 then remove_val (n-1) xs else xs

  let rec set_val t n new_val =
    match t with
    | [] -> failwith "Can not set val of an empty Vector"
    | x :: xs -> if n > 0 then x :: set_val xs (n-1) new_val else  new_val :: xs

  let rec add_val n vl (v:t) =
    match v with
    | [] -> if n == 0 then [vl] else failwith "Entry does not exist"
    | x :: xs -> if n > 0 then add_val (n-1) vl xs else x :: vl :: xs

  let mul_by_constant c v =
    List.map (function x -> x * c) v

  let divide_by_const v c =
    List.map (function x -> x / c) v

  let rec apply_on_entry f n v =
   match v with
   | x :: xs -> if n > 0 then (x :: apply_on_entry f (n - 1) xs) else f x :: xs
   | [] -> failwith "Entry does not exist"

  let is_constant v =
   let rec const_check v =
     match v with
     | [] -> true
     | x :: [] -> true
     | x :: xs -> if x != 0 then false else const_check xs
   in
   const_check v

  let get_val n (v: t) =
   List.nth v n

  let get_colnum_not_zero t =
    let rec get_colnum last curr_col t =
      match t with
      | [] -> last
      | x :: xs -> if x != 0 then get_colnum curr_col (curr_col - 1) xs
                    else get_colnum last (curr_col - 1) xs
                  in
                  let res = get_colnum (-1) (List.length t) (List.rev t)
                in if M.tracing then M.tracel "affEq" "colnum %i \n" res; res

  let show t =
    let rec list_str l =
      match l with
      | [] -> "]"
      | x :: xs -> (Format.asprintf "%i" x) ^(list_str xs)
    in
    "["^list_str t^"\n"
end

module Matrix =
struct
  type t = int list list
  let empty () = ([] : t)
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

  let rec get_col (a:t) n : Vector.t =
    List.map (function x -> List.nth x n) a
  let rec remove_column (m : t) pos =
    List.map (Vector.remove_val pos) m

  let dim_x (m : t) =
    match m with
    | [] -> 0
    | x :: xs -> List.length x
  let dim_y (m : t) =
    List.length m

  let create_zero_col (m : t)  =
    let rec create_zero_list l n =
      if n > 0 then create_zero_list (List.append [0] l) (n-1)
      else l
    in
    create_zero_list [] (dim_y m)

  let show (x:t) =
    List.fold_left (^) "" (List.map (Vector.show) x)

   let rec equal (a: t) (b: t) =
    match (a, b) with
    | [], [] -> true
    | x1 :: xs1, x2 :: xs2 -> if List.equal Int.equal x1 x2 then equal xs1 xs2 else false
    | _ -> false

  let rec transpose t =
  match t with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x::xs) :: xss ->
      (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

  let subtract_rows row1 row2 =
    List.map2 (function x -> function y -> x - y) row1 row2

  let reduce_row_to_zero t row_n =
    let red_row = List.nth t row_n
          in List.map (function x -> subtract_rows x red_row) t

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
    let rec add_cols pos =
      match pos with
      | [] -> m
      | x :: xs -> Matrix.add_column m (Matrix.create_zero_col m) x
    in
     add_cols (Array.to_list ch.dim)

  let dim_remove (ch: Apron.Dim.change) m =
    let rec remove_cols pos =
      match pos with
      | [] -> m
      | x :: xs -> Matrix.remove_column m  x (* TODO Every row that has an entry > 0 in this col should be removed as well!*)
    in
     remove_cols (Array.to_list ch.dim)

  let add_vars a vars =
    let vs' = get_filtered_vars_add (a.env) vars in
      let env' = Environment.add a.env vs' [||] in
        let d' = (match a.d with
          | None -> None
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

module DummyBounds: EnvDomain.ConvBounds with type d = VarManagement.t = (* ToDo Implement proper bounds calculation*)
struct
  type d = VarManagement.t
  let bound_texpr t texpr = (Some (Z.of_int 0), Some (Z.of_int 0))
end

module Tracked = (* ToDo Move to another place*)
  struct
    let type_tracked typ =
      isIntegralType typ

    let varinfo_tracked vi =
      (* no vglob check here, because globals are allowed in apron, but just have to be handled separately *)
      type_tracked vi.vtype && not vi.vaddrof
  end

module Convert = EnvDomain.Convert(Tracked) (DummyBounds)
open Apron.Texpr1

module MyD2: RelationDomain.RelD2 with type var = EnvDomain.Var.t =
struct

  include VarManagement
  let show a =
    let d_str = (match a.d with
    | None -> "⟂"
    | Some ([]) -> "top"
    | Some (m) -> Matrix.show m)
    in
   Format.asprintf "%s (env: %a)" d_str (Environment.print:Format.formatter -> Environment.t -> unit) a.env

  let pretty () (x:t) = text (show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>\nmatrix\n</key>\n<value>\n%s</value>\n<key>\nenv\n</key>\n<value>\n%s</value>\n</map>\n</value>\n" (XmlUtil.escape (Format.asprintf "%s" (show x) )) (XmlUtil.escape (Format.asprintf "%a" (Environment.print: Format.formatter -> Environment.t -> unit) (x.env)))
  let equal a b =
    match a.d, b.d with
    | None, None -> Environment.equal (a.env) (b.env)
    | Some (x1), Some(x2) -> Matrix.equal x1 x2 && Environment.equal (a.env) (b.env)
    | _ -> false

  let equal a b =
    let res = equal a b in
      if M.tracing then M.tracel "ops" "equal a: %s b: %s -> %b \n" (show a) (show b) res ;
      res

  let hash a = 0
  let compare a b = 0

  let compare a b =
    let res = compare a b in
    if M.tracing then M.tracel "ops" "compare a: %s b: %s -> %i \n" (show a) (show b) res ;
    res

  let name () = "affeq"
  let to_yojson a = failwith "ToDo Implement in future"
  let invariant a b = Invariant.none
  let arbitrary () = failwith "no arbitrary"
  let leq a b = false

  let leq a b =
    let res = leq a b in
      if M.tracing then M.tracel "ops" "leq a: %s b: %s -> %b \n" (show a) (show b) res ;
      res

  let join a b = b

  let join a b =
    let res = join a b in
      if M.tracing then M.tracel "ops" "join a: %s b: %s -> %s \n" (show a) (show b) (show res) ;
      res

  let meet a b = b

  let meet a b =
    let res = meet a b in
      if M.tracing then M.tracel "ops" "meet a: %s b: %s -> %s \n" (show a) (show b) (show res) ;
      res
  let widen a b = join a b
  let narrow a b = meet a b
  let pretty_diff () (x, y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let bot () =
     {d = None; env = Environment.make [||] [||]}
  let is_bot a = a.d == None
  let is_bot_env a =
    match a.d with
    | None -> true
    | _ -> false
  let top () =
    {d = Some (Matrix.empty ()); env = Environment.make [||] [||] }
  let is_top a = a.d == Some (Matrix.empty ())

  let create_affineEq_vec env texp =
    let zero_vec = Vector.create_zero_vec ((Environment.size env) + 1) in
    let rec convert_texpr env texp =
    (match texp with
    | Cst x ->  Some (Vector.set_val zero_vec ((List.length zero_vec) - 1) (Convert.int_of_cst x))
    | Var x ->  Some (Vector.set_val zero_vec (Environment.dim_of_var env x) 1)
    | Unop (u, e, _, _) -> (
      match u with
      | Neg -> (match convert_texpr env e with
            | None -> None
            | Some (x) -> Some (Vector.neg x)) (* Multiply by -1*)
      | Cast -> convert_texpr env e (*Ignore*)
      | Sqrt -> None )(*Return None - Maybe works if only constant?*)
      | Binop (b, e1, e2, _, _) -> (
        match b with
        | Add -> (match convert_texpr env e1, convert_texpr env e2 with
            | None, _ -> None
            | _, None -> None
            | Some (x1), Some (x2) -> Some (Vector.add_vecs x1 x2)) (*Simply add vectors*)
        | Sub -> (match convert_texpr env  e1, convert_texpr env e2 with
             | None, _ -> None
             | _, None -> None
            | Some (x1), Some (x2) -> Some (Vector.add_vecs x1 (Vector.neg x2))) (*Subtract them*)
    | Mul -> (match convert_texpr env  e1, convert_texpr env e2 with
            | None, _ -> None
            | _, None -> None
            | Some (x1), Some (x2) -> if Vector.is_constant x1 || Vector.is_constant x2
                                      then Some (Vector.mul_vecs x1 x2)
                                      else None ) (*Var * Var is invalid!*)
    | _ -> None (*None, rest is not valid!*)) )
                                    in convert_texpr env texp
  let create_affineEq_vec env texp =
    let res = create_affineEq_vec env texp in
    (match res with
    | None -> if M.tracing then M.tracel "affEq" "Invalid expr created\n"
    | Some (x) -> if M.tracing then M.tracel "affEq" "Created expr vector: %s\n" (Vector.show x))
      ;res

  let assign_invertible_rels x var b env=
  if M.tracing then M.tracel "affEq" "Assigning:\n %s <- %s \n" (Matrix.show x) (Vector.show b);
      let j0 = Environment.dim_of_var env var in
        let a_j0 = Matrix.get_col x j0  in (*Corresponds to Axj0*)
         let b0 = (Vector.get_val j0 b) in
           let reduced_a = Vector.divide_by_const a_j0 b0 in  (*Corresponds to Axj0/Bj0*)
           let rec recalc_entries m rd_a =
            match m, rd_a with
            | [], [] -> []
            | x::xs, y::ys -> List.map2i (function j -> function z -> function d ->
                              if j == j0 then y
                              else if j < ((List.length b) -1) then z - y * d
                              else z + y * d) x b :: recalc_entries xs ys
           | _, _ -> failwith "Unequal sizes"
                            in {d = Some (recalc_entries x reduced_a); env = env}

  let assign_uninvertible_rel x var b env =
    let neg_vec = List.mapi (function i -> function z -> if i < (List.length b) - 1 then (-1) * z else z) b
        in let var_vec = Vector.set_val neg_vec (Environment.dim_of_var env var) 1
            in {d = Some (Matrix.append_row x var_vec); env = env}

  let remove_rels_with_var x var env =
    if M.tracing then M.tracel "affEq" "Removing rel\n";
    let j0 = Environment.dim_of_var env var
      in let n = Vector.get_colnum_not_zero (Matrix.get_col x j0)
          in if n > (-1) then Matrix.reduce_row_to_zero x n (*ToDo normalize rows*)
              else x (*Nothing to remove*)

  let assign_texpr t var texp =
    if M.tracing then M.tracel "affEq" "trying to assign texpr \n:\n";
    let is_invertible = function v -> Vector.get_val (Environment.dim_of_var t.env var) v != 0
    in
    let affineEq_vec = create_affineEq_vec t.env texp in
      match t.d, affineEq_vec with
      | Some ([]), Some(v)
      | None, Some(v) -> if is_invertible v then t (*top/bottom and inv. assign = top*)
                         else assign_uninvertible_rel [] var v t.env
      | Some (x), Some (v) -> if is_invertible v then assign_invertible_rels x var v t.env
                            else let new_x = remove_rels_with_var x var t.env in
                            assign_uninvertible_rel new_x var v t.env
      | _, _ -> t

  let assign_exp t var exp  =
   let res = assign_texpr t var (Convert.texpr1_expr_of_cil_exp t t.env exp)
    in if M.tracing then M.tracel "affEq" "assign_exp \n %s:\n" (show res); res

  let assign_var t v v' =
  let texpr1 = Texpr1.of_expr (t.env) (Var v') in
  assign_texpr t v (Apron.Texpr1.to_expr texpr1)

  let assign_var t v v' =
    let res = assign_var t v v' in
      if M.tracing then M.tracel "ops" "assign_var\n";
      res

  let assign_var_parallel a b = a

  let assign_var_parallel a b =
    let res = assign_var_parallel a b in
      if M.tracing then M.tracel "ops" "assign_var parallel\n";
      res

  let substitute_exp t var exp = assign_exp t var exp (*This is not correct!*)

  let assert_inv a b c = a
  let eval_int a e =
    let module ID = Queries.ID in
      ID.top ()
  let unify a b = a

  let unify a b =
    let res = unify a b in
      if M.tracing then M.tracel "ops" "unify\n";
      res
end