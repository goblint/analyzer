(** OCaml implementation of the affine equalities domain.

    @see <http://doi.acm.org/10.1145/2049706.2049710> A. Flexeder, M. Petter, and H. Seidl Fast Interprocedural Linear Two-Variable Equalities. *)

(** TODO: description *)

open Batteries
open GoblintCil
open Pretty
module M = Messages
open Apron
open VectorMatrix
open Printf

(** TODO: modify code *)

module Mpqf = struct (* multi-precision rational numbers *)
  include Mpqf
  let compare = cmp
  let zero = of_int 0
  let one = of_int 1
  let mone = of_int (-1)

  let get_den x = Z_mlgmpidl.z_of_mpzf @@ Mpqf.get_den x

  let get_num x = Z_mlgmpidl.z_of_mpzf @@ Mpqf.get_num x
  let hash x = 31 * (Z.hash (get_den x)) + Z.hash (get_num x)
end


module Equality = struct
  (* (Some i, k) represents a sum of a variable with index i and the number k.
     (None, k) represents the number k. *)
  type t = (int option * Z.t) [@@deriving eq, ord, hash]
  let zero = (None, Z.zero)
  let var_zero i = (Some i, Z.zero)
  let to_int x = Z.to_int @@ snd x
  let print : t -> unit = fun (a, b) -> match a with
    | None -> print_endline @@ "(None , " ^ Z.to_string b ^ ")"
    | Some x -> print_endline @@ "(Some " ^ string_of_int x ^ ", " ^ Z.to_string b ^ ")"
end

module EqualitiesArray = struct
  include Array
  type t = Equality.t Array.t [@@deriving eq, ord]

  let hash : t -> int = (fun x -> 31 + Equality.to_int x.(0)) (* TODO **)

  let make_empty_array len = Array.mapi (fun i (x, y) -> (Some i, Z.zero)) (make len Equality.zero)

  let add_element arr index = 
    let num_vars = length arr in
    if index > num_vars then failwith "n too large" else
      let new_array = make (num_vars + 1) (Equality.var_zero index) in
      if index = 0 then blit arr 0 new_array 1 (num_vars - 1) else
        blit arr 0 new_array 0 index; if index <> num_vars then blit arr index new_array (index + 1) (num_vars - index);
      new_array

  let add_elements m indexes = (** same as add_empty_columns for Matrix (see vectorMatrix.ml)*)
    let nnc = length indexes in
    if nnc = 0 then m else
      let nc = length m in
      let m' = make_empty_array (nc + nnc) in
      let offset = ref 0 in
      for j = 0 to nc - 1 do
        while  !offset < nnc &&  !offset + j = indexes.(!offset) do incr offset done;
        m'.(j + !offset) <- m.(j);
      done;
      m'

  let del_cols : ('a option * Z.t) mappable ->int mappable ->('a option * Z.t) mappable = fun m cols ->
    let n_c = length cols in
    if n_c = 0 || length m = 0 then m
    else
      let m_c = length m in
      if m_c = n_c then [||] else
        let m' = make_empty_array (m_c - n_c) in
        let offset = ref 0 in
        for j = 0 to (m_c - n_c) - 1 do
          while  !offset < n_c &&  !offset + j = cols.(!offset) do incr offset done;
          m'.(j) <- m.(j + !offset);
        done;
        m'

  let del_cols m cols = timing_wrap "del_cols" (del_cols m) cols


end


module Var = SharedFunctions.Var
module V = RelationDomain.V(Var)

(** It defines the type t of the affine equality domain (a struct that contains an optional matrix and an apron environment) and provides the functions needed for handling variables (which are defined by RelationDomain.D2) such as add_vars remove_vars.
    Furthermore, it provides the function get_coeff_vec that parses an apron expression into a vector of coefficients if the apron expression has an affine form. *)
module VarManagement =
struct
  include SharedFunctions.EnvOps
  module EArray = EqualitiesArray

  type t = {
    mutable d : EArray.t option;
    mutable env : Environment.t
  }
  [@@deriving eq, ord, hash] 

  let empty_env = Environment.make [||] [||]

  (* For debugging *)
  let print_env = Environment.print (Format.std_formatter)
  let print_opt x = match x with
    | Some x -> printf "%d " x
    | None -> printf "None "
  let print_d = Array.iter (fun (var, off) -> print_opt var; Z.print off; printf "; ")
  let print_t t = begin match t.d with
    | Some x -> (print_d x; print_endline "")
    | None -> printf "None " end; print_env t.env; print_endline ""

  let bot () =
    {d = Some [||]; env = empty_env}

  let bot_env = {d = None; env = empty_env}

  let is_bot_env t = t.d = None

  let copy t = {t with d = Option.map EArray.copy t.d}

  let size t = match t.d with
    | None -> 0 
    | Some d -> EArray.length d

  let dim_add (ch: Apron.Dim.change) m =
    EArray.iteri (fun i x -> ch.dim.(i) <- x + i) ch.dim; (* ?? *)
    EArray.add_elements m ch.dim

  let dim_add ch m = timing_wrap "dim add" (dim_add ch) m (*?*)

  let dim_remove (ch: Apron.Dim.change) m del =
    if EArray.length ch.dim = 0 || (EArray.length m = 0) then m else (
      EArray.iteri (fun i x -> ch.dim.(i) <- x + i) ch.dim;(* ?? *)
      let m' = if not del then let m = EArray.copy m in EArray.add_elements m ch.dim else m in
      EArray.del_cols m' ch.dim)

  let dim_remove ch m del = timing_wrap "dim remove" (dim_remove ch m) del

  let change_d t new_env add del =
    if Environment.equal t.env new_env then t else
      let dim_change = if add then Environment.dimchange t.env new_env
        else Environment.dimchange new_env t.env
      in match t.d with
      | None -> bot_env
      | Some m -> {d = Some (if add then dim_add dim_change m else dim_remove dim_change m del); env = new_env}

  let change_d t new_env add del = timing_wrap "dimension change" (change_d t new_env add) del

  let add_vars t vars =
    let t = copy t in
    let env' = add_vars t.env vars in
    change_d t env' true false

  let add_vars t vars = timing_wrap "add_vars" (add_vars t) vars

  let drop_vars t vars del =
    let t = copy t in
    let env' = remove_vars t.env vars in
    change_d t env' false del

  let drop_vars t vars = timing_wrap "drop_vars" (drop_vars t) vars

  let remove_vars t vars = drop_vars t vars false

  let remove_vars t vars = timing_wrap "remove_vars" (remove_vars t) vars

  let remove_vars_with t vars =
    let t' = remove_vars t vars in
    t.d <- t'.d;
    t.env <- t'.env

  let remove_filter t f =
    let env' = remove_filter t.env f in
    change_d t env' false false

  let remove_filter t f = timing_wrap "remove_filter" (remove_filter t) f

  let remove_filter_with t f =
    let t' = remove_filter t f in
    t.d <- t'.d;
    t.env <- t'.env

  let keep_filter t f =
    let t = copy t in
    let env' = keep_filter t.env f in
    change_d t env' false false

  let keep_filter t f = timing_wrap "keep_filter" (keep_filter t) f

  let keep_vars t vs =
    let t = copy t in
    let env' = keep_vars t.env vs in
    change_d t env' false false

  let keep_vars t vs = timing_wrap "keep_vars" (keep_vars t) vs

  let forget_var t var_index =
    match t.d with 
    | None -> t
    | Some d -> d.(var_index) <- Equality.var_zero var_index;
      {t with d = Some d}


  let forget_var t var = timing_wrap "forget_var" (forget_var t) var

  (*let forget_vars t vars =
    if is_bot t || is_top_env t then t
    else
      let m = Option.get t.d in
      if List.is_empty vars then t else
        (*let rec rem_vars m vars' =
           begin match vars' with
             |            [] -> m
             | x :: xs -> rem_vars (remove_rels_with_var m x t.env true) xs end 
          in *){d = Some m; env = t.env}

    let forget_vars t vars =
    let res = forget_vars t vars in
    if M.tracing then M.tracel "ops" "forget_vars %s -> %s\n" (show t) (show res);
    res

    let forget_vars t vars = timing_wrap "forget_vars" (forget_vars t) vars*)

  let vars t = vars t.env

  let mem_var t var = Environment.mem_var t.env var

  (* Returns the constant represented by an equality, if the equality represents a constant without a variable *)
  let get_constant (var, off) = match var with
    | None -> Some off
    | _ -> None

  let get_coeff (t: t) texp =
    (*Parses a Texpr to obtain a (variable, offset) pair to repr. a sum of a variable and an offset, 
      where the variable is a reference variable in the current state t.
      Returns None if the expression is not a sum between a variable (without coefficient) and a constant. 
    *)
    let open Apron.Texpr1 in
    let exception NotLinear2Var in
    let exception NotIntegerOffset in
    let mpqf_to_int x = 
      if not(Z.equal (Mpqf.get_den x) Z.one) then raise NotIntegerOffset
      else Mpqf.get_num x in
    let rec convert_texpr texp =
      begin match texp with
        (*If x is a constant, replace it with its const. val. immediately*)
        | Cst x -> let of_union union =
                     let open Coeff in
                     match union with
                     | Interval _ -> failwith "Not a constant"
                     | Scalar x -> begin match x with
                         | Float x -> raise NotIntegerOffset
                         | Mpqf x -> (None, mpqf_to_int x)
                         | Mpfrf x -> raise NotIntegerOffset end in of_union x
        | Var x -> 
          let var_dim = print Format.std_formatter (Texpr1.of_expr t.env (Var x)); Environment.dim_of_var t.env x in
          begin match t.d with 
            | Some m -> m.(var_dim)
            | None -> (Some var_dim, Z.zero) end
        | Unop (u, e, _, _) ->
          begin match u with
            | Neg -> raise NotLinear2Var
            | Cast -> convert_texpr e (*Ignore since casts in apron are used for floating point nums and rounding in contrast to CIL casts*)
            | Sqrt -> raise NotLinear2Var end
        | Binop (b, e1, e2, _, _) ->
          begin match b with
            | Add -> begin match convert_texpr e1, convert_texpr e2 with
                | (None, off1), (var2, off2) -> (var2, Z.(off1 + off2))
                | (var1, off1), (None, off2) -> (var1, Z.(off1 + off2))
                | (_, _), (_, _) -> raise NotLinear2Var end
            | Sub -> begin match convert_texpr e1, convert_texpr e2 with
                | (None, off1), (var2, off2) -> raise NotLinear2Var
                | (var1, off1), (None, off2) -> (var1, Z.(off1 - off2))
                | (var1, off1), (var2, off2) -> if var1 = var2 then (None, Z.(off1 - off2)) else raise NotLinear2Var end
            | Mul ->
              let x1, x2 = convert_texpr e1, convert_texpr e2 in
              begin match get_constant x1, get_constant x2 with
                | Some c1, Some c2 -> (None, Z.(c1 * c2))
                | _, _ -> raise NotLinear2Var end
            | _ -> raise NotLinear2Var end
      end
    in match convert_texpr texp with
    | exception NotLinear2Var -> None
    | exception NotIntegerOffset -> None
    | x -> Some(x)

  let get_coeff t texp = timing_wrap "coeff_vec" (get_coeff t) texp

  let find_reference_variable d env var = fst d.(Environment.dim_of_var env var)

  let find_vars_in_the_connected_component d env ref_var = 
    EArray.filter (fun i -> let (var, _) = d.(i) in var = ref_var) (EArray.mapi const d)

  let find_var_in_the_connected_component_with_least_index d env ref_var = 
    EArray.fold_left (fun curr_min (var, _) -> if var = ref_var then match curr_min with
        | None -> var
        | Some curr_min ->
          match var with 
          |Some i -> if i < curr_min then Some i else Some curr_min
          | None -> Some curr_min else curr_min) None d

  let abstract_exists var t = match t.d with 
    | Some d -> 
      let var_to_remove = Environment.dim_of_var t.env var in
      begin match find_reference_variable d t.env var with 
        | None -> (* the variable is equal to a constant *) t
        | Some ref_var ->
          if ref_var <> var_to_remove then forget_var t var_to_remove
          else
            (* x_i is the reference variable of its connected component *)
            let dim_of_var = Some (Environment.dim_of_var t.env var) in
            let connected_component = find_vars_in_the_connected_component d t.env dim_of_var in
            if EArray.length connected_component = 1 
            then t  (* x_i is the only element of its connected component *) 
            else
              (* x_i is the reference variable -> we need to find a new reference variable *)
              let var_least_index = Option.get @@ find_var_in_the_connected_component_with_least_index d t.env dim_of_var in
              let (_, off) = d.(var_least_index) in 
              EArray.iteri (fun _ x -> let (_, off2) = d.(x) in d.(x) <- (Some var_least_index, Z.(off2 - off))) connected_component;
              {d = Some d; env = t.env}
      end
    | None -> t (* there are no  variables in the current environment *)

  let assign_const t var const = match t.d with 
    | None -> t
    | Some d -> d.(var) <- (None, const); t

  let subtract_const_from_var t var const = 
    match t.d with 
    | None -> t
    | Some d -> 
      let subtract_const_from_var_for_single_equality const index element =
        let (eq_var_opt, off2) = d.(index) in 
        if index = var then
          match eq_var_opt with
          | None -> d.(index) <- (None, Z.(off2 + const))
          | Some eq_var -> begin if eq_var <> index then d.(index) <- (None, Z.(off2 + const)) end
        else 
          begin if Option.is_some eq_var_opt
            then let eq_var = Option.get eq_var_opt
              in begin if eq_var = var then d.(index) <- (Some eq_var, Z.(off2 - const)) end
          end
      in
      EArray.iteri (subtract_const_from_var_for_single_equality const) d; {d = Some d; env = t.env}
end


(*end*)

(*
(** As it is specifically used for the new affine equality domain, it can only provide bounds if the expression contains known constants only and in that case, min and max are the same. *)
module ExpressionBounds (Vc: AbstractVector) (Mx: AbstractMatrix): (SharedFunctions.ConvBounds with type t = VarManagement(Vc).t) =
struct
  include VarManagement (Vc)

  let bound_texpr t texpr =
    let texpr = Texpr1.to_expr texpr in
    match get_coeff_vec t texpr  with
    | Some v -> begin match get_c v with
        | Some c when Mpqf.get_den c = IntOps.BigIntOps.one ->
          let int_val = Mpqf.get_num c
          in Some int_val, Some int_val
        | _ -> None, None end
    | _ -> None, None


  let bound_texpr d texpr1 =
    let res = bound_texpr d texpr1 in
    match res with
    | Some min, Some max ->  if M.tracing then M.tracel "bounds" "min: %s max: %s" (IntOps.BigIntOps.to_string min) (IntOps.BigIntOps.to_string max); res
    | _ -> res

  let bound_texpr d texpr1 = timing_wrap "bounds calculation" (bound_texpr d) texpr1
end
*)
module D(Vc: AbstractVector) (Mx: AbstractMatrix) =
struct
  include Printable.Std
  include ConvenienceOps (Mpqf)
  include VarManagement 

  (* module Bounds = ExpressionBounds (Vc) (Mx) 

     module Convert = SharedFunctions.Convert (V) (Bounds) (struct let allow_global = true end) (SharedFunctions.Tracked) *)

  type var = V.t
  (* prints the current variable equalities with resolved variable names *)
  let show varM =
    let lookup i = Var.to_string (Environment.var_of_dim varM.env i) in
    let show_var i tuple =
        match tuple with
        | (None, offset) -> "Variable " ^ string_of_int i ^ " named " ^ (lookup i) ^ " equals " ^ Z.to_string offset
        | (Some index, offset) -> "Variable " ^ string_of_int i ^ " named " ^ (lookup i) ^ " equals " ^ lookup index ^ " + " ^ Z.to_string offset
    in match varM.d with
    | None -> "No equalities available"
    | Some arr -> Array.fold_left (fun acc elem -> acc ^ elem ) "" (Array.mapi show_var arr)  


  let pretty () (x:t) = text (show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>\nmatrix\n</key>\n<value>\n%s</value>\n<key>\nenv\n</key>\n<value>\n%s</value>\n</map>\n</value>\n" (XmlUtil.escape (Format.asprintf "%s" (show x) )) (XmlUtil.escape (Format.asprintf "%a" (Environment.print: Format.formatter -> Environment.t -> unit) (x.env)))

  let name () = "lin2vareq"

  let to_yojson _ = failwith "ToDo Implement in future"


  let is_bot t = equal t (bot ())

  let bot_env = {d = None; env = Environment.make [||] [||]}

  let is_bot_env t = t.d = None

  let top () = failwith "D.top ()"

  let is_top _ = false

  let is_top_env t = (not @@ Environment.equal empty_env t.env) (*&& GobOption.exists Matrix.is_empty t.d*)

  let meet t1 t2 =
    let sup_env = Environment.lce t1.env t2.env in
    let t1, t2 = change_d t1 sup_env true false, change_d t2 sup_env true false
    in if is_bot t1 || is_bot t2 then bot() else
      let m1, m2 = Option.get t1.d, Option.get t2.d in
      match m1, m2 with
      | x, y when is_top_env t1-> {d = Some (dim_add (Environment.dimchange t2.env sup_env) y); env = sup_env}
      | x, y when is_top_env t2 -> {d = Some (dim_add (Environment.dimchange t1.env sup_env) x); env = sup_env}
      | x, y -> bot()
  (*let rref_matr = Matrix.rref_matrix_with (Matrix.copy x) (Matrix.copy y) in
    if Option.is_none rref_matr then bot () else
    {d = rref_matr; env = sup_env}*)


  let meet t1 t2 =
    let res = meet t1 t2 in
    if M.tracing then M.tracel "meet" "meet a: %s b: %s -> %s \n" (show t1) (show t2) (show res) ;
    res

  let meet t1 t2 = timing_wrap "meet" (meet t1) t2

  let leq t1 t2 =
    let env_comp = Environment.compare t1.env t2.env in (* Apron's Environment.compare has defined return values. *)
    if env_comp = -2 || env_comp > 0 then false else
    if is_bot t1 || is_top_env t2 then true else
    if is_bot t2 || is_top_env t1 then false else (
      let m1, m2 = Option.get t1.d, Option.get t2.d in
      let m1' = if env_comp = 0 then m1 else dim_add (Environment.dimchange t1.env t2.env) m1 in
      true)

  let leq a b = timing_wrap "leq" (leq a) b

  let leq t1 t2 =
    let res = leq t1 t2 in
    if M.tracing then M.tracel "leq" "leq a: %s b: %s -> %b \n" (show t1) (show t2) res ;
    res

  let join a b = a
  (*let rec lin_disjunc r s a b =
    if s >= Vector.length a then a else
      let case_two a r col_b =
        let a_r = Matrix.get_row a r in
        Matrix.map2i_with (fun i x y -> if i < r then
                              Vector.map2_with (fun u j -> u +: y *: j) x a_r; x) a col_b;
        Matrix.remove_row a r
      in
      let case_three a b col_a col_b max =
        let col_a, col_b = Vector.copy col_a, Vector.copy col_b in
        let col_a, col_b = Vector.keep_vals col_a max, Vector.keep_vals col_b max in
        if Vector.equal col_a col_b then (a, b, max) else
          let a_rev, b_rev = (Vector.rev_with col_a; col_a), (Vector.rev_with col_b; col_b) in
          let i = Vector.find2i (fun x y -> x <>: y) a_rev b_rev in
          let (x, y) = Vector.nth a_rev i, Vector.nth b_rev i in
          let r, diff = Vector.length a_rev - (i + 1), x -: y  in
          let a_r, b_r = Matrix.get_row a r, Matrix.get_row b r in
          let sub_col =
            Vector.map2_with (fun x y -> x -: y) a_rev b_rev;
            Vector.rev_with a_rev;
            a_rev
          in
          let multiply_by_t m t =
            Matrix.map2i_with (fun i' x c -> if i' <= max then (let beta = c /: diff in
                                                                Vector.map2_with (fun u j -> u -: (beta *: j)) x t); x) m sub_col;
            m
          in
          Matrix.remove_row (multiply_by_t a a_r) r, Matrix.remove_row (multiply_by_t b b_r) r, (max - 1)
      in
      let col_a, col_b = Matrix.get_col a s, Matrix.get_col b s in
      let nth_zero v i =  match Vector.nth v i with
        | exception Invalid_argument _ -> Mpqf.zero
        | x -> x
      in
      let a_rs, b_rs = nth_zero col_a r, nth_zero col_b r in
      if not (Z.equal (Mpqf.get_den a_rs) Z.one) || not (Z.equal (Mpqf.get_den b_rs) Z.one) then failwith "Matrix not in rref form" else
        begin match Int.of_float @@ Mpqf.to_float @@ a_rs, Int.of_float @@ Mpqf.to_float @@ b_rs with (* TODO: is it safe to go through floats? *)
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
      {d = Some (lin_disjunc 0 0 (Matrix.copy mod_x) (Matrix.copy mod_y)); env = sup_env}
    | x, y when Matrix.equal x y -> {d = Some x; env = a.env}
    | x, y  -> {d = Some(lin_disjunc 0 0 (Matrix.copy x) (Matrix.copy y)); env = a.env}
  *)
  let join a b = timing_wrap "join" (join a) b

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

  (* implemented as described on page 10 in the paper about Fast Interprocedural Linear Two-Variable Equalities in the Section "Abstract Effect of Statements" *)
  let assign_texpr (t: VarManagement.t) var texp =
    let assigned_var = Environment.dim_of_var t.env var  (* this is the variable we are assigning to *) in
    begin match t.d with 
      | Some d ->   
        begin match d.(assigned_var) with
          | exception Failure _ -> bot() (* we don't have any information about the variable assigned_var yet*)
          | rhs -> (* rhs is the current equality with assigned_var on the left hand side *) 
            let abstract_exists_var = abstract_exists var t in
            begin match get_coeff t texp with
              | None -> (* Statement "assigned_var = ?" (non-linear assignment) *) abstract_exists_var
              | Some (exp_var_opt, off) -> 
                begin match exp_var_opt with
                  | None -> (* Statement "assigned_var = off" (constant assignment) *) 
                    assign_const abstract_exists_var assigned_var off
                  | Some exp_var (* Statement "assigned_var = exp_var + off" (linear assignment) *) 
                    -> begin if assigned_var = exp_var then 
                          (* Statement "assigned_var = assigned_var + off" *)
                          subtract_const_from_var t assigned_var off
                        else
                          let empty_array = EqualitiesArray.make_empty_array (VarManagement.size t) in
                          let added_equality = empty_array.(exp_var) <- (Some assigned_var, off); empty_array in
                          meet abstract_exists_var {d = Some added_equality; env = t.env}  (* change reference variable*)
                      end 
                end
            end

        end 
      | None -> bot () end 



  let assign_texpr t var texp = timing_wrap "assign_texpr" (assign_texpr t var) texp

  let assign_exp (t: VarManagement(Vc)(Mx).t) var exp (no_ov: bool Lazy.t) =
    let t = if not @@ Environment.mem_var t.env var then add_vars t [var] else t in
    match Convert.texpr1_expr_of_cil_exp t t.env exp (Lazy.force no_ov) with
    | exp -> assign_texpr t var exp
    | exception Convert.Unsupported_CilExp _ ->
      if is_bot t then t else forget_vars t [var]

  let assign_exp t var exp no_ov =
    let res = assign_exp t var exp no_ov in
    if M.tracing then M.tracel "ops" "assign_exp t:\n %s \n var: %s \n exp: %a\n no_ov: %b -> \n %s\n"
        (show t) (Var.to_string var) d_exp exp (Lazy.force no_ov) (show res) ;
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
    let primed_vars = List.init (List.length assigned_vars) (fun i -> Var.of_string (Int.to_string i  ^"'")) in (* TODO: we use primed vars in analysis, conflict? *)
    let t_primed = add_vars t primed_vars in
    let multi_t = List.fold_left2 (fun t' v_prime (_,v') -> assign_var t' v_prime v') t_primed primed_vars vv's in
    match multi_t.d with
    | Some m when not @@ is_top_env multi_t -> let replace_col m x y = let dim_x, dim_y = Environment.dim_of_var multi_t.env x, Environment.dim_of_var multi_t.env y in
                                                 let col_x = Matrix.get_col m dim_x in
                                                 Matrix.set_col_with m col_x dim_y in
      let m_cp = Matrix.copy m in
      let switched_m = List.fold_left2 (fun m' x y -> replace_col m' x y) m_cp primed_vars assigned_vars in
      let res = drop_vars {d = Some switched_m; env = multi_t.env} primed_vars true in
      let x = Option.get res.d in
      if Matrix.normalize_with x then {d = Some x; env = res.env} else bot ()
    | _ -> t

  let assign_var_parallel t vv's =
    let res = assign_var_parallel t vv's in
    if M.tracing then M.tracel "ops" "assign_var parallel: %s -> %s \n" (show t) (show res);
    res

  let assign_var_parallel t vv's = timing_wrap "var_parallel" (assign_var_parallel t) vv's

  let assign_var_parallel_with t vv's =
    let t' = assign_var_parallel t vv's in
    t.d <- t'.d;
    t.env <- t'.env

  let assign_var_parallel_with t vv's =
    if M.tracing then M.tracel "var_parallel" "assign_var parallel'\n";
    assign_var_parallel_with t vv's

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
    in if M.tracing then M.tracel "ops" "Substitute_expr t: \n %s \n var: %s \n exp: %a \n -> \n %s\n" (show t) (Var.to_string var) d_exp exp (show res);
    res

  let substitute_exp t var exp ov = timing_wrap "substitution" (substitute_exp t var exp) ov

  (** Assert a constraint expression.

      Additionally, we now also refine after positive guards when overflows might occur and there is only one variable inside the expression and the expression is an equality constraint check (==).
      We check after the refinement if the new value of the variable is outside its integer bounds and if that is the case, either revert to the old state or set it to bottom. *)

  exception NotRefinable

  let meet_tcons_one_var_eq res expr =
    let overflow_res res = if IntDomain.should_ignore_overflow (Cilfacade.get_ikind_exp expr) then res else raise NotRefinable in
    match Convert.find_one_var expr with
    | None -> overflow_res res
    | Some v ->
      let ik = Cilfacade.get_ikind v.vtype in
      match Bounds.bound_texpr res (Convert.texpr1_of_cil_exp res res.env (Lval (Cil.var v)) true) with
      | Some _, Some _ when not (Cil.isSigned ik) -> raise NotRefinable (* TODO: unsigned w/o bounds handled differently? *)
      | Some min, Some max ->
        assert (Z.equal min max); (* other bounds impossible in affeq *)
        let (min_ik, max_ik) = IntDomain.Size.range ik in
        if Z.compare min min_ik < 0 || Z.compare max max_ik > 0 then
          if IntDomain.should_ignore_overflow ik then bot () else raise NotRefinable
        else res
      | exception Convert.Unsupported_CilExp _
      | _, _ -> overflow_res res

  let meet_tcons t tcons expr =
    let check_const cmp c = if cmp c Mpqf.zero then bot_env else t
    in
    let meet_vec e =
      (*Flip the sign of the const. val in coeff vec*)
      Vector.mapi_with (fun i x -> if Vector.compare_length_with e (i + 1) = 0 then Mpqf.mone *: x else x) e;
      let res = if is_bot t then bot () else
          let opt_m = Matrix.rref_vec_with (Matrix.copy @@ Option.get t.d) e
          in if Option.is_none opt_m then bot () else {d = opt_m; env = t.env} in
      meet_tcons_one_var_eq res expr
    in
    match get_coeff_vec t (Texpr1.to_expr @@ Tcons1.get_texpr1 tcons) with
    | Some v ->
      begin match get_c v, Tcons1.get_typ tcons with
        | Some c, DISEQ -> check_const (=:) c
        | Some c, SUP -> check_const (<=:) c
        | Some c, EQ -> check_const (<>:) c
        | Some c, SUPEQ -> check_const (<:) c
        | None, DISEQ
        | None, SUP ->
          begin match meet_vec v with
            | exception NotRefinable -> t
            | res -> if equal res t then bot_env else t
          end
        | None, EQ ->
          begin match meet_vec v with
            | exception NotRefinable -> t
            | res -> if is_bot res then bot_env else res
          end
        | _, _ -> t
      end
    | None -> t

  let meet_tcons t tcons expr = timing_wrap "meet_tcons" (meet_tcons t tcons) expr

  let unify a b =
    meet a b

  let unify a b =
    let res = unify a b  in
    if M.tracing then M.tracel "ops" "unify: %s %s -> %s\n" (show a) (show b) (show res);
    res

  let assert_cons d e negate no_ov =
    let no_ov = Lazy.force no_ov in
    if M.tracing then M.tracel "assert_cons" "assert_cons with expr: %a %b" d_exp e no_ov;
    match Convert.tcons1_of_cil_exp d d.env e negate no_ov with
    | tcons1 -> meet_tcons d tcons1 e
    | exception Convert.Unsupported_CilExp _ -> d

  let assert_cons d e negate no_ov = timing_wrap "assert_cons" (assert_cons d e negate) no_ov

  let relift t = t

  let invariant t =
    match t.d with
    | None -> []
    | Some m ->
      let eEqualitiesArray = Lincons1.EqualitiesArray_make t.env (Matrix.num_rows m) in
      for i = 0 to Lincons1.EqualitiesArray_length eEqualitiesArray do
        let row = Matrix.get_row m i in
        let coeff_vars = List.map (fun x ->  Coeff.s_of_mpqf @@ Vector.nth row (Environment.dim_of_var t.env x), x) (vars t) in
        let cst = Coeff.s_of_mpqf @@ Vector.nth row (Vector.length row - 1) in
        Lincons1.set_list (Lincons1.EqualitiesArray_get eEqualitiesArray i) coeff_vars (Some cst)
      done;
      let {lincons0_EqualitiesArray; EqualitiesArray_env}: Lincons1.eEqualitiesArray = eEqualitiesArray in
      EqualitiesArray.enum lincons0_EqualitiesArray
      |> Enum.map (fun (lincons0: Lincons0.t) ->
          Lincons1.{lincons0; env = EqualitiesArray_env}
        )
      |> List.of_enum

  let cil_exp_of_lincons1 = Convert.cil_exp_of_lincons1

  let env (t: Bounds.t) = t.env

  type marshal = Bounds.t

  let marshal t = t

  let unmarshal t = t
end

module D2(Vc: AbstractVector) (Mx: AbstractMatrix): RelationDomain.S3 with type var = Var.t =
struct
  module D =  D (Vc) (Mx)
  include SharedFunctions.AssertionModule (V) (D)
  include D

end

