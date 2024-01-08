(** OCaml implementation of the linear two-Variable equalitie domain.

    @see <http://doi.acm.org/10.1145/2049706.2049710> A. Flexeder, M. Petter, and H. Seidl Fast Interprocedural Linear Two-Variable Equalities. *)

(** Abstract states in this domain are represented by structs containing an array and an apron environment.
    The arrays are modeled as proposed in the paper: Each variable is assigned to an index and each array element represents a linear relationship that must hold at the corresponding program point.
    The apron environment is hereby used to organize the order of columns and variables.
*)

open Batteries
open GoblintCil
open Pretty
module M = Messages
open Apron
open VectorMatrix



module Mpqf = SharedFunctions.Mpqf

module Equality = struct
  (* (Some i, k) represents a sum of a variable with index i and the number k.
     (None, k) represents the number k. *)
  type t = (int option * Z.t) [@@deriving eq, ord, hash]
  let zero = (None, Z.zero)
  let var_zero i = (Some i, Z.zero)
  let to_int x = Z.to_int @@ snd x
  let print (a, b) = match a with
    | None -> print_endline @@ "(None , " ^ Z.to_string b ^ ")"
    | Some x -> print_endline @@ "(Some " ^ string_of_int x ^ ", " ^ Z.to_string b ^ ")"
end

module EqualitiesArray = struct
  include Array
  type t = Equality.t Array.t [@@deriving eq, ord]

  let print = Array.iter (fun k -> Equality.print k)

  let hash : t -> int = Array.fold_left (fun acc a -> 31 * acc + Equality.hash a) 0

  let empty () = [||]

  let make_empty_array len = Array.init len (fun i -> (Some i, Z.zero))

  let add_empty_column arr index =
    let num_vars = length arr in
    if index > num_vars then failwith "n too large" else
      let new_array = make (num_vars + 1) (Equality.var_zero index) in
      if index = 0
      then blit arr 0 new_array 1 (num_vars - 1)
      else blit arr 0 new_array 0 index;
      if index <> num_vars
      then blit arr index new_array (index + 1) (num_vars - index);
      new_array

  let add_empty_columns m indexes = (** same as add_empty_columns for Matrix (see vectorMatrix.ml)*)
    let nnc = length indexes in
    if nnc = 0 then m else
      let nc = length m in
      let offset = ref 0 in
      let offset_map = Array.init nc (fun j ->
          while !offset < nnc && !offset + j = indexes.(!offset) do incr offset; done;
          !offset)
      in let add_offset_to_array_entry (var, offs) =
           Option.map (fun var_index -> var_index + offset_map.(var_index)) var, offs in
      let m' = make_empty_array (nc + nnc)
      in Array.iteri (fun j eq -> m'.(j + offset_map.(j)) <- add_offset_to_array_entry eq) m;
      m'

  let del_cols m indexes =
    let nrc = length indexes in
    if nrc = 0 || length m = 0 then m
    else
      let nc = length m in
      if nc = nrc then [||] else
        let offset = ref 0 in
        let offset_map = Array.init nc (fun j ->
            if !offset < nrc && indexes.(!offset) = j then (incr offset; !offset) else !offset)
        in let remove_offset_from_array_entry (var, offs) =
             Option.map (fun var_index -> var_index - offset_map.(var_index)) var, offs in
        Array.init (nc - nrc) (fun j -> remove_offset_from_array_entry m.(j + offset_map.(j)))

  let del_cols m cols = timing_wrap "del_cols" (del_cols m) cols

  let is_empty m = length m = 0

  let is_top_array = GobArray.for_alli (fun i (a, e) -> GobOption.exists ((=) i) a && Z.(e = zero))

  let find_reference_variable d var_index = fst d.(var_index)


  let find_vars_in_the_connected_component d ref_var =
    filter (fun i -> let (var, _) = d.(i) in var = ref_var) (mapi const d)

  (* find a variable in the connected component with the least index, but not the reference variable. *)
  let find_var_in_the_connected_component_with_least_index connected_component ref_var =
    fold_left (fun curr_min i -> match curr_min with
        | None -> if i <> ref_var then Some i else None
        | Some curr_min -> if i < curr_min && i <> ref_var then Some i else Some curr_min) None connected_component

  (* Forget information about variable var in-place.
     The name reduce_col_with is because the affineEqualitiesDomain also defines this function,
     and it represents the equalities with a matrix, not like in this case with an array.
     We could think about changing this name, then we would need to change it also in
     shared_Functions.apron.ml and vectorMatrix.ml and affineEqualitiesDomain.ml *)
  let reduce_col_with d var =
    let ref_var_opt = find_reference_variable d var in
    d.(var) <- Equality.var_zero var;
    begin match ref_var_opt with
      | None -> (* the variable is equal to a constant *) ()
      | Some ref_var ->
        if ref_var <> var then ()
        else
          (* x_i is the reference variable of its connected component *)
          let dim_of_var = Some var in
          let connected_component = find_vars_in_the_connected_component d dim_of_var in
          if length connected_component = 1
          then ()   (* x_i is the only element of its connected component *)
          else
            (* x_i is the reference variable -> we need to find a new reference variable *)
            let var_least_index = Option.get @@ find_var_in_the_connected_component_with_least_index connected_component ref_var in
            let (_, off) = d.(var_least_index) in
            iteri (fun _ x -> let (_, off2) = d.(x) in if x <> ref_var then d.(x) <- (Some var_least_index, Z.(off2 - off))) connected_component;
    end

  (* Forget information about variable i but not in-place *)
  let reduce_col m j =
    let copy = copy m in
    reduce_col_with copy j;
    copy

  let remove_zero_rows t = t

end


module V = RelationDomain.V

(** It defines the type t of the affine equality domain (a struct that contains an optional matrix and an apron environment) and provides the functions needed for handling variables (which are defined by RelationDomain.D2) such as add_vars remove_vars.
    Furthermore, it provides the function get_coeff_vec that parses an apron expression into a vector of coefficients if the apron expression has an affine form. *)
module VarManagement =
struct
  module EArray = EqualitiesArray
  include SharedFunctions.VarManagementOps (EArray)


  let size t = BatOption.map_default (fun d -> EArray.length d) 0 t.d

  (* Returns the constant value of a variable,
     if we know the constant value of this variable.
     Else it returns None. *)
  let get_variable_value_if_it_is_a_constant t var =
    let get_constant (var, off) = match var with
      | None -> Some off
      | _ -> None
    in
    Option.bind t.d (fun d -> get_constant d.(var))

  let get_coeff_vec (t: t) texp =
    (*Parses a Texpr to obtain a (coefficient, variable) pair list to repr. a sum of a variables that have a coefficient. If variable is None, the coefficient represents a constant offset.
    *)
    let open Apron.Texpr1 in
    let exception NotLinearExpr in
    let exception NotIntegerOffset in
    let negate coeff_var_list = List.map (fun (coeff, var) -> (Z.(-coeff), var)) coeff_var_list in
    let multiply_with_Z number coeff_var_list =
      List.map (fun (coeff, var) -> (Z.(number * coeff, var))) coeff_var_list in
    let multiply a b =
      (* if one of them is a constant, then multiply. Otherwise, the expression is not linear*)
      match a, b with
      | [(a_coeff, None)], b -> multiply_with_Z a_coeff b
      | a, [(b_coeff, None)] -> multiply_with_Z b_coeff a
      | _ -> raise NotLinearExpr
    in
    let mpqf_to_Z x =
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
                         | Mpqf x -> [(mpqf_to_Z x, None)]
                         | Mpfrf x -> raise NotIntegerOffset end in of_union x
        | Var x ->
          let var_dim = Environment.dim_of_var t.env x in
          begin match get_variable_value_if_it_is_a_constant t var_dim with
            | None -> [(Z.one, Some var_dim)]
            | Some constant -> [(constant, None)]
          end
        | Unop (u, e, _, _) ->
          begin match u with
            | Neg -> negate (convert_texpr e)
            | Cast -> convert_texpr e (*Ignore since casts in apron are used for floating point nums and rounding in contrast to CIL casts*)
            | Sqrt -> raise NotLinearExpr end
        | Binop (b, e1, e2, _, _) ->
          begin match b with
            | Add -> List.concat [convert_texpr e1; convert_texpr e2]
            | Sub -> List.concat [convert_texpr e1; negate (convert_texpr e2)]
            | Mul -> multiply (convert_texpr e1) (convert_texpr e2)
            | _ -> raise NotLinearExpr end
      end
    in match convert_texpr texp with
    | exception NotLinearExpr -> None
    | x -> Some(x)

  let get_coeff (t: t) texp =
    (*Parses a Texpr to obtain a (variable, offset) pair to repr. a sum of a variable and an offset.
      Returns None if the expression is not a sum between a variable (without coefficient) and a constant.
    *)
    let exception Not2VarExpr in
    let sum_next_coefficient (var, current_var_offset, curr_offset) (next_coeff, next_var) =
      begin match next_var with
        | None -> (* this element represents a constant offset *)
          (var, current_var_offset, Z.(curr_offset + next_coeff))
        | Some _ -> (* this element represents a variable with a coefficient
                              -> it must be always the same variable, else it's not a two-variable equality*)
          begin if Option.is_none var || next_var = var then
              (next_var, Z.(current_var_offset + next_coeff), curr_offset)
            else raise Not2VarExpr end
      end in
    let sum_coefficients summands_list_opt =
      Option.map (List.fold_left sum_next_coefficient (None, Z.zero, Z.zero)) summands_list_opt
    in
    match sum_coefficients (get_coeff_vec t texp) with
    | exception _ -> None
    | None -> None
    | Some (var, var_coeff, offset) ->
      if Option.is_none var then Some (None, offset)
      else if Z.(var_coeff = one) then Some (var, offset)
      else None


  let get_coeff t texp = timing_wrap "coeff_vec" (get_coeff t) texp

  let abstract_exists var t = match t.d with
    | Some d -> {t with d = Some (EArray.reduce_col d (Environment.dim_of_var t.env var))}
    | None -> t (* there are no  variables in the current environment *)

  let assign_const t var const = match t.d with
    | None -> t
    | Some t_d -> let d = EArray.copy t_d in d.(var) <- (None, const);  {d = Some d; env = t.env}

  let subtract_const_from_var t var const =
    match t.d with
    | None -> t
    | Some t_d -> let d = EArray.copy t_d in
      let subtract_const_from_var_for_single_equality index element =
        let (eq_var_opt, off2) = d.(index) in
        if index = var then
          match eq_var_opt with
          | None -> d.(index) <- (None, Z.(off2 + const))
          | Some eq_var -> begin if eq_var <> index then d.(index) <- (eq_var_opt, Z.(off2 + const)) end
        else
          begin match eq_var_opt with
            | Some eq_var ->
              if eq_var = var then d.(index) <- (eq_var_opt, Z.(off2 - const))
            | None -> ()
          end
      in
      EArray.iteri (subtract_const_from_var_for_single_equality) d; {d = Some d; env = t.env}
end


(** TODO: overflow checking *)
module ExpressionBounds: (SharedFunctions.ExtendedConvBounds with type t = VarManagement.t) =
struct
  include VarManagement

  let bound_texpr t texpr = let texpr = Texpr1.to_expr texpr in
    match get_coeff t texpr with
    | Some (None, offset) -> Some offset, Some offset
    | _ -> None, None


  let bound_texpr d texpr1 =
    let res = bound_texpr d texpr1 in
    match res with
    | Some min, Some max ->  if M.tracing then M.tracel "bounds" "min: %s max: %s" (IntOps.BigIntOps.to_string min) (IntOps.BigIntOps.to_string max); res
    | _ -> res

  let bound_texpr d texpr1 = timing_wrap "bounds calculation" (bound_texpr d) texpr1
end

module D =
struct
  include Printable.Std
  include ConvenienceOps (Mpqf)
  include VarManagement

  module Bounds = ExpressionBounds

  module Convert = SharedFunctions.Convert (V) (Bounds) (struct let allow_global = true end) (SharedFunctions.Tracked)

  type var = V.t

  let name () = "lin2vareq"

  let to_yojson _ = failwith "ToDo Implement in future"

  let is_bot t = equal t (bot ())
  let is_bot_env t = t.d = None

  (*this shows "top" for a specific environment to enable the calculations. It is the top_of of all equalities*)
  let top_of env = {d = Some (EArray.make_empty_array (Environment.size env)); env = env}

  (*Should never be called but implemented for completeness *)
  let top () = {d = Some (EArray.empty()); env = empty_env}

  (*is_top returns true for top_of array and empty array *)
  let is_top t = GobOption.exists EArray.is_top_array t.d

  (* prints the current variable equalities with resolved variable names *)
  let show varM =
    let lookup i = Var.to_string (Environment.var_of_dim varM.env i) in
    let show_var i tuple =
      match tuple with
      | (None, offset) -> "Variable " ^ string_of_int i ^ " named " ^ (lookup i) ^ " equals " ^ Z.to_string offset ^ "\n"
      | (Some index, offset) -> "Variable " ^ string_of_int i ^ " named " ^ (lookup i) ^ " equals " ^ lookup index ^ " + " ^ Z.to_string offset ^ "\n"
    in if is_top varM then "⊤\n" else
      match varM.d with
      | None -> "⊥\n"
      | Some arr -> if is_bot varM then "Bot \n" else Array.fold_left (fun acc elem -> acc ^ elem ) "" (Array.mapi show_var arr)

  let pretty () (x:t) = text (show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>\nequalities-array\n</key>\n<value>\n%s</value>\n<key>\nenv\n</key>\n<value>\n%s</value>\n</map>\n</value>\n" (XmlUtil.escape (Format.asprintf "%s" (show x) )) (XmlUtil.escape (Format.asprintf "%a" (Environment.print: Format.formatter -> Environment.t -> unit) (x.env)))

  let meet t1 t2 =
    let sup_env = Environment.lce t1.env t2.env in
    let t1, t2 = change_d t1 sup_env ~add:true ~del:false, change_d t2 sup_env ~add:true ~del:false in
    let subst_var ts x (vart, bt) =
      let adjust (vare, b') =
        if Option.eq ~eq:Int.equal (Some x) vare then (vart, Z.(b' + bt)) else (vare, b') in Option.may (BatArray.modify adjust) !ts
    in
    let add_conj ts (var, b) i =
      let adjust ts' =
        let (var1, b1) = ts'.(i) in
        (match var, var1 with
         | None, None -> if not @@ Z.equal b b1 then ts := None;
         | None, Some h1 -> subst_var ts h1 (None, Z.(b - b1))
         | Some j, None -> subst_var ts j (None, Z.(b1 - b))
         | Some j, Some h1 ->
           (match ts'.(j) with
            | (None, b2) -> subst_var ts i (None, Z.(b2 + b))
            | (Some h2, b2) ->
              if h1 = h2 then
                (if Z.(b1 <> (b2 + b)) then ts := None)
              else if h1 < h2 then subst_var ts h2 (Some h1, Z.(b1 - (b + b2)))
              else subst_var ts h1 (Some h2, Z.(b + (b2 - b1)))))
      in
      Stdlib.Option.iter adjust !ts
    in
    match t1.d, t2.d with
    | Some d1', Some d2' -> (
        let ds = ref (Some (Array.copy d1')) in
        Array.iteri (fun j e -> add_conj ds e j) d2';
        {d = !ds; env = sup_env} )
    | _ -> {d = None; env = sup_env}

  let meet t1 t2 =
    let res = meet t1 t2 in
    if M.tracing then M.tracel "meet" "meet a: %s b: %s -> %s \n" (show t1) (show t2) (show res) ;
    res

  let meet t1 t2 = timing_wrap "meet" (meet t1) t2

  let leq t1 t2 =
    let env_comp = Environment.compare t1.env t2.env in (* Apron's Environment.compare has defined return values. *)
    let implies ts (var, b) i =
      let tuple_cmp = Tuple2.eq (Option.eq ~eq:Int.equal) (Z.equal) in
      match var with
      | None -> tuple_cmp (var, b) ts.(i)
      | Some j -> tuple_cmp ts.(i) @@ Tuple2.map2 (Z.add b) ts.(j)
    in
    if env_comp = -2 || env_comp > 0 then false else
    if is_bot_env t1 || is_top t2 then true else
    if is_bot_env t2 || is_top t1 then false else
      let m1, m2 = Option.get t1.d, Option.get t2.d in
      let m1' = if env_comp = 0 then m1 else dim_add (Environment.dimchange t1.env t2.env) m1 in
      GobArray.for_alli (fun i t -> implies m1' t i) m2

  let leq a b = timing_wrap "leq" (leq a) b

  let leq t1 t2 =
    let res = leq t1 t2 in
    if M.tracing then M.tracel "leq" "leq a: %s b: %s -> %b \n" (show t1) (show t2) res ;
    res

  let join a b =
    let join_d ad bd =
      (*use copy of ad because result is later saved in there*)
      let ad = Array.copy ad in
      (*This is the table which is later grouped*)
      let table = BatList.map2i (fun i (ai, aj) (bi,bj) -> (i, Z.(aj - bj), (ai, aj), (bi,bj))) (Array.to_list ad) (Array.to_list bd) in
      (*let diff t1 t2 = Z.(snd t1 - snd t2) in*)
      (*compare two variables for grouping depending on delta function and reference index*)
      let cmp_z (_, t0i, t1i, t2i) (_, t0j, t1j, t2j) =
        let cmp_ref = Option.compare ~cmp:Int.compare in
        Tuple3.compare ~cmp1:cmp_ref ~cmp2:cmp_ref ~cmp3:Z.compare (fst t1i, fst t2i, t0i) (fst t1j, fst t2j, t0j)
      in
      (*Calculate new components as groups*)
      let new_components = BatList.group cmp_z table in
      (*Adjust the domain array to represent the new components*)
      let modify idx_h b_h (idx, _, (opt1, z1), (opt2, z2)) =
        if opt1 = opt2 && Z.equal z1 z2 then ()
        else ad.(idx) <- (Some idx_h, Z.(z1 - b_h))
      in
      let iterate l =
        match l with
        | (idx_h, _, (_, b_h), _) :: t ->  List.iter (modify idx_h b_h) l
        | [] -> () (*This should not happen, consider throwing exception*)
      in
      List.iter iterate new_components; Some ad
    in
    (*Normalize the two domains a and b such that both talk about the same variables*)
    if is_bot_env a then b else if is_bot_env b then a
    else
      match Option.get a.d, Option.get b.d with
      | x, y when is_top a || is_top b -> let new_env = Environment.lce a.env b.env
        in (top_of new_env)
      | x, y when (Environment.compare a.env b.env <> 0) ->
        let sup_env = Environment.lce a.env b.env in
        let mod_x = dim_add (Environment.dimchange a.env sup_env) x in
        let mod_y = dim_add (Environment.dimchange b.env sup_env) y in
        {d = join_d mod_x mod_y; env = sup_env}
      | x, y when EArray.equal x y -> {d = Some x; env = a.env}
      | x, y  -> {d = join_d x y; env = a.env}

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

  let remove_rels_with_var x var env imp =
    let j0 = Environment.dim_of_var env var in
    if imp then (EArray.reduce_col_with x j0; x) else EArray.reduce_col x j0

  let narrow a b = a
  let pretty_diff () (x, y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  let forget_vars t vars =
    if is_bot_env t || is_top t then t
    else
      let m = Option.get t.d in
      if List.is_empty vars then t else
        let rec rem_vars m vars' =
          begin match vars' with
            |            [] -> m
            | x :: xs -> rem_vars (remove_rels_with_var m x t.env true) xs end
        in {d = Some (EArray.remove_zero_rows @@ rem_vars (EArray.copy m) vars); env = t.env}

  let forget_vars t vars =
    let res = forget_vars t vars in
    if M.tracing then M.tracel "ops" "forget_vars %s -> %s\n" (show t) (show res);
    res

  let forget_vars t vars = timing_wrap "forget_vars" (forget_vars t) vars

  (* implemented as described on page 10 in the paper about Fast Interprocedural Linear Two-Variable Equalities in the Section "Abstract Effect of Statements"
     This makes a copy of the data structure, it doesn't change it in-place. *)
  let assign_texpr (t: VarManagement.t) var texp =
    let assigned_var = Environment.dim_of_var t.env var (* this is the variable we are assigning to *) in
    begin match t.d with
      | Some d ->
        let abstract_exists_var = abstract_exists var t in
        begin match get_coeff t texp with
          | None -> (* Statement "assigned_var = ?" (non-linear assignment) *)
            abstract_exists_var
          | Some (None, off) ->
            (* Statement "assigned_var = off" (constant assignment) *)
            assign_const abstract_exists_var assigned_var off
          | Some (Some exp_var, off) when assigned_var = exp_var ->
            (* Statement "assigned_var = assigned_var + off" *)
            subtract_const_from_var t assigned_var off
          | Some (Some exp_var, off) ->
            (* Statement "assigned_var = exp_var + off" (assigned_var is not the same as exp_var) *)
            let added_equality = EqualitiesArray.make_empty_array (VarManagement.size t) in
            added_equality.(assigned_var) <- (Some exp_var, off);
            meet abstract_exists_var {d = Some added_equality; env = t.env}
        end
      | None -> bot_env end



  let assign_texpr t var texp = timing_wrap "assign_texpr" (assign_texpr t var) texp

  (* no_ov -> no overflow
     if it's true then there is no overflow
      -> Convert.texpr1_expr_of_cil_exp handles overflow *)
  let assign_exp (t: VarManagement.t) var exp (no_ov: bool Lazy.t) =
    let t = if not @@ Environment.mem_var t.env var then add_vars t [var] else t in
    match Convert.texpr1_expr_of_cil_exp t t.env exp (Lazy.force no_ov) with
    | exp -> assign_texpr t var exp
    | exception Convert.Unsupported_CilExp _ ->
      if is_bot_env t then t else forget_vars t [var]

  let assign_exp t var exp no_ov =
    let res = assign_exp t var exp no_ov in
    if M.tracing then M.tracel "ops" "assign_exp t:\n %s \n var: %s \n exp: %a\n no_ov: %b -> \n %s\n"
        (show t) (Var.to_string var) d_exp exp (Lazy.force no_ov) (show res) ;
    res
  let assign_var (t: VarManagement.t) v v' =
    let t = add_vars t [v; v'] in
    let texpr1 = Texpr1.of_expr (t.env) (Var v') in
    assign_texpr t v (Apron.Texpr1.to_expr texpr1)

  let assign_var t v v' =
    let res = assign_var t v v' in
    if M.tracing then M.tracel "ops" "assign_var t:\n %s \n v: %s \n v': %s\n -> %s\n" (show t) (Var.to_string v) (Var.to_string v') (show res) ;
    res
  (* This functionality is not common to C and is used for assignments of the form: x = y, y=x; which is not legitimate C grammar
     x and y should be assigned to the value of x and y before the assignment respectively.
     ==> x = y_old , y = x_old;
     Therefore first apply the assignments to temporary variables x' and y' to keep the old dependencies of x and y
     and in a second round assign x' to x and y' to y
  *)
  let assign_var_parallel t vv's =
    let assigned_vars = List.map (function (v, _) -> v) vv's in
    let t = add_vars t assigned_vars in
    let primed_vars = List.init (List.length assigned_vars) (fun i -> Var.of_string (Int.to_string i  ^"'")) in (* TODO: we use primed vars in analysis, conflict? *)
    let t_primed = add_vars t primed_vars in
    let multi_t = List.fold_left2 (fun t' v_prime (_,v') -> assign_var t' v_prime v') t_primed primed_vars vv's in
    match multi_t.d with
    | Some arr when not @@ is_top multi_t ->
      let switched_arr = List.fold_left2 (fun multi_t assigned_var primed_var-> assign_var multi_t assigned_var primed_var) multi_t assigned_vars primed_vars in
      let res = drop_vars switched_arr primed_vars ~del:true in
      let x = Option.get res.d in
      {d = Some x; env = res.env}
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

  let print_coeff_vec l (env : Environment.t) =
    let print_element e = match e with
      | (a, Some x) -> print_string ((Z.to_string a) ^ " * " ^ (Var.to_string ( Environment.var_of_dim env x)) ^ " + ")
      | (a, None) -> print_string ((Z.to_string a) ^ "+") in
    List.iter print_element  l; print_newline ()

  let print_final_expr l (env : Environment.t) =
    let print_element i a = if i = 0 then print_string ((Z.to_string a) ^ " + ") else
        print_string ((Z.to_string a) ^ " * " ^ (Var.to_string ( Environment.var_of_dim env (i-1))) ^ " + ") in
    List.iteri print_element l; print_newline ()

  (** Assert a constraint expression.

      Additionally, we now also refine after positive guards when overflows might occur and there is only one variable inside the expression and the expression is an equality constraint check (==).
      We check after the refinement if the new value of the variable is outside its integer bounds and if that is the case, either revert to the old state or set it to bottom. *)

  (* meet_tcons -> meet with guard in if statement
     texpr -> tree expr (right hand side of equality)
     -> expression used to derive tcons -> used to check for overflow
     tcons -> tree constraint (expression < 0)
     -> does not have types (overflow is type dependent)
  *)
  module BoundsCheck = SharedFunctions.BoundsCheckMeetTcons (Bounds) (V)

  let meet_tcons t tcons original_expr =
    (* The expression is evaluated using an array of coefficients. The first element of the array belongs to the constant followed by the coefficients of all variables
       depending on the result in the array after the evaluating including resolving the constraints in t.d the tcons can be evaluated and additional constraints can be added to t.d *)
    match t.d with
    | None -> bot_env
    | Some d ->
      let overflow_handling res original_expr =
        match BoundsCheck.meet_tcons_one_var_eq res original_expr with
        | exception BoundsCheck.NotRefinable -> t
        | res -> res
      in
      let expr = Array.init (Environment.size t.env) (fun _ -> Z.zero) in
      let constant = ref (Z.zero) in
      if is_bot_env t then bot_env else
        match get_coeff_vec t (Texpr1.to_expr @@ Tcons1.get_texpr1 tcons) with
        | None -> t (*The (in-) equality is not linear, therefore we don't know anything about it. *)
        | Some cv's ->
          let update (c, v) =
            match v with
            | None -> constant := Z.(!constant + c)
            | Some idx -> match d.(idx) with
              | (Some idx_i, c_i) -> constant := Z.(!constant + (c * c_i));
                expr.(idx_i) <- Z.(expr.(idx_i) + c)
              | (None, c_i) -> constant := Z.(!constant + (c * c_i))
          in
          List.iter update cv's;
          let var_count = GobArray.count_matchingi (fun _ a -> a <> Z.zero) expr in
          if var_count = 0 then
            match Tcons1.get_typ tcons with
            | EQ when Z.equal !constant Z.zero -> t
            | SUPEQ when Z.geq !constant Z.zero -> t
            | SUP when Z.gt !constant Z.zero -> t
            | DISEQ when not @@ Z.equal !constant Z.zero -> t
            | EQMOD scalar -> t
            | _ -> bot_env (*Not supported right now
                                  if Float.equal ( Float.modulo (Z.to_float expr.(0)) (convert_scalar scalar )) 0. then t else {d = None; env = t.env}*)
          else if var_count = 1 then
            let index = Array.findi (fun a -> not @@ Z.equal a Z.zero) expr in
            let var = (index, expr.(index)) in
            let c = if Z.divisible !constant @@ Tuple2.second var then Some (Z.(-(!constant) / (Tuple2.second var)))
              else None in
            match Tcons1.get_typ tcons, c with
            | EQ, Some c ->
              let expression = Texpr1.to_expr @@ Texpr1.cst t.env (Coeff.s_of_int @@ Z.to_int c) in
              let res = meet t (assign_texpr (top_of t.env) (Environment.var_of_dim t.env (Tuple2.first var)) expression)
              in overflow_handling res original_expr
            | _ -> t (*Not supported right now*)
          else if var_count = 2 then
            let get_vars i a l = if Z.equal a Z.zero then l else (i, a)::l in
            let v12 = Array.fold_righti get_vars expr [] in
            let a1 = Tuple2.second (List.hd v12) in
            let a2 = Tuple2.second (List.hd @@ List.tl v12) in
            let var1 = Environment.var_of_dim t.env (Tuple2.first (List.hd v12)) in
            let var2 = Environment.var_of_dim t.env (Tuple2.first (List.hd @@ List.tl v12)) in
            match Tcons1.get_typ tcons with
            | EQ ->
              let res = if Z.equal a1 Z.one && Z.equal a2 Z.one
                then meet t (assign_var (top_of t.env) var1 var2)
                else t
              in overflow_handling res original_expr
            | _-> t (*Not supported right now*)
          else
            t (*For any other case we don't know if the (in-) equality is true or false or even possible therefore we just return t *)

  let meet_tcons t tcons expr = timing_wrap "meet_tcons" (meet_tcons t tcons) expr

  let unify a b =
    meet a b

  let unify a b =
    let res = unify a b  in
    if M.tracing then M.tracel "ops" "unify: %s %s -> %s\n" (show a) (show b) (show res);
    res

  (* Assert a constraint expression. Defined in apronDomain.apron.ml

     If the constraint is never fulfilled, then return bottom.
     Else the domain can be modified with the new information given by the constraint.

     It basically just calls the function meet_tcons.

     It is called by eval (defined in sharedFunctions), but also when a guard in
     e.g. an if statement is encountered in the C code.

  *)
  let assert_cons d e negate no_ov =
    let no_ov = Lazy.force no_ov in
    if M.tracing then M.tracel "assert_cons" "assert_cons with expr: %a %b\n" d_exp e no_ov;
    match Convert.tcons1_of_cil_exp d d.env e negate no_ov with
    | tcons1 -> meet_tcons d tcons1 e
    | exception Convert.Unsupported_CilExp _ -> d

  let assert_cons d e negate no_ov = timing_wrap "assert_cons" (assert_cons d e negate) no_ov

  let relift t = t

  (* representation as C expression

     This function returns all the equalities that are saved in our datastructure t.

     Lincons -> linear constraint *)
  (*TODO*)
  let invariant t = []
  (*let invariant t =
    match t.d with
    | None -> []
    | Some m ->
      let linear_constraints =
        EArray.fold_left
        (fun acc row ->
          let coeff_vars = List.map (fun(var,off) -> Coeff.s_of_int off, Some var) row in
          let cst = Coeff.s_of_int (snd (List.hd row)) in
          Lincons1.make (Linexpr1.make t.env) Lincons1.EQ
          |> Lincons1.set_list coeff_vars (Some cst)
          |> (fun lc -> Lincons1.{lincons0 = Lincons0.of_lincons1 lc; env = t.env})
          :: acc)
        [] m
      in
      List.rev linear_constraints *)

  (* let invariant t =
     match t.d with
     | None -> []
     | Some m ->
       let linear_constraints =
         EArray.fold_left
           (fun acc row ->
             let lc =
               List.fold_left
                 (fun lc (var, off) ->
                   let coeff = Coeff.s_of_int off in
                   let var_opt = Some var in
                   Lincons1.set_coeff lc var_opt coeff)
                 (Lincons1.make (Linexpr1.make t.env) Lincons1.EQ)
                 row
               |> fun lc -> Lincons1.set_cst lc (Coeff.s_of_int (snd (List.hd row)))
             in
             Lincons1.{ lincons0 = Lincons0.of_lincons1 lc; env = t.env } :: acc)
           [] m
       in
       List.rev linear_constraints    *)


  let cil_exp_of_lincons1 = Convert.cil_exp_of_lincons1

  let env (t: Bounds.t) = t.env

  type marshal = Bounds.t
  (* marshal is not compatible with apron, therefore we don't have to implement it *)
  let marshal t = t

  let unmarshal t = t

end

module D2: RelationDomain.S3 with type var = Var.t =
struct
  module D = D
  include SharedFunctions.AssertionModule (V) (D)
  include D
end
