(** OCaml implementation of the linear two-variable equality domain.

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
  type t = (int option * (Z.t [@printer Z.pp_print])) [@@deriving eq, ord, hash, show]
  let zero = (None, Z.zero)
  let var_zero i = (Some i, Z.zero)
  let to_int x = Z.to_int @@ snd x
end

module EqualitiesArray = struct
  include Array
  type t = Equality.t Array.t [@@deriving eq, ord]

  let show m =
    Array.fold_right (fun k result -> Equality.show k ^ "\n" ^ result) m ""

  let hash : t -> int = Array.fold_left (fun acc a -> 31 * acc + Equality.hash a) 0

  let empty () = [||]

  let make_empty_array len = Array.init len (fun i -> (Some i, Z.zero))

  (** add new variables to domain with particular indices; translates old indices to keep consistency 
      the semantics of indexes can be retrieved from apron: https://antoinemine.github.io/Apron/doc/api/ocaml/Dim.html *)
  let add_variables_to_domain m indexes = 
    if length indexes = 0 then m else
      let offset_map = Array.make (Array.length m) 0 (* maps each variable to the number of variables that are added before this variable *)
      in
      let _ =
        let rec shift (offset, list) index = (* bumps offset & pops list, if/while index is heading the list *)
          match list with
          | hd::tl when hd = index -> shift (offset+1, tl) index
          | _ -> (offset, list)
        in  
        Array.fold_lefti (* this is not a textbook fold. We rather use it as a means to iterate over the range 
                            of all indices of offset_map, initializing the array at these indices as a side-effect. 
                            We use fold here as a means of having an accumulator to keep track of the current offset 
                            and the rest of the offset list. In case of more frequent use of this pattern, consider this as
                            a candidate template for a new library function *)
          (fun offsetcontext index _ ->
             let newoffset, newlist = shift offsetcontext index in
             offset_map.(index) <- newoffset;
             (newoffset, newlist))
          (0, Array.to_list indexes) offset_map
      in
      let add_offset_to_array_entry (var, offs) = (* uses offset_map to obtain a new var_index, that is consistent with the new reference indices *)
        Option.map (fun var_index -> var_index + offset_map.(var_index)) var, offs in
      let m' = make_empty_array (length m + length indexes)
      in Array.iteri (fun j eq -> m'.(j + offset_map.(j)) <- add_offset_to_array_entry eq) m;
      m'(* produces a consistent new conj. of equalities *)

  let remove_variables_from_domain m indexes =
    let nr_removed_colums = length indexes in
    if nr_removed_colums = 0 || length m = 0 then m
    else
    if length m = nr_removed_colums then [||] else
      let offset_map = Array.make (Array.length m) 0
      (* maps each variable to the number of variables that are removed before this variable *)
      in let _ = Array.fold_lefti
             (fun offset index _ ->
                if offset < nr_removed_colums && indexes.(offset) = index then offset + 1
                else (offset_map.(index) <- offset; offset))
             0 offset_map in
      let remove_offset_from_array_entry (var, offs) =
        Option.map (fun var_index -> var_index - offset_map.(var_index)) var, offs in
      Array.(filteri (fun i _ -> not @@ Array.mem i indexes) m (* filter out removed variables*)
             |> map remove_offset_from_array_entry) (* adjust variable indexes *)

  let remove_variables_from_domain m cols = timing_wrap "del_cols" (remove_variables_from_domain m) cols

  let is_empty m = length m = 0

  let is_top_array = GobArray.for_alli (fun i (a, e) -> GobOption.exists ((=) i) a && Z.equal e Z.zero)

  (* Forget information about variable var in-place. *)
  let forget_variable_with d var =
    (let ref_var_opt = fst d.(var) in
     match ref_var_opt with
     | Some ref_var when ref_var = var ->
       (* var is the reference variable of its connected component *)
       (let cluster = List.tl @@ fold_righti
            (fun i (ref, offset) l -> if ref = ref_var_opt then i::l else l) d [] in
        (* obtain cluster with common reference variable ref_var*)
        match cluster with (* new ref_var is taken from head of the cluster *)
        | head :: tail -> let headconst = snd d.(head) in (* take offset between old and new reference variable *)
          List.iter (fun i -> d.(i) <- Z.(Some head, snd d.(i) - headconst)) cluster (* shift offset to match new reference variable *)
        | _ -> ()) (* empty cluster means no work for us *)
     | _ -> ()) (* variable is either a constant or expressed by another refvar *)
  ; d.(var) <- Equality.var_zero var (* set d(var) to unknown, finally *)

  (* Forget information about variable i but not in-place *)
  let forget_variable m j =
    let copy = copy m in
    forget_variable_with copy j;
    copy

  let dim_add (ch: Apron.Dim.change) m =
    add_variables_to_domain m ch.dim

  let dim_add ch m = timing_wrap "dim add" (dim_add ch) m

  let dim_remove (ch: Apron.Dim.change) m ~del =
    if Array.length ch.dim = 0 || is_empty m then
      m
    else (
      Array.modifyi (+) ch.dim;
      let m' = Array.fold_left (fun y x -> forget_variable_with y x; y) (copy m) ch.dim in
      remove_variables_from_domain m' ch.dim)

  let dim_remove ch m ~del = VectorMatrix.timing_wrap "dim remove" (fun del -> dim_remove ch m ~del:del) del


end

(** [VarManagement] defines the type t of the affine equality domain (a record that contains an optional matrix and an apron environment) and provides the functions needed for handling variables (which are defined by [RelationDomain.D2]) such as [add_vars], [remove_vars].
    Furthermore, it provides the function [get_coeff_vec] that parses an apron expression into a vector of coefficients if the apron expression has an affine form. *)
module VarManagement =
struct
  module EArray = EqualitiesArray
  include SharedFunctions.VarManagementOps (EArray)

  let dim_add = EArray.dim_add
  let size t = BatOption.map_default (fun d -> EArray.length d) 0 t.d

  (** Parses a Texpr to obtain a (coefficient, variable) pair list to repr. a sum of a variables that have a coefficient. If variable is None, the coefficient represents a constant offset.
    **)
  let get_coeff_vec (t: t) texp =
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
    let rec convert_texpr texp =
      begin match texp with
        (*If x is a constant, replace it with its const. val. immediately*)
        | Cst (Interval _) ->failwith "Not a constant"
        | Cst (Scalar x) ->
          begin match SharedFunctions.int_of_scalar ?round:None x with
            | Some x -> [(x, None)]
            | None -> raise NotIntegerOffset end
        | Var x ->
          let var_dim = Environment.dim_of_var t.env x in
          begin match t.d with
            | None -> [(Z.one, Some var_dim)]
            | Some d ->
              (if Option.is_some (fst d.(var_dim)) then [(Z.one, fst d.(var_dim))]
               else [])
              @ [(snd d.(var_dim), None)]
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
    let d = Option.get t.d in
    let expr = Array.make (Environment.size t.env) Z.zero in
    let constant = ref (Z.zero) in
    match get_coeff_vec t texp with
    | None -> None (*The (in-) equality is not linear, therefore we don't know anything about it. *)
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
      let var_count = BatArray.count_matching (fun a -> not @@ Z.equal a Z.zero) expr in
      if var_count = 0 then Some (None, !constant)
      else if var_count = 1 then (
        let var = Array.findi (fun a -> not @@ Z.equal a Z.zero) expr in
        if Z.equal expr.(var) Z.one then Some (Some var, !constant)
        else None
      )
      else None


  let get_coeff t texp = timing_wrap "coeff_vec" (get_coeff t) texp

  let abstract_exists var t = match t.d with
    | Some d -> {t with d = Some (EArray.forget_variable d (Environment.dim_of_var t.env var))}
    | None -> t (* there are no  variables in the current environment *)

  (* Copy because function is not "with" so should not mutate inputs *)
  let assign_const t var const = match t.d with
    | None -> t
    | Some t_d ->
      let d = EArray.copy t_d in d.(var) <- (None, const);  {d = Some d; env = t.env}

  let subtract_const_from_var t var const =
    match t.d with
    | None -> t
    | Some t_d ->
      let d = EArray.copy t_d in
      let subtract_const_from_var_for_single_equality index (eq_var_opt, off2) =
        if index <> var then
          begin match eq_var_opt with
            | Some eq_var when eq_var = var ->
              d.(index) <- (eq_var_opt, Z.(off2 - const))
            | _ -> ()
          end
      in
      begin if d.(var) = (Some var, Z.zero)
      (* var is a reference variable -> it can appear on the right-hand side of an equality *)
        then
          EArray.iteri (subtract_const_from_var_for_single_equality) d
        else
          (* var never appears on the right hand side-> we only need to modify the array entry at index var *)
          d.(var) <- Tuple2.map2 (Z.add const) d.(var)
      end;
      {d = Some d; env = t.env}

end


module ExpressionBounds: (SharedFunctions.ConvBounds with type t = VarManagement.t) =
struct
  include VarManagement

  let bound_texpr t texpr =
    match get_coeff t (Texpr1.to_expr texpr) with
    | Some (None, offset) ->
      (if M.tracing then M.tracel "bounds" "min: %s max: %s" (IntOps.BigIntOps.to_string offset) (IntOps.BigIntOps.to_string offset);
       Some offset, Some offset)
    | _ -> None, None

  let bound_texpr d texpr1 = timing_wrap "bounds calculation" (bound_texpr d) texpr1
end

module D =
struct
  include Printable.Std
  include ConvenienceOps (Mpqf)
  include VarManagement

  module Bounds = ExpressionBounds
  module V = RelationDomain.V
  module Arg = struct
    let allow_global = true
    let do_overflow_check = false
  end
  module Convert = SharedFunctions.Convert (V) (Bounds) (Arg) (SharedFunctions.Tracked)

  type var = V.t

  let name () = "lin2vareq"

  let to_yojson _ = failwith "ToDo Implement in future"

  let is_bot t = equal t (bot ())
  let is_bot_env t = t.d = None

  (* this shows "top" for a specific environment to enable the calculations. It is the top_of of all equalities *)
  let top_of env = {d = Some (EArray.make_empty_array (Environment.size env)); env = env}

  (** Is not expected to be called but implemented for completeness *)
  let top () = {d = Some (EArray.empty()); env = empty_env}

  (** is_top returns true for top_of array and empty array *)
  let is_top t = GobOption.exists EArray.is_top_array t.d

  (** prints the current variable equalities with resolved variable names *)
  let show varM =
    let lookup i = Var.to_string (Environment.var_of_dim varM.env i) in
    let show_offs o = if Z.equal o Z.zero then "" else " + " ^ Z.to_string o in
    let show_var i = function
      | (None, o) -> (lookup i) ^ " = " ^ Z.to_string o ^ ";\n"
      | (Some index, o) when i <> index -> 
        (lookup i) ^ " = " ^ lookup index ^ show_offs o ^ ";\n"
      | _ -> ""
    in 
    match varM.d with
    | None -> "⊥\n"
    | Some arr when EArray.is_top_array arr -> "⊤\n"
    | Some arr ->
      if is_bot varM then 
        "Bot \n"
      else
        Array.fold_lefti (fun acc i elem -> acc ^ show_var i elem) "" arr

  let pretty () (x:t) = text (show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>\nequalities-array\n</key>\n<value>\n%s</value>\n<key>\nenv\n</key>\n<value>\n%s</value>\n</map>\n</value>\n" (XmlUtil.escape (Format.asprintf "%s" (show x) )) (XmlUtil.escape (Format.asprintf "%a" (Environment.print: Format.formatter -> Environment.t -> unit) (x.env)))
  let eval_interval ask = Bounds.bound_texpr

  exception Contradiction

  let meet_with_one_conj_with ts i (var, b) =
    let subst_var ts x (vart, bt) =
      let adjust = function
        | (Some vare, b') when vare = x -> (vart, Z.(b' + bt))
        | e -> e
      in
      BatArray.modify adjust ts
    in
    let (var1, b1) = ts.(i) in
    (match var, var1 with
     | None, None -> if not @@ Z.equal b b1 then raise Contradiction
     | None, Some h1 -> subst_var ts h1 (None, Z.(b - b1))
     | Some j, None -> subst_var ts j (None, Z.(b1 - b))
     | Some j, Some h1 ->
       (match ts.(j) with
        | (None, b2) -> subst_var ts i (None, Z.(b2 + b))
        | (Some h2, b2) ->
          if h1 = h2 then
            (if not @@ Z.equal b1 Z.(b2 + b) then raise Contradiction)
          else if h1 < h2 then subst_var ts h2 (Some h1, Z.(b1 - (b + b2)))
          else subst_var ts h1 (Some h2, Z.(b + (b2 - b1)))))

  let meet_with_one_conj t i e =
    match t.d with
    | None -> t
    | Some d -> 
      let res_d = Array.copy d in
      try
        meet_with_one_conj_with res_d i e;
        {d = Some res_d; env = t.env} 
      with Contradiction -> 
        {d = None; env = t.env} 

  let meet t1 t2 =
    let sup_env = Environment.lce t1.env t2.env in
    let t1 = change_d t1 sup_env ~add:true ~del:false in
    let t2 = change_d t2 sup_env ~add:true ~del:false in
    match t1.d, t2.d with
    | Some d1', Some d2' -> (
        try
          let res_d = Array.copy d1' in
          Array.iteri (meet_with_one_conj_with res_d) d2';
          {d = Some res_d; env = sup_env}
        with Contradiction -> 
          {d = None; env = sup_env}
      )
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
      let m1' = if env_comp = 0 then m1 else VarManagement.dim_add (Environment.dimchange t1.env t2.env) m1 in
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
        | [] -> let exception EmptyComponent in raise EmptyComponent
      in
      List.iter iterate new_components; Some ad
    in
    (*Normalize the two domains a and b such that both talk about the same variables*)
    if is_bot_env a then 
      b
    else if is_bot_env b then 
      a
    else
      match Option.get a.d, Option.get b.d with
      | x, y when is_top a || is_top b -> 
        let new_env = Environment.lce a.env b.env in 
        top_of new_env
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
    join a b

  let widen a b =
    let res = widen a b in
    if M.tracing then M.tracel "widen" "widen a: %s b: %s -> %s \n" (show a) (show b) (show res) ;
    res

  let narrow a b = meet a b

  let narrow a b =
    let res = narrow a b in
    if M.tracing then M.tracel "narrow" "narrow a: %s b: %s -> %s \n" (show a) (show b) (show res) ;
    res

  let pretty_diff () (x, y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  let forget_vars t vars =
    if is_bot_env t || is_top t || List.is_empty vars then 
      t
    else
      let m = EArray.copy @@ Option.get t.d in
      List.iter
        (fun var ->
           EArray.forget_variable_with m (Environment.dim_of_var t.env var))
        vars;
      {d = Some m; env = t.env}

  let forget_vars t vars =
    let res = forget_vars t vars in
    if M.tracing then M.tracel "ops" "forget_vars %s -> %s\n" (show t) (show res);
    res

  let forget_vars t vars = timing_wrap "forget_vars" (forget_vars t) vars

  (** implemented as described on page 10 in the paper about Fast Interprocedural Linear Two-Variable Equalities in the Section "Abstract Effect of Statements"
     This makes a copy of the data structure, it doesn't change it in-place. *)
  let assign_texpr (t: VarManagement.t) var texp =
    let assigned_var = Environment.dim_of_var t.env var (* this is the variable we are assigning to *) in
    match t.d with
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
          meet_with_one_conj abstract_exists_var assigned_var (Some exp_var, off)
      end
    | None -> bot_env

  let assign_texpr t var texp = timing_wrap "assign_texpr" (assign_texpr t var) texp

  (* no_ov -> no overflow
     if it's true then there is no overflow
      -> Convert.texpr1_expr_of_cil_exp handles overflow *)
  let assign_exp ask (t: VarManagement.t) var exp (no_ov: bool Lazy.t) =
    let t = if not @@ Environment.mem_var t.env var then add_vars t [var] else t in
    match Convert.texpr1_expr_of_cil_exp ask t t.env exp no_ov with
    | texp -> assign_texpr t var texp
    | exception Convert.Unsupported_CilExp _ ->
      if is_bot_env t then t else forget_vars t [var]

  let assign_exp ask t var exp no_ov =
    let res = assign_exp ask t var exp no_ov in
    if M.tracing then M.tracel "ops" "assign_exp t:\n %s \n var: %s \n exp: %a\n no_ov: %b -> \n %s\n"
        (show t) (Var.to_string var) d_exp exp (Lazy.force no_ov) (show res) ;
    res
  let assign_var (t: VarManagement.t) v v' =
    let t = add_vars t [v; v'] in
    let texpr1 = Texpr1.of_expr (t.env) (Var v') in
    assign_texpr t v @@ Apron.Texpr1.to_expr texpr1

  let assign_var t v v' =
    let res = assign_var t v v' in
    if M.tracing then M.tracel "ops" "assign_var t:\n %s \n v: %s \n v': %s\n -> %s\n" (show t) (Var.to_string v) (Var.to_string v') (show res) ;
    res

  (** Parallel assignment of variables.
      First apply the assignments to temporary variables x' and y' to keep the old dependencies of x and y
     and in a second round assign x' to x and y' to y
  *)
  let assign_var_parallel t vv's =
    let assigned_vars = List.map fst vv's in
    let t = add_vars t assigned_vars in
    let primed_vars = List.init (List.length assigned_vars) (fun i -> Var.of_string (Int.to_string i  ^"'")) in (* TODO: we use primed vars in analysis, conflict? *)
    let t_primed = add_vars t primed_vars in
    let multi_t = List.fold_left2 (fun t' v_prime (_,v') -> assign_var t' v_prime v') t_primed primed_vars vv's in
    match multi_t.d with
    | Some arr when not @@ is_top multi_t ->
      let switched_arr = List.fold_left2 (fun multi_t assigned_var primed_var-> assign_var multi_t assigned_var primed_var) multi_t assigned_vars primed_vars in
      drop_vars switched_arr primed_vars ~del:true
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

  let substitute_exp ask t var exp no_ov =
    let t = if not @@ Environment.mem_var t.env var then add_vars t [var] else t in
    let res = assign_exp ask t var exp no_ov in
    forget_vars res [var]

  let substitute_exp ask t var exp no_ov =
    let res = substitute_exp ask t var exp no_ov in 
    if M.tracing then M.tracel "ops" "Substitute_expr t: \n %s \n var: %s \n exp: %a \n -> \n %s\n" (show t) (Var.to_string var) d_exp exp (show res);
    res

  let substitute_exp ask t var exp no_ov = timing_wrap "substitution" (substitute_exp ask t var exp) no_ov

  let show_coeff_vec l (env : Environment.t) =
    let show_element = function
      | (a, Some x) -> ((Z.to_string a) ^ " * " ^ (Var.to_string (Environment.var_of_dim env x)) ^ " + ")
      | (a, None) -> ((Z.to_string a) ^ "+")
    in
    List.fold_right (fun k result -> show_element k ^ "\n" ^ result) l ""

  (** Assert a constraint expression.
      The overflow is completely handled by the flag "no_ov",
      which is set in relationAnalysis.ml via the function no_overflow.
      In case of a potential overflow, "no_ov" is set to false
      and Convert.tcons1_of_cil_exp will raise the exception Unsupported_CilExp Overflow

      meet_tcons -> meet with guard in if statement
     texpr -> tree expr (right hand side of equality)
     -> expression used to derive tcons -> used to check for overflow
     tcons -> tree constraint (expression < 0)
     -> does not have types (overflow is type dependent)
  *)
  let meet_tcons ask t tcons original_expr no_ov =
    (* The expression is evaluated using an array of coefficients. The first element of the array belongs to the constant followed by the coefficients of all variables
       depending on the result in the array after the evaluating including resolving the constraints in t.d the tcons can be evaluated and additional constraints can be added to t.d *)
    match t.d with
    | None -> bot_env (* same as is_bot_env t *)
    | Some d ->
      match get_coeff_vec t (Texpr1.to_expr @@ Tcons1.get_texpr1 tcons) with
      | None -> t (*The (in-) equality is not linear, therefore we don't know anything about it. *)
      | Some cv's ->
        let expr = Array.make (Environment.size t.env) Z.zero in
        let refconstant = ref (Z.zero) in
          let update (c, v) =
          match v with
          | None -> refconstant := Z.(!refconstant + c)
          | Some idx -> match d.(idx) with
            | (Some idx_i, c_i) -> refconstant := Z.(!refconstant + (c * c_i));
              expr.(idx_i) <- Z.(expr.(idx_i) + c)
            | (None, c_i) -> refconstant := Z.(!refconstant + (c * c_i))
        in
        List.iter update cv's; (* abstract simplification of the guard wrt. reference variables *)
        let var_count = Array.count_matching (fun a -> not @@ Z.equal a Z.zero) expr in
        let constant = !refconstant in (* containing the reference locally *)
        if var_count = 0 then
          match Tcons1.get_typ tcons with
          | EQ when Z.equal constant Z.zero -> t
          | SUPEQ when Z.geq constant Z.zero -> t
          | SUP when Z.gt constant Z.zero -> t
          | DISEQ when not @@ Z.equal constant Z.zero -> t
          | EQMOD scalar -> t
          | _ -> bot_env
        else if var_count = 1 then
          let index = Array.findi (fun a -> not @@ Z.equal a Z.zero) expr in
          let varexpr = expr.(index) in
          if Z.divisible constant varexpr && Tcons1.get_typ tcons = EQ then 
              meet_with_one_conj t index (None,  (Z.(-(constant) / varexpr)))
          else  
             t (*Not supported right now*)
        else if var_count = 2 then
          let get_vars i a l = if Z.equal a Z.zero then l else (i, a)::l in
          (* WIP: the following line sums up the next 5, but would yields warning
             let (var1,a1)::(var2,a2)::_ = Array.fold_righti get_vars expr [] in *)
          let v12 = Array.fold_righti get_vars expr [] in
          let a1 = snd (List.hd v12) in
          let a2 = snd (List.hd @@ List.tl v12) in
          let var1 = fst (List.hd v12) in
          let var2 = fst (List.hd @@ List.tl v12) in
          match Tcons1.get_typ tcons with
          | EQ ->
            let res =
              if Z.equal a1 Z.one && Z.equal a2 Z.(-one)
              then meet_with_one_conj t var2 (Some var1, constant)
              else if Z.equal a1 Z.(-one) && Z.equal a2 Z.one
              then meet_with_one_conj t var1 (Some var2, constant)
              else t
            in res
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

  (** Assert a constraint expression. Defined in apronDomain.apron.ml

     If the constraint is never fulfilled, then return bottom.
     Else the domain can be modified with the new information given by the constraint.

     It basically just calls the function meet_tcons.

     It is called by eval (defined in sharedFunctions), but also when a guard in
     e.g. an if statement is encountered in the C code.

  *)
  let assert_constraint ask d e negate (no_ov: bool Lazy.t) =
    if M.tracing then M.tracel "assert_constraint" "assert_constraint with expr: %a %b\n" d_exp e (Lazy.force no_ov);
    match Convert.tcons1_of_cil_exp ask d d.env e negate no_ov with
    | tcons1 -> meet_tcons ask d tcons1 e no_ov
    | exception Convert.Unsupported_CilExp _ -> d

  let assert_constraint ask d e negate no_ov = timing_wrap "assert_constraint" (assert_constraint ask d e negate) no_ov

  let relift t = t

  (** representation as C expression

     This function returns all the equalities that are saved in our datastructure t.

     Lincons -> linear constraint *)
  let invariant t =
    let of_coeff xi coeffs o =
      let typ = (Option.get @@ V.to_cil_varinfo xi).vtype in
      let ikind = Cilfacade.get_ikind typ in
      let cst = Coeff.s_of_mpqf @@ Mpqf.of_mpz (Z_mlgmpidl.mpz_of_z @@ IntDomain.Size.cast ikind o) in
      let lincons = Lincons1.make (Linexpr1.make t.env) Lincons1.EQ in
      Lincons1.set_list lincons coeffs (Some cst);
      lincons
    in
    let get_const acc i = function
      | (None, o) -> 
        let xi = Environment.var_of_dim t.env i in
        of_coeff xi [(Coeff.s_of_int (-1), xi)] o :: acc
      | (Some r, _) when r = i -> acc
      | (Some r, o) -> 
        let xi = Environment.var_of_dim t.env i in
        let ri = Environment.var_of_dim t.env r in
        of_coeff xi [(Coeff.s_of_int (-1), xi); (Coeff.s_of_int 1, ri)] o :: acc
    in
    BatOption.map_default (Array.fold_lefti get_const []) [] t.d

  let cil_exp_of_lincons1 = Convert.cil_exp_of_lincons1

  let env t = t.env

  type marshal = t
  (* marshal is not compatible with apron, therefore we don't have to implement it *)
  let marshal t = t

  let unmarshal t = t

end

module D2: RelationDomain.RD with type var = Var.t =
struct
  module D = D
  module ConvArg = struct
    let allow_global = false
    let do_overflow_check = false
  end
  include SharedFunctions.AssertionModule (D.V) (D) (ConvArg)
  include D
end
