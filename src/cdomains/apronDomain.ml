open Prelude
open Cil
open Pretty
open Analyses

open Apron

module Man =
struct
  (* type mt = Oct.t *)
  type mt = Polka.strict Polka.t
  type t = mt Manager.t

  (* let mgr = Oct.manager_alloc () *)
  let mgr = Polka.manager_alloc_strict ()
  let eenv = Environment.make [||] [||]
end

module A = Abstract1

module D =
struct
  type t = Man.mt A.t

  let name () = "APRON numerical abstract domain"

  let topE = A.top    Man.mgr
  let botE = A.bottom Man.mgr

  let top () = topE Man.eenv
  let bot () = botE Man.eenv
  let is_top = A.is_top    Man.mgr
  let is_bot = A.is_bottom Man.mgr

  let join x y =
    if is_bot x then
      y
    else if is_bot y then
      x
    else
      A.join (Man.mgr) x y

  let meet x y =
    if is_top x then y else
    if is_top y then x else
    if is_bot x || is_bot y then bot () else
      A.meet Man.mgr x y

  let widen x y =
    if is_bot x then
      y
    else if is_bot y then
      x
    else
      A.widening (Man.mgr) x y

  let narrow = meet

  let equal x y =
    if is_bot x then is_bot y
    else if is_bot y then false
    else if is_top x then is_top y
    else if is_top y then false
    else A.is_eq Man.mgr x y

  let leq x y =
    if is_bot x || is_top y then true else
    if is_bot y || is_top x then false else
      A.is_leq (Man.mgr) x y

  let hash (x:t) = Hashtbl.hash x
  let compare (x:t) y = Stdlib.compare x y
  let isSimple x = true
  let short n x =
    A.print Legacy.Format.str_formatter x;
    Legacy.Format.flush_str_formatter ()
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (short 80 x))
  let toXML_f s (x:t) = Xml.Element ("Leaf",["text", "APRON:"^Goblintutil.escape (s 90 x)],[])
  let toXML = toXML_f short
  let pretty_f s () (x:t) = text (s 10 x)
  let pretty = pretty_f short
  let pretty_diff () (x,y) = text "pretty_diff"

  open Texpr1
  open Lincons0
  open Lincons1

  let typesort =
    let f (is,fs) v =
      if isIntegralType v.vtype then
        (v.vname::is,fs)
      else if isArithmeticType v.vtype then
        (is,v.vname::fs)
      else
        (is,fs)
    in
    List.fold_left f ([],[])

  let rec cil_exp_to_cil_lhost =
    function
    | Lval (Var v,NoOffset) when isArithmeticType v.vtype && (not v.vglob) ->
      Var (Var.of_string v.vname)
    | Const (CInt64 (i,_,_)) ->
      Cst (Coeff.s_of_int (Int64.to_int i))
    | Const (CReal (f,_,_)) ->
      Cst (Coeff.s_of_float f)
    | UnOp  (Neg ,e,_) ->
      Unop (Neg,cil_exp_to_cil_lhost e,Int,Near)
    | BinOp (PlusA,e1,e2,_) ->
      Binop (Add,cil_exp_to_cil_lhost e1,cil_exp_to_cil_lhost e2,Int,Near)
    | BinOp (MinusA,e1,e2,_) ->
      Binop (Sub,cil_exp_to_cil_lhost e1,cil_exp_to_cil_lhost e2,Int,Near)
    | BinOp (Mult,e1,e2,_) ->
      Binop (Mul,cil_exp_to_cil_lhost e1,cil_exp_to_cil_lhost e2,Int,Near)
    | BinOp (Div,e1,e2,_) ->
      Binop (Div,cil_exp_to_cil_lhost e1,cil_exp_to_cil_lhost e2,Int,Zero)
    | BinOp (Mod,e1,e2,_) ->
      Binop (Mod,cil_exp_to_cil_lhost e1,cil_exp_to_cil_lhost e2,Int,Near)
    | CastE (TFloat (FFloat,_),e) -> Unop(Cast,cil_exp_to_cil_lhost e,Texpr0.Single,Zero)
    | CastE (TFloat (FDouble,_),e) -> Unop(Cast,cil_exp_to_cil_lhost e,Texpr0.Double,Zero)
    | CastE (TFloat (FLongDouble,_),e) -> Unop(Cast,cil_exp_to_cil_lhost e,Texpr0.Extended,Zero)
    | CastE (TInt _,e) -> Unop(Cast,cil_exp_to_cil_lhost e,Int,Zero)
    | _ -> raise (Invalid_argument "cil_exp_to_apron_texpr1")


  let add_t x y =
    match x, y with
    | `int x, `int y -> `int (x+y)
    | `float x, `float y -> `float (x+.y)
    | `int x, `float y | `float y, `int x -> `float (float_of_int x+.y)

  let add_t' x y =
    match x, y with
    | `none, x | x, `none -> x
    | `int x, `int y -> `int (x+y)
    | `float x, `float y -> `float (x+.y)
    | `int x, `float y | `float y, `int x -> `float (float_of_int x+.y)

  let neg_t = function `int x -> `int (-x) | `float x -> `float (0.0-.x)
  let neg_t' = function `int x -> `int (-x) | `float x -> `float (0.0-.x) | `none -> `none

  let negate (xs,x,r) =
    let xs' = List.map (fun (x,y) -> (x,neg_t y)) xs in
    xs', neg_t' x, r

  type lexpr = (string * [`int of int | `float of float]) list

  let rec cil_exp_to_lexp =
    let add ((xs:lexpr),x,r) ((ys:lexpr),y,r') =
      let add_one xs (var_name, var_coefficient) =
        let found_var_in_list var_name var_coeff_list =
          let find found_already (var_name_in_list, _)  =
            found_already || (String.compare var_name var_name_in_list) == 0 in
          List.fold_left find false var_coeff_list in
        if (found_var_in_list var_name xs) then
          List.modify var_name (fun x -> add_t x var_coefficient) xs
        else (var_name, var_coefficient)::xs in
      match r, r' with
      | EQ, EQ -> List.fold_left add_one xs ys, add_t' x y, EQ
      | _ -> raise (Invalid_argument "cil_exp_to_lexp")
    in
    function
    | Lval (Var v,NoOffset) when isArithmeticType v.vtype && (not v.vglob) ->
      [v.vname,`int 1], `none, EQ
    | Const (CInt64 (i,_,_)) ->
      [], `int (Int64.to_int i), EQ
    | Const (CReal (f,_,_)) ->
      [], `float f, EQ
    | UnOp  (Neg ,e,_) ->
      negate (cil_exp_to_lexp e)
    | BinOp (PlusA,e1,e2,_) ->
      add (cil_exp_to_lexp e1) (cil_exp_to_lexp e2)
    | BinOp (MinusA,e1,e2,_) ->
      add (cil_exp_to_lexp e1) (negate (cil_exp_to_lexp e2))
    | BinOp (Mult,e1,e2,_) ->
      begin match cil_exp_to_lexp e1, cil_exp_to_lexp e2 with
        | ([], `int x, EQ), ([], `int y, EQ) -> ([], `int (x*y), EQ)
        | ([], `float x, EQ), ([], `float y, EQ) -> ([], `float (x*.y), EQ)
        | (xs, `none, EQ), ([], `int y, EQ) | ([], `int y, EQ), (xs, `none, EQ) ->
          (List.map (function (n,`int x) -> n, `int (x*y) | (n,`float x) -> n, `float (x*.float_of_int y)) xs, `none, EQ)
        | (xs, `none, EQ), ([], `float y, EQ) | ([], `float y, EQ), (xs, `none, EQ) ->
          (List.map (function (n,`float x) -> n, `float (x*.y) | (n,`int x) -> (n,`float (float_of_int x*.y))) xs, `none, EQ)
        | _ -> raise (Invalid_argument "cil_exp_to_lexp")
      end
    | BinOp (r,e1,e2,_) ->
      let comb r = function
        | (xs,y,EQ) -> (xs,y,r)
        | _ -> raise (Invalid_argument "cil_exp_to_lexp")
      in
      begin match r with
        | Lt -> comb SUP   (add (cil_exp_to_lexp e2) (negate (cil_exp_to_lexp e1)))
        | Gt -> comb SUP   (add (cil_exp_to_lexp e1) (negate (cil_exp_to_lexp e2)))
        | Le -> comb SUPEQ (add (cil_exp_to_lexp e2) (negate (cil_exp_to_lexp e1)))
        | Ge -> comb SUPEQ (add (cil_exp_to_lexp e1) (negate (cil_exp_to_lexp e2)))
        | Eq -> comb EQ    (add (cil_exp_to_lexp e1) (negate (cil_exp_to_lexp e2)))
        | Ne -> comb DISEQ (add (cil_exp_to_lexp e1) (negate (cil_exp_to_lexp e2)))
        | _ -> raise (Invalid_argument "cil_exp_to_lexp")
      end
    | CastE (_,e) -> cil_exp_to_lexp e
    | _ ->
      raise (Invalid_argument "cil_exp_to_lexp")

  let cil_exp_to_apron_linexpr1 environment cil_exp should_negate =
    let inverse_comparator comparator =
      match comparator with
      | EQ -> DISEQ
      | DISEQ -> EQ
      | SUPEQ -> SUP
      | SUP -> SUPEQ
      | EQMOD x -> EQMOD x in
    let var_name_coeff_pairs, constant, comparator = cil_exp_to_lexp (Cil.constFold false cil_exp) in
    let var_name_coeff_pairs, constant, comparator = if should_negate then negate (var_name_coeff_pairs, constant, (inverse_comparator comparator)) else var_name_coeff_pairs, constant, comparator in
    let apron_var_coeff_pairs = List.map (function (x,`int y) -> Coeff.s_of_int y, Var.of_string x | (x,`float f) -> Coeff.s_of_float f, Var.of_string x) var_name_coeff_pairs in
    let apron_constant = match constant with `int x -> Some (Coeff.s_of_int x) | `float f -> Some (Coeff.s_of_float f) | `none -> None in
    let all_variables_known_to_environment = List.fold_left (fun known (_,var) -> known && (Environment.mem_var environment var)) true apron_var_coeff_pairs in
    if not(all_variables_known_to_environment) then None, None
    else
      begin
        let linexpr1 = Linexpr1.make environment in
        Linexpr1.set_list linexpr1 apron_var_coeff_pairs apron_constant;
        Some linexpr1, Some comparator
      end

  let cil_exp_to_apron_linecons environment cil_exp should_negate =
    (* ignore (Pretty.printf "exptolinecons '%a'\n" d_plainexp x); *)
    let linexpr1, comparator = cil_exp_to_apron_linexpr1 environment cil_exp should_negate in
    match linexpr1, comparator with
    | Some linexpr1, Some comparator -> Some (Lincons1.make linexpr1 comparator)
    | _ -> None

  let assert_inv d x b =
    try
      (* if assert(x) then convert it to assert(x != 0) *)
      let x = match x with
        | Lval (Var v,NoOffset) when isArithmeticType v.vtype ->
          UnOp(LNot, (BinOp (Eq, x, (Const (CInt64(Int64.of_int 0, IInt, None))), intType)), intType)
        | _ -> x in
      let linecons = cil_exp_to_apron_linecons (A.env d) x b in
      match linecons with
      | Some linecons ->
        let ea = { lincons0_array = [|Lincons1.get_lincons0 linecons |]
                 ; array_env = A.env d
                 }
        in
        A.meet_lincons_array Man.mgr d ea
      | None -> d
    with Invalid_argument "cil_exp_to_lexp" -> d

  let cil_exp_to_apron_texpr1 env exp =
    (* ignore (Pretty.printf "exptotexpr1 '%a'\n" d_plainexp x); *)
    Texpr1.of_expr env (cil_exp_to_cil_lhost exp)

  let assign_var_eq_with d v v' =
    A.assign_texpr_with Man.mgr d (Var.of_string v)
      (Texpr1.of_expr (A.env d) (Var (Var.of_string v'))) None

  let substitute_var_eq_with d v v' =
    A.substitute_texpr_with Man.mgr d (Var.of_string v)
      (Texpr1.of_expr (A.env d) (Var (Var.of_string v'))) None


  let assign_var_with d v e =
    (* ignore (Pretty.printf "assign_var_with %a %s %a\n" pretty d v d_plainexp e); *)
    begin try
        A.assign_texpr_with Man.mgr d (Var.of_string v)
          (cil_exp_to_apron_texpr1 (A.env d) (Cil.constFold false e)) None
      with Invalid_argument "cil_exp_to_apron_texpr1" ->
        A.forget_array_with Man.mgr d [|Var.of_string v|] false
        (* | Manager.Error q -> *)
        (* ignore (Pretty.printf "Manager.Error: %s\n" q.msg); *)
        (* ignore (Pretty.printf "Manager.Error: assign_var_with _ %s %a\n" v d_plainexp e); *)
        (* raise (Manager.Error q) *)
    end

  let assign_var d v e =
    let newd = A.copy Man.mgr d in
    assign_var_with newd v e;
    newd

  let forget_all_with d xs =
    A.forget_array_with Man.mgr d (Array.of_enum (List.enum (List.map Var.of_string xs))) false

  let forget_all d xs =
    let newd = A.copy Man.mgr d in
    forget_all_with newd xs;
    newd

  let substitute_var_with d v e =
    (* ignore (Pretty.printf "substitute_var_with %a %s %a\n" pretty d v d_plainexp e); *)
    begin try
        A.substitute_texpr_with Man.mgr d (Var.of_string v)
          (cil_exp_to_apron_texpr1 (A.env d) (Cil.constFold false e)) None
      with Invalid_argument "cil_exp_to_apron_texpr1" ->
        A.forget_array_with Man.mgr d [|Var.of_string v|] false
        (* | Manager.Error q ->
           ignore (Pretty.printf "Manager.Error: %s\n" q.msg);
           ignore (Pretty.printf "Manager.Error: assign_var_with _ %s %a\n" v d_plainexp e);
           raise (Manager.Error q) *)
    end

  let get_vars d =
    let xs, ys = Environment.vars (A.env d) in
    List.of_enum (Array.enum xs), List.of_enum (Array.enum ys)

  let add_vars_with newd (newis, newfs) =
    let rec remove_duplicates list =
      match list with
      | [] -> []
      | head::tail -> head::(remove_duplicates (List.filter (fun x -> x <> head) tail)) in
    let oldis, oldfs = get_vars newd in
    let oldvs = oldis@oldfs in
    let environment = (A.env newd) in
    let newis = remove_duplicates newis in
    let newfs = remove_duplicates newfs in
    let cis = List.filter (fun x -> not (List.mem x oldvs) && (not (Environment.mem_var environment x))) (List.map Var.of_string newis) in
    let cfs = List.filter (fun x -> not (List.mem x oldvs) && (not (Environment.mem_var environment x))) (List.map Var.of_string newfs) in
    let cis, cfs = Array.of_enum (List.enum cis), Array.of_enum (List.enum cfs) in
    let newenv = Environment.add environment cis cfs in
    A.change_environment_with Man.mgr newd newenv false

  let add_vars d vars =
    let newd = A.copy Man.mgr d in
    add_vars_with newd vars;
    newd

  let remove_all_but_with d xs =
    let is', fs' = get_vars d in
    let vs = List.append (List.filter (fun x -> not (List.mem (Var.to_string x) xs)) is')
        (List.filter (fun x -> not (List.mem (Var.to_string x) xs)) fs') in
    let env = Environment.remove (A.env d) (Array.of_enum (List.enum vs)) in
    A.change_environment_with Man.mgr d env false

  let remove_all_with d xs =
    (* let vars = List.filter (fun v -> isArithmeticType v.vtype) xs in *)
    let vars = Array.of_enum (List.enum (List.map (fun v -> Var.of_string v) xs)) in
    let env = Environment.remove (A.env d) vars in
    A.change_environment_with Man.mgr d env false

  let remove_all d vars =
    let newd = A.copy Man.mgr d in
    forget_all_with newd vars;
    newd

  let copy = A.copy Man.mgr

  let get_int_interval_for_cil_exp d cil_exp =
    let get_int_for_apron_scalar (scalar: Scalar.t) =
      match scalar with
      | Float scalar -> Some (Stdlib.int_of_float scalar)
      | Mpqf scalar ->
        begin
          match Mpqf.to_string scalar with
          (* apron has an internal representation of -1/0 as -infinity and 1/0 as infinity.*)
          | "-1/0" | "1/0" -> None
          | _ -> Some (Stdlib.int_of_float (Mpqf.to_float scalar))
        end
      | Mpfrf scalar -> Some (Stdlib.int_of_float (Mpfrf.to_float scalar)) in
    try
      let linexpr1, _  = cil_exp_to_apron_linexpr1 (A.env d) cil_exp false in
      match linexpr1 with
      | Some linexpr1 -> (
          let interval_of_variable = A.bound_linexpr Man.mgr d linexpr1 in
          let infimum = get_int_for_apron_scalar interval_of_variable.inf in
          let supremum = get_int_for_apron_scalar interval_of_variable.sup in
          match infimum, supremum with
          | Some infimum, Some supremum -> Some (Int64.of_int (infimum)),  Some (Int64.of_int (supremum))
          | Some infimum, None -> Some (Int64.of_int (-infimum)), None
          | None, Some supremum ->  None, Some (Int64.of_int (-supremum))
          | _, _ -> None, None)
      | _ -> None, None
    with Invalid_argument "cil_exp_to_lexp" -> None, None

  let get_int_val_for_cil_exp d cil_exp =
    match get_int_interval_for_cil_exp d cil_exp with
    | Some infimum, Some supremum ->
      begin
        if (supremum = infimum) then
          (Some infimum)
        else None
      end
    | _ -> None

  let cil_exp_equals d exp1 exp2 =
    if (is_bot d) then false
    else
      begin
        let compare_expression = BinOp (Eq, exp1, exp2, TInt (IInt, [])) in
        equal d (assert_inv d compare_expression false)
      end

end
