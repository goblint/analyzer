open Prelude
open Cil
open Pretty

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
module ID = IntDomain.IntDomTuple

module type ApronRelationalIntDomainSignature =
sig
  include RelationalIntDomainSignature.RelationalIntDomainSignature
end

module type PolyDomainSignature =
sig
  include Lattice.S
  val add_vars: t -> string list * string list -> t
  val assert_inv: t -> Cil.exp -> bool -> t
  val assign_var: t -> string -> Cil.exp -> t
  val assign_var_eq_with: t -> string -> string -> unit
  val assign_var_with: t -> string -> Cil.exp -> unit
  val cil_exp_equals: t -> Cil.exp -> Cil.exp -> bool
  val forget_all: t -> string list -> t
  val forget_all_with: t -> string list -> unit
  val get_int_interval_for_cil_exp: t -> Cil.exp -> int64 option * int64 option
  val get_int_val_for_cil_exp: t -> Cil.exp -> int64 option
  val get_vars: t -> Apron.Var.t list * Apron.Var.t list
  val remove_all_but_with: t -> string list -> unit
  val remove_all_with: t -> string list -> unit
  val substitute_var_eq_with: t -> string -> string -> unit
  val substitute_var_with: t -> string -> Cil.exp -> unit
  val topE: Apron.Environment.t -> t
  val typesort: Cil.varinfo list -> string list * string list
end

module type ApronRelationalStructDomainSignature =
sig
  include StructDomain.Relational
end


(* contains functions, used by all apron domains *)
module ApronDomain =
struct
  type apronType = Man.mt A.t
  let topE = A.top    Man.mgr
  let botE = A.bottom Man.mgr
  let top () = topE Man.eenv
  let bot () = botE Man.eenv
  let is_top = A.is_top Man.mgr
  let is_bot = A.is_bottom Man.mgr

  let adjust_environments x y =
    let environment_x, environment_y = (A.env x), (A.env y) in
    let (vars_x_int, vars_x_real), (vars_y_int, vars_y_real) =
      Environment.vars environment_x, Environment.vars environment_y
    in
    let missing_variables all_variables environment =
      (Array.filter (fun variable -> not(Environment.mem_var environment variable)) all_variables)
    in
    let environment_x, environment_y =
      (Environment.add environment_x
         (missing_variables vars_y_int environment_x)
         (missing_variables vars_y_real environment_x)),
      (Environment.add environment_y
         (missing_variables vars_x_int environment_y)
         (missing_variables vars_x_real environment_y))
    in
    A.change_environment Man.mgr x environment_x false,
    A.change_environment Man.mgr y environment_y false

  let join x y =
    if is_bot x then
      y
    else if is_bot y then
      x
    else
      let x, y = adjust_environments x y in
      A.join (Man.mgr) x y

  let meet x y =
    if is_top x then y else
    if is_top y then x else
    if is_bot x || is_bot y then bot () else
      let x, y = adjust_environments x y in
      A.meet Man.mgr x y

  let widen x y =
    if is_bot x then
      y
    else if is_bot y then
      x
    else
      let x, y = adjust_environments x y in
      A.widening (Man.mgr) x y

  let narrow = meet

  let equal x y =
    if is_bot x then is_bot y
    else if is_bot y then false
    else if is_top x then is_top y
    else if is_top y then false
    else
      let x, y = adjust_environments x y in
      A.is_eq Man.mgr x y

  let leq x y =
    if is_bot x || is_top y then true else
    if is_bot y || is_top x then false else
      let x, y = adjust_environments x y in
      A.is_leq (Man.mgr) x y

  let hash (x:apronType) = Hashtbl.hash x
  let compare (x:apronType) y = let x, y = adjust_environments x y in Pervasives.compare x y
  let isSimple x = true
  let short n x =
    A.print Legacy.Format.str_formatter x;
    Legacy.Format.flush_str_formatter ()
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (short 80 x))
  let toXML_f s (x:apronType) = Xml.Element ("Leaf",["text", "APRON:"^Goblintutil.escape (s 90 x)],[])
  let toXML = toXML_f short
  let pretty_f s () (x:apronType) = text (s 10 x)
  let pretty = pretty_f short
  let pretty_diff () (x,y) = text "prettydiff"

  open Texpr1
  open Lincons0
  open Lincons1
  type lexpr = (string * [`int of int | `float of float]) list

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
    | Lval (Var v,Field(field,_)) when field.fcomp.cstruct ->
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

  let get_int_for_apron_scalar (scalar: Scalar.t) =
    let open Scalar in
    match scalar with
    | Float scalar -> Some (Pervasives.int_of_float scalar)
    | Mpqf scalar ->
      begin
        match Mpqf.to_string scalar with
        (* apron has an internal representation of -1/0 as -infinity and 1/0 as infinity.*)
        | "-1/0" | "1/0" -> None
        | _ -> Some (Pervasives.int_of_float (Mpqf.to_float scalar))
      end
    | Mpfrf scalar -> Some (Pervasives.int_of_float (Mpfrf.to_float scalar))

  let get_int_interval_for_cil_exp d cil_exp =
    try
      let linexpr1, _  = cil_exp_to_apron_linexpr1 (A.env d) cil_exp false in
      match linexpr1 with
      | Some linexpr1 -> (
          let open Interval in
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

  let cil_exp_to_apron_texpr1 env exp =
    (* ignore (Pretty.printf "exptotexpr1 '%a'\n" d_plainexp x); *)
    Texpr1.of_expr env (cil_exp_to_cil_lhost exp)
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

  let get_vars d =
    let xs, ys = Environment.vars (A.env d) in
    List.of_enum (Array.enum xs), List.of_enum (Array.enum ys)

  let remove_all_but_with d xs =
    let is', fs' = get_vars d in
    let vs = List.append (List.filter (fun x -> not (List.mem (Var.to_string x) xs)) is')
        (List.filter (fun x -> not (List.mem (Var.to_string x) xs)) fs') in
    let env = Environment.remove (A.env d) (Array.of_enum (List.enum vs)) in
    A.change_environment_with Man.mgr d env false

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

  let forget_all_with d xs =
    A.forget_array_with Man.mgr d (Array.of_enum (List.enum (List.map Var.of_string xs))) false

  let remove_all_with d xs =
    let environment = (A.env d) in
    let vars = List.filter (fun v -> Environment.mem_var environment (Var.of_string v)) xs in
    let vars = Array.of_enum (List.enum (List.map (fun v -> Var.of_string v) vars)) in
    Pervasives.print_endline "remove_all_with";
    let env = Environment.remove environment vars in
    A.change_environment_with Man.mgr d env false

end


module PolyDomain : PolyDomainSignature with type t = ApronDomain.apronType =
struct
  open ApronDomain
  type t = apronType
  let name () = "aprondomain"

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

  let meet = meet
  let join = join
  let widen = widen
  let narrow = narrow
  let is_top = is_top
  let is_bot = is_bot
  let top = top
  let bot = bot
  let leq = leq
  let printXml = printXml
  let toXML_f = toXML_f
  let pretty_f = pretty_f
  let toXML = toXML
  let pretty_diff = pretty_diff
  let pretty = pretty
  let isSimple = isSimple
  let short = short
  let compare = compare
  let hash = hash
  let equal = equal

  let add_vars = add_vars
  let assert_inv = assert_inv
  let assign_var_with = assign_var_with
  let assign_var = assign_var
  let forget_all_with = forget_all_with
  let get_int_interval_for_cil_exp = get_int_interval_for_cil_exp
  let get_vars = get_vars
  let remove_all_but_with = remove_all_but_with
  let remove_all_with = remove_all_with
  let topE = topE

  let substitute_var_eq_with d v v' =
    let open Texpr1 in
    A.substitute_texpr_with Man.mgr d (Var.of_string v)
      (Texpr1.of_expr (A.env d) (Var (Var.of_string v'))) None

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

  let get_int_val_for_cil_exp d cil_exp =
    match get_int_interval_for_cil_exp d cil_exp with
    | Some infimum, Some supremum ->
      begin
        if (supremum = infimum) then
          (Some infimum)
        else None
      end
    | _ -> None

  let forget_all d xs =
    let newd = A.copy Man.mgr d in
    forget_all_with newd xs;
    newd

  let cil_exp_equals d exp1 exp2 =
    if (is_bot d) then false
    else
      begin
        let compare_expression = BinOp (Eq, exp1, exp2, TInt (IInt, [])) in
        equal d (assert_inv d compare_expression false)
      end

  let assign_var_eq_with d v v' =
    let open Texpr1 in
    A.assign_texpr_with Man.mgr d (Var.of_string v)
      (Texpr1.of_expr (A.env d) (Var (Var.of_string v'))) None

end

(* contains functions, used by both aprpn relational  domains (structs and ints) *)
module ApronRelationalDomain =
struct
  open ApronDomain
  let local_identifier_char = ' '
  let local_identifier = Char.escaped local_identifier_char

  let remove_all_local_or_global_variables abstract_value should_remove_local =
    let environment = (A.env abstract_value) in
    let (vars_int, vars_real) = Environment.vars environment in
    let local_vars all_variables =
      (Array.filter (fun variable ->
           let variable_name = (Var.to_string variable) in
           let length = String.length local_identifier in
           let is_local_variable = local_identifier = (String.sub variable_name ((String.length variable_name) - length) length)
           in ((should_remove_local && is_local_variable) || ((not should_remove_local) && (not is_local_variable)))
         ) all_variables) in
    let environment = Environment.remove environment (local_vars vars_int) in
    let environment = Environment.remove environment (local_vars vars_real) in
    A.change_environment Man.mgr abstract_value environment false

  let remove_all_local_variables abstract_value =
    remove_all_local_or_global_variables abstract_value true

  let add_variable_with_name variable_name abstract_value =
    let var = Var.of_string variable_name in
    let environment = (A.env abstract_value) in
    let environment =
      if not(Environment.mem_var environment var) then
        let int_vars = (Array.of_enum (List.enum [var])) in
        let real_vars = (Array.of_enum (List.enum [])) in
        Environment.add environment int_vars real_vars
      else environment
    in
    A.change_environment Man.mgr abstract_value environment false

  let remove_variable_with_name variable_name abstract_value =
    let var = Var.of_string variable_name in
    let environment = (A.env abstract_value) in
    if not(Environment.mem_var environment var) then abstract_value
    else (
      let vars = (Array.of_enum (List.enum [var])) in
      let environment = Environment.remove environment vars in
      A.change_environment Man.mgr abstract_value environment false
    )
  type linexpr_type = Linexpr1.t
  let assign_int_value_to_variable_name abstract_value int_val variable_name =
    let get_linecons_for_minimum environment variable minimum =
      let apron_variable = Var.of_string variable_name in
      let expr = Linexpr1.make environment in
      Linexpr1.set_array expr
        [|
          (Coeff.Scalar (Scalar.Float 1.0), apron_variable)
        |]
        (Some
           (Coeff.Scalar (Scalar.Float (Pervasives.float_of_int (-(Int64.to_int minimum)))))
        )
      ;
      Lincons1.make expr Lincons1.SUPEQ
    in
    let get_linecons_for_maximum environment variable_name maximum =
      let apron_variable = Var.of_string variable_name in
      let expr = Linexpr1.make environment in
      Linexpr1.set_array expr
        [|
          (Coeff.Scalar (Scalar.Float (-1.0)), apron_variable)
        |]
        (Some
           (Coeff.Scalar (Scalar.Float (Pervasives.float_of_int (Int64.to_int maximum))))
        )
      ;
      Lincons1.make expr Lincons1.SUPEQ
    in
    if ID.is_top int_val then add_variable_with_name variable_name abstract_value
    else (
      let minimum, maximum = ID.minimal int_val, ID.maximal int_val in
      let minimum = match minimum with
        | Some minimum -> if (ID.equal (ID.starting minimum) int_val) || ((Int64.compare minimum (Int64.neg 2147483648L)) <= 0) then None else Some minimum
        | _ -> None
      in
      let maximum = match maximum with

        | Some maximum ->
          if (ID.equal (ID.ending maximum) int_val) || ((Int64.compare maximum 2147483647L) >= 0) then
            None
          else
            Some maximum
        | _ -> Pervasives.print_endline "no max"; None
      in
      let abstract_value = add_variable_with_name variable_name abstract_value in
      match minimum, maximum with
      | Some minimum, Some maximum ->
        if minimum = maximum then
          try
            assign_var abstract_value variable_name (Const(CInt64(minimum,IInt,None)))
          with Manager.Error x ->
            Manager.print_exclog Format.std_formatter x;
            abstract_value
        else
          let environment = (A.env abstract_value) in
          let tab = Lincons1.array_make environment 2 in
          Lincons1.array_set tab 0 (get_linecons_for_minimum environment variable_name minimum);
          Lincons1.array_set tab 1 (get_linecons_for_maximum environment variable_name maximum);
          let abstract_val_new_constraints = Abstract1.of_lincons_array Man.mgr environment tab in
          meet abstract_val_new_constraints (remove_variable_with_name variable_name abstract_value)
      | Some minimum, _ ->
        let environment = (A.env abstract_value) in
        let tab = Lincons1.array_make environment 1 in
        Lincons1.array_set tab 0 (get_linecons_for_minimum environment variable_name minimum);
        let abstract_val_new_constraints = Abstract1.of_lincons_array Man.mgr environment tab in
        meet abstract_val_new_constraints (remove_variable_with_name variable_name abstract_value)
      | _, Some maximum ->
        let environment = (A.env abstract_value) in
        let tab = Lincons1.array_make environment 1 in
        Lincons1.array_set tab 0 (get_linecons_for_maximum environment variable_name maximum);
        let abstract_val_new_constraints = Abstract1.of_lincons_array Man.mgr environment tab in
        meet abstract_val_new_constraints (remove_variable_with_name variable_name abstract_value)
      | _ -> abstract_value
    )

  let meet_local_and_global_state local_state global_state =
    let local_state = remove_all_local_or_global_variables local_state false in
    let global_state = remove_all_local_or_global_variables global_state true in
    meet local_state global_state

  let add_variable_value_list get_variable_name variable_value_list abstract_value =
    try
      let variable_names = List.fold_right (
          fun (variable, _) (int_variables, real_variables) ->
            match variable.vtype with
            | TInt _ -> ([(get_variable_name variable)] @ int_variables, real_variables)
            | TFloat _ -> (int_variables,[(get_variable_name variable)] @real_variables)
            | _ -> (int_variables, real_variables)
        ) variable_value_list ([],[]) in
      let abstract_value = add_vars abstract_value variable_names in
      List.fold_right (
        fun (variable, value) abstract_val ->
          assign_int_value_to_variable_name abstract_val value (get_variable_name variable)
      ) variable_value_list abstract_value
    with Manager.Error x ->
      Manager.print_exclog Format.std_formatter x;
      bot ()

end

module ApronRelationalIntDomain: ApronRelationalIntDomainSignature =
struct
  open ApronDomain
  open ApronRelationalDomain
  type t = apronType

  let name () = "aprondomain"

  let meet = meet
  let join = join
  let widen = widen
  let narrow = narrow
  let is_top = is_top
  let is_bot = is_bot
  let top = top
  let bot = bot
  let leq = leq
  let printXml = printXml
  let toXML_f = toXML_f
  let pretty_f = pretty_f
  let toXML = toXML
  let pretty_diff = pretty_diff
  let pretty = pretty
  let isSimple = isSimple
  let short = short
  let compare = compare
  let hash = hash
  let equal = equal

  let get_variable_name variable =
    if variable.vglob then variable.vname
    else variable.vname ^ local_identifier

  let add_variable_value_list = add_variable_value_list get_variable_name
  let meet_local_and_global_state = meet_local_and_global_state
  let remove_all_local_variables = remove_all_local_variables

  let original_variable_name variable =
    if variable.vglob then variable.vname
    else String.sub variable.vname 0 ((String.length variable.vname) - 1)

  let remove_variable varinfo = remove_variable_with_name (get_variable_name varinfo)

  let remove_all_top_variables abstract_value =
    abstract_value

  let get_value_of_variable var abstract_value =
    let old_var_name = var.vname in
    var.vname <- (get_variable_name var);
    let interval = get_int_interval_for_cil_exp abstract_value (Lval((Var var), NoOffset)) in
    var.vname <- old_var_name;
    let result = match interval with
      | Some infimum, Some supremum ->
        if Int64.compare infimum supremum > 0 then (ID.bot ())
        else (ID.of_interval (infimum, supremum))
      | Some infimum, _ -> (ID.starting infimum)
      | _, Some supremum -> (ID.ending supremum)
      | _ -> (ID.top ())
    in
    result

  let eval_assign_int_value variable int_val abstract_value =
    assign_int_value_to_variable_name (add_variable_with_name (get_variable_name variable) abstract_value) int_val (get_variable_name variable)

  let eval_assign_cil_exp variable rval abstract_value =
    assign_var (add_variable_with_name (get_variable_name variable) abstract_value) (get_variable_name variable) rval

  let rec rename_cil_variables cil_exp add_local_identifier =
    match cil_exp with
    | BinOp(op, exp1, exp2, typ) -> BinOp(op, (rename_cil_variables exp1 add_local_identifier), (rename_cil_variables exp2 add_local_identifier), typ)
    | Lval (Var v, offs) -> (if add_local_identifier then v.vname <- (get_variable_name v) else v.vname <- (original_variable_name v)); Lval (Var v, offs)
    | UnOp (op, exp, typ) -> UnOp (op, (rename_cil_variables exp add_local_identifier), typ)
    | _ -> cil_exp

  let eval_assert_cil_exp cil_exp abstract_value =
    let cil_exp = rename_cil_variables cil_exp true in
    let result = assert_inv abstract_value cil_exp false in
    let _ = rename_cil_variables cil_exp false in
    result

end

module type Compound =
sig
  include Lattice.S
  val of_int_val: IntDomain.IntDomTuple.t -> t
  val to_int_val: t -> IntDomain.IntDomTuple.t
end

module ApronRelationalStructDomain
    (Compound: Compound)(EquationField: Equation.GroupableLatticeS with type t = ([`Top | `Bot | `Field of Basetype.VariableFields.t]))
    : ApronRelationalStructDomainSignature
      with type t = ApronDomain.apronType * MapDomain.MapTop_LiftBot(Lattice.Prod(Basetype.Strings)(Basetype.Strings))(EquationField).t
       and type field = EquationField.t
       and type value = Compound.t
=
struct
  open ApronDomain
  open ApronRelationalDomain

  module StructMapKey = Lattice.Prod(Basetype.Strings)(Basetype.Strings)
  module StructMap = MapDomain.MapTop_LiftBot(Lattice.Prod(Basetype.Strings)(Basetype.Strings) )(EquationField)
  type t = apronType  * StructMap.t
  type field = EquationField.t
  type value = Compound.t

  let name () = "apron"

  let is_top (x, _) = is_top x
  let is_bot (x, _) = is_bot x
  let top () = top (), StructMap.top()
  let bot () = bot(), StructMap.bot()
  let short w (x, m) = (short w x)
  let isSimple _ = true
  let printXml out (x, _) = printXml out x
  let toXML (x, _) = toXML x
  let toXML_f s x = Xml.Element ("Leaf",["text", "APRON:"^Goblintutil.escape (s 90 x)],[])
  let pretty_f s () x = text (s 10 x)
  let pretty_diff () (x, y)= text ((short 100 x) ^ " vs. " ^ (short 100 y))
  let pretty () x = pretty_f short () x

  let compare (x, _) (y, _) = compare x y
  let hash (x, _) = hash x

  let remove_all_top_variables x = x

  let character_between_field_and_comp_name_char = '.'
  let character_between_field_and_comp_name = Char.escaped character_between_field_and_comp_name_char
  let local_identifier_char = ' '
  let local_identifier = Char.escaped local_identifier_char

  let original_variable_name unique_field_name =
    let start_substring = (String.index unique_field_name character_between_field_and_comp_name_char) in
    String.sub unique_field_name 0 start_substring

  (* a generated field name looks like "comname.fieldname" example: field xx.i could be named "xx.i " the . in the middle symbolizes the gap between fieldname and compname and the space in the end symbolizes that the variable is local. The space and . work, as spaces and .s are not allowed in identifiers in C, but apron accepts it
  *)
  let get_unique_field_name field =
    match field with
    | `Field(var, field) ->
      let struct_name, is_local = match var with Some var -> var.vname, not(var.vglob) | _ -> "", true in
      let unique_field_name = struct_name ^ character_between_field_and_comp_name ^ field.fname in
      let unique_field_name = if is_local then unique_field_name ^ local_identifier else unique_field_name in
      unique_field_name
    | _ -> raise (Invalid_argument "")

  type local_global_both = [`Local | `Global | `Both ]

  let get_all_local_or_global_variables environment local_global_both =
    let (vars_int, vars_real) = Environment.vars environment in
    let filter_vars all_variables =
      (Array.filter (
          fun variable ->
            let variable_name = (Var.to_string variable) in
            let length = String.length local_identifier in
            let is_local_variable = local_identifier = (String.sub variable_name ((String.length variable_name) - length) length)
            in
            match local_global_both with
            | `Local -> is_local_variable
            | `Global -> not is_local_variable
            | `Both -> true
        )
          all_variables) in
    match local_global_both with
    | `Both ->  (Array.to_list vars_int) @  (Array.to_list vars_real)
    | _ ->
      let vars_int = Array.to_list (filter_vars vars_int) in
      let vars_real = Array.to_list (filter_vars vars_real) in
      vars_int @ vars_real

  let remove_all_local_variables (apron_abstract_value, struct_name_mapping) =
    remove_all_local_variables apron_abstract_value, struct_name_mapping

  let remove_variable varinfo (apron_abstract_value, struct_mapping) =
    let field_names_to_remove =
      StructMap.fold(fun _ field result ->
          result @ [get_unique_field_name field]) struct_mapping [] in
    remove_all_with apron_abstract_value field_names_to_remove;
    apron_abstract_value,
    (*    StructNameMap.remove_variable varinfo.vname*) struct_mapping

  let join (apron_abstract_valuex, struct_mappingx) (apron_abstract_valuey, struct_mappingy) =
    join apron_abstract_valuex apron_abstract_valuey, StructMap.join struct_mappingx struct_mappingy

  let meet (apron_abstract_valuex, struct_mappingx) (apron_abstract_valuey, struct_mappingy) =
    meet apron_abstract_valuex apron_abstract_valuey, StructMap.meet struct_mappingx struct_mappingy

  let narrow (apron_abstract_valuex, struct_mappingx) (apron_abstract_valuey, struct_mappingy) =
    Pervasives.print_endline "BEFORE NARROW";
    Pervasives.print_endline (short 100 (apron_abstract_valuex, struct_mappingx));
    let result =
      narrow apron_abstract_valuex apron_abstract_valuey, StructMap.narrow struct_mappingx struct_mappingy
    in
    Pervasives.print_endline "Narrow Result";
    Pervasives.print_endline (short 100 result);
    result

  let widen (apron_abstract_valuex, struct_mappingx) (apron_abstract_valuey, struct_mappingy) =
    Pervasives.print_endline "BEFORE WIDEN";
    Pervasives.print_endline (short 100 (apron_abstract_valuex, struct_mappingx));
    let result =
      widen apron_abstract_valuex apron_abstract_valuey, StructMap.widen struct_mappingx struct_mappingy in
    Pervasives.print_endline "WIDEN Result";
    Pervasives.print_endline (short 100 result);
    result

  let leq (apron_abstract_valuex, struct_mappingx) (apron_abstract_valuey, struct_mappingy) =
    leq apron_abstract_valuex apron_abstract_valuey && StructMap.leq struct_mappingx struct_mappingy

  let replace (apron_abstract_value, struct_map) field compound_val =
    Pervasives.print_endline "Replace with the following compound value: ";
    Pretty.fprint Pervasives.stdout 0 (Compound.pretty () compound_val);
    Pervasives.print_endline ("replace field: " ^ (EquationField.short 1000 field));
    Pervasives.print_endline "Before replace:";
    Pervasives.print_endline (short 1000 (apron_abstract_value, struct_map) );
    let int_val = Compound.to_int_val compound_val in
    let new_field_name = get_unique_field_name field in
    let var_name, field_name = match field with | `Field(Some var, f) -> `Lifted var.vname, `Lifted f.fname | `Field (_, f) -> `Lifted "", `Lifted f.fname | _ -> raise (Invalid_argument "") in
    let struct_map = StructMap.add (var_name, field_name) field struct_map in
    let apron_abstract_value = assign_int_value_to_variable_name apron_abstract_value int_val new_field_name in
    Pervasives.print_endline "After replace:";
    Pervasives.print_endline (short 1000 (apron_abstract_value, struct_map) );
    apron_abstract_value, struct_map

  let equal (apron_abstract_valuex,_) (apron_abstract_valuey, _) =
    equal apron_abstract_valuex apron_abstract_valuey

  let get_int_val_for_field_name field_name apron_abstract_value =
    Pervasives.print_endline ("\nget_int_val_for_field_name: " ^ field_name ^ ".");
    Pervasives.print_endline (ApronDomain.short 1000 apron_abstract_value);
    let environment = (A.env apron_abstract_value) in
    let var = (Var.of_string field_name) in
    if not (Environment.mem_var environment var) then IntDomain.IntDomTuple.bot()(*raise (Invalid_argument ("get_int_val_for_field_name field_name not found: " ^ field_name))*)
    else
      let var_coeff_pair = Coeff.s_of_int 1, var in
      let linexpr1 = Linexpr1.make environment in
      Linexpr1.set_list linexpr1 [var_coeff_pair] None;
      let interval_of_variable = A.bound_linexpr Man.mgr apron_abstract_value linexpr1 in
      let get_int64_for_apron_scalar scalar is_min_int =
        match get_int_for_apron_scalar scalar with
        | Some x -> Int64.of_int x
        | _ -> if is_min_int then Int64.min_int else Int64.max_int
      in
      let open Interval in
      let infimum = get_int64_for_apron_scalar interval_of_variable.inf true in
      let supremum = get_int64_for_apron_scalar interval_of_variable.sup false in
      if infimum = Int64.min_int && supremum = Int64.max_int then
        IntDomain.IntDomTuple.top()
      else (
        if infimum = Int64.min_int then
          IntDomain.IntDomTuple.ending supremum
        else (
          if supremum = Int64.max_int then
            IntDomain.IntDomTuple.starting infimum
          else
            IntDomain.IntDomTuple.of_interval (infimum,supremum)
        )
      )

  let get_field_and_struct_name_from_variable_name variable_name =
    let index_character_between_field_and_comp_name_char = (String.index variable_name character_between_field_and_comp_name_char) in
    let last_index_field_name = if String.contains variable_name local_identifier_char then (String.length variable_name) - 2 else (String.length variable_name) - 1  in
    let first_index_field_name = index_character_between_field_and_comp_name_char + 1 in
    let length_field_name = last_index_field_name - first_index_field_name + 1 in
    String.sub variable_name (index_character_between_field_and_comp_name_char + 1) length_field_name,
    String.sub variable_name 0 index_character_between_field_and_comp_name_char

  let rename_struct_of_field unique_field_name struct_name =
    let field_name, old_struct_name = get_field_and_struct_name_from_variable_name unique_field_name in
    if old_struct_name = struct_name then unique_field_name
    else (
      if (String.contains unique_field_name local_identifier_char) then
        struct_name ^ character_between_field_and_comp_name ^ field_name ^ local_identifier
      else struct_name ^ character_between_field_and_comp_name ^ field_name
    )

  let map func (apron_abstract_value, struct_mapping) =
    Pervasives.print_endline "MAP";
    Pretty.fprint Pervasives.stdout 0 (pretty () (apron_abstract_value, struct_mapping));
    let environment = (A.env apron_abstract_value) in
    let (vars_int, vars_real) = Environment.vars environment in
    let all_vars = (Array.to_list vars_int) @ (Array.to_list vars_real) in
    let result, struct_mapping, variables_to_remove = List.fold_left (
        fun (result, struct_mapping, variables_to_remove) var ->
          let field_name = Var.to_string var in
          let variable_name =  (original_variable_name field_name) in
          let int_val = get_int_val_for_field_name field_name apron_abstract_value in
          let value = Compound.of_int_val int_val in
          let compound_val_result_func = (func value) in
          let result = remove_variable_with_name field_name result in
          let int_val_result_func = Compound.to_int_val compound_val_result_func in
          let struct_map_key = (`Lifted variable_name, `Lifted field_name) in
          if not(StructMap.mem struct_map_key struct_mapping) then (result, struct_mapping, variables_to_remove)
          else
            let comp_in_map = StructMap.find struct_map_key struct_mapping in
            let struct_mapping = StructMap.add struct_map_key comp_in_map struct_mapping in
            assign_int_value_to_variable_name result int_val_result_func field_name, struct_mapping, variables_to_remove
      ) (apron_abstract_value, struct_mapping, []) all_vars
    in
    (*    let struct_name_mapping = List.fold_left (fun struct_name_mapping variable_to_remove -> StructNameMap.remove variable_to_remove struct_name_mapping) struct_name_mapping variables_to_remove in*)
    Pervasives.print_endline "Result map!";
    Pretty.fprint Pervasives.stdout 0 (pretty () (result, struct_mapping));
    result, struct_mapping

  let fold func (apron_abstract_value, struct_mapping) init_value =
    Pervasives.print_endline "FOLD";
    Pretty.fprint Pervasives.stdout 0 (pretty () (apron_abstract_value, struct_mapping));
    let environment = (A.env apron_abstract_value) in
    let (vars_int, vars_real) = Environment.vars environment in
    let all_vars = (Array.to_list vars_int) @ (Array.to_list vars_real) in
    List.fold_left (
      fun result var ->
        let unique_field_name = (Var.to_string var) in
        let int_val = get_int_val_for_field_name unique_field_name apron_abstract_value in
        Pervasives.print_endline "unique_field_name";
        Pervasives.print_endline (unique_field_name ^ ".");
        (*        let int_val =
          if IntDomain.IntDomTuple.id_top int_val then

          else
            int_val
          in *)
        let field_name, struct_name = get_field_and_struct_name_from_variable_name unique_field_name in
        let map_key = `Lifted struct_name, `Lifted field_name in
        let value = Compound.of_int_val int_val in
        if not (StructMap.mem map_key struct_mapping) then (
          Pervasives.print_endline (StructMapKey.short 100 map_key);
          raise (Invalid_argument (struct_name ^ " not in mapping!"))
        )
        else
          let field = StructMap.find map_key struct_mapping in
          func field value result
    ) init_value all_vars

  let meet_local_and_global_state (local_apron_abstract_value, local_struct_mapping) (global_apron_abstract_value, global_struct_mapping) =
    Pervasives.print_endline "meet_local_and_global_state";
    Pervasives.print_endline (short 1000 (local_apron_abstract_value, local_struct_mapping));
    Pervasives.print_endline (short 1000 (global_apron_abstract_value, global_struct_mapping));
    let result =
      meet_local_and_global_state local_apron_abstract_value global_apron_abstract_value, StructMap.meet local_struct_mapping global_struct_mapping
    in
    Pervasives.print_endline (short 1000 result);
    result

  let get_value_of_variable_and_possibly_globals varinfo (apron_abstract_value, struct_mapping) should_return_globals  =
    let fields_not_to_remove =
      StructMap.fold (fun _ value fields_not_to_remove ->
          match value, varinfo with
          | `Field(Some v, field), Some varinfo ->
            if (v.vid = varinfo.vid) || (v.vglob && should_return_globals) then
              [value] @ fields_not_to_remove
            else fields_not_to_remove
          | `Field(Some v, field), Some varinfo when should_return_globals ->
            if v.vglob then
              [value] @ fields_not_to_remove
            else fields_not_to_remove
          | _ -> fields_not_to_remove
        ) struct_mapping [] in
    let fields_not_to_remove =
      List.map (fun field ->
          get_unique_field_name field) fields_not_to_remove in
    remove_all_but_with apron_abstract_value fields_not_to_remove;
    apron_abstract_value, struct_mapping

  let get_value_of_variable_and_globals varinfo (apron_abstract_value, struct_mapping) =
    get_value_of_variable_and_possibly_globals (Some varinfo) (apron_abstract_value, struct_mapping) true

  let get_value_of_variable varinfo (apron_abstract_value, struct_mapping) =
    get_value_of_variable_and_possibly_globals (Some varinfo) (apron_abstract_value, struct_mapping) false

  let get_value_of_globals (apron_abstract_value, struct_mapping) =
    get_value_of_variable_and_possibly_globals None (apron_abstract_value, struct_mapping) true

  let get key (apron_abstract_value, struct_mapping) =
    match key with
    | `Field(var, field) -> (
          let field_name = get_unique_field_name key in
          let int_val = get_int_val_for_field_name field_name apron_abstract_value in
          Compound.of_int_val int_val
      )
    | _ -> raise (Invalid_argument "")

  let rec rename_cil_variables cil_exp add_local_identifier struct_name_mapping =
    match cil_exp with
    | BinOp(op, exp1, exp2, typ) -> BinOp(op, (rename_cil_variables exp1 add_local_identifier struct_name_mapping), (rename_cil_variables exp2 add_local_identifier struct_name_mapping), typ)
    | Lval (Var v, (Field (field, offs))) -> (
        (if add_local_identifier then (
            let new_var_name = (get_unique_field_name (`Field (Some v, field))) in
            v.vname <- new_var_name;
          )
         else (
           let _, variable_name = get_field_and_struct_name_from_variable_name v.vname in
           v.vname <- variable_name;
         )
        );
        Lval (Var v, (Field (field, offs)))
      )
    | UnOp (op, exp, typ) -> UnOp (op, (rename_cil_variables exp add_local_identifier struct_name_mapping), typ)
    | _ -> cil_exp

  let eval_assert_cil_exp cil_exp (apron_abstract_value, struct_name_mapping) =
    let cil_exp = rename_cil_variables cil_exp true struct_name_mapping in
    let result = assert_inv apron_abstract_value cil_exp false in
    let _ = rename_cil_variables cil_exp false struct_name_mapping in
    result, struct_name_mapping

  let get_value_of_cil_exp cil_exp (apron_abstract_value, struct_mapping) =
    match cil_exp with
    | Lval (Var v, _) -> get_value_of_variable v (apron_abstract_value, struct_mapping)
    | _ -> top ()

  let rename_variable_of_field abstract_value old_key value_old_key new_variable =
    match old_key with
    | `Field _ -> (
        match abstract_value with
        | (apron_val, struct_map) ->
          let value_old_key, old_struct_map = value_old_key in
          let old_field_name = get_unique_field_name old_key in
          Pervasives.print_endline ("old field name: " ^ old_field_name);
          let new_key = match old_key with | `Field(_, new_field) -> (match new_variable with Some variable -> `Field(new_variable,new_field) | _ -> raise (Invalid_argument "")) | _ -> raise (Invalid_argument "") in
          let new_struct_map_key = match new_key with | `Field(Some var, new_field) -> `Lifted var.vname, `Lifted new_field.fname | `Field(_, new_field) -> `Lifted "", `Lifted new_field.fname | _ -> raise (Invalid_argument "") in
          let new_field_name = get_unique_field_name new_key in
          Pervasives.print_endline ("new field name: " ^ new_field_name);
          Pervasives.print_endline ("old val: " ^ (short 1000 (value_old_key, old_struct_map)));
          Pervasives.print_endline ("new val: " ^ (short 1000 (abstract_value)));
          let struct_map = StructMap.add new_struct_map_key new_key struct_map in
          let environment = A.env value_old_key in
          if Environment.mem_var environment (Var.of_string old_field_name) &&
             not (Environment.mem_var environment (Var.of_string new_field_name))
          then
            let renamed_old_val = A.rename_array Man.mgr value_old_key (Array.of_list [Var.of_string old_field_name]) (Array.of_list [Var.of_string new_field_name]) in
            let new_apron_val, _ = remove_variable (Var.of_string new_field_name) abstract_value in
            meet (new_apron_val, struct_map) (renamed_old_val, old_struct_map)
          else (
            if not (Environment.mem_var environment (Var.of_string new_field_name)) then (
              Pervasives.print_endline "here not mem var";
              let value_old_key = A.rename_array Man.mgr apron_val (Array.of_list [Var.of_string old_field_name]) (Array.of_list [Var.of_string new_field_name]) in
              meet (apron_val,struct_map) (value_old_key, old_struct_map)
            )
            else (
              if Environment.mem_var environment (Var.of_string old_field_name) then (
                let int_val_of_old_field = get_int_val_for_field_name old_field_name apron_val in
                let apron_val = assign_int_value_to_variable_name apron_val int_val_of_old_field new_field_name  in
                Pervasives.print_endline "HERE";
                apron_val, struct_map
              )
              else (
                Pervasives.print_endline "else";
                apron_val, struct_map
              )
          )
          )
      )
    | _ -> abstract_value

  let get_apron_keys_of_variable varinfo =
    match varinfo.vtype with
    | TNamed (t, _) -> (
        match t.ttype with
        | TComp (comp, _) when comp.cstruct ->
          (List.map (fun fieldinfo -> (`Field (Some varinfo, fieldinfo))) comp.cfields)
        | _ -> []
      )
    | _ -> []

  let add_variable_value_list lhost_val_list abstract_value =
(*    let abstract_value = List.fold_left (fun abstract_value variable_to_remove ->
        replace abstract_value variable_to_remove (Compound.top())) abstract_value variables_to_remove in *)
    let result = List.fold_left (fun abstract_value (old_variable, new_lhost, abstract_value) ->
        Pervasives.print_endline "\n\nadd_variable_value_list";
        Pretty.fprint Pervasives.stdout 0 (Cil.printExp Cil.defaultCilPrinter () (Lval (new_lhost, NoOffset)));
        Pervasives.print_endline (": " ^ (short 100 abstract_value));
        let value_old_key, struct_mapping_old_key = abstract_value in
        let environment = (A.env value_old_key) in
        let (vars_int, vars_real) = Environment.vars environment in
        let all_vars = (Array.to_list vars_int) @ (Array.to_list vars_real) in
        let keys_of_old_var, old_var =
          match old_variable with
          | Some old_variable -> (
              let apron_keys_of_variable = get_apron_keys_of_variable old_variable in
              List.fold_right (fun field_of_old_var (keys_of_old_var, _) ->
                  let unique_field_name_of_old_var = get_unique_field_name field_of_old_var in
                  let _, field_of_old_var =
                    get_field_and_struct_name_from_variable_name unique_field_name_of_old_var in
                  let old_key = StructMap.find (`Lifted old_variable.vname, `Lifted field_of_old_var) struct_mapping_old_key in
                  match old_key with
                  | `Field(Some var, _) -> [old_key] @ keys_of_old_var, Some old_variable
                  | _ -> (keys_of_old_var), Some old_variable
                ) apron_keys_of_variable ([], None)
            )
          | _ -> (
              List.fold_right (fun apron_variable (keys_of_old_var, old_var) ->
                  let field_name, struct_name =
                    get_field_and_struct_name_from_variable_name (Var.to_string apron_variable)
                  in
                  let old_key = StructMap.find (`Lifted struct_name, `Lifted field_name) struct_mapping_old_key
                  in
                  match old_key with
                  | `Field(Some var, _) -> [old_key] @ keys_of_old_var, Some var
                  | _ -> (keys_of_old_var, old_var)
                ) all_vars ([], None)
            )
        in
        let new_var =
          match new_lhost with
          | Var v -> (
              match v.vtype with
              | TNamed (t, _) -> (
                  match t.ttype with
                  | TComp (comp, _) ->
                    Some v
                  | _ -> None
                )
                | TVoid _ -> (* this is the case for the return variable *)
                  Some v
                | _ -> None
            )
          | _ -> None
        in
        if List.length keys_of_old_var > 0 then
          let apron_value_after_renaming, _ =
            List.fold_left (
              fun (abstract_value, value_old_key) old_key ->
                let result =
                  rename_variable_of_field abstract_value old_key value_old_key new_var in
                Pervasives.print_endline "Result of rename";
                Pervasives.print_endline (short 1000 result);
                result, result
            ) (abstract_value, (value_old_key, struct_mapping_old_key)) keys_of_old_var
          in
          apron_value_after_renaming
        else
          abstract_value
      ) abstract_value lhost_val_list
    in
    Pervasives.print_endline "Result of add_var_val_list";
    Pervasives.print_endline (short 1000 result);
    result

end
