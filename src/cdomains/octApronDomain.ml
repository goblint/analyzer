open Prelude
open Cil
open Pretty
open Format

(* This is to be able to use their implementation of octagons *)
open Apron

(* Module definition *)
module Man =
struct
  (* Manager type, parameter for the command below *)
  type mt = Oct.t
  (* A type of manager allocated by the underlying octagon domain *)
  type t = mt Manager.t

  (* Allocate a new manager to manipulate octagons *)
  let mgr = Oct.manager_alloc ()
  (* Making an environment from a set of integer and real variables. 
  Raise Failure in case of name conflict. 
  In this case the environment is empty to begin with. *)
  let eenv = Environment.make [||] [||]
end

(* Generic operations on abstract values at level 1 of interface, there is also Abstract0 *)
module A = Abstract1 

module D =
struct
  type t = Man.mt A.t

  let name () = "APRON octagon numerical abstract domain"

  let topE = A.top    Man.mgr
  let botE = A.bottom Man.mgr

  let top () = topE Man.eenv
  let bot () = botE Man.eenv
  let is_top = A.is_top    Man.mgr
  let is_bot = A.is_bottom Man.mgr

  let to_yojson x = failwith "TODO implement to_yojson"
  let invariant _ _ = Invariant.none
  let tag _ = failwith "Std: no tag"
  let arbitrary () = failwith "no arbitrary"

  let short n (x:t) =
    A.print Legacy.Format.str_formatter x;
    Legacy.Format.flush_str_formatter ()

  (* This function joins two octagons.
  If one of them is equal to bottom, the other one is the result of the join. 
  Otherwise, we use the manager form Apron to join the octagons. *)
  let join x y =
    let () = print_endline "Joining octagons:" in
    let () = print_endline (short 30 x) in
    let () = print_endline (short 30 y) in
    let ret = if is_bot x then
      y
    else if is_bot y then
      x
    else
      A.join (Man.mgr) x y in
    let () = print_endline "Result of join:" in
    let () = print_endline (short 30 ret) in
    ret

  (* This function performs a meet two octagons.
  If one of them is equal to top, the other one is the result of the meet. 
  If either of them is equal to bottom, bottom is the result of the meet.
  Otherwise, we use the manager form Apron to join the octagons. *)
  let meet x y =
    let () = print_endline "Meeting octagons:" in
    let () = print_endline (short 30 x) in
    let () = print_endline (short 30 y) in
    let ret = if is_top x then y else
    if is_top y then x else
    if is_bot x || is_bot y then bot () else
      A.meet Man.mgr x y in
    
    let () = print_endline (short 30 ret) in
    ret

  (* This function performs a widening of two octagons.
  If one of them is equal to bottom, the other one is the result of the widening. 
  Otherwise, we use the manager from Apron to do the widening. *)
  let widen x y =
    let () = print_endline "Widening octagons:" in
    let () = print_endline (short 30 x) in
    let () = print_endline (short 30 y) in
    let ret = if is_bot x then
      y
    else if is_bot y then
      x
    else
      A.widening (Man.mgr) x y in
    let () = print_endline (short 30 ret) in
    ret

  let narrow = meet

  (* This function is used to compare two octagons for equality. *)
  let equal x y =
    (* If both octagons are equal to bottom then they are equal. *)
    (* If only the first octagon is equal to bottom, then they are different. *)
    if is_bot x then is_bot y 
    (* If only the second octagon is equal to bottom, then they are different. *)
    else if is_bot y then false
    (* If both octagons are equal to top then they are equal. *)
    (* If only the first octagon is equal to top, then they are different. *)
    else if is_top x then is_top y 
    (* If only the second octagon is equal to top, then they are different. *)
    else if is_top y then false 
    (* Otherwise, we use Apron to check for equality. *)
    else A.is_eq Man.mgr x y 

  (* Compares octagons for <= *)
  let leq x y =
    (* There is nothing less than bottom or greater than top,
    the following two ifs logically follow from that. *)
    if is_bot x || is_top y then true else
    if is_bot y || is_top x then false else
    (* Otherwise, Apron does the comparison. *)
      A.is_leq (Man.mgr) x y

  let hash (x:t) = Hashtbl.hash x
  let compare (x:t) y = Stdlib.compare x y
  let isSimple x = true
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (short 80 x))
  let pretty_f s () (x:t) = text (s 10 x)
  let pretty = pretty_f short
  let pretty_diff () (x,y) = text "pretty_diff"

  (* Apron expressions of level 1 *)
  open Texpr1
  (* Apron linear constraints of level 1 *)
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

  let print_lincons l = Lincons0.print string_of_int Format.std_formatter l

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
    ignore (Pretty.printf "cil_exp_to_apron_linecons exptolinecons '%a'\n" d_plainexp cil_exp);
    let linexpr1, comparator = cil_exp_to_apron_linexpr1 environment cil_exp should_negate in
    match linexpr1, comparator with
    | Some linexpr1, Some comparator -> 
      Some (Lincons1.make linexpr1 comparator)
    | _ -> None


  (* Assert an invariant *)
  (* Gives the result of the meet operation of the given octagon 
  with the linear constraints coming from the given expression *)
  let assert_inv d x b =
    let () = print_endline "Asserting" in 
    let () = print_endline (Pretty.sprint 20 (Cil.d_exp () x))  in 
    try
      (* if assert(x) then convert it to assert(x != 0) *)
      let x = match x with
        | Lval (Var v,NoOffset) when isArithmeticType v.vtype ->
          UnOp(LNot, (BinOp (Eq, x, (Const (CInt64(Int64.of_int 0, IInt, None))), intType)), intType)
        | _ -> x in
      (* Linear constraints from an expression x in an environment of octagon d *)
      let linecons = cil_exp_to_apron_linecons (A.env d) x b in 
      (* Linear constraints are optional, so we check if there are any. *)
      match linecons with
      | Some linecons ->
        let () = print_endline "Linecons + oct" in
        let () = Lincons1.print Format.std_formatter linecons in
        let () = print_endline "" in
        let l0 = linecons.lincons0 in
        let () = print_lincons l0 in
        let () = print_endline "" in
        (* Get the underlying linear constraint of level 0. 
        Modifying the constraint of level 0 (not advisable) 
        modifies correspondingly the linear constraint and conversely, 
        except for changes of environments *)
        let ea = { lincons0_array = [|Lincons1.get_lincons0 linecons |]
                 ; array_env = A.env d
                 }
        in
        (* We perform a meet of the current octagon with the linear constraints 
        that come from the expression we wish to assert. *)
        let () = print_endline (short 30 d) in
        let meet_res = A.meet_lincons_array Man.mgr d ea in
        let () = print_endline (short 30 meet_res) in
        meet_res
      | None -> d
    with Invalid_argument "cil_exp_to_lexp" -> d

  let assert_fn ctrlctx octa e warn =  
    let check_assert e state =
      if equal state (assert_inv state e false) then
        `True
      else
        `False
    in
    let expr = sprint 30 (Cil.d_exp () e) in
    let warn ?annot msg = if warn then
        if GobConfig.get_bool "dbg.regression" then ( (* This only prints unexpected results (with the difference) as indicated by the comment behind the assert (same as used by the regression test script). *)
          let loc = !Messages.current_loc in
          let line = List.at (List.of_enum @@ File.lines_of loc.file) (loc.line-1) in
          let open Str in
          let expected = if string_match (regexp ".+//.*\\(FAIL\\|UNKNOWN\\).*") line 0 then Some (matched_group 1 line) else None in
          if expected <> annot then (
            let result = if annot = None && (expected = Some ("NOWARN") || (expected = Some ("UNKNOWN") && not (String.exists line "UNKNOWN!"))) then "improved" else "failed" in
            (* Expressions with logical connectives like a && b are calculated in temporary variables by CIL. Instead of the original expression, we then see something like tmp___0. So we replace expr in msg by the original source if this is the case. *)
            let assert_expr = if string_match (regexp ".*assert(\\(.+\\));.*") line 0 then matched_group 1 line else expr in
            let msg = if expr <> assert_expr then String.nreplace msg expr assert_expr else msg in
            Messages.warn_each ~ctx:ctrlctx (msg ^ " Expected: " ^ (expected |? "SUCCESS") ^ " -> " ^ result)
          )
        ) else
        Messages.warn_each ~ctx:ctrlctx msg
    in
    match check_assert e octa with
    | `False ->
      warn ~annot:"FAIL" ("{red}Assertion \"" ^ expr ^ "\" will fail.");
      octa
    | `True ->
      warn ("{green}Assertion \"" ^ expr ^ "\" will succeed");
      octa

  (* Converts CIL expressions to Apron expressions of level 1 *)
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

  (* This function is used by the query function to compare expressions for equality. *)
  (* The arguments are the octagon and the two expressions to be compared. *)
  let cil_exp_equals d exp1 exp2 =
    (* If the octagon is equal to bottom, we return false. *)
    if (is_bot d) then false
    else
      begin
        (* let () = print_endline (String.concat " compare_expression " [(Pretty.sprint 20 (Cil.d_exp () exp1)); (Pretty.sprint 20 (Cil.d_exp () exp2))])  in *)
        (* Create a compare expression from two expressions *)
        let compare_expression = BinOp (Eq, exp1, exp2, TInt (IInt, [])) in
        (* We compare the octagon with the octagon we get by performing meet of it with the linear constraints coming from the expression *)
        let () = print_endline "cil_exp_equals will compare exps" in
        let () = print_endline (Pretty.sprint 20 (Cil.d_exp () exp1))  in 
        let () = print_endline (Pretty.sprint 20 (Cil.d_exp () exp2))  in 
        let resulting_oct = (assert_inv d compare_expression false) in
        let comp_result = equal d resulting_oct in
        let () = print_endline "comparing..." in
        let () = print_endline (short 30 d) in
        let () = print_endline "...and..." in
        let () = print_endline (short 30 resulting_oct) in
        comp_result
      end

end