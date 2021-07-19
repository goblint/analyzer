open Prelude
open Cil
open Pretty
(* For Apron implementation of octagons *)
open Apron

module M = Messages


module Var =
struct
  include Var

  let equal x y = Var.compare x y = 0
end


module Man =
struct
  (* Manager type, parameter for the command below *)
  type mt = Oct.t
  (* type mt = Polka.equalities Polka.t *)
  (* type mt = Polka.loose Polka.t *)
  (* A type of manager allocated by the underlying octagon domain *)
  type t = mt Manager.t

  (* Allocate a new manager to manipulate octagons *)
  let mgr = Oct.manager_alloc ()
  (* let mgr = Polka.manager_alloc_equalities () *)
  (* let mgr = Polka.manager_alloc_loose () *)
end

(** Conversion from CIL expressions to Apron. *)
module Convert =
struct
  open Texpr1
  open Tcons1

  exception Unsupported_CilExp

  module BI = IntOps.BigIntOps

  (* TODO: move this into some general place *)
  let is_cast_injective from_type to_type =
    let (from_min, from_max) = IntDomain.Size.range_big_int (Cilfacade.get_ikind from_type) in
    let (to_min, to_max) = IntDomain.Size.range_big_int (Cilfacade.get_ikind to_type) in
    BI.compare to_min from_min <= 0 && BI.compare from_max to_max <= 0

  let texpr1_expr_of_cil_exp env =
    (* recurse without env argument *)
    let rec texpr1_expr_of_cil_exp = function
      | Lval (Var v, NoOffset) when isIntegralType v.vtype && not v.vglob ->
        let var = Var.of_string v.vname in
        if Environment.mem_var env var then
          Var var
        else
          raise Unsupported_CilExp
      | Const (CInt64 (i, _, s)) ->
        let str = match s with
          | Some s -> s
          | None -> Int64.to_string i
        in
        Cst (Coeff.s_of_mpqf (Mpqf.of_string str))
      | UnOp (Neg, e, _) ->
        Unop (Neg, texpr1_expr_of_cil_exp e, Int, Near)
      | BinOp (PlusA, e1, e2, _) ->
        Binop (Add, texpr1_expr_of_cil_exp e1, texpr1_expr_of_cil_exp e2, Int, Near)
      | BinOp (MinusA, e1, e2, _) ->
        Binop (Sub, texpr1_expr_of_cil_exp e1, texpr1_expr_of_cil_exp e2, Int, Near)
      | BinOp (Mult, e1, e2, _) ->
        Binop (Mul, texpr1_expr_of_cil_exp e1, texpr1_expr_of_cil_exp e2, Int, Near)
      | BinOp (Div, e1, e2, _) ->
        Binop (Div, texpr1_expr_of_cil_exp e1, texpr1_expr_of_cil_exp e2, Int, Zero)
      | BinOp (Mod, e1, e2, _) ->
        Binop (Mod, texpr1_expr_of_cil_exp e1, texpr1_expr_of_cil_exp e2, Int, Near)
      | CastE (TInt _ as t, e) when is_cast_injective (Cilfacade.typeOf e) t ->
        Unop (Cast, texpr1_expr_of_cil_exp e, Int, Zero) (* TODO: what does Apron Cast actually do? just for floating point and rounding? *)
      | _ ->
        raise Unsupported_CilExp
    in
    texpr1_expr_of_cil_exp

  let texpr1_of_cil_exp env e =
    let e = Cil.constFold false e in
    Texpr1.of_expr env (texpr1_expr_of_cil_exp env e)

  let tcons1_of_cil_exp env e negate =
    let e = Cil.constFold false e in
    let (texpr1_plus, texpr1_minus, typ) =
      match e with
      | BinOp (r, e1, e2, _) ->
        let texpr1_1 = texpr1_expr_of_cil_exp env e1 in
        let texpr1_2 = texpr1_expr_of_cil_exp env e2 in
        (* Apron constraints always compare with 0 and only have comparisons one way *)
        begin match r with
          | Lt -> (texpr1_2, texpr1_1, SUP)   (* e1 < e2   ==>  e2 - e1 > 0  *)
          | Gt -> (texpr1_1, texpr1_2, SUP)   (* e1 > e2   ==>  e1 - e2 > 0  *)
          | Le -> (texpr1_2, texpr1_1, SUPEQ) (* e1 <= e2  ==>  e2 - e1 >= 0 *)
          | Ge -> (texpr1_1, texpr1_2, SUPEQ) (* e1 >= e2  ==>  e1 - e2 >= 0 *)
          | Eq -> (texpr1_1, texpr1_2, EQ)    (* e1 == e2  ==>  e1 - e2 == 0 *)
          | Ne -> (texpr1_1, texpr1_2, DISEQ) (* e1 != e2  ==>  e1 - e2 != 0 *)
          | _ -> raise Unsupported_CilExp
        end
      | _ -> raise Unsupported_CilExp
    in
    let inverse_typ = function
      | EQ -> DISEQ
      | DISEQ -> EQ
      | SUPEQ -> SUP
      | SUP -> SUPEQ
      | EQMOD _ -> failwith "tcons1_of_cil_exp: cannot invert EQMOD"
    in
    let (texpr1_plus, texpr1_minus, typ) =
      if negate then
        (texpr1_minus, texpr1_plus, inverse_typ typ)
      else
        (texpr1_plus, texpr1_minus, typ)
    in
    let texpr1' = Binop (Sub, texpr1_plus, texpr1_minus, Int, Near) in
    Tcons1.make (Texpr1.of_expr env texpr1') typ
end

(* Generic operations on abstract values at level 1 of interface, there is also Abstract0 *)
module A = Abstract1

(** Convenience operations on A. *)
module AOps =
struct
  type t = Man.mt A.t

  let copy = A.copy Man.mgr

  let vars d =
    let ivs, fvs = Environment.vars (A.env d) in
    assert (Array.length fvs = 0); (* shouldn't ever contain floats *)
    List.of_enum (Array.enum ivs)

  let mem_var d v = Environment.mem_var (A.env d) v

  let add_vars_with nd vs =
    let env = A.env nd in
    let vs' =
      vs
      |> List.enum
      |> Enum.filter (fun v -> not (Environment.mem_var env v))
      |> Array.of_enum
    in
    let env' = Environment.add env vs' [||] in
    A.change_environment_with Man.mgr nd env' false

  let add_vars d vs =
    let nd = copy d in
    add_vars_with nd vs;
    nd

  let remove_vars_with nd vs =
    let env = A.env nd in
    let vs' =
      vs
      |> List.enum
      |> Enum.filter (fun v -> Environment.mem_var env v)
      |> Array.of_enum
    in
    let env' = Environment.remove env vs' in
    A.change_environment_with Man.mgr nd env' false

  let remove_vars d vs =
    let nd = copy d in
    remove_vars_with nd vs;
    nd

  let remove_filter_with nd f =
    let env = A.env nd in
    let vs' =
      vars nd
      |> List.enum
      |> Enum.filter f
      |> Array.of_enum
    in
    let env' = Environment.remove env vs' in
    A.change_environment_with Man.mgr nd env' false

  let remove_filter d f =
    let nd = copy d in
    remove_filter_with nd f;
    nd

  let keep_vars_with nd vs =
    let env = A.env nd in
    (* Instead of iterating over all vars in env and doing a linear lookup in vs just to remove them,
       make a new env with just the desired vs. *)
    let vs' =
      vs
      |> List.enum
      |> Enum.filter (fun v -> Environment.mem_var env v)
      |> Array.of_enum
    in
    let env' = Environment.make vs' [||] in
    A.change_environment_with Man.mgr nd env' false

  let keep_vars d vs =
    let nd = copy d in
    keep_vars_with nd vs;
    nd

  let keep_filter_with nd f =
    (* Instead of removing undesired vars,
       make a new env with just the desired vars. *)
    let vs' =
      vars nd
      |> List.enum
      |> Enum.filter f
      |> Array.of_enum
    in
    let env' = Environment.make vs' [||] in
    A.change_environment_with Man.mgr nd env' false

  let keep_filter d f =
    let nd = copy d in
    keep_filter_with nd f;
    nd

  let forget_vars_with nd vs =
    (* Unlike keep_vars_with, this doesn't check mem_var, but assumes valid vars, like assigns *)
    let vs' = Array.of_list vs in
    A.forget_array_with Man.mgr nd vs' false

  let forget_vars d vs =
    let nd = copy d in
    forget_vars_with nd vs;
    nd

  let assign_exp_with nd v e =
    try
      let texpr1 = Convert.texpr1_of_cil_exp (A.env nd) e in
      A.assign_texpr_with Man.mgr nd v texpr1 None
    with Convert.Unsupported_CilExp ->
      forget_vars_with nd [v]

  let assign_exp d v e =
    let nd = copy d in
    assign_exp_with nd v e;
    nd

  let assign_var_with nd v v' =
    let texpr1 = Texpr1.of_expr (A.env nd) (Var v') in
    A.assign_texpr_with Man.mgr nd v texpr1 None

  let assign_var d v v' =
    let nd = copy d in
    assign_var_with nd v v';
    nd

  let assign_var_parallel d vs v's =
    (* TODO: _with version? *)
    let env = A.env d in
    let vs = Array.of_list vs in
    let texpr1s =
      v's
      |> List.enum
      |> Enum.map (Texpr1.var env)
      |> Array.of_enum
    in
    A.assign_texpr_array Man.mgr d vs texpr1s None

  let substitute_exp_with nd v e =
    (* TODO: non-_with version? *)
    try
      let texpr1 = Convert.texpr1_of_cil_exp (A.env nd) e in
      A.substitute_texpr_with Man.mgr nd v texpr1 None
    with Convert.Unsupported_CilExp ->
      forget_vars_with nd [v]

  let substitute_var_with nd v v' =
    (* TODO: non-_with version? *)
    let texpr1 = Texpr1.of_expr (A.env nd) (Var v') in
    A.substitute_texpr_with Man.mgr nd v texpr1 None
end


module type SPrintable =
sig
  include Printable.S with type t = Man.mt A.t

  (* Functions for bot and top for particular environment. *)
  val top_env: Environment.t -> t
  val bot_env: Environment.t -> t
  val is_top_env: t -> bool
  val is_bot_env: t -> bool
end

module DBase: SPrintable =
struct
  type t = Man.mt A.t

  let name () = "OctApron"

  (* Functions for bot and top for particular environment. *)
  let top_env = A.top    Man.mgr
  let bot_env = A.bottom Man.mgr
  let is_top_env = A.is_top Man.mgr
  let is_bot_env = A.is_bottom Man.mgr

  let to_yojson x = failwith "TODO implement to_yojson"
  let invariant _ _ = Invariant.none
  let tag _ = failwith "Std: no tag"
  let arbitrary () = failwith "no arbitrary"
  let relift x = x

  let show (x:t) =
    Format.asprintf "%a (env: %a)" A.print x (Environment.print: Format.formatter -> Environment.t -> unit) (A.env x)
  let pretty () (x:t) = text (show x)

  let equal x y =
    Environment.equal (A.env x) (A.env y) && A.is_eq Man.mgr x y

  let hash (x:t) =
    A.hash Man.mgr x

  let compare (x:t) y: int =
    (* there is no A.compare, but polymorphic compare should delegate to Abstract0 and Environment compare's implemented in Apron's C *)
    Stdlib.compare x y
  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>\nconstraints\n</key>\n<value>\n%s</value>\n<key>\nenv\n</key>\n<value>\n%s</value>\n</map>\n</value>\n" (XmlUtil.escape (Format.asprintf "%a" A.print x)) (XmlUtil.escape (Format.asprintf "%a" (Environment.print: Format.formatter -> Environment.t -> unit) (A.env x)))

  let pretty_diff () (x,y) = text "pretty_diff"
end


module type SLattice =
sig
  include SPrintable
  include Lattice.S with type t := t
end

module DWithOps (D: SLattice) =
struct
  include D

  include AOps

  let type_tracked typ =
    isIntegralType typ
    && (not (GobConfig.get_bool "ana.octapron.no_uints") || Cil.isSigned (Cilfacade.get_ikind typ))

  let varinfo_tracked vi =
    (* TODO: vglob? vaddof? *)
    type_tracked vi.vtype

  let exp_is_cons = function
    (* constraint *)
    | BinOp ((Lt | Gt | Le | Ge | Eq | Ne), _, _, _) -> true
    (* expression *)
    | _ -> false

  (** Assert a constraint expression. *)
  let rec assert_cons d e negate =
    match e with
    (* Apron doesn't properly meet with DISEQ constraints: https://github.com/antoinemine/apron/issues/37.
       Join Gt and Lt versions instead. *)
    | BinOp (Ne, lhs, rhs, intType) when not negate ->
      let assert_gt = assert_cons d (BinOp (Gt, lhs, rhs, intType)) negate in
      let assert_lt = assert_cons d (BinOp (Lt, lhs, rhs, intType)) negate in
      join assert_gt assert_lt
    | BinOp (Eq, lhs, rhs, intType) when negate ->
      let assert_gt = assert_cons d (BinOp (Gt, lhs, rhs, intType)) (not negate) in
      let assert_lt = assert_cons d (BinOp (Lt, lhs, rhs, intType)) (not negate) in
      join assert_gt assert_lt
    | _ ->
      let env = A.env d in
      begin match Convert.tcons1_of_cil_exp env e negate with
        | tcons1 ->
          let ea = { Tcons1.tcons0_array = [|Tcons1.get_tcons0 tcons1 |]
                    ; array_env = env
                    }
          in
          A.meet_tcons_array Man.mgr d ea
        | exception Convert.Unsupported_CilExp ->
          d
      end

  (** Assert any expression. *)
  let assert_inv d e negate =
    let x =
      if exp_is_cons e then
        e
      else
        (* convert non-constraint expression, such that we assert(e != 0) *)
        BinOp (Ne, e, zero, intType)
    in
    assert_cons d x negate

  let check_assert (e:exp) state =
    match e with
    | Const (CInt64(i, kind, str)) -> `Top (* Octagon doesn't handle constant integers as assertions *)
    | CastE(t, e) -> `Top (* Octagon doesn't handle casts as assertions *)
    | Const(CChr c) -> `Top (*  Octagon doesn't handle character constants as assertions *)
    | _ ->
      let result_state = (assert_inv state e false) in
      let result_state_op = (assert_inv state e true) in
      if is_bot_env result_state then
        `False
      else if is_bot_env result_state_op then
        `True
      else
        `Top

  let get_int_interval_for_cil_exp d cil_exp =
    let get_int_for_apron_scalar (scalar: Scalar.t) =
      if Scalar.is_infty scalar <> 0 then (* infinity means unbounded *)
        None
      else
        match scalar with
        | Mpqf scalar when Mpzf.cmp_int (Mpqf.get_den scalar) 1 = 0 -> (* rational must be integer (denominator 1) *)
          Some (IntOps.BigIntOps.of_string (Mpqf.to_string scalar))
        | _ ->
          failwith "get_int_for_apron_scalar: not integer"
    in
    try
      let texpr1 = Convert.texpr1_of_cil_exp (A.env d) cil_exp in
      let interval_of_variable = A.bound_texpr Man.mgr d texpr1 in
      let infimum = get_int_for_apron_scalar interval_of_variable.inf in
      let supremum = get_int_for_apron_scalar interval_of_variable.sup in
      (infimum, supremum)
    with Convert.Unsupported_CilExp -> (None, None)

  let get_int_val_for_cil_exp d cil_exp =
    match get_int_interval_for_cil_exp d cil_exp with
    | Some infimum, Some supremum when IntOps.BigIntOps.equal infimum supremum ->
      Some infimum
    | _ ->
      None

  let cil_exp_equals d exp1 exp2 =
    if (is_bot_env d) then false
    else
      begin
        let compare_expression = BinOp (Eq, exp1, exp2, TInt (IInt, [])) in
        (* We compare the octagon with the octagon we get by performing meet of it with the linear constraints coming from the expression *)
        let resulting_oct = (assert_inv d compare_expression false) in
        let comp_result = equal d resulting_oct in
        comp_result
      end


  let assign_var_handling_underflow_overflow oct v e =
    let ikind = Cilfacade.get_ikind v.vtype in
    let signed = Cil.isSigned ikind in
    let new_oct = assign_exp oct (Var.of_string v.vname) e in
    let lower_limit, upper_limit = IntDomain.Size.range_big_int ikind in
    let check_max =
      check_assert (BinOp (Le, Lval (Cil.var @@ v), (Cil.kintegerCilint ikind (Cilint.cilint_of_big_int upper_limit)), intType)) new_oct in
    let check_min =
      check_assert (BinOp (Ge, Lval (Cil.var @@ v), (Cil.kintegerCilint ikind (Cilint.cilint_of_big_int lower_limit)), intType)) new_oct in
    if signed then
      if check_max <> `True || check_min <> `True then
        if GobConfig.get_bool "ana.octapron.no_signed_overflow" then
          new_oct
        else
          (* Signed overflows are undefined behavior, so octagon goes to top if it might have happened. *)
          top_env (A.env oct)
      else
        new_oct
    else if check_max <> `True || check_min <> `True then
      (* Unsigned overflows are defined, but for now
          the variable in question goes to top if there is a possibility of overflow. *)
      forget_vars oct [Var.of_string v.vname]
    else
      new_oct
end


module DLift: SLattice =
struct
  include DBase

  let lift_var = Var.of_string "##LIFT##"

  (** Environment (containing a unique variable [lift_var]) only used for lifted bot and top. *)
  let lift_env = Environment.make [|lift_var|] [||]

  (* Functions for lifted bot and top to implement [Lattice.S]. *)
  let top () = top_env lift_env
  let bot () = bot_env lift_env
  let is_top x = Environment.equal (A.env x) lift_env && is_top_env x
  let is_bot x = Environment.equal (A.env x) lift_env && is_bot_env x

  (* Apron can not join two abstract values have different environments.
     That hapens when we do a join with dead code and for that reason we need
     to handle joining with bottom manually.
     A similar if-based structure with is_top and is_bottom is also there for:
     meet, widen, narrow, equal, leq.*)

  let join x y =
    if is_bot x then
      y
    else if is_bot y then
      x
    else (
      if M.tracing then M.tracel "apron" "join %a %a\n" pretty x pretty y;
      A.join (Man.mgr) x y
      (* TODO: return lifted top if different environments? and warn? *)
    )

  let meet x y =
    if is_top x then y else
    if is_top y then x else
      A.meet Man.mgr x y
      (* TODO: return lifted bot if different environments? and warn? *)

  let widen x y =
    if is_bot x then
      y
    else if is_bot y then
      x (* TODO: is this right? *)
    else
      A.widening (Man.mgr) x y
      (* TODO: return lifted top if different environments? and warn? *)

  let narrow = meet

  let leq x y =
    if is_bot x || is_top y then true else
    if is_bot y || is_top x then false else (
      if M.tracing then M.tracel "apron" "leq %a %a\n" pretty x pretty y;
      Environment.equal (A.env x) (A.env y) && A.is_leq (Man.mgr) x y
      (* TODO: warn if different environments? *)
    )
end

module D = DWithOps (DLift)


(** With heterogeneous environments. *)
module DHetero: SLattice =
struct
  include DBase

  let gce (x: Environment.t) (y: Environment.t): Environment.t =
    let (xi, xf) = Environment.vars x in
    (* TODO: check type compatibility *)
    let i = Array.filter (Environment.mem_var y) xi in
    let f = Array.filter (Environment.mem_var y) xf in
    Environment.make i f

  let join x y =
    let x_env = A.env x in
    let y_env = A.env y in
    let c_env = gce x_env y_env in
    let x_c = A.change_environment Man.mgr x c_env false in
    let y_c = A.change_environment Man.mgr y c_env false in
    let join_c = A.join Man.mgr x_c y_c in
    let j_env = Environment.lce x_env y_env in
    A.change_environment Man.mgr join_c j_env false

  let strengthening j x y =
    if M.tracing then M.traceli "apron" "strengthening %a\n" pretty j;
    let x_env = A.env x in
    let y_env = A.env y in
    let j_env = A.env j in
    let x_j = A.change_environment Man.mgr x j_env false in
    let y_j = A.change_environment Man.mgr y j_env false in
    let x_cons = A.to_lincons_array Man.mgr x_j in
    let y_cons = A.to_lincons_array Man.mgr y_j in
    let try_add_con j con1 =
      let con0: Lincons0.t = con1.Lincons1.lincons0 in
      let cons1: Lincons1.earray = {lincons0_array = [|con0|]; array_env = j_env} in
      if M.tracing then M.tracei "apron" "try_add_con %s\n" (Format.asprintf "%a" (Lincons1.array_print: Format.formatter -> Lincons1.earray -> unit) cons1);
      let t = A.meet_lincons_array Man.mgr j cons1 in
      let t_x = A.change_environment Man.mgr t x_env false in
      let t_y = A.change_environment Man.mgr t y_env false in
      let leq_x = A.is_leq Man.mgr x t_x in
      let leq_y = A.is_leq Man.mgr y t_y in
      if M.tracing then M.trace "apron" "t: %a\n" pretty t;
      if M.tracing then M.trace "apron" "t_x (leq x %B): %a\n" leq_x pretty t_x;
      if M.tracing then M.trace "apron" "t_y (leq y %B): %a\n" leq_y pretty t_y;
      if leq_x && leq_y then (
        if M.tracing then M.traceu "apron" "added\n";
        t
      )
      else (
        if M.tracing then M.traceu "apron" "not added\n";
        j
      )
    in
    let x_cons1 = Array.map (fun con0 ->
        {Lincons1.lincons0 = con0; env = x_cons.array_env}
      ) x_cons.lincons0_array
    in
    let y_cons1 = Array.map (fun con0 ->
        {Lincons1.lincons0 = con0; env = y_cons.array_env}
      ) y_cons.lincons0_array
    in
    let cons1 =
      (* Whether [con1] contains a var in [env]. *)
      let env_exists_mem_con1 env con1 =
        try
          Lincons1.iter (fun _ var ->
              if Environment.mem_var env var then
                raise Not_found
            ) con1;
          false
        with Not_found ->
          true
      in
      (* Heuristically reorder constraints to pass 36/12 with singlethreaded->multithreaded mode switching. *)
      (* Put those constraints which strictly are in one argument's env first, to (hopefully) ensure they remain. *)
      let (x_cons1_some_y, x_cons1_only_x) = Array.partition (env_exists_mem_con1 y_env) x_cons1 in
      let (y_cons1_some_x, y_cons1_only_y) = Array.partition (env_exists_mem_con1 x_env) y_cons1 in
      Array.concat [x_cons1_only_x; y_cons1_only_y; x_cons1_some_y; y_cons1_some_x]
    in
    let j = Array.fold_left try_add_con j cons1 in
    if M.tracing then M.traceu "apron" "-> %a\n" pretty j;
    j

  let empty_env = Environment.make [||] [||]

  let bot () =
    top_env empty_env

  let top () =
    failwith "D2.top"

  let is_bot = equal (bot ())
  let is_top _ = false

  let join x y =
    (* just to optimize joining folds, which start with bot *)
    if is_bot x then
      y
    else if is_bot y then
      x
    else (
      if M.tracing then M.traceli "apron" "join %a %a\n" pretty x pretty y;
      let j = join x y in
      if M.tracing then M.trace "apron" "j = %a\n" pretty j;
      let j = strengthening j x y in
      if M.tracing then M.traceu "apron" "-> %a\n" pretty j;
      j
    )

  let meet x y =
    A.unify Man.mgr x y

  let leq x y =
    (* TODO: float *)
    let x_env = A.env x in
    let y_env = A.env y in
    let (x_vars, _) = Environment.vars x_env in
    if Array.for_all (Environment.mem_var y_env) x_vars then (
      let y' = A.change_environment Man.mgr y x_env false in
      A.is_leq Man.mgr x y'
    )
    else
      false

  let widen x y =
    let x_env = A.env x in
    let y_env = A.env y in
    if Environment.equal x_env y_env then
      A.widening Man.mgr x y (* widen if env didn't increase *)
    else
      y (* env increased, just use joined value in y, assuming env doesn't increase infinitely *)

  (* TODO: better narrow *)
  let narrow x y = x
end

module D2 = DWithOps (DHetero)


(* Copy-paste from BaseDomain... *)
type 'a octaproncomponents_t = {
  oct: D2.t;
  priv: 'a;
} [@@deriving eq, ord, to_yojson]

module OctApronComponents (PrivD: Lattice.S):
sig
  include Lattice.S with type t = PrivD.t octaproncomponents_t
  val op_scheme: (D2.t -> D2.t -> D2.t) -> (PrivD.t -> PrivD.t -> PrivD.t) -> t -> t -> t
end =
struct
  type t = PrivD.t octaproncomponents_t [@@deriving eq, ord, to_yojson]

  include Printable.Std
  open Pretty
  let hash r  = D2.hash r.oct + PrivD.hash r.priv * 33


  let show r =
    let first  = D2.show r.oct in
    let third  = PrivD.show r.priv in
    "(" ^ first ^ ", " ^ third  ^ ")"

  let pretty () r =
    text "(" ++
    D2.pretty () r.oct
    ++ text ", " ++
    PrivD.pretty () r.priv
    ++ text ")"

  let printXml f r =
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (Goblintutil.escape (D2.name ())) D2.printXml r.oct (Goblintutil.escape (PrivD.name ())) PrivD.printXml r.priv

  let name () = D2.name () ^ " * " ^ PrivD.name ()

  let invariant c {oct; priv} =
    Invariant.(D2.invariant c oct && PrivD.invariant c priv)

  let of_tuple(oct, priv):t = {oct; priv}
  let to_tuple r = (r.oct, r.priv)

  let arbitrary () =
    let tr = QCheck.pair (D2.arbitrary ()) (PrivD.arbitrary ()) in
    QCheck.map ~rev:to_tuple of_tuple tr

  let bot () = { oct = D2.bot (); priv = PrivD.bot ()}
  let is_bot {oct; priv} = D2.is_bot oct && PrivD.is_bot priv
  let top () = {oct = D2.top (); priv = PrivD.bot ()}
  let is_top {oct; priv} = D2.is_top oct && PrivD.is_top priv

  let leq {oct=x1; priv=x3 } {oct=y1; priv=y3} =
    D2.leq x1 y1 && PrivD.leq x3 y3

  let pretty_diff () (({oct=x1; priv=x3}:t),({oct=y1; priv=y3}:t)): Pretty.doc =
    if not (D2.leq x1 y1) then
      D2.pretty_diff () (x1,y1)
    else
      PrivD.pretty_diff () (x3,y3)

  let op_scheme op1 op3 {oct=x1; priv=x3} {oct=y1; priv=y3}: t =
    {oct = op1 x1 y1; priv = op3 x3 y3 }
  let join = op_scheme D2.join PrivD.join
  let meet = op_scheme D2.meet PrivD.meet
  let widen = op_scheme D2.widen PrivD.widen
  let narrow = op_scheme D2.narrow PrivD.narrow
end


module type VarMetadata =
sig
  type t
  val var_name: t -> string
end

module VarMetadataTbl (VM: VarMetadata) =
struct
  module VH = Hashtbl.Make (Var)

  let vh = VH.create 113

  let make_var ?name metadata =
    let name = Option.default_delayed (fun () -> VM.var_name metadata) name in
    let var = Var.of_string name in
    VH.replace vh var metadata;
    var

  let find_metadata var =
    VH.find_option vh var
end

module VM =
struct
  type t =
    | Local (** Var for function local variable (or formal argument). *) (* No varinfo because local Var with the same name may be in multiple functions. *)
    | Arg (** Var for function formal argument entry value. *) (* No varinfo because argument Var with the same name may be in multiple functions. *)
    | Return (** Var for function return value. *)
    | Global of varinfo

  let var_name = function
    | Local -> failwith "var_name of Local"
    | Arg -> failwith "var_name of Arg"
    | Return -> "#ret"
    | Global g -> g.vname
end

module V =
struct
  include VarMetadataTbl (VM)
  open VM

  let local x = make_var ~name:x.vname Local
  let arg x = make_var ~name:(x.vname ^ "'") Arg (* TODO: better suffix, like #arg *)
  let return = make_var Return
  let global g = make_var (Global g)
end
