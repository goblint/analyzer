open Cil
open Prelude
open Apron
open Pretty


module BI = IntOps.BigIntOps
module type Tracked =
sig
  val type_tracked: typ -> bool
  val varinfo_tracked: varinfo -> bool
end

module Var =
struct
  include Var
  let equal x y = Var.compare x y = 0
end

module M = Messages

module type ConvBounds =
sig
  type d
  val bound_texpr: d -> Texpr1.t -> Z.t option * Z.t option
end

module Tracked =
struct
  let type_tracked typ =
    isIntegralType typ

  let varinfo_tracked vi =
    (* no vglob check here, because globals are allowed in apron, but just have to be handled separately *)
    type_tracked vi.vtype && not vi.vaddrof
end

module Convert (Bounds: ConvBounds) =
struct
  open Texpr1
  open Tcons1
  (* module Bounds = Bounds(Man) *)
  exception Unsupported_CilExp

  (* TODO: move this into some general place *)
  let is_cast_injective from_type to_type =
    let (from_min, from_max) = IntDomain.Size.range_big_int (Cilfacade.get_ikind from_type) in
    let (to_min, to_max) = IntDomain.Size.range_big_int (Cilfacade.get_ikind to_type) in
    BI.compare to_min from_min <= 0 && BI.compare from_max to_max <= 0

  let texpr1_expr_of_cil_exp d env no_ov =
    (* recurse without env argument *)
    let rec texpr1_expr_of_cil_exp = function
      | Lval (Var v, NoOffset) when Tracked.varinfo_tracked v ->
        if not v.vglob then
          let var = Var.of_string v.vname in
          if Environment.mem_var env var then
            Var var
          else
            raise Unsupported_CilExp
        else
          failwith "texpr1_expr_of_cil_exp: globals must be replaced with temporary locals"
      | Const (CInt64 (i, _, s)) ->
        let str = match s with
          | Some s -> s
          | None -> Int64.to_string i
        in
        Cst (Coeff.s_of_mpqf (Mpqf.of_string str))
      | exp ->
        let expr =
          match exp with
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
          | CastE (TInt _ as t, e) when is_cast_injective (Cilfacade.typeOf e) t -> (* TODO: unnecessary cast check due to overflow check below? or maybe useful in general to also assume type bounds based on argument types? *)
            Unop (Cast, texpr1_expr_of_cil_exp e, Int, Zero) (* TODO: what does Apron Cast actually do? just for floating point and rounding? *)
          | _ ->
            raise Unsupported_CilExp
        in
        let ik = Cilfacade.get_ikind_exp exp in
        if no_ov then (
          let (type_min, type_max) = IntDomain.Size.range_big_int ik in
          let texpr1 = Texpr1.of_expr env expr in
          match Bounds.bound_texpr d texpr1 with
          | Some min, Some max when BI.compare type_min min <= 0 && BI.compare max type_max <= 0 -> ()
          | _ ->
            (* ignore (Pretty.printf "apron may overflow %a\n" dn_exp exp); *)
            raise Unsupported_CilExp
        );
        expr
    in
    texpr1_expr_of_cil_exp

  let texpr1_of_cil_exp d env e ov =
    let e = Cil.constFold false e in
    of_expr env (texpr1_expr_of_cil_exp d env ov e)

  let tcons1_of_cil_exp d env e negate ov =
    let e = Cil.constFold false e in
    let (texpr1_plus, texpr1_minus, typ) =
      match e with
      | BinOp (r, e1, e2, _) ->
        let texpr1_1 = texpr1_expr_of_cil_exp d env ov e1 in
        let texpr1_2 = texpr1_expr_of_cil_exp d env ov e2 in
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
    make (of_expr env texpr1') typ

end

let mpqf_of_cst cst =
  let open Coeff in
  match cst with
  | Interval _ -> failwith "Not a constant"
  | Scalar x -> (match x with
      | Float x -> Mpqf.of_float x
      | Mpqf x -> x
      | Mpfrf x -> Mpfr.to_mpq x)

module EnvOps =
struct
  let vars env =
    let ivs, fvs = Environment.vars env in
    assert (Array.length fvs = 0); (* shouldn't ever contain floats *)
    List.of_enum (Array.enum ivs)

  let mem_var env v = Environment.mem_var env v

  let add_vars_with env vs =
    let vs' =
      vs
      |> List.enum
      |> Enum.filter (fun v -> not (Environment.mem_var env v))
      |> Array.of_enum
    in
    Environment.add env vs' [||]

  let remove_vars_with env vs =
    let vs' =
      vs
      |> List.enum
      |> Enum.filter (fun v -> Environment.mem_var env v)
      |> Array.of_enum
    in
    Environment.remove env vs'

  let get_filtered_vars_add env vs =
    vs
    |> List.enum
    |> Enum.filter (fun v -> not (Environment.mem_var env v))
    |> Array.of_enum

  let get_filtered_vars_remove env vs =
    vs
    |> List.enum
    |> Enum.filter (fun v -> Environment.mem_var env v)
    |> Array.of_enum

  let remove_filter_with env f =
    let vs' =
      vars env
      |> List.enum
      |> Enum.filter f
      |> Array.of_enum
    in
    Environment.remove env vs'

  let filter_vars env f =
    vars env
    |> List.enum
    |> Enum.filter f
    |> Array.of_enum

  let keep_vars_with env vs =
    let vs' =
      vs
      |> List.enum
      |> Enum.filter (fun v -> Environment.mem_var env v)
      |> Array.of_enum
    in
    Environment.make vs' [||]

  let keep_filter_with env f =
    (* Instead of removing undesired vars,
       make a new env with just the desired vars. *)
    let vs' =
      vars env
      |> List.enum
      |> Enum.filter f
      |> Array.of_enum
    in
    Environment.make vs' [||]

end

module type AssertionRelD2 =
sig
  module Bounds: ConvBounds

  include RelationDomain.D2 with type t = Bounds.d

  val meet_with_tcons: t -> Tcons1.t -> t

  val is_bot_env: t -> bool

  val env: t -> Environment.t

  val assert_cons: t -> exp -> bool -> bool -> t
end

module AssertionModule (AD2: AssertionRelD2) =
struct
  include AD2

  module Convert = Convert (Bounds)

  let type_tracked typ =
    isIntegralType typ

  let varinfo_tracked vi =
    (* no vglob check here, because globals are allowed in apron, but just have to be handled separately *)
    type_tracked vi.vtype && not vi.vaddrof

  let exp_is_cons = function
    (* constraint *)
    | BinOp ((Lt | Gt | Le | Ge | Eq | Ne), _, _, _) -> true
    (* expression *)
    | _ -> false

  (** Assert any expression. *)
  let assert_inv d e negate no_ov =
    let e' =
      if exp_is_cons e then
        e
      else
        (* convert non-constraint expression, such that we assert(e != 0) *)
        BinOp (Ne, e, zero, intType)
    in
    assert_cons d e' negate no_ov


  let eval_interval_expr d e =
    match Convert.texpr1_of_cil_exp d (env d) e false with
    | texpr1 -> Bounds.bound_texpr d texpr1
    | exception Convert.Unsupported_CilExp -> (None, None)

  let check_asserts d e =
    if is_bot_env (assert_inv d e false false) then
      `False
    else if is_bot_env (assert_inv d e true false) then
      `True
    else
      `Top


  (** Evaluate constraint or non-constraint expression as integer. *)
  let eval_int d e =
    let module ID = Queries.ID in
    let ik = Cilfacade.get_ikind_exp e in
    if exp_is_cons e then
      match check_asserts d e with
      | `True -> ID.of_bool ik true
      | `False -> ID.of_bool ik false
      | `Top -> ID.top ()
    else
      match eval_interval_expr d e with
      | (Some min, Some max) -> ID.of_interval ik (min, max)
      | (Some min, None) -> ID.starting ik min
      | (None, Some max) -> ID.ending ik max
      | (None, None) -> ID.top ()
end