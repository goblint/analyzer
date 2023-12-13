(** Relational value domain utilities. *)

open GoblintCil
open Batteries
open GobApron
module M = Messages


module BI = IntOps.BigIntOps

let int_of_scalar ?round (scalar: Scalar.t) =
  if Scalar.is_infty scalar <> 0 then (* infinity means unbounded *)
    None
  else
    let open GobOption.Syntax in
    match scalar with
    | Float f -> (* octD, boxD *)
      (* bound_texpr on bottom also gives Float even with MPQ *)
      let+ f = match round with
        | Some `Floor -> Some (Float.floor f)
        | Some `Ceil -> Some (Float.ceil f)
        | None when Stdlib.Float.is_integer f-> Some f
        | None -> None
      in
      BI.of_bigint (Z.of_float f)
    | Mpqf scalar -> (* octMPQ, boxMPQ, polkaMPQ *)
      let n = Mpqf.get_num scalar in
      let d = Mpqf.get_den scalar in
      let+ z =
        if Mpzf.cmp_int d 1 = 0 then (* exact integer (denominator 1) *)
          Some n
        else
          begin match round with
            | Some `Floor -> Some (Mpzf.fdiv_q n d) (* floor division *)
            | Some `Ceil -> Some (Mpzf.cdiv_q n d) (* ceiling division *)
            | None -> None
          end
      in
      Z_mlgmpidl.z_of_mpzf z
    | _ ->
      failwith ("int_of_scalar: unsupported: " ^ Scalar.to_string scalar)


module type ConvertArg =
sig
  val allow_global: bool
end

module type SV =  RelationDomain.RV with type t = Var.t

type unsupported_cilExp =
  | Var_not_found of CilType.Varinfo.t (** Variable not found in Apron environment. *)
  | Cast_not_injective of CilType.Typ.t (** Cast is not injective, i.e. may under-/overflow. *)
  | Exp_not_supported (** Expression constructor not supported. *)
  | Overflow (** May overflow according to Apron bounds. *)
  | Exp_typeOf of exn [@printer fun ppf e -> Format.pp_print_string ppf (Printexc.to_string e)] (** Expression type could not be determined. *)
  | BinOp_not_supported (** BinOp constructor not supported. *)
[@@deriving show { with_path = false }]

(** Interface for Bounds which calculates bounds for expressions and is used inside the - Convert module. *)
module type ConvBounds =
sig
  type t
  val bound_texpr: t -> Texpr1.t -> Z.t option * Z.t option
end

(** Conversion from CIL expressions to Apron. *)
module ApronOfCil (V: SV) (Bounds: ConvBounds) (Arg: ConvertArg) (Tracked: RelationDomain.Tracked) =
struct
  open Texpr1
  open Tcons1

  exception Unsupported_CilExp of unsupported_cilExp

  let () = Printexc.register_printer (function
      | Unsupported_CilExp reason -> Some (show_unsupported_cilExp reason)
      | _ -> None (* for other exception *)
    )

  let texpr1_expr_of_cil_exp d env exp no_ov =
    (* recurse without env argument *)
    let rec texpr1_expr_of_cil_exp = function
      | Lval (Var v, NoOffset) when Tracked.varinfo_tracked v ->
        if not v.vglob || Arg.allow_global then
          let var =
            if v.vglob then
              V.global v
            else
              V.local v
          in
          if Environment.mem_var env var then
            Var var
          else
            raise (Unsupported_CilExp (Var_not_found v))
        else
          failwith "texpr1_expr_of_cil_exp: globals must be replaced with temporary locals"
      | Const (CInt (i, _, _)) ->
        Cst (Coeff.s_of_mpqf (Mpqf.of_mpz (Z_mlgmpidl.mpz_of_z i)))
      | exp ->
        match Cilfacade.get_ikind_exp exp with
        | ik ->
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
            | CastE (TInt (t_ik, _) as t, e) ->
              begin match IntDomain.should_ignore_overflow t_ik || IntDomain.Size.is_cast_injective ~from_type:(Cilfacade.typeOf e) ~to_type:t with (* TODO: unnecessary cast check due to overflow check below? or maybe useful in general to also assume type bounds based on argument types? *)
                | true ->
                  Unop (Cast, texpr1_expr_of_cil_exp e, Int, Zero) (* TODO: what does Apron Cast actually do? just for floating point and rounding? *)
                | false
                | exception Cilfacade.TypeOfError _ (* typeOf inner e, not outer exp *)
                | exception Invalid_argument _ -> (* get_ikind in is_cast_injective *)
                  raise (Unsupported_CilExp (Cast_not_injective t))
              end
            | _ ->
              raise (Unsupported_CilExp Exp_not_supported)
          in
          if not no_ov then (
            let (type_min, type_max) = IntDomain.Size.range ik in
            let texpr1 = Texpr1.of_expr env expr in
            match Bounds.bound_texpr d texpr1 with
            | Some min, Some max when BI.compare type_min min <= 0 && BI.compare max type_max <= 0 -> ()
            | min_opt, max_opt ->
              if M.tracing then M.trace "apron" "may overflow: %a (%a, %a)\n" CilType.Exp.pretty exp (Pretty.docOpt (IntDomain.BigInt.pretty ())) min_opt (Pretty.docOpt (IntDomain.BigInt.pretty ())) max_opt;
              raise (Unsupported_CilExp Overflow)
          );
          expr
        | exception (Cilfacade.TypeOfError _ as e)
        | exception (Invalid_argument _ as e) ->
          raise (Unsupported_CilExp (Exp_typeOf e))
    in
    texpr1_expr_of_cil_exp exp

  let texpr1_of_cil_exp d env e no_ov =
    let e = Cil.constFold false e in
    Texpr1.of_expr env (texpr1_expr_of_cil_exp d env e no_ov)

  let tcons1_of_cil_exp d env e negate no_ov =
    let e = Cil.constFold false e in
    let (texpr1_plus, texpr1_minus, typ) =
      match e with
      | BinOp (r, e1, e2, _) ->
        let texpr1_1 = texpr1_expr_of_cil_exp d env e1 no_ov in
        let texpr1_2 = texpr1_expr_of_cil_exp d env e2 no_ov in
        (* Apron constraints always compare with 0 and only have comparisons one way *)
        begin match r with
          | Lt -> (texpr1_2, texpr1_1, SUP)   (* e1 < e2   ==>  e2 - e1 > 0  *)
          | Gt -> (texpr1_1, texpr1_2, SUP)   (* e1 > e2   ==>  e1 - e2 > 0  *)
          | Le -> (texpr1_2, texpr1_1, SUPEQ) (* e1 <= e2  ==>  e2 - e1 >= 0 *)
          | Ge -> (texpr1_1, texpr1_2, SUPEQ) (* e1 >= e2  ==>  e1 - e2 >= 0 *)
          | Eq -> (texpr1_1, texpr1_2, EQ)    (* e1 == e2  ==>  e1 - e2 == 0 *)
          | Ne -> (texpr1_1, texpr1_2, DISEQ) (* e1 != e2  ==>  e1 - e2 != 0 *)
          | _ -> raise (Unsupported_CilExp BinOp_not_supported)
        end
      | _ -> raise (Unsupported_CilExp Exp_not_supported)
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

  let find_one_var e =
    Basetype.CilExp.get_vars e
    |> List.filter Tracked.varinfo_tracked
    |> function
    | [v] -> Some v
    | _ -> None
end

(** Conversion from Apron to CIL expressions. *)
module CilOfApron (V: SV) =
struct
  exception Unsupported_Linexpr1

  let cil_exp_of_linexpr1 (linexpr1:Linexpr1.t) =
    let longlong = TInt(ILongLong,[]) in
    let coeff_to_const consider_flip (c:Coeff.union_5) = match c with
      | Scalar c ->
        (match int_of_scalar c with
         | Some i ->
           let ci,truncation = truncateCilint ILongLong i in
           if truncation = NoTruncation then
             if not consider_flip || Z.compare i Z.zero >= 0 then
               Const (CInt(i,ILongLong,None)), false
             else
               (* attempt to negate if that does not cause an overflow *)
               let cneg, truncation = truncateCilint ILongLong (Z.neg i) in
               if truncation = NoTruncation then
                 Const (CInt((Z.neg i),ILongLong,None)), true
               else
                 Const (CInt(i,ILongLong,None)), false
           else
             (M.warn ~category:Analyzer "Invariant Apron: coefficient is not int: %s" (Scalar.to_string c); raise Unsupported_Linexpr1)
         | None -> raise Unsupported_Linexpr1)
      | _ -> raise Unsupported_Linexpr1
    in
    let expr = ref (fst @@ coeff_to_const false (Linexpr1.get_cst linexpr1)) in
    let append_summand (c:Coeff.union_5) v =
      match V.to_cil_varinfo v with
      | Some vinfo ->
        (* TODO: What to do with variables that have a type that cannot be stored into ILongLong to avoid overflows? *)
        let var = Cilfacade.mkCast ~e:(Lval(Var vinfo,NoOffset)) ~newt:longlong in
        let coeff, flip = coeff_to_const true c in
        let prod = BinOp(Mult, coeff, var, longlong) in
        if flip then
          expr := BinOp(MinusA,!expr,prod,longlong)
        else
          expr := BinOp(PlusA,!expr,prod,longlong)
      | None -> M.warn ~category:Analyzer "Invariant Apron: cannot convert to cil var: %s"  (Var.to_string v); raise Unsupported_Linexpr1
    in
    Linexpr1.iter append_summand linexpr1;
    !expr


  let cil_exp_of_lincons1 (lincons1:Lincons1.t) =
    let zero = Cil.kinteger ILongLong 0 in
    try
      let linexpr1 = Lincons1.get_linexpr1 lincons1 in
      let cilexp = cil_exp_of_linexpr1 linexpr1 in
      match Lincons1.get_typ lincons1 with
      | EQ -> Some (Cil.constFold false @@ BinOp(Eq, cilexp, zero, TInt(IInt,[])))
      | SUPEQ -> Some (Cil.constFold false @@ BinOp(Ge, cilexp, zero, TInt(IInt,[])))
      | SUP -> Some (Cil.constFold false @@ BinOp(Gt, cilexp, zero, TInt(IInt,[])))
      | DISEQ -> Some (Cil.constFold false @@ BinOp(Ne, cilexp, zero, TInt(IInt,[])))
      | EQMOD _ -> None
    with
      Unsupported_Linexpr1 -> None
end

(** Conversion between CIL expressions and Apron. *)
module Convert (V: SV) (Bounds: ConvBounds) (Arg: ConvertArg) (Tracked: RelationDomain.Tracked)=
struct
  include ApronOfCil (V) (Bounds) (Arg) (Tracked)
  include CilOfApron (V)
end


(* Abstraction for the domain representations of the relational analyses:
   affineEqualityDomain (uses a matrix) and linearTwoVarEqualityDomain (uses an Array)*)
module type AbstractRelationalDomainRepresentation =
sig
  type t
  val hash: t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val empty : unit -> t
  val copy : t -> t
  val add_empty_columns : t -> int array -> t
  val is_empty : t -> bool
  val reduce_col_with : t -> int -> unit

  val remove_zero_rows : t -> t

  val del_cols : t -> int array -> t

end

(* Shared operations for the management of the Matrix or Array representations of the domains,
   used by affineEqualityDomain and linearTwoVarEqualityDomain *)
module VarManagementOps (RelDomain : AbstractRelationalDomainRepresentation) =
struct
  type t = {
    mutable d :  RelDomain.t option;
    mutable env : Environment.t
  }
  [@@deriving eq, ord, hash]

  let empty_env = Environment.make [||] [||]

  let bot () =
    {d = Some (RelDomain.empty ()); env = empty_env}

  let bot_env = {d = None; env = empty_env}

  let is_bot_env t = t.d = None

  let copy t = {t with d = Option.map RelDomain.copy t.d}

  let dim_add (ch: Apron.Dim.change) m =
    Array.iteri (fun i x -> ch.dim.(i) <- x + i) ch.dim;
    RelDomain.add_empty_columns m ch.dim

  let dim_add ch m = VectorMatrix.timing_wrap "dim add" (dim_add ch) m

  let dim_remove (ch: Apron.Dim.change) m del =
    if Array.length ch.dim = 0 || RelDomain.is_empty m then m else (
      Array.iteri (fun i x-> ch.dim.(i) <- x + i) ch.dim;
      let m' = if not del then let m = RelDomain.copy m in Array.fold_left (fun y x -> RelDomain.reduce_col_with y x; y) m ch.dim else m in
      RelDomain.remove_zero_rows @@ RelDomain.del_cols m' ch.dim)

  let dim_remove ch m del = VectorMatrix.timing_wrap "dim remove" (dim_remove ch m) del


  let change_d t new_env add del =
    if Environment.equal t.env new_env then t else
      let dim_change = if add then Environment.dimchange t.env new_env
        else Environment.dimchange new_env t.env
      in match t.d with
      | None -> bot_env
      | Some m -> {d = Some (if add then dim_add dim_change m else dim_remove dim_change m del); env = new_env}

  let change_d t new_env add del = VectorMatrix.timing_wrap "dimension change" (change_d t new_env add) del

  let vars x = Environment.ivars_only x.env
  let add_vars t vars =
    let t = copy t in
    let env' = Environment.add_vars t.env vars in
    change_d t env' true false

  let add_vars t vars = VectorMatrix.timing_wrap "add_vars" (add_vars t) vars

  let drop_vars t vars del =
    let t = copy t in
    let env' = Environment.remove_vars t.env vars in
    change_d t env' false del

  let drop_vars t vars = VectorMatrix.timing_wrap "drop_vars" (drop_vars t) vars

  let remove_vars t vars = drop_vars t vars false

  let remove_vars t vars = VectorMatrix.timing_wrap "remove_vars" (remove_vars t) vars

  let remove_vars_with t vars =
    let t' = remove_vars t vars in
    t.d <- t'.d;
    t.env <- t'.env

  let remove_filter t f =
    let env' = Environment.remove_filter t.env f in
    change_d t env' false false

  let remove_filter t f = VectorMatrix.timing_wrap "remove_filter" (remove_filter t) f

  let remove_filter_with t f =
    let t' = remove_filter t f in
    t.d <- t'.d;
    t.env <- t'.env

  let keep_filter t f =
    let t = copy t in
    let env' = Environment.keep_filter t.env f in
    change_d t env' false false

  let keep_filter t f = VectorMatrix.timing_wrap "keep_filter" (keep_filter t) f

  let keep_vars t vs =
    let t = copy t in
    let env' = Environment.keep_vars t.env vs in
    change_d t env' false false

  let keep_vars t vs = VectorMatrix.timing_wrap "keep_vars" (keep_vars t) vs

  let mem_var t var = Environment.mem_var t.env var

end



(** A more specific module type for RelationDomain.RelD2 with ConvBounds integrated and various apron elements.
    It is designed to be the interface for the D2 modules in affineEqualityDomain and apronDomain and serves as a functor argument for AssertionModule. *)
module type AssertionRelS =
sig
  type t
  module Bounds: ConvBounds with type t = t

  val is_bot_env: t -> bool

  val env: t -> Environment.t

  val assert_cons: t -> exp -> bool -> bool Lazy.t -> t
end

module Tracked: RelationDomain.Tracked =
struct
  let is_pthread_int_type = function
    | TNamed ({tname = ("pthread_t" | "pthread_key_t" | "pthread_once_t" | "pthread_spinlock_t"); _}, _) -> true (* on Linux these pthread types are integral *)
    | _ -> false

  let type_tracked typ =
    isIntegralType typ && not (is_pthread_int_type typ)

  let varinfo_tracked vi =
    (* no vglob check here, because globals are allowed in relation, but just have to be handled separately *)
    let hasTrackAttribute = List.exists (fun (Attr(s,_)) -> s = "goblint_relation_track") in
    type_tracked vi.vtype && (not @@ GobConfig.get_bool "annotation.goblint_relation_track" || hasTrackAttribute vi.vattr)
end

module AssertionModule (V: SV) (AD: AssertionRelS) =
struct
  include AD
  type nonrec var = V.t
  module Tracked = Tracked

  module Convert = Convert (V) (Bounds) (struct let allow_global = false end) (Tracked)

  let rec exp_is_cons = function
    (* constraint *)
    | BinOp ((Lt | Gt | Le | Ge | Eq | Ne), _, _, _) -> true
    | BinOp ((LAnd | LOr), e1, e2, _) -> exp_is_cons e1 && exp_is_cons e2
    | UnOp (LNot,e,_) -> exp_is_cons e
    (* expression *)
    | _ -> false

  (* TODO: move logic-handling assert_cons from Apron back to here, after fixing affeq bot-bot join *)

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

  let check_assert d e no_ov =
    if is_bot_env (assert_inv d e false no_ov) then
      `False
    else if is_bot_env (assert_inv d e true no_ov) then
      `True
    else
      `Top

  (** Evaluate non-constraint expression as interval. *)
  let eval_interval_expr d e =
    match Convert.texpr1_of_cil_exp d (env d) e false with (* why implicit false no_ov false here? *)
    | texpr1 ->
      Bounds.bound_texpr d texpr1
    | exception Convert.Unsupported_CilExp _ ->
      (None, None)

  (** Evaluate constraint or non-constraint expression as integer. *)
  let eval_int d e no_ov =
    let module ID = Queries.ID in
    match Cilfacade.get_ikind_exp e with
    | exception Cilfacade.TypeOfError _
    | exception Invalid_argument _ ->
      ID.top () (* real top, not a top of any ikind because we don't even know the ikind *)
    | ik ->
      if M.tracing then M.trace "relation" "eval_int: exp_is_cons %a = %B\n" d_plainexp e (exp_is_cons e);
      if exp_is_cons e then
        match check_assert d e no_ov with
        | `True -> ID.of_bool ik true
        | `False -> ID.of_bool ik false
        | `Top -> ID.top_of ik
      else
        match eval_interval_expr d e with
        | (Some min, Some max) -> ID.of_interval ~suppress_ovwarn:true ik (min, max)
        | (Some min, None) -> ID.starting ~suppress_ovwarn:true ik min
        | (None, Some max) -> ID.ending ~suppress_ovwarn:true ik max
        | (None, None) -> ID.top_of ik

end

(* Multi-precision rational numbers, defined by Apron.
   Used by affineEqualityDomain and linearTwoVarEqualityDomain *)
module Mpqf = struct 
  include Mpqf
  let compare = cmp
  let zero = of_int 0
  let one = of_int 1
  let mone = of_int (-1)

  let get_den x = Z_mlgmpidl.z_of_mpzf @@ Mpqf.get_den x

  let get_num x = Z_mlgmpidl.z_of_mpzf @@ Mpqf.get_num x
  let hash x = 31 * (Z.hash (get_den x)) + Z.hash (get_num x)
end
