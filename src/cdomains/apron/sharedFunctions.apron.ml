(** Relational value domain utilities. *)
open GoblintCil

open GobApron


module M = Messages

let int_of_scalar ?(scalewith=Z.one) ?round (scalar: Scalar.t) =
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
        | None when Stdlib.Float.is_integer f -> Some f
        | None -> None
      in
      Z.(of_float f * scalewith)
    | Mpqf scalar -> (* octMPQ, boxMPQ, polkaMPQ *)
      let n = Mpqf.get_num scalar in
      let d = Mpqf.get_den scalar in
      let scale = Z_mlgmpidl.mpz_of_z scalewith in
      let+ z =
        if Mpzf.cmp_int d 1 = 0 then (* exact integer (denominator 1) *)
          Some (Mpzf.mul scale n)
        else
          begin match round with
            | Some `Floor -> Some (Mpzf.mul scale (Mpzf.fdiv_q n d)) (* floor division *)
            | Some `Ceil ->  Some (Mpzf.mul scale (Mpzf.cdiv_q n d)) (* ceiling division *)
            | None -> let product = Mpzf.mul scale n in if Mpz.divisible_p product d then
                Some (Mpzf.divexact product d) (* scale, preferably with common denominator *)
              else None
          end
      in
      Z_mlgmpidl.z_of_mpzf z
    | _ ->
      failwith ("int_of_scalar: unsupported: " ^ Scalar.show scalar)


module type ConvertArg =
sig
  val allow_global: bool
end

module type SV =  RelationDomain.RV with type t = Var.t

type unsupported_cilExp =
  | Var_not_found of CilType.Varinfo.t (** Variable not found in Apron environment. *)
  | Cast_not_injective of CilType.Typ.t (** Cast is not injective, i.e. may under-/overflow. *)
  | Exp_not_supported of exp[@printer fun ppf e -> Format.pp_print_string ppf (GobPretty.show (Cil.d_plainexp () e))]
  | Overflow (** May overflow according to Apron bounds. *)
  | Exp_typeOf of exn [@printer fun ppf e -> Format.pp_print_string ppf (Printexc.to_string e)] (** Expression type could not be determined. *)
  | BinOp_not_supported of binop [@printer fun ppf op -> Format.pp_print_string ppf (match op with
        | BAnd | BOr | BXor -> "Bitwise binop"
        | Shiftlt | Shiftrt -> "Shift binop"
        | PlusPI | MinusPI | IndexPI | MinusPP -> "Pointer binop"
        | LAnd | LOr -> "Logical binop"
        | Lt | Gt | Le | Ge | Eq | Ne -> "Comparison binop"
        | _ -> "other binop")](** BinOp constructor not supported. *)
  | Ikind_non_integer of string (** Exception during trying to get ikind of a non-integer typed expression *)
[@@deriving show { with_path = false }]

(** Interface for Bounds which calculates bounds for expressions and is used inside the - Convert module. *)
module type ConvBounds =
sig
  type t
  val bound_texpr: t -> Texpr1.t -> Z.t option * Z.t option
end

(** Conversion from CIL expressions to Apron.
    This is used by the domains "affine equalities" and "linear two variable equalities".
    It also handles the overflow through the flag "no_ov".
    For this reason it was divided from the Convert module for the pure apron domains "ApronOfCilForApronDomains",
*)
module ApronOfCil (V: SV) (Bounds: ConvBounds) (Arg: ConvertArg) (Tracked: RelationDomain.Tracked) =
struct
  open Texpr1
  open Tcons1

  exception Unsupported_CilExp of unsupported_cilExp

  let () = Printexc.register_printer (function
      | Unsupported_CilExp reason -> Some (show_unsupported_cilExp reason)
      | _ -> None (* for other exception *)
    )

  (** This still tries to establish bounds via Bounds.bound_texpr, which may be more precise in case ana.int.interval
      is disabled and the relational analysis manages to evaluate a value to an interval, which can then not be represented
      as the result of an EvalInt query. This is a workaround and works as long as only one relational domain is used.
      With multiple domains and disabled interval domain, the queries will not be able to exchange interval information,
      and each analysis will only be able to establish constant bounds, but only its own interval bounds and not interval bounds
      established by other analyses.*)
  let overflow_handling no_ov ik env expr d exp =
    match Cilfacade.get_ikind_exp exp with
    | exception Invalid_argument a ->  raise (Unsupported_CilExp (Ikind_non_integer a))       (* expression is not an integer expression, i.e. float *)
    | ik ->
      if IntDomain.should_wrap ik || not (Lazy.force no_ov) then (
        let (type_min, type_max) = IntDomain.Size.range ik in
        let texpr1 = Texpr1.of_expr env expr in
        match Bounds.bound_texpr d texpr1 with
        | Some min, Some max when Z.compare type_min min <= 0 && Z.compare max type_max <= 0 ->
          ()
        | min_opt, max_opt ->
          if M.tracing then M.trace "apron" "may overflow: %a (%a, %a)" CilType.Exp.pretty exp (Pretty.docOpt (IntOps.BigIntOps.pretty ())) min_opt (Pretty.docOpt (IntOps.BigIntOps.pretty ())) max_opt;
          raise (Unsupported_CilExp Overflow)
      )

  let texpr1_expr_of_cil_exp (ask:Queries.ask) d env exp no_ov =
    let conv exp  =
      let query e ik =
        let res =
          match ask.f (EvalInt e) with
          | `Bot  (* This happens when called on a pointer type; -> we can safely return top *)
          | `Top -> IntDomain.IntDomTuple.top_of ik
          | `Lifted x -> x (* Cast should be unnecessary because it should be taken care of by EvalInt. *) in
        if M.tracing then M.trace "relation-query" "texpr1_expr_of_cil_exp/query: %a -> %a" d_plainexp e IntDomain.IntDomTuple.pretty res;
        res
      in
      (* recurse without env and ask arguments *)
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
          Cst (Coeff.s_of_z i)
        | exp ->
          match Cilfacade.get_ikind_exp exp with
          | ik ->
            let expr =
              (** simplify asks for a constant value of some subexpression e, similar to a constant fold. In particular but not exclusively
                  this query is answered by the 2 var equalities domain itself. This normalizes arbitrary expressions to a point where they
                  might be able to be represented by means of 2 var equalities

                  This simplification happens during a time, when there are temporary variables a#in and a#out part of the expression,
                  but are not represented in the man, thus queries may result in top for these variables. Wrapping this in speculative
                  mode is a stop-gap measure to avoid flagging overflows. We however should address simplification in a more generally useful way.
                  outside of the apron-related expression conversion.
              *)
              let simplify e =
                GobRef.wrap AnalysisState.executing_speculative_computations true @@ fun () ->
                let ikind = try (Cilfacade.get_ikind_exp e) with Invalid_argument a -> raise (Unsupported_CilExp (Ikind_non_integer a))   in
                let simp = query e ikind in
                let const = IntDomain.IntDomTuple.to_int @@ IntDomain.IntDomTuple.cast_to ikind simp in
                if M.tracing then M.trace "relation" "texpr1_expr_of_cil_exp/simplify: %a -> %a" d_plainexp e IntDomain.IntDomTuple.pretty simp;
                BatOption.map_default (fun c -> Const (CInt (c, ikind, None))) e const
              in
              let texpr1 e = texpr1_expr_of_cil_exp (simplify e) in
              let bop_near op e1 e2 =  Binop (op, texpr1 e1, texpr1 e2, Int, Near) in
              match exp with
              | UnOp (Neg, e, _) -> Unop (Neg, texpr1 e, Int, Near)
              | BinOp (PlusA, e1, e2, _) -> bop_near Add e1 e2
              | BinOp (MinusA, e1, e2, _) -> bop_near Sub e1 e2
              | BinOp (Mult, e1, e2, _) -> bop_near Mul e1 e2
              | BinOp (Mod, e1, e2, _) -> bop_near Mod e1 e2
              | BinOp (Div, e1, e2, _) ->
                Binop (Div, texpr1 e1, texpr1 e2, Int, Zero)
              | CastE (t, e) when Cil.isIntegralType t ->
                begin match  IntDomain.Size.is_cast_injective ~from_type:(Cilfacade.typeOf e) ~to_type:t with (* TODO: unnecessary cast check due to overflow check below? or maybe useful in general to also assume type bounds based on argument types? *)
                  | exception Invalid_argument a -> raise (Unsupported_CilExp (Ikind_non_integer a))
                  | true -> texpr1 e
                  | false -> (* Cast is not injective - we now try to establish suitable ranges manually  *)
                    let t_ik = Cilfacade.get_ikind t in
                    (* retrieving a valuerange for a non-injective cast works by a query to the value-domain with subsequent value extraction from domtuple - which should be speculative, since it is not program code *)
                    let const,res = GobRef.wrap AnalysisState.executing_speculative_computations true @@ fun () ->
                      (* try to evaluate e by EvalInt Query *)
                      let res = try (query e @@ Cilfacade.get_ikind_exp e) with Invalid_argument a -> raise (Unsupported_CilExp (Ikind_non_integer a))  in
                      (* convert response to a constant *)
                      IntDomain.IntDomTuple.to_int @@ IntDomain.IntDomTuple.cast_to t_ik res, res in
                    match const with
                    | Some c -> Cst (Coeff.s_of_z c) (* Got a constant value -> use it straight away *)
                    (* I gotten top, we can not guarantee injectivity *)
                    | None -> if IntDomain.IntDomTuple.is_top_of t_ik res then raise (Unsupported_CilExp (Cast_not_injective t))
                      else ( (* Got a ranged value different from top, so let's check bounds manually *)
                        let (ik_min, ik_max) = IntDomain.Size.range t_ik in
                        match IntDomain.IntDomTuple.minimal res, IntDomain.IntDomTuple.maximal res with
                        | Some min, Some max when min >= ik_min && max <= ik_max -> texpr1_expr_of_cil_exp e
                        | _ -> raise (Unsupported_CilExp (Cast_not_injective t)))
                    | exception Cilfacade.TypeOfError _ (* typeOf inner e, not outer exp *)
                    | exception Invalid_argument _ ->
                      raise (Unsupported_CilExp (Cast_not_injective t))
                end
              | BinOp (op, _,_,_) ->
                raise (Unsupported_CilExp (BinOp_not_supported op))
              | e ->
                raise (Unsupported_CilExp (Exp_not_supported e))
            in
            overflow_handling no_ov ik env expr d exp;
            expr
          | exception (Cilfacade.TypeOfError _ as e) ->
            raise (Unsupported_CilExp (Exp_typeOf e))
          | exception (Invalid_argument a) ->
            raise (Unsupported_CilExp (Ikind_non_integer a))
      in
      texpr1_expr_of_cil_exp exp
    in
    let exp = Cil.constFold false exp in
    if M.tracing then
      match conv exp with
      | exception Unsupported_CilExp ex ->
        M.tracel "rel-texpr-cil-conv" "unsuccessfull: %s" (show_unsupported_cilExp ex);
        raise (Unsupported_CilExp ex)
      | res ->
        M.trace "relation" "texpr1_expr_of_cil_exp: %a -> %a (%b)" d_plainexp exp Texpr1.Expr.pretty res (Lazy.force no_ov);
        M.tracel "rel-texpr-cil-conv" "successfull: Good";
        res
    else conv exp

  let texpr1_of_cil_exp ask d env e no_ov =
    let res = texpr1_expr_of_cil_exp ask d env e no_ov in
    Texpr1.of_expr env res

  let tcons1_of_cil_exp ask d env e negate no_ov =
    let e = Cil.constFold false e in
    let (texpr1_plus, texpr1_minus, typ) =
      match e with
      | BinOp (r, e1, e2, _) ->
        let texpr1_1 = texpr1_expr_of_cil_exp ask d env e1 no_ov in
        let texpr1_2 = texpr1_expr_of_cil_exp ask d env e2 no_ov in
        (* Apron constraints always compare with 0 and only have comparisons one way *)
        begin match r with
          | Lt -> (texpr1_2, texpr1_1, SUP)   (* e1 < e2   ==>  e2 - e1 > 0  *)
          | Gt -> (texpr1_1, texpr1_2, SUP)   (* e1 > e2   ==>  e1 - e2 > 0  *)
          | Le -> (texpr1_2, texpr1_1, SUPEQ) (* e1 <= e2  ==>  e2 - e1 >= 0 *)
          | Ge -> (texpr1_1, texpr1_2, SUPEQ) (* e1 >= e2  ==>  e1 - e2 >= 0 *)
          | Eq -> (texpr1_1, texpr1_2, EQ)    (* e1 == e2  ==>  e1 - e2 == 0 *)
          | Ne -> (texpr1_1, texpr1_2, DISEQ) (* e1 != e2  ==>  e1 - e2 != 0 *)
          | _ -> raise (Unsupported_CilExp (BinOp_not_supported r))
        end
      | _ -> raise (Unsupported_CilExp (Exp_not_supported e))
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

(** Conversion from Apron to CIL expressions. *)
module CilOfApron (V: SV) =
struct
  exception Unsupported_Linexpr1

  let longlong = TInt(ILongLong,[])


  (** Returned boolean indicates whether returned expression should be negated. *)
  let coeff_to_const ~scalewith (c:Coeff.union_5) =
    match c with
    | Scalar c ->
      (match int_of_scalar ?scalewith c with
       | Some i ->
         let ci,truncation = truncateCilint ILongLong i in
         if truncation = NoTruncation then
           if Z.compare i Z.zero >= 0 then
             false, Const (CInt(i,ILongLong,None))
           else
             (* attempt to negate if that does not cause an overflow *)
             let cneg, truncation = truncateCilint ILongLong (Z.neg i) in
             if truncation = NoTruncation then
               true, Const (CInt((Z.neg i),ILongLong,None))
             else
               false, Const (CInt(i,ILongLong,None))
         else
           (M.warn ~category:Analyzer "Invariant Apron: coefficient is not int: %a" Scalar.pretty c; raise Unsupported_Linexpr1)
       | None -> raise Unsupported_Linexpr1)
    | _ -> raise Unsupported_Linexpr1

  (** Returned boolean indicates whether returned expression should be negated. *)
  let cil_exp_of_linexpr1_term ~scalewith (c: Coeff.t) v =
    match V.to_cil_varinfo v with
    | Some vinfo when IntDomain.Size.is_cast_injective ~from_type:vinfo.vtype ~to_type:(TInt(ILongLong,[]))   ->
      let var = Cilfacade.mkCast ~e:(Lval(Var vinfo,NoOffset)) ~newt:longlong in
      let flip, coeff = coeff_to_const ~scalewith c in
      let prod = BinOp(Mult, coeff, var, longlong) in
      flip, prod
    | None ->
      M.warn ~category:Analyzer "Invariant Apron: cannot convert to cil var: %a" Var.pretty v;
      raise Unsupported_Linexpr1
    | _ ->
      M.warn ~category:Analyzer "Invariant Apron: cannot convert to cil var in overflow preserving manner: %a" Var.pretty v;
      raise Unsupported_Linexpr1

  (** Returned booleans indicates whether returned expressions should be negated. *)
  let cil_exp_of_linexpr1 ?scalewith (linexpr1:Linexpr1.t) =
    let terms = ref [coeff_to_const ~scalewith (Linexpr1.get_cst linexpr1)] in
    let append_summand (c:Coeff.union_5) v =
      if not (Coeff.is_zero c) then
        terms := cil_exp_of_linexpr1_term ~scalewith c v :: !terms
    in
    Linexpr1.iter append_summand linexpr1;
    !terms


  let lcm_den linexpr1 =
    let exception UnsupportedScalar
    in
    let frac_of_scalar scalar =
      if Scalar.is_infty scalar <> 0 then (* infinity means unbounded *)
        None
      else match scalar with
        | Float f -> if Stdlib.Float.is_integer f then Some (Q.of_float f) else None
        | Mpqf f -> Some (Z_mlgmpidl.q_of_mpqf f)
        | _ -> raise UnsupportedScalar
    in
    let extract_den (c:Coeff.union_5) =
      match c with
      | Scalar c -> BatOption.map Q.den (frac_of_scalar c)
      | _ -> None
    in
    let lcm_denom = ref (BatOption.default Z.one (extract_den (Linexpr1.get_cst linexpr1))) in
    let lcm_coeff (c:Coeff.union_5) _ =
      match (extract_den c) with
      | Some z -> lcm_denom := Z.lcm z !lcm_denom
      | _      -> ()
    in
    try
      Linexpr1.iter lcm_coeff linexpr1; !lcm_denom
    with UnsupportedScalar -> Z.one

  let cil_exp_of_lincons1 (lincons1:Lincons1.t) =
    let zero = Cil.kinteger ILongLong 0 in
    try
      let linexpr1 = Lincons1.get_linexpr1 lincons1 in
      let common_denominator = lcm_den linexpr1 in
      let terms = cil_exp_of_linexpr1 ~scalewith:common_denominator linexpr1 in
      let (nterms, pterms) = BatTuple.Tuple2.mapn (List.map snd) (List.partition fst terms) in (* partition terms into negative (nterms) and positive (pterms) *)
      let fold_terms terms =
        List.fold_left (fun acc term ->
            match acc with
            | None -> Some term
            | Some exp -> Some (BinOp (PlusA, exp, term, longlong))
          ) None terms
        |> BatOption.default zero
      in
      let lhs = fold_terms pterms in
      let rhs = fold_terms nterms in (* negative terms are moved from Apron's lhs to our rhs, so they all become positive there *)
      let binop =
        match Lincons1.get_typ lincons1 with
        | EQ -> Eq
        | SUPEQ -> Ge
        | SUP -> Gt
        | DISEQ -> Ne
        | EQMOD _ -> raise Unsupported_Linexpr1
      in
      Some (Cil.constFold false @@ BinOp(binop, lhs, rhs, TInt(IInt,[]))) (* constFold removes multiplication by factor 1 *)
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
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val copy : t -> t

  (** empty creates a domain representation of dimension 0 *)
  val empty : unit -> t

  (** is_empty is true, if the domain representation has a dimension size of zero *)
  val is_empty : t -> bool
  (** interpret dimension addition in change2.add semantics, see https://antoinemine.github.io/Apron/doc/api/ocaml/Dim.html*)
  val dim_add : Apron.Dim.change -> t -> t
  (** interpret dimension removal in change2.remove semantics, see https://antoinemine.github.io/Apron/doc/api/ocaml/Dim.html*)
  val dim_remove : Apron.Dim.change -> t -> t
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

  let get_env t = t.env
  let bot_env = {d = None; env = empty_env}

  let is_bot_env t = t.d = None

  let copy t = {t with d = Option.map RelDomain.copy t.d}

  let dimchange2_add t new_env =
    if Environment.equal t.env new_env then
      t
    else
      match t.d with
      | None -> bot_env
      | Some m ->
        let dim_change2 = Environment.dimchange2 t.env new_env in
        {
          d = Some (RelDomain.dim_add (Option.get dim_change2.add) m );
          env = new_env
        }

  let dimchange2_remove t new_env =
    if Environment.equal t.env new_env then
      t
    else
      match t.d with
      | None -> bot_env
      | Some m ->
        let dim_change2 = Environment.dimchange2 t.env new_env in
        {
          d = Some (RelDomain.dim_remove (Option.get dim_change2.remove) m);
          env = new_env
        }

  let vars x = Environment.ivars_only x.env

  let add_vars t vars =
    let t = copy t in
    let env' = Environment.add_vars t.env vars in
    dimchange2_add t env'

  let add_vars t vars = Vector.timing_wrap "add_vars" (add_vars t) vars

  let remove_vars t vars =
    let t = copy t in
    let env' = Environment.remove_vars t.env vars in
    dimchange2_remove t env'

  let remove_vars t vars = Vector.timing_wrap "remove_vars" (remove_vars t) vars

  let remove_vars_with t vars =
    let t' = remove_vars t vars in
    t.d <- t'.d;
    t.env <- t'.env

  let remove_filter t f =
    let env' = Environment.remove_filter t.env f in
    dimchange2_remove t env'

  let remove_filter t f = Vector.timing_wrap "remove_filter" (remove_filter t) f

  let remove_filter_with t f =
    let t' = remove_filter t f in
    t.d <- t'.d;
    t.env <- t'.env

  let keep_filter t f =
    let t = copy t in
    let env' = Environment.keep_filter t.env f in
    dimchange2_remove t env'

  let keep_filter t f = Vector.timing_wrap "keep_filter" (keep_filter t) f

  let keep_vars t vs =
    let t = copy t in
    let env' = Environment.keep_vars t.env vs in
    dimchange2_remove t env'

  let keep_vars t vs = Vector.timing_wrap "keep_vars" (keep_vars t) vs

  let mem_var t var = Environment.mem_var t.env var

end



(** A more specific module type for RelationDomain.RelD2 with various apron elements.
    It is designed to be the interface for the D2 modules in affineEqualityDomain and apronDomain and serves as a functor argument for AssertionModule. *)
module type AssertionRelS =
sig
  type t

  module Bounds: ConvBounds with type t = t
  val is_bot_env: t -> bool

  val env: t -> Environment.t

  val assert_constraint: Queries.ask -> t -> exp -> bool -> bool Lazy.t -> t
  val eval_interval : Queries.ask -> t -> Texpr1.t -> Z.t option * Z.t option
end

module Tracked = RelationCil.Tracked

module AssertionModule (V: SV) (AD: AssertionRelS) (Arg: ConvertArg) =
struct
  include AD
  type nonrec var = V.t
  module Tracked = Tracked
  module Convert = Convert (V) (Bounds) (Arg) (Tracked)

  let rec exp_is_constraint = function
    (* constraint *)
    | BinOp ((Lt | Gt | Le | Ge | Eq | Ne), _, _, _) -> true
    | BinOp ((LAnd | LOr), e1, e2, _) -> exp_is_constraint e1 && exp_is_constraint e2
    | UnOp (LNot,e,_) -> exp_is_constraint e
    (* expression *)
    | _ -> false

  (* TODO: move logic-handling assert_constraint from Apron back to here, after fixing affeq bot-bot join *)

  (** Assert any expression. *)
  let assert_inv ask d e negate no_ov =
    let e' =
      if exp_is_constraint e then
        e
      else
        (* convert non-constraint expression, such that we assert(e != 0) *)
        BinOp (Ne, e, zero, intType)
    in
    assert_constraint ask d e' negate no_ov

  let check_assert ask d e no_ov =
    if is_bot_env (assert_inv ask d e false no_ov) then
      `False
    else if is_bot_env (assert_inv ask d e true no_ov) then
      `True
    else
      `Top

  (** Evaluate non-constraint expression as interval. *)
  let eval_interval_expr ask d e no_ov =
    match Convert.texpr1_of_cil_exp ask d (env d) e no_ov with
    | texpr1 ->
      eval_interval ask d texpr1
    | exception Convert.Unsupported_CilExp _ ->
      (None, None)

  (** Evaluate constraint or non-constraint expression as integer. *)
  let eval_int ask d e no_ov =
    let module ID = Queries.ID in
    match Cilfacade.get_ikind_exp e with
    | exception Cilfacade.TypeOfError _
    | exception Invalid_argument _ ->
      ID.top () (* real top, not a top of any ikind because we don't even know the ikind *)
    | ik ->
      if M.tracing then M.trace "relation" "eval_int: exp_is_constraint %a = %B" d_plainexp e (exp_is_constraint e);
      if exp_is_constraint e then
        match check_assert ask d e no_ov with
        | `True -> ID.of_bool ik true
        | `False -> ID.of_bool ik false
        | `Top -> ID.top_of ik
      else
        match eval_interval_expr ask d e no_ov with
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
