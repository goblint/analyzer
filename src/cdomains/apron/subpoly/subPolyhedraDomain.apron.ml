(** OCaml implementation of the subpolyhedra domain.

    @see <https://www.microsoft.com/en-us/research/wp-content/uploads/2011/06/subpolyhedra.pdf>  Subpolyhedra. *)

open Batteries
open GoblintCil
open Pretty
module M = Messages
open GobApron
open SubPolyhedraCore

module Mpqf = SharedFunctions.Mpqf
module RationalInterval = Rationalinterval.RationalInterval

(** Variable
 * type t, basically ordered and printable
*)
module type Var = sig
  type t [@@deriving hash]
  val compare : t -> t -> int
  val string_of : t -> string
end

module VarManagement = struct
  module Int = struct
    type t = int
    let equal = Int.equal
    let compare = Int.compare
    let string_of = Int.to_string
    let hash = Hashtbl.hash
    let to_int = identity
    let to_t = identity
  end
  module SubPolyDomain = SubPoly(Int)(RationalInterval)
  include SharedFunctions.VarManagementOps (SubPolyDomain)

  let dim_add = SubPolyDomain.dim_add
  (*potentially add dim_remove here, not sure though*)  
end

module Linexpr_managment = struct
  include VarManagement
  include RatOps.ConvenienceOps (Mpqf)

  module V = RelationDomain.V
  module CoeffVector = VarManagement.SubPolyDomain.CoeffVector
  type linexpr = CoeffVector.t

  (* Adapted version of Leonie's Texpr parsing from her SparseOctagon domain. Instead of monomials we now use coeff vector. *)
  let mpqf_of_scalar (x: Scalar.t) =
    match x with
    | Float f -> Mpqf.of_float f
    | Mpqf q -> q
    | Mpfrf m -> Mpfr.to_mpq m

  (** [to_constant_opt v] is [Some c] iff [v] has no variable coefficients, i.e. it
     represents just the constant [c] (the first non-zero entry is the last slot). *)
  let to_constant_opt (v: linexpr) : Mpqf.t option =
    match CoeffVector.find_first_non_zero v with
    | None -> Some Mpqf.zero
    | Some (i, value) when i = CoeffVector.length v - 1 -> Some value
    | _ -> None

  let mpqf_of_z z = Mpqf.of_mpz @@ Z_mlgmpidl.mpzf_of_z z

  (** [gcd_list v] gcd of coeffvec. Gcd with all coefficients. *)
  let gcd_list (v: linexpr) : Z.t =
    (* fold Z.gcd over the numerators of every stored (non-zero) coefficient *)
    let gcd =
      CoeffVector.to_sparse_list v
      |> List.fold_left (fun acc (_, c) -> Z.gcd acc (Mpqf.get_num c)) Z.zero
    in
    (* an all-zero vector has gcd 0, so fall back to 1 to make dividing a no-op *)
    if Z.equal gcd Z.zero then Z.one else gcd

  (** [normalize_info v] takes a. linear expression, calculates gcd, then proceeds
      to normalize expression with gcd and sign normalization. 
      Returns ([normalized_expr] * [factor])*)
  let normalize_info (v: linexpr) : linexpr * Z.t =
    let gcd = gcd_list v in
    (* sign normalization, flip so the leading (lowest-index) coefficient is positive *)
    let sign = match CoeffVector.find_first_non_zero v with
      | Some (_, leading) when leading <: Mpqf.zero -> Z.minus_one
      | _ -> Z.one
    in
    (* the factor we divide out carries both the magnitude (gcd) and the sign *)
    let sign_factor = Z.mul sign gcd in
    let factor_mpqf = mpqf_of_z sign_factor in
    (* divide every (non-zero) coefficient, zeros are left untouched *)
    CoeffVector.map_f_preserves_zero (fun c -> c /: factor_mpqf) v, sign_factor

  let negate v = CoeffVector.map_f_preserves_zero Mpqf.neg v

  (* if one of them is a constant, then multiply. Otherwise, the expression is not linear, return None *)
(** [multiply], multiplies two [linexpr]s. Return s Some [value] iff. exactly one of the two [linexpr]s is a constant.*)
  let multiply (a : linexpr) (b : linexpr) =
    match to_constant_opt a, to_constant_opt b with
    | _, Some c -> Some (CoeffVector.map_f_preserves_zero (fun x -> c *: x) a)
    | Some c, _ -> Some (CoeffVector.map_f_preserves_zero (fun x -> c *: x) b)
    | _ -> None

    (** [get_coeff_vec], get a coeffvec from a linexpr.  *)
  let get_coeff_vec (t: t) (texp : Texpr1.expr) : linexpr option =
    let open Apron.Texpr1 in
    let exception NotLinearExpr in
    let num_slacks = match t.d with Some d -> SubPolyDomain.num_slacks d | None -> 0 in
    let zero_vec = CoeffVector.zero_vec (Environment.size t.env + num_slacks + 1) in
    let const_idx = CoeffVector.length zero_vec - 1 in
    let rec convert_texpr texp =
      begin match texp with
        | Cst (Interval _) -> 
          (* interval constants are not supported *)
          raise NotLinearExpr
        | Cst (Scalar x) ->
          (* convert the scalar to an Mpqf *)
          let c = mpqf_of_scalar x in
          CoeffVector.set_nth zero_vec const_idx c
        | Var x -> CoeffVector.set_nth zero_vec (Environment.dim_of_var t.env x) Mpqf.one
        | Unop  (Neg,  e, _, _) -> negate (convert_texpr e)
        | Unop  (Cast, e, _, _) -> convert_texpr e
        | Binop (Add, e1, e2, _, _) -> CoeffVector.map2_f_preserves_zero (+:) (convert_texpr e1) (convert_texpr e2)
        | Binop (Sub, e1, e2, _, _) -> CoeffVector.map2_f_preserves_zero (+:) (convert_texpr e1) (negate (convert_texpr e2))
        | Binop (Mul, e1, e2, _, _) -> 
          begin match multiply (convert_texpr e1) (convert_texpr e2) with
            | Some v -> v
            | None -> raise NotLinearExpr end
        | Binop (Div, e1, e2, _, _) ->
          let v1 = convert_texpr e1 in
          begin match to_constant_opt (convert_texpr e2) with
            | Some c when not (c =: Mpqf.zero) -> CoeffVector.map_f_preserves_zero (fun x -> x /: c) v1
            | _ -> raise NotLinearExpr end
        | _  -> raise NotLinearExpr end
    in match convert_texpr texp with
    | exception NotLinearExpr -> None
    | x -> Some(x)
end

module Slack_managment = struct
  include Linexpr_managment
  include RatOps.ConvenienceOps (Mpqf)

  (** [is_slack t col] is [true] iff column [col] is a slack column. *)
  let is_slack (t: t) (col: int) : bool =
    (* Get the interval map from the domain, false if domain option is None *)
    match t.d with
    | None -> false
    | Some d -> SubPolyDomain.mem_intv col d

  (** [fold_slacks f t acc] folds [f col interval info] over every slack,
      where [info] is the slack's linear definition over the real variables*)
  let fold_slacks (f: int -> RationalInterval.t -> SubPolyDomain.info option -> 'a -> 'a) (t: t) (acc: 'a) : 'a =
    match t.d with
    | None -> acc
    | Some d ->
      SubPolyDomain.VarMap.fold (fun col iv acc ->
          f col iv (SubPolyDomain.VarMap.find_opt col d.infos) acc
        ) d.intervals acc

  (** [add_slack_constraint t linexpr interval] introduces a fresh slack [s = linexpr]
      constrained to [interval]. *)
  let add_slack_constraint (t: t) (linexpr: linexpr) (interval: RationalInterval.t) : t =
    if is_bot_env t then t
    else
      match t.d with
      | None -> t
      | Some d ->
        (* normalize expr and then insert when adding slacks *)
        let normalized, sign_factor = normalize_info linexpr in
        (* Tweak interval *)
        let interval = RationalInterval.scale (Mpqf.one /: mpqf_of_z sign_factor) interval in
        (* the new slack goes at column n+m = the current constant-column index *)
        let slack_col = Environment.size t.env + SubPolyDomain.num_slacks d in
        { t with d = Some (SubPolyDomain.insert_slack slack_col normalized interval d) }
end

module ExpressionBounds: (SharedFunctions.ConvBounds with type t = VarManagement.t) = struct
  include Linexpr_managment

  (* TODO: this only answers for constant expressions (min = max = c). Once the lattice ops are implemented we can get more advanced  *)
  (* Again inspired by the LTVE and Affeq implementations *)
  let bound_texpr t texpr =
    match Option.bind (get_coeff_vec t (Texpr1.to_expr texpr)) to_constant_opt with
    (* get den just chekcs denominator = 1, if so we have whole number and we just return that as bound, else npothing *)
    | Some c when Z.equal (Mpqf.get_den c) Z.one ->
      let n = Mpqf.get_num c in
      Some n, Some n
    | _ -> None, None
end

module D =
struct
  include Printable.Std
  include RatOps.ConvenienceOps (Mpqf)
  include VarManagement
  include Linexpr_managment
  include Slack_managment

  module Bounds = ExpressionBounds
  module V = RelationDomain.V
  module Arg = struct
    let allow_global = true
  end
  module Convert = SharedFunctions.Convert (V) (Bounds) (Arg) (SharedFunctions.Tracked)

  let name () = "subpoly"

  let to_yojson _ = failwith "doesn't exist"

    (* pretty printing *)
  let show (t: t) =
    let env = Environment.show t.env in
    match t.d with
    | None -> "\tBot env = " ^ env ^ "\n"
    | Some d -> SubPolyDomain.string_of d ^ "; env = " ^ env
  
    let pretty () (x: t) = text (show x)
  
  let pretty_diff () ((x, y): t * t) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  
    let printXml (f: _ BatInnerIO.output) (x: t) = BatPrintf.fprintf f "<value>\n%s</value>\n" (XmlUtil.escape (show x))

  (* basic lattice handling *)
  let top () = { d = Some (SubPolyDomain.empty ()); env = empty_env }
  let is_top (t: t) = GobOption.exists SubPolyDomain.is_empty t.d
  let is_bot = is_bot_env


  (* fixpoint iteration handling *)
  (* here we wire up the things from Core *)
  let meet _a _b = failwith "SubPolyhedraDomain.meet: not implemented"
  let leq _a _b = failwith "SubPolyhedraDomain.leq: not implemented"
  let join _a _b = failwith "SubPolyhedraDomain.join: not implemented"
  let widen _a _b = failwith "SubPolyhedraDomain.widen: not implemented"
  let narrow _a _b = failwith "SubPolyhedraDomain.narrow: not implemented"
  let unify _a _b = failwith "SubPolyhedraDomain.unify: not implemented"

  (* transfer functions *)

  (**************
    Removes all rows in the affeq Matrix containing the vars, removes the corresponding entry in the 
  **************)
  let forget_vars t vars =
    if vars = [] || is_bot t || is_top t then t
    else 
      let d = Option.get t.d in 
      let dims = List.map (Environment.dim_of_var t.env) vars in (*map of vars in Env. to dimensions in matrix.*)
      {t with d = Some (SubPolyDomain.forget_vars dims d)}
  
  let forget_var (t: t) (v: V.t) = forget_vars t [v]
  
  let assign_exp _ask _t _var _exp _ = failwith "SubPolyhedraDomain.assign_exp: not implemented"
  let assign_var _t _v _v' = failwith "SubPolyhedraDomain.assign_var: not implemented"
  let assign_var_parallel _t _vvs = failwith "SubPolyhedraDomain.assign_var_parallel: not implemented"
  let assign_var_parallel_with _t _vvs = failwith "SubPolyhedraDomain.assign_var_parallel_with: not implemented"
  let assign_var_parallel' _t _vvs = failwith "SubPolyhedraDomain.assign_var_parallel': not implemented"
  let substitute_exp _ask _t _var _exp _no_ov = failwith "SubPolyhedraDomain.substitute_exp: not implemented"

  let cil_exp_of_lincons1 = Convert.cil_exp_of_lincons1

  (* tried to adapt something from LTVE, dont quite get what is, to my understandinf its checking if some expr holds.
    We either have true, go through control flow with new constraint, or false and go with negated? *)
  let meet_tcons _ask (t: t) tcons1 _e _no_ov =
    if is_bot_env t then t
    else
      match get_coeff_vec t (Texpr1.to_expr @@ Tcons1.get_texpr1 tcons1) with
      | None -> t (* non-linear expression: no information gained *)
      | Some v ->
        begin match to_constant_opt v, Tcons1.get_typ tcons1 with
          (* expr collapses to a constant c: immediate feasibility check *)
          | Some c, EQ    -> if c <>: Mpqf.zero then bot_env else t
          | Some c, SUPEQ -> if c <:  Mpqf.zero then bot_env else t
          | Some c, SUP   -> if c <=: Mpqf.zero then bot_env else t
          | Some c, DISEQ -> if c =:  Mpqf.zero then bot_env else t
          (* expr has variables: record it (inconsistency caught later, at rref) *)
          | None, EQ            -> { t with d = Some (SubPolyDomain.add_affeq_row v (Option.get t.d)) }
          | None, (SUPEQ | SUP) -> add_slack_constraint t v (RationalInterval.of_bounds ~lower:(Some Mpqf.zero) ~upper:None)
          | _ -> t (* DISEQ / EQMOD over variables: not representable, give up (sound) *)
        end

  (* Module AssertionRels demands: *)
  (* cehck if constraints hold. Copied from LTVE *)
    let assert_constraint ask d e negate (no_ov: bool Lazy.t) =
    match Convert.tcons1_of_cil_exp ask d d.env e negate no_ov with
    | tcons1 -> meet_tcons ask d tcons1 e no_ov
    | exception Convert.Unsupported_CilExp _ -> d

  let env t = t.env
  let eval_interval _ask = Bounds.bound_texpr
  let invariant _t = failwith "SubPolyhedraDomain.invariant:   not implemented"

  type marshal = t
  (* marshal is not compatible with apron, therefore we don't have to implement it *)
  let marshal t = t
  let unmarshal t = t
  let relift t = t
end

module D2: RelationDomain.RD with type var = Var.t =
struct
  module D = D
  module ConvArg = struct
    let allow_global = false
  end
  include SharedFunctions.AssertionModule (D.V) (D) (ConvArg)
  include D
end
