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

module VarManagement =
struct
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
  let size _t = failwith "SubPolyhedraDomain.size: not   implemented"

   (* aus LTVE, aber überarbeitet. *)
  (** Parses a Texpr to obtain a (coefficient, variable) pair list to repr. a sum of a variables that have a coefficient. If variable is None, the coefficient represents a constant offset. *)
  let monomials_from_texp (t: t) texp =
    let open Apron.Texpr1 in
    let exception NotLinearExpr in 
    let exception ScalarIsInfinity in 
    let negate coeff_var_list =
      List.map (fun (monom, offs) -> Z.(BatOption.map (fun (coeff, i) -> (neg coeff, i)) monom, neg offs)) coeff_var_list
    in
    let canonicalize (v,o,d) =
      let gcd = Z.gcd o d in (* gcd of coefficients *)
      let gcd = Option.map_default (fun (c,_) -> Z.gcd c gcd) gcd v in (* include monomial in gcd computation *)
      let commondivisor = if Z.(lt d zero) then Z.neg gcd else gcd in (* canonical form dictates d being positive *)
      (BatOption.map (fun (coeff,i) -> (Z.div coeff commondivisor,i)) v, Z.div o commondivisor, Z.div d commondivisor)
    in
    let multiply_with_Q dividend coeff_var_list = 
      List.map (fun (monom, offs) ->
        let (v, o, d) = canonicalize Z.(BatOption.map (fun (coeff,i) -> (dividend*coeff,i)) monom, dividend*offs, one) in
        (v, o)
      ) coeff_var_list
    in
    let multiply a b = (* if one of them is a constant, then multiply. Otherwise, the expression is not linear *)
      match a, b with
      | [(None,coeff)], c
      | c, [(None,coeff)] -> multiply_with_Q coeff c
      | _ -> raise NotLinearExpr
    in 
    let rec convert_texpr texp =
      begin match texp with
        | Cst (Interval _) -> failwith "constant was an interval; this is not supported"
        | Cst (Scalar x) -> 
          begin match SharedFunctions.int_of_scalar ?round:None x with
            | Some x -> [(None, x)]
            | None -> raise ScalarIsInfinity end (* bedeutet, dass es keine exakte ganze zahl ist *)
        | Var x -> 
          let var_dim = Environment.dim_of_var t.env x in
          [(Some (Z.one, var_dim), Z.zero)] 
        | Unop  (Neg,  e, _, _) -> negate (convert_texpr e)
        | Unop  (Cast, e, _, _) -> convert_texpr e 
        | Binop (Add, e1, e2, _, _) -> convert_texpr e1 @ convert_texpr e2
        | Binop (Sub, e1, e2, _, _) -> convert_texpr e1 @ negate (convert_texpr e2)
        | Binop (Mul, e1, e2, _, _ ) -> multiply (convert_texpr e1) (convert_texpr e2) 
        | Binop (Div, e1, e2, _, _ ) -> failwith "todo: brauchen wir diesen fall?"
        | _  -> raise NotLinearExpr end
    in match convert_texpr texp with
    | exception NotLinearExpr -> None
    | exception ScalarIsInfinity -> None
    | x -> Some(x)

  (* aus LTVE, aber überarbeitet. *)
  (** convert and simplify a texpr into a list of monomials (coeff,varidx) and a constant offset *)
  let simplified_monomials_from_texp (t: t) texp =
    BatOption.bind (monomials_from_texp t texp) (* wenn None, dann return None, sonst so weitermachen: *)
      (fun monomiallist ->
        let module IMap = Map.Make(Int) in
        let accumulate_constants (exprcache, aconst) (v, offs) =
           let constant = Z.add aconst offs in
           match v with
           | None -> (exprcache, constant)
           | Some (coeff, idx) ->
             let old_coeff = BatOption.default Z.zero (IMap.find_opt idx exprcache) in
             let new_coeff = Z.add old_coeff coeff in
             let newcache =
               if Z.equal new_coeff Z.zero then IMap.remove idx exprcache
               else IMap.add idx new_coeff exprcache
             in (newcache, constant)
        in 
        let (expr, constant) = List.fold_left accumulate_constants (IMap.empty, Z.zero) monomiallist in
        Some (IMap.fold (fun v c acc -> if Z.equal c Z.zero then acc else (c, v) :: acc) expr [], constant))

end

module ExpressionBounds: (SharedFunctions.ConvBounds with type t = VarManagement.t) =
struct
  include VarManagement
  let bound_texpr _t _texpr = failwith "SubPolyhedraDomain.bound_texpr: not   implemented"
end


(* The way we model linexpr for slacks as discussed in the meeting. We use a vector from sparseimplementation of affeq. 
(i think this is the one we were meant to use) *)

(* When changing from coeffvector to Var.t * Mpqf.t ai wrote the add_term and add_linexpr functions. *)
(* it also touched linexpr of exp *)
(*
  QUESTION: why do we not use sparse Vector implementation for Linexpr_management?
  Not sure if this needs to be changed to work with the new representation of a polyhedron.
*)

module Linexpr_managment = struct
  include RatOps.ConvenienceOps (Mpqf)

  module V = RelationDomain.V
  type linexpr = {
    terms: (Var.t * Mpqf.t) list;
    const: Mpqf.t; (*QUESTION: here again, are the constants not in the slack var anyways?*)
  }

  (* Helper functionts for linexpr management, specifically for linexpr of c expr *)
  let mpqf_of_z (z: Z.t) =
    Mpqf.of_mpz @@ Z_mlgmpidl.mpzf_of_z z

  let const_mpqf_of_exp (exp: exp) = match exp with
    | Const (CInt (i, _, _)) -> Some (mpqf_of_z i)
    | _ -> None

  let option_map2 (f: 'a -> 'b -> 'c) (a: 'a option) (b: 'b option) =
    match a, b with
    | Some a, Some b -> Some (f a b)
    | _ -> None

  let zero_linexpr = { terms = []; const = Mpqf.zero }

  let add_term var coeff terms =
    let rec add_term_aux acc = function
      | [] ->
        if coeff =: Mpqf.zero then List.rev acc
        else List.rev ((var, coeff) :: acc)
      | (v, c) :: rest when Var.equal v var ->
        let c' = c +: coeff in
        if c' =: Mpqf.zero then List.rev_append acc rest
        else List.rev_append acc ((v, c') :: rest)
      | term :: rest ->
        add_term_aux (term :: acc) rest
    in
    add_term_aux [] terms

  let add_linexpr (a: linexpr) (b: linexpr) =
    {
      terms = List.fold_left (fun terms (var, coeff) ->
          add_term var coeff terms
        ) a.terms b.terms;
      const = a.const +: b.const;
    }

  let neg_linexpr (a: linexpr) =
    { terms = List.map (fun (var, coeff) -> var, Mpqf.neg coeff) a.terms; const = Mpqf.neg a.const }

  let scale_linexpr (c: Mpqf.t) (a: linexpr) =
    {
      terms = List.map (fun (var, coeff) -> var, c *: coeff) a.terms;
      const = c *: a.const;
    }

  let sub_linexpr (a: linexpr) (b: linexpr) =
    add_linexpr a (neg_linexpr b)

  (* Special functions for linexpr of exp management *)
  (* When parsing a C exp like 5x + y, we need a way to convert that to linexpr *)
  let rec linexpr_of_exp (exp: exp) = match exp with
  | Const (CInt (i, _, _)) ->
    Some { zero_linexpr with const = mpqf_of_z i }
  | Lval (Var v, NoOffset) ->
    Some { zero_linexpr with terms = [V.local v, Mpqf.one] }
  | UnOp (Neg, e, _) -> Option.map neg_linexpr (linexpr_of_exp e)
  | CastE (_, _, e) -> linexpr_of_exp e
  | BinOp (PlusA, e1, e2, _) ->
    option_map2 add_linexpr (linexpr_of_exp e1) (linexpr_of_exp e2)
  | BinOp (MinusA, e1, e2, _) ->
    option_map2 sub_linexpr (linexpr_of_exp e1) (linexpr_of_exp e2)
  | BinOp (Mult, e1, e2, _) ->
    begin match const_mpqf_of_exp e1, const_mpqf_of_exp e2 with
      | Some c, _ -> Option.map (scale_linexpr c) (linexpr_of_exp e2)
      | _, Some c -> Option.map (scale_linexpr c) (linexpr_of_exp e1)
      | _ -> None
    end
  | _ -> None

  let linexpr_var_count (linexpr: linexpr) =
    List.length linexpr.terms

    let normalize_linexpr (linexpr: linexpr) : (linexpr * Mpqf.t) option =
      match linexpr.terms with
      | [] -> None
      | (_, pivot) :: _ ->
        let linexpr' =
          scale_linexpr (Mpqf.one /: pivot) linexpr
        in
        Some (linexpr', pivot)
  end

(* chekc the subpoly trees branch for old (ai) implementation *)
module Slack_managment = struct
  include Linexpr_managment
  include VarManagement
  include RatOps.ConvenienceOps (Mpqf)

  (* Helper functions for slack management *)

  let map_d (_f: SubPolyDomain.t -> SubPolyDomain.t) (_t: t) =
    failwith "TODO!"

  let var_key (_var: Var.t) = failwith "TODO!"

  let const_q_of_exp (_exp: exp) = failwith "TODO!"

  let rec vars_of_exp (_exp: exp) = failwith "TODO!"

  let interval_of_constraint_op (_op: binop) (_bound: Q.t) = failwith "TODO!"

  let absorb_linexpr_const_into_interval (_linexpr: linexpr) (_interval: RationalInterval.t) =
    failwith "TODO!"

  let rec simple_constraint (_t: t) (_exp: exp) : (t * linexpr * RationalInterval.t) option =
    failwith "TODO!"

  let slack_var_of_constraint (_linexpr: linexpr) (_interval: RationalInterval.t) =
    failwith "TODO!"

  let row_of_slack (_env: Environment.t) (_slack: Var.t) (_linexpr: linexpr) =
    failwith "TODO!"

  let add_constant_interval (_t: t) (_var: Var.t) (_c: Q.t) = failwith "TODO!"

  let add_slack_constraint (_t: t) (_linexpr: linexpr) (_interval: RationalInterval.t) =
    failwith "TODO!"
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

  let to_yojson _ = failwith "SubPolyhedraDomain.to_yojson: not implemented"


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
  
  (*< Copy-pasted from octagons >*) 
  let leq a b = 
    let env_comp = Environment.cmp a.env b.env in
    if env_comp = -2 || env_comp > 0 then false else
    if is_bot_env a || is_top b then true else
    if is_bot_env b || is_top a then false else
    (* bis hier: macht mann das immer so -> Rückgabewerte in AffineEq anschauen *)
    failwith "SubPolyhedraDomain.leq: not implemented"
  (*</ Copy-pasted from octagons >*)
  let join _a _b = failwith "SubPolyhedraDomain.join: not implemented"


 (* Copy-pasted from octagons for reference:
    let meet a b = (* same as join but calls cap instead of cup *)
    match a.d,b.d with
    | None, _ -> b
    | _, None -> a
    | Some octa, Some octb when (Environment.cmp a.env b.env <> 0)->
      let sup_env = Environment.lce a.env b.env in
      let mod_a = SparseOctagon.dim_add (Environment.dimchange a.env sup_env) octa in
      let mod_b = SparseOctagon.dim_add (Environment.dimchange b.env sup_env) octb in
      {d=cap mod_a mod_b; env = sup_env}
    | Some octa, Some octb -> { d = cap octa octb ; env = a.env} (* same environment, so we can just meet the octagons*) 


  let join a b = 
    match a.d,b.d with
    | None, _ -> b
    | _, None -> a
    | Some octa, Some octb when (Environment.cmp a.env b.env <> 0)->
      let sup_env = Environment.lce a.env b.env in
      let mod_a = SparseOctagon.dim_add (Environment.dimchange a.env sup_env) octa in
      let mod_b = SparseOctagon.dim_add (Environment.dimchange b.env sup_env) octb in
      {d=cup mod_a mod_b; env = sup_env}
    | Some octa, Some octb -> { d = cup octa octb ; env = a.env} (* same environment, so we can just join the octagons*) 
 *)

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
  

  
  let assign_texpr _t _var _texpr = failwith "SubPolyhedraDomain.assign_texpr: not implemented"
  (*< Copy-pasted from ltve >*)
  let assign_exp ask (t: VarManagement.t) var exp (no_ov: bool Lazy.t) : VarManagement.t =
    let t = if not @@ Environment.mem_var t.env var then add_vars t [var] else t in
    match Convert.texpr1_expr_of_cil_exp ask t t.env exp no_ov with
    | texp -> assign_texpr t var texp
    | exception Convert.Unsupported_CilExp _ -> forget_vars t [var]
  (*</ Copy-pasted from ltve >*)

  (*< Copy-pasted from ltve >*)
  let assign_var (t: VarManagement.t) v v' =
    let t = add_vars t [v; v'] in
    assign_texpr t v (Var v') (* TODO Leonie: Find mistake *)
  (*</ Copy-pasted from ltve >*)

  (*< Copy-pasted from ltve >*)
  let assign_var_parallel t vv's =
    let assigned_vars = List.map fst vv's in
    let t = add_vars t assigned_vars in
    let primed_vars = List.init (List.length assigned_vars) (fun i -> Var.of_string (string_of_int i  ^"'")) in
    let t_primed = add_vars t primed_vars in
    let multi_t = List.fold_left2 (fun t' v_prime (_,v') -> assign_var t' v_prime v') t_primed primed_vars vv's in
    match multi_t.d with
    | Some arr when not @@ is_top multi_t ->
      let switched_arr = List.fold_left2 (fun multi_t assigned_var primed_var-> assign_var multi_t assigned_var primed_var) multi_t assigned_vars primed_vars in
      remove_vars switched_arr primed_vars
    | _ -> t
  (*</ Copy-pasted from ltve >*)

  (*< Copy-pasted from ltve >*)
  let assign_var_parallel_with t vv's =
    (* TOD0: If we are angling for more performance, this might be a good place ot try. `assign_var_parallel_with` is used whenever a function is entered (body),
       in unlock, at sync edges, and when entering multi-threaded mode. *)
    let t' = assign_var_parallel t vv's in
    t.d <- t'.d;
    t.env <- t'.env
  (*</ Copy-pasted from ltve >*)

  (*< Copy-pasted from ltve >*)
  let assign_var_parallel' t vs1 vs2 =
    let vv's = List.combine vs1 vs2 in
    assign_var_parallel t vv's
  (*</ Copy-pasted from ltve >*)

  (*< Copy-pasted from ltve >*)
  let substitute_exp ask t var exp no_ov =
    let t = if not @@ Environment.mem_var t.env var then add_vars t [var] else t in
    let res = assign_exp ask t var exp no_ov in
    forget_vars res [var] 
  (*</ Copy-pasted from ltve >*)

  (*< Copy-pasted from ltve >*)
  let cil_exp_of_lincons1 = Convert.cil_exp_of_lincons1
  (*</ Copy-pasted from ltve >*)


  (* Module AssertionRels demands: *)
    let assert_constraint (_ask: Queries.ask) (d: t) (e: exp) (negate: bool) (_no_ov: bool Lazy.t) =
    let res =
      (*check if constraint is negated*)
      if negate then 
        (*TODO: add the negated branch fo the assert constraint*)
        d 
      else
        (* parse cil expression into form we can work with,
          here linexpr and intercal *)
        match simple_constraint d e with
        | Some (d, linexpr, interval) ->
          (* Absorb constant from expression into interval tied to expression, like in paper*)
          let linexpr, interval = absorb_linexpr_const_into_interval linexpr interval in
          (*normalize linexpr liek discussed in meeting*)
          begin match normalize_linexpr linexpr with
            | Some (normalized_linexpr, pivot) ->
              (* convert pivot to q, which is what we do the relational interval with *)
              let pivot_q = Q.make (Mpqf.get_num pivot) (Mpqf.get_den pivot) in
              let inv_piv = Q.div Q.one pivot_q in
              let new_interval = RationalInterval.scale inv_piv interval in
              (*add slack and normalized expr, and adjusted interval by constant*)
              add_slack_constraint d normalized_linexpr new_interval
            | None -> d
          end
        | None -> d
    in
    if M.tracing then M.tracel "relation" "subpoly assert_constraint %a -> %s" d_exp e (show res);
    res

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
