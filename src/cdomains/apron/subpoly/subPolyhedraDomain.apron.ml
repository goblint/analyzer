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
    type t = int [@@deriving hash]
    let equal = Int.equal
    let compare = Int.compare
    let string_of = string_of_int
  end
  module SubPolyDomain = SubPoly(Int)(RationalInterval)
  include SharedFunctions.VarManagementOps (SubPolyDomain)

  let dim_add = SubPolyDomain.dim_add
  (*potentially add dim_remove here, not sure though*)  
  let size _t = failwith "SubPolyhedraDomain.size: not   implemented"
end

module ExpressionBounds: (SharedFunctions.ConvBounds with type t = VarManagement.t) =
struct
  include VarManagement
  let bound_texpr _t _texpr = failwith "SubPolyhedraDomain.bound_texpr: not   implemented"
end

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
  let leq _a _b = failwith "SubPolyhedraDomain.leq: not implemented"
  let join _a _b = failwith "SubPolyhedraDomain.join: not implemented"
  let widen _a _b = failwith "SubPolyhedraDomain.widen: not implemented"
  let narrow _a _b = failwith "SubPolyhedraDomain.narrow: not implemented"
  let unify _a _b = failwith "SubPolyhedraDomain.unify: not implemented"

  (* transfer functions *)

  (*******************
    This function first uses the Matrix interface to get a list of the rows, then filters based on if the var 
    is 0 which essentially removes all rows where var is non-zero. It then builds the matrix again from the list.
    It is a bit clunky because Ocaml does not know that the Matrix is already implemented as a list of Vectors.
  ********************)
  let rem_rows_containing_var (affeq : SubPolyDomain.affeq) (var: int) : SubPolyDomain.affeq =
    if SubPolyDomain.Matrix.is_empty affeq then affeq 
    else
      let rows = List.init (SubPolyDomain.Matrix.num_rows affeq) (SubPolyDomain.Matrix.get_row affeq) in
      let filtered = List.filter (fun row -> SubPolyDomain.CoeffVector.nth row var =: Mpqf.zero) rows in
      List.fold_left SubPolyDomain.Matrix.append_row (SubPolyDomain.Matrix.empty ()) filtered

  (*******************
    This function takes a slack_map and removes all slack variables whose info contains mention of the var.
    Used in forget_vars.
  *******************)    
  let rem_slacks_containing_var (slacks : SubPolyDomain.slack_map) (var : int) : SubPolyDomain.slack_map = 
     SubPolyDomain.VarMap.filter (fun _ (slack : SubPolyDomain.slack) -> not (List.mem var (List.map fst slack.info.terms))) slacks 

  (* transfer functions *)
  let forget_var (t: t) (v: V.t) = remove_vars t [v]
    
  
  (**************
    Removes all rows in the affeq Matrix containing the vars, removes the corresponding entry in the 
  **************)
  let forget_vars t vars =
    if vars = [] || is_bot t || is_top t then t
    else 
      let d = Option.get t.d in 
      let dims = List.map (Environment.dim_of_var t.env) vars in (*map of vars in Env. to dimensions in matrix.*)
      let new_affeq = List.fold_left rem_rows_containing_var d.affeq dims in
      let new_intervals = List.fold_left (flip SubPolyDomain.VarMap.remove) d.intervals dims in
      let new_slacks = List.fold_left rem_slacks_containing_var d.slacks dims in
      {d = Some {affeq = new_affeq ; intervals = new_intervals ; slacks = new_slacks}; env = t.env} 

  let assign_exp _ask _t _var _exp _ = failwith "SubPolyhedraDomain.assign_exp: not implemented"
  let assign_var _t _v _v' = failwith "SubPolyhedraDomain.assign_var: not implemented"
  let assign_var_parallel _t _vvs = failwith "SubPolyhedraDomain.assign_var_parallel: not implemented"
  let assign_var_parallel_with _t _vvs = failwith "SubPolyhedraDomain.assign_var_parallel_with: not implemented"
  let assign_var_parallel' _t _vvs = failwith "SubPolyhedraDomain.assign_var_parallel': not implemented"
  let substitute_exp _ask _t _var _exp _no_ov = failwith "SubPolyhedraDomain.substitute_exp: not implemented"
  let cil_exp_of_lincons1 = Convert.cil_exp_of_lincons1

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
