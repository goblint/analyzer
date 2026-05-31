(** OCaml implementation of the subpolyhedra domain.

    @see <https://www.microsoft.com/en-us/research/wp-content/uploads/2011/06/subpolyhedra.pdf>  Subpolyhedra. *)

open Batteries
open GoblintCil
open Pretty
module M = Messages
open GobApron
open Subpolycore

module Mpqf = SharedFunctions.Mpqf
module RationalInterval = Rationalinterval.RationalInterval

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
  let size (t: t) = Environment.size t.env
end

module ExpressionBounds: (SharedFunctions.ConvBounds with type t = VarManagement.t) =
struct
  include VarManagement
  let bound_texpr (_t: t) (_texpr: Texpr1.t) = None, None
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
    const: Mpqf.t; 
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

(* part of what was originally wired by ai for the smoke test, def to be reviewed. *)
(* Nico: I had ai rewrite this module for the sake of compilation with the new representation of our type in subpolycore.
         => still not handwritten/actually useful
*)
module Slack_managment = struct
  include Linexpr_managment
  include VarManagement
  include RatOps.ConvenienceOps (Mpqf)

  (* Helper functions for slack management *)

  let map_d (f: SubPolyDomain.t -> SubPolyDomain.t) (t: t) =
    match t.d with
    | None -> t
    | Some d -> { t with d = Some (f d) }

  let var_key (var: Apron.Var.t) = Apron.Var.to_string var

  let const_q_of_exp (exp: exp) = match exp with
  | Const (CInt (i, _, _)) -> Some (Q.of_bigint i)
  | _ -> None

  let rec vars_of_exp (exp: exp) = match exp with
    | Lval (Var v, NoOffset) -> [V.local v]
    | UnOp (_, e, _) -> vars_of_exp e
    | CastE (_, _, e) -> vars_of_exp e
    | BinOp (_, e1, e2, _) -> vars_of_exp e1 @ vars_of_exp e2
    | _ -> []

  let interval_of_constraint_op (op: binop) (bound: Q.t) =
    match op with
    | Lt
    | Le -> Some (RationalInterval.of_bounds ~lower:None ~upper:(Some bound))
    | Gt
    | Ge -> Some (RationalInterval.of_bounds ~lower:(Some bound) ~upper:None)
    | Eq -> Some (RationalInterval.of_bounds ~lower:(Some bound) ~upper:(Some bound))
    | _ -> None

  (** Move [linexpr.const] into the rational interval so the linear form has constant zero *)
  let absorb_linexpr_const_into_interval (linexpr: linexpr) (interval: RationalInterval.t) =
    if Mpqf.compare linexpr.const Mpqf.zero = 0 then
      linexpr, interval
    else
      let c_q = Q.make (Mpqf.get_num linexpr.const) (Mpqf.get_den linexpr.const) in
      { linexpr with const = Mpqf.zero }, RationalInterval.add_const (Q.neg c_q) interval

  let rec simple_constraint (t: t) (exp: exp) : (t * linexpr * RationalInterval.t) option = match exp with
    | CastE (_, _, e) -> simple_constraint t e
    | BinOp ((Lt | Le | Gt | Ge | Eq as op), lhs, rhs, _) ->
      let t = add_vars t (vars_of_exp lhs) in
      begin match linexpr_of_exp lhs, const_q_of_exp rhs with
        | Some linexpr, Some bound when linexpr_var_count linexpr > 1 ->
          Option.map (fun interval -> t, linexpr, interval) (interval_of_constraint_op op bound)
        | _ -> None
      end
    | _ -> None

  let slack_var_of_constraint (linexpr: linexpr) (interval: RationalInterval.t) =
    let term_key = List.map (fun (var, coeff) -> Apron.Var.to_string var, Mpqf.hash coeff) linexpr.terms in
    let key = Hashtbl.hash (term_key, Mpqf.hash linexpr.const, RationalInterval.hash interval) in
    Apron.Var.of_string ("#subpoly_slack:" ^ string_of_int key)

  let row_of_slack (env: Environment.t) (slack: Apron.Var.t) (linexpr: linexpr) =
    let row = SubPolyDomain.CoeffVector.zero_vec (Environment.size env + 1) in
    let row = SubPolyDomain.CoeffVector.set_nth row (Environment.dim_of_var env slack) Mpqf.one in
    let row = List.fold_left (fun row (var, coeff) ->
        let dim = Environment.dim_of_var env var in
        let coeff' = SubPolyDomain.CoeffVector.nth row dim -: coeff in
        SubPolyDomain.CoeffVector.set_nth row dim coeff'
      ) row linexpr.terms
    in
    let const_dim = SubPolyDomain.CoeffVector.length row - 1 in
    let const' = SubPolyDomain.CoeffVector.nth row const_dim -: linexpr.const in
    SubPolyDomain.CoeffVector.set_nth row const_dim const'

  let add_constant_interval (t: t) (var: Apron.Var.t) (c: Q.t) =
    let interval = RationalInterval.of_bounds ~lower:(Some c) ~upper:(Some c) in
    map_d (SubPolyDomain.set_intv (Environment.dim_of_var t.env var) interval) t

  let add_slack_constraint (t: t) (linexpr: linexpr) (interval: RationalInterval.t) =
    let slack = slack_var_of_constraint linexpr interval in
    let t = add_vars t [slack] in
    let row = row_of_slack t.env slack linexpr in
    let slack_dim = Environment.dim_of_var t.env slack in
    let slack_info : SubPolyDomain.info = List.map (fun (var, coeff) -> Environment.dim_of_var t.env var, coeff) linexpr.terms in
    map_d (fun d ->
        if SubPolyDomain.mem_info slack_dim d then
          d
        else
          d
          |> SubPolyDomain.add_affeq_row row
          |> SubPolyDomain.set_info slack_dim slack_info
          |> SubPolyDomain.set_intv slack_dim interval
      ) t
  
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
  let meet (a: t) (b: t) =
    match a.d, b.d with
    | None, _
    | _, None -> bot ()
    | Some ad, Some bd when Environment.equal a.env b.env ->
      { d = Some (SubPolyDomain.meet ad bd); env = a.env }
    | _ -> top ()

  let leq (a: t) (b: t) =
    is_bot a || is_top b || equal a b

  let join (a: t) (b: t) =
    match a.d, b.d with
    | None, _ -> b
    | _, None -> a
    | Some ad, Some bd when Environment.equal a.env b.env ->
      { d = Some (SubPolyDomain.join ad bd); env = a.env }
    | _ -> top ()

  let widen = join
  
  let narrow (a: t) (_b: t) = a
  
  let unify = meet


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
  
  let assign_exp (_ask: Queries.ask) (t: t) (var: V.t) (exp: exp) (_no_ov: bool Lazy.t) =
    let t = add_vars t [var] in
    match const_q_of_exp exp with
    | Some c -> add_constant_interval t var c
    | None -> t
  
    let assign_var (t: t) (v: V.t) (v': V.t) = add_vars t [v; v']
  
  let assign_var_parallel (t: t) (vvs: (V.t * V.t) list) =
    add_vars t (List.concat_map (fun (v, v') -> [v; v']) vvs)
  
    let assign_var_parallel_with (t: t) (vvs: (V.t * V.t) list) =
    let t' = assign_var_parallel t vvs in
    t.d <- t'.d;
    t.env <- t'.env
  
    let assign_var_parallel' (t: t) (vs1: V.t list) (vs2: V.t list) =
    assign_var_parallel t (List.combine vs1 vs2)
  
    let substitute_exp (_ask: Queries.ask) (t: t) (var: V.t) (_exp: exp) (_no_ov: bool Lazy.t) = forget_var t var
  
  let cil_exp_of_lincons1 = Convert.cil_exp_of_lincons1

  (* This is where we normalize the linexpr and add the slack constraint. before adding to our domain. *)
  (* Basically the transfer function. *)
  let assert_constraint (_ask: Queries.ask) (d: t) (e: exp) (negate: bool) (_no_ov: bool Lazy.t) =
    let res =
      if negate then d else
        match simple_constraint d e with
        | Some (d, linexpr, interval) ->
          let linexpr, interval = absorb_linexpr_const_into_interval linexpr interval in
          begin match normalize_linexpr linexpr with
            | Some (normalized_linexpr, pivot) ->
              (* convert pivot to q, which is what we do the relational interval with *)
              let pivot_q = Q.make (Mpqf.get_num pivot) (Mpqf.get_den pivot) in
              let inv_piv = Q.div Q.one pivot_q in
              let new_interval = RationalInterval.scale inv_piv interval in
              add_slack_constraint d normalized_linexpr new_interval
            | None -> d
          end
        | None -> d
    in
    if M.tracing then M.tracel "relation" "subpoly assert_constraint %a -> %s" d_exp e (show res);
    res
    
  let env (t: t) = t.env
  let eval_interval _ask = Bounds.bound_texpr
  let invariant (_t: t) = []

  type marshal = t
  (* marshal is not compatible with apron, therefore we don't have to implement it *)
  let marshal (t: t) = t
  let unmarshal (t: marshal) = t
  let relift (t: t) = t
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
