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

module Slack_managment = struct
  include Linexpr_managment
  include VarManagement
  include RatOps.ConvenienceOps (Mpqf)

  module CoeffVector = SubPolyDomain.CoeffVector

  (** [is_slack t col] is [true] iff column [col] is a slack column, i.e. it carries
      an interval bound (the slack columns are exactly the keys of the interval map). *)
  let is_slack (t: t) (col: int) : bool =
    (* Get the interval map from the domain, false if domain option is None *)
    match t.d with
    | None -> false
    | Some d -> SubPolyDomain.mem_intv col d

  (** [fold_slacks f t acc] folds [f col interval info] over every slack,
      where [info] is the slack's linear definition over the real variables
      ([None] if no definition is stored for that slack). *)
  let fold_slacks (f: int -> RationalInterval.t -> SubPolyDomain.info option -> 'a -> 'a) (t: t) (acc: 'a) : 'a =
    match t.d with
    | None -> acc
    | Some d ->
      SubPolyDomain.VarMap.fold (fun col iv acc ->
          f col iv (SubPolyDomain.VarMap.find_opt col d.infos) acc
        ) d.intervals acc

  (* returns the first slack variable name [__slack#k] (scanning k = 0, 1, ...) that is not yet present in the environment. *)
  let fresh_slack_var (t: t) =
    let rec find k =
      let v = Var.of_string (Printf.sprintf "__slack#%d" k) in
      if Environment.mem_var t.env v then find (k + 1) else v
    in
    find 0

  (** [add_slack_constraint t linexpr interval] introduces a fresh slack [s] for the
      linear expression [linexpr] and constrains [s] to [interval]. *)
  let add_slack_constraint (t: t) (linexpr: linexpr) (interval: RationalInterval.t) : t =
    if is_bot_env t then t
    else
      (* first register the slack in the env, add_vars performs all index shifting. *)
      let slack_var = fresh_slack_var t in
      let t = add_vars t [slack_var] in
      match t.d with
      | None -> t
      | Some d ->
        let slack_dim = Environment.dim_of_var t.env slack_var in
        (* build the info coeffvector over the (now final) columns. *)
        let width = Environment.size t.env + 1 in
        let term_entries =
          linexpr.terms
          |> List.map (fun (var, coeff) -> (Environment.dim_of_var t.env var, coeff))
          |> List.sort (fun (i, _) (j, _) -> Int.compare i j)
        in
        let entries =
          if linexpr.const =: Mpqf.zero then term_entries
          else term_entries @ [(width - 1, linexpr.const)]
        in
        let info = CoeffVector.of_sparse_list width entries in
        let d = SubPolyDomain.set_info slack_dim info d in
        let d = SubPolyDomain.set_intv slack_dim interval d in
        { t with d = Some d }
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

  (* Constraint parsing: turn a CIL comparison expression into the linear expression
     being constrained plus the interval it must fall into. Used by assert_constraint. *)

  (** [const_q_of_exp exp] returns the rational value of [exp] if it is a constant. *)
  let const_q_of_exp (_exp: exp) : Q.t option = failwith "TODO"

  (** [vars_of_exp exp] collects the program variables occurring in [exp]. *)
  let rec vars_of_exp (_exp: exp) : Var.t list = failwith "TODO"

  (** [interval_of_constraint_op op bound] turns a comparison operator [op] against
      [bound] into the interval that the constrained expression must lie in
      (e.g. [<= bound] -> [(-inf, bound)]). *)
  let interval_of_constraint_op (_op: binop) (_bound: Q.t) : RationalInterval.t = failwith "TODO"

  (** [simple_constraint t exp] parses a linear comparison [exp] into the linear
      expression being constrained and the interval it has to fall into.
      Returns [None] if [exp] is not a supported linear comparison.

      Per design: the constant is NOT absorbed into the interval here; it stays as
      part of the returned [linexpr]. *)
  let rec simple_constraint (_t: t) (_exp: exp) : (t * linexpr * RationalInterval.t) option = failwith "TODO"

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
          (* Per design: the constant stays in the linexpr (not absorbed into the interval). *)
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
