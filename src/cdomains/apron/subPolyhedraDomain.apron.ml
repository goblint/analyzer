(** OCaml implementation of the subpolyhedra domain.

    @see <https://www.microsoft.com/en-us/research/wp-content/uploads/2011/06/subpolyhedra.pdf>  Subpolyhedra. *)

open Batteries
open GoblintCil
open Pretty
module M = Messages
open GobApron

module Mpqf = SharedFunctions.Mpqf

(* Since we need a custom interval domain, we define it here. *)
module type IntervalDomain = sig
  type t [@@deriving eq, ord, hash]

  val top : t
  val is_top : t -> bool
  val of_bounds : lower:Q.t option -> upper:Q.t option -> t

  val meet : t -> t -> t option
  val join : t -> t -> t
  val leq : t -> t -> bool

  val show : t -> string
end

(* Example instantiation of the IntervalDomain signature. *)
module RationalInterval : IntervalDomain = struct
  type t = Q.t option * Q.t option

  let equal ((l1, u1) as interval_1) ((l2, u2) as interval_2) =
    match interval_1, interval_2 with
    | (None, None) , (None, None) -> true
    | (Some l1, Some l2), (Some u1, Some u2) -> Q.equal l1 l2 && Q.equal u1 u2
    | _ -> false

  let compare ((l1, u1) as i1) ((l2, u2) as i2) =
    match i1, i2 with
    | (None, None) , (None, None) -> 0
    | (Some l1, Some l2), (Some u1, Some u2) ->
      let c_lower = Q.compare l1 l2 in
      if c_lower <> 0 then c_lower else Q.compare u1 u2
    | _ -> 1

    let hash_q q =
      Hashtbl.hash (q.Q.num, q.Q.den)
    
    let hash (u, l)= 
      Hashtbl.hash (Option.map hash_q l, Option.map hash_q u)

  let top = (None, None)
  let of_bounds ~lower ~upper = (lower, upper)

  let is_top = function (None, None) -> true | _ -> false

  let min_bound a b = match a, b with
    | None, _ | _, None -> None
    | Some a, Some b -> Some (if Q.compare a b <= 0 then a else b)

  let max_bound a b = match a, b with
    | None, x | x, None -> x
    | Some a, Some b -> Some (if Q.compare a b >= 0 then a else b)

  let meet (l1, u1) (l2, u2) =
    let lower = max_bound l1 l2 in
    let upper = min_bound u1 u2 in
    match lower, upper with
    | Some l, Some u when Q.compare l u > 0 -> None
    | _ -> Some (lower, upper)

  let join (l1, u1) (l2, u2) =
    (min_bound l1 l2, max_bound u1 u2)

  let lower_leq a b = match a, b with
    | None, _ -> true
    | Some _, None -> false
    | Some a, Some b -> Q.compare a b <= 0

  let upper_leq a b = match a, b with
    | _, None -> true
    | None, Some _ -> false
    | Some a, Some b -> Q.compare a b <= 0

  let leq (l1, u1) (l2, u2) =
    lower_leq l2 l1 && upper_leq u1 u2

  let show_bound = function
    | None -> "inf"
    | Some x ->
      if Z.equal x.Q.den Z.one then Z.to_string x.Q.num
      else Z.to_string x.Q.num ^ "/" ^ Z.to_string x.Q.den

  let show (l, u) =
    let lower = match l with None -> "-inf" | Some _ -> show_bound l in
    let upper = show_bound u in
    "[" ^ lower ^ ", " ^ upper ^ "]"
end

(** Variable
 * type t, basically ordered and printable
*)
module type Var = sig
  type t [@@deriving hash]
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val string_of : t -> string
end

(**
 * SubPoly module
 * - internal representation of a consistent subpolyhedron
 * TODO: pick a type t, maybe (affine-equality, interval map)
 *)
module SubPoly (Var : Var) (I : IntervalDomain) = struct

  (* Reuse the SparseVector and ListMatrix modules from the AffineEqualityDomain. *)
  module Vector = SparseVector.SparseVector
  module CoeffVector = Vector(Mpqf)
  module Matrix =
    AffineEqualityDomain.AffineEqualityMatrix
      (Vector)
      (ListMatrix.ListMatrix)

  (* Map keyed by variables. *)
  module VarMap = Map.Make(Var) 

  (*alias affine_equalities, interval, intervalmap, slackintervals*)
  type affeq = Matrix.t [@@deriving eq, ord, hash]
  type interval = I.t [@@deriving eq, ord, hash]
  type interval_map = interval VarMap.t [@@deriving eq, ord]
  type slackintervals = interval_map [@@deriving eq, ord]
  type slack_map = interval VarMap.t [@@deriving eq, ord]

  (*hash function for the interval map*)
  let hash_interval_map (m: interval_map) =
  VarMap.fold (fun var interval acc ->
    Hashtbl.hash (Var.hash var, I.hash interval, acc)
  ) m 0
  let hash_slackintervals = hash_interval_map

  (*hash function for the slack map*)
  let hash_slack_map = hash_interval_map

  (*internal representation of a consistent subpolyhedron*)
  type t = {
    affeq: affeq;
    intervals: slackintervals;
    slacks: slack_map;
  } [@@deriving eq, ord, hash]

  let copy = Fun.id

  let empty () = { affeq = Matrix.empty (); intervals = VarMap.empty; slacks = VarMap.empty }
 
  let is_empty (t: t) = Matrix.is_empty t.affeq && VarMap.is_empty t.intervals && VarMap.is_empty t.slacks
  
  let set_interval (var: Var.t) (interval: interval) (t: t) =
    { t with intervals = VarMap.add var interval t.intervals }
  
  let set_slack (var: Var.t) (interval: interval) (t: t) =
    { t with slacks = VarMap.add var interval t.slacks }
  
  let mem_slack (var: Var.t) (t: t) =
    VarMap.mem var t.slacks
  
  let add_affeq_row (row: CoeffVector.t) (t: t) =
    { t with affeq = Matrix.append_row t.affeq row }
  
  (*reuse leanies index shifts to implement dim add and remove.*)
  let dim_add (ch: Apron.Dim.change) (t: t) =
    { t with affeq = Matrix.dim_add ch t.affeq }
  let dim_remove (ch: Apron.Dim.change) (t: t) =
    { t with affeq = Matrix.dim_remove ch t.affeq }

  let string_of_interval_map (m: interval_map) =
    VarMap.bindings m
    |> List.map (fun (var, interval) -> Var.string_of var ^ " -> " ^ I.show interval)
    |> String.concat "; "

  let string_of (t: t) =
    "{ affeq = " ^ Matrix.show t.affeq
    ^ "; intervals = [" ^ string_of_interval_map t.intervals ^ "]"
    ^ "; slacks = [" ^ string_of_interval_map t.slacks ^ "] }"

  (* include meet/join/widen etc. *)
  (*get the subpoly methods implemented here, befor ewiring to oursite world in D*)
  let meet (a: t) (b: t) =
    if equal a b then a else empty ()
  let leq (a: t) (b: t) =
    equal a b || is_empty b
  let join (a: t) (b: t) =
    if equal a b then a else empty ()
  let widen = join
  let narrow (a: t) (_b: t) = a
  let unify = meet


  let _ = Var.string_of (* silence unused-functor-arg warning until Var is actually used *)
end

module VarManagement =
struct
  module Str = struct
    type t = string
    let equal = String.equal
    let compare = String.compare
    let string_of = Fun.id
    let hash = Hashtbl.hash
  end
  module SubPolyDomain = SubPoly(Str)(RationalInterval)
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

module D =
struct
  include Printable.Std
  include RatOps.ConvenienceOps (Mpqf)
  include VarManagement

  module Bounds = ExpressionBounds
  module V = RelationDomain.V
  module Arg = struct
    let allow_global = true
  end
  module Convert = SharedFunctions.Convert (V) (Bounds) (Arg) (SharedFunctions.Tracked)

  let name () = "subpoly"

  let mpqf_of_z (z: Z.t) =
    Mpqf.of_mpz @@ Z_mlgmpidl.mpzf_of_z z

  let var_key (var: Var.t) = Var.to_string var

  let map_d (f: SubPolyDomain.t -> SubPolyDomain.t) (t: t) =
    match t.d with
    | None -> t
    | Some d -> { t with d = Some (f d) }

  let const_mpqf_of_exp (exp: exp) = match exp with
    | Const (CInt (i, _, _)) -> Some (mpqf_of_z i)
    | _ -> None

  let const_q_of_exp (exp: exp) = match exp with
    | Const (CInt (i, _, _)) -> Some (Q.of_bigint i)
    | _ -> None

  type linexpr = {
    terms: (Var.t * Mpqf.t) list;
    const: Mpqf.t;
  }

  let zero_linexpr = { terms = []; const = Mpqf.zero }

  let add_linexpr (a: linexpr) (b: linexpr) =
    { terms = a.terms @ b.terms; const = a.const +: b.const }

  let neg_linexpr (a: linexpr) =
    { terms = List.map (fun (var, coeff) -> var, Mpqf.neg coeff) a.terms; const = Mpqf.neg a.const }

  let sub_linexpr (a: linexpr) (b: linexpr) =
    add_linexpr a (neg_linexpr b)

  let option_map2 (f: 'a -> 'b -> 'c) (a: 'a option) (b: 'b option) =
    match a, b with
    | Some a, Some b -> Some (f a b)
    | _ -> None

  let rec linexpr_of_exp (exp: exp) = match exp with
    | Const (CInt (i, _, _)) -> Some { zero_linexpr with const = mpqf_of_z i }
    | Lval (Var v, NoOffset) -> Some { zero_linexpr with terms = [V.local v, Mpqf.one] }
    | UnOp (Neg, e, _) -> Option.map neg_linexpr (linexpr_of_exp e)
    | CastE (_, _, e) -> linexpr_of_exp e
    | BinOp (PlusA, e1, e2, _) ->
      option_map2 add_linexpr (linexpr_of_exp e1) (linexpr_of_exp e2)
    | BinOp (MinusA, e1, e2, _) ->
      option_map2 sub_linexpr (linexpr_of_exp e1) (linexpr_of_exp e2)
    | _ -> None

  let interval_of_constraint_op (op: binop) (bound: Q.t) =
    match op with
    | Lt
    | Le -> Some (RationalInterval.of_bounds ~lower:None ~upper:(Some bound))
    | Gt
    | Ge -> Some (RationalInterval.of_bounds ~lower:(Some bound) ~upper:None)
    | Eq -> Some (RationalInterval.of_bounds ~lower:(Some bound) ~upper:(Some bound))
    | _ -> None

  let rec simple_constraint (exp: exp) = match exp with
    | CastE (_, _, e) -> simple_constraint e
    | BinOp ((Lt | Le | Gt | Ge | Eq as op), lhs, rhs, _) ->
      begin match linexpr_of_exp lhs, const_q_of_exp rhs with
        | Some linexpr, Some bound when List.length linexpr.terms > 1 ->
          Option.map (fun interval -> linexpr, interval) (interval_of_constraint_op op bound)
        | _ -> None
      end
    | _ -> None

  let slack_var_of_constraint (linexpr: linexpr) (interval: RationalInterval.t) =
    let term_key = List.map (fun (var, coeff) -> Var.to_string var, Mpqf.hash coeff) linexpr.terms in
    let key = Hashtbl.hash (term_key, Mpqf.hash linexpr.const, RationalInterval.hash interval) in
    Var.of_string ("#subpoly_slack:" ^ string_of_int key)

  let row_of_slack (env: Environment.t) (slack: Var.t) (linexpr: linexpr) =
    let row = SubPolyDomain.CoeffVector.zero_vec (Environment.size env + 1) in
    let row = SubPolyDomain.CoeffVector.set_nth row (Environment.dim_of_var env slack) Mpqf.one in
    let row = List.fold_left (fun row (var, coeff) ->
        let dim = Environment.dim_of_var env var in
        let coeff' = SubPolyDomain.CoeffVector.nth row dim -: coeff in
        SubPolyDomain.CoeffVector.set_nth row dim coeff'
      ) row linexpr.terms
    in
    let const_dim = SubPolyDomain.CoeffVector.length row - 1 in
    SubPolyDomain.CoeffVector.set_nth row const_dim (Mpqf.neg linexpr.const)

  let add_constant_interval (t: t) (var: Var.t) (c: Q.t) =
    let interval = RationalInterval.of_bounds ~lower:(Some c) ~upper:(Some c) in
    map_d (SubPolyDomain.set_interval (var_key var) interval) t

  let add_slack_constraint (t: t) (linexpr: linexpr) (interval: RationalInterval.t) =
    let slack = slack_var_of_constraint linexpr interval in
    let vars = slack :: List.map fst linexpr.terms in
    let t = add_vars t vars in
    let row = row_of_slack t.env slack linexpr in
    map_d (fun d ->
        if SubPolyDomain.mem_slack (var_key slack) d then
          d
        else
        d
        |> SubPolyDomain.add_affeq_row row
        |> SubPolyDomain.set_slack (var_key slack) interval
      ) t

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
  let forget_var (t: t) (v: V.t) = remove_vars t [v]
  let forget_vars = remove_vars
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

  (* Module AssertionRels demands: *)
  let assert_constraint (_ask: Queries.ask) (d: t) (e: exp) (negate: bool) (_no_ov: bool Lazy.t) =
    let res =
      if negate then
        d
      else
        match simple_constraint e with
        | Some (linexpr, interval) -> add_slack_constraint d linexpr interval
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
