module Mpqf = SharedFunctions.Mpqf
open Intervalsig


(** Variable type used by the subpolyhedra core. *)
module type Var = sig
  type t [@@deriving hash]
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val string_of : t -> string
end

(** Internal representation of a consistent subpolyhedron. *)
module SubPoly (Var : Var) (I : IntervalSig) = struct
  (* Reuse the SparseVector and ListMatrix modules from the AffineEqualityDomain. *)
  module Vector = SparseVector.SparseVector
  module CoeffVector = Vector(Mpqf)
  module Matrix =
    AffineEqualityDomain.AffineEqualityMatrix
      (Vector)
      (ListMatrix.ListMatrix)

  (* Map keyed by variables. *)
  module VarMap = Map.Make(Var)

  type affeq = Matrix.t [@@deriving eq, ord, hash]
  type interval = I.t [@@deriving eq, ord, hash]
  type interval_map = interval VarMap.t [@@deriving eq, ord]
  type slackintervals = interval_map [@@deriving eq, ord]
  type slack_expr = {
    terms: (Var.t * Mpqf.t) list;
  } [@@deriving eq, ord, hash]
  type slack = {
    expr: slack_expr;
    info: interval;
  } [@@deriving eq, ord, hash]
  type slack_map = slack VarMap.t [@@deriving eq, ord]

  let hash_interval_map (m: interval_map) =
    VarMap.fold (fun var interval acc ->
        Hashtbl.hash (Var.hash var, I.hash interval, acc)
      ) m 0

  let hash_slackintervals = hash_interval_map

  let hash_slack_map (m: slack_map) =
    VarMap.fold (fun var slack acc ->
        Hashtbl.hash (Var.hash var, hash_slack slack, acc)
      ) m 0

  type t = {
    affeq: affeq;
    intervals: slackintervals;
    slacks: slack_map;
  } [@@deriving eq, ord, hash]


  (* Everything here is TODO, it has AI generated placeholds so i could run the regtest *)

  let copy = Fun.id

  let empty () = { affeq = Matrix.empty (); intervals = VarMap.empty; slacks = VarMap.empty }

  let is_empty (t: t) =
    Matrix.is_empty t.affeq && VarMap.is_empty t.intervals && VarMap.is_empty t.slacks

  let set_interval (var: Var.t) (interval: interval) (t: t) =
    { t with intervals = VarMap.add var interval t.intervals }

  let set_slack (var: Var.t) (slack: slack) (t: t) =
    { t with slacks = VarMap.add var slack t.slacks }

  let mem_slack (var: Var.t) (t: t) =
    VarMap.mem var t.slacks

  let add_affeq_row (row: CoeffVector.t) (t: t) =
    { t with affeq = Matrix.append_row t.affeq row }

  let dim_add (ch: Apron.Dim.change) (t: t) =
    { t with affeq = Matrix.dim_add ch t.affeq }

  let dim_remove (ch: Apron.Dim.change) (t: t) =
    { t with affeq = Matrix.dim_remove ch t.affeq }

  let string_of_interval_map (m: interval_map) =
    VarMap.bindings m
    |> List.map (fun (var, interval) -> Var.string_of var ^ " -> " ^ I.show interval)
    |> String.concat "; "

  let string_of_slack_expr (e: slack_expr) =
    let term_str =
      match e.terms with
      | [] -> ""
      | terms ->
        terms
        |> List.map (fun (v, c) -> Mpqf.to_string c ^ "*" ^ Var.string_of v)
        |> String.concat " + "
    in
    term_str

  let string_of_slack (s: slack) =
    I.show s.info ^ "  (" ^ string_of_slack_expr s.expr ^ ")"

  let string_of_slack_map (m: slack_map) =
    VarMap.bindings m
    |> List.map (fun (var, slack) ->
        Var.string_of var ^ " -> " ^ string_of_slack slack)
    |> String.concat "; "

  let string_of (t: t) =
    "{ affeq = " ^ Matrix.show t.affeq
    ^ "; intervals = [" ^ string_of_interval_map t.intervals ^ "]"
    ^ "; slacks = [" ^ string_of_slack_map t.slacks ^ "] }"

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

