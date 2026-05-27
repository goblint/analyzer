module Mpqf = SharedFunctions.Mpqf
open Intervalsig
include Batteries


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
      info: slack_expr;
    intv: interval;
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
    let new_affeq = Matrix.dim_add ch t.affeq in
    let list = Array.to_list ch.dim in
    let grouped_indices = List.group Int.compare list in 
    let occ_cols = List.map (fun group -> ((List.hd group, List.length group))) grouped_indices in 
    (* Approach from listMatrix.ml: add_empty_columns; Example: cols_list = [1; 3; 3; 5] -> grouped_indices = [[1]; [3; 3]; [5]] -> occ_cols = [(1, 1); (3, 2); (5, 1)] *)
    let shift_index_add (old_index : Var.t) (occ_cols : (int * int) list) : Var.t = 
      (* find all entries that are less or equal to old_index in occ_cols, and count them (=k), then new_index = old_index + k , return new_index *)
      (let k = List.fold_left (fun acc (index, count) -> if index <= (Var.to_int old_index) then acc + count else acc) 0 occ_cols
      in let new_index = (Var.to_int old_index) + k 
      in Var.to_t new_index)
    in
    let new_slacks_add slacks occ_cols  : slack VarMap.t =
      VarMap.fold (fun var slack acc ->
        let new_var = shift_index_add var occ_cols in
        let new_slack = {
          info = {
            terms = List.rev (List.fold_left (fun acc (v, c) -> (shift_index_add v occ_cols, c) :: acc) [] slack.info.terms);
            const = slack.info.const;
          };
          intv = slack.intv;
        } in
        VarMap.add new_var new_slack acc
      ) slacks VarMap.empty
    in
    let new_slacks = new_slacks_add t.slacks occ_cols in 
    {t with affeq = new_affeq; slacks = new_slacks}

  let dim_remove (ch: Apron.Dim.change) (t: t) = 
    let new_affeq = Matrix.dim_remove ch t.affeq in
    let new_t = failwith "forget_vars (List.map Var.to_t dim_list) t" in (* TODO: forget_vars is corretly in module D, but we need to use this here *)
    let dim_list = Array.to_list ch.dim in
    let dim_list = List.sort_uniq Int.compare dim_list in (* remove duplicates *)
    let shift_index_remove (old_index : Var.t) (dim_list : int list) : Var.t = 
      (let k = List.fold_left (fun acc index -> if index < (Var.to_int old_index) then acc + 1 else acc) 0 dim_list
      in let new_index = (Var.to_int old_index) - k 
      in Var.to_t new_index)
    in
    let new_slacks_remove slacks dim_list : slack VarMap.t =
      VarMap.fold (fun var slack acc ->
        let new_var = shift_index_remove var dim_list in
        let new_slack = {
          info = {
            terms = List.rev (List.fold_left (fun acc (v, c) -> (shift_index_remove v dim_list, c) :: acc) [] slack.info.terms);
            const = slack.info.const;
          };
          intv = slack.intv;
        } in
        VarMap.add new_var new_slack acc
      ) slacks VarMap.empty
    in   
    let new_slacks = new_slacks_remove new_t.slacks dim_list in 
    {t with affeq = new_affeq; slacks = new_slacks}



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
    I.show s.intv ^ "  (" ^ string_of_slack_expr s.info ^ ")"

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

