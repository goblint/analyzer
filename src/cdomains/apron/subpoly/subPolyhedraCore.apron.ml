module Mpqf = SharedFunctions.Mpqf
open Intervalsig

include Batteries


(** Variable type used by the subpolyhedra core. *)
module type Var = sig
  type t [@@deriving hash]
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val string_of : t -> string
  val to_int : t -> int
  val to_t : int -> t
end

(** Internal representation of a consistent subpolyhedron. *)
module SubPoly (Var : Var) (I : IntervalSig) = struct
  (* Reuse the SparseVector and ListMatrix modules from the AffineEqualityDomain. *)
  include RatOps.ConvenienceOps (Mpqf)

  module Vector = SparseVector.SparseVector
  module CoeffVector = Vector(Mpqf)
  module Matrix =
    AffineEqualityDomain.AffineEqualityMatrix
      (Vector)
      (ListMatrix.ListMatrix) (*Question: do we actually use this, if we just use the Matrix and Vector implementations? *)

  (* Map keyed by variables. *)
  module VarMap = Map.Make(Var)

  type affeq = Matrix.t [@@deriving eq, ord, hash] (*Our affine equality matrix.*)
  type interval_map = I.t VarMap.t [@@deriving eq, ord] (*Map from Var to Interval*)
  type info = CoeffVector.t [@@deriving eq, ord, hash] (*Coefficient vector over the matrix columns (constant in the last position)*)
  type info_map = info VarMap.t [@@deriving eq, ord] (*Map from Var to info*)

  let hash_interval_map (m: interval_map) =
    VarMap.fold (fun var interval acc ->
        Hashtbl.hash (Var.hash var, I.hash interval, acc)
      ) m 0

  let hash_slackintervals = hash_interval_map

  let hash_info_map (m: info_map) =
    VarMap.fold (fun var info acc ->
        Hashtbl.hash (Var.hash var, hash_info info, acc)
      ) m 0

  type t = {
    affeq: affeq; (*Affine Equalities stored as (sparse?) Matrix*)
    intervals: interval_map; (*Map of slack vars to intervals*)
    infos: info_map; (*Map of slack vars to info*)
  } [@@deriving eq, ord, hash]


  (* Everything here is TODO, it has AI generated placeholds so i could run the regtest *)

  let copy = Fun.id

  let empty () = { affeq = Matrix.empty (); intervals = VarMap.empty; infos = VarMap.empty }

  let is_empty (t: t) =
    Matrix.is_empty t.affeq && VarMap.is_empty t.intervals && VarMap.is_empty t.infos

  let set_info (var: Var.t) (info: info) (t : t)  =
    {t with infos = VarMap.add var info t.infos}

  let set_intv (var: Var.t) (intv: I.t) (t: t) = 
    {t with intervals = VarMap.add var intv t.intervals}
  let mem_info (var: Var.t) (t: t) = 
    VarMap.mem var t.infos

  let mem_intv (var: Var.t) (t: t) =
    VarMap.mem var t.intervals

  let add_affeq_row (row: CoeffVector.t) (t: t) =
    { t with affeq = Matrix.append_row t.affeq row }

  (** Number of slack columns = size of the trailing slack block. Every slack has an
      interval *)
  let num_slacks (t: t) = VarMap.cardinal t.intervals

  let insert_slack (slack_col: int) (expr: info) (interval: I.t) (t: t) : t =
    let widen v = CoeffVector.insert_zero_at_indices v [(slack_col, 1)] 1 in
    let affeq = Matrix.add_empty_columns t.affeq [| slack_col |] in
    let expr  = widen expr in                                          (* slack col now 0, const shifted right *)
    let row   = CoeffVector.set_nth expr slack_col (Mpqf.neg Mpqf.one) in (* expr - slack = 0 *)
    let key   = Var.to_t slack_col in
    { affeq     = Matrix.append_row affeq row;
      infos     = VarMap.add key expr (VarMap.map widen t.infos);
      intervals = VarMap.add key interval t.intervals }

  (**
    [rem_row_containing_var affeq var] uses [Matrix.reduce_col] and [Matrix.remove_zero_rows] to remove all occurences of the variable from a matrix. 

    Used in forget_vars.
  *)  
  let rem_rows_containing_var (affeq : affeq) (var : Var.t) : affeq = 
    if Matrix.is_empty affeq then affeq
    else 
      Matrix.remove_zero_rows @@ Matrix.reduce_col affeq (Var.to_int var)

  (**
    [rem_infos_containing_var slacks var] takes a slack_map and removes all slack variables whose info contains mention of the var.

    Used in forget_vars.
  *) 
  let rem_infos_containing_var (slacks : info_map) (var : Var.t) : info_map = 
     VarMap.filter (fun _ (info : info) -> CoeffVector.nth info (Var.to_int var) =: Mpqf.zero) slacks 

  (**
    [forget_vars vars t] forgets a list of variables in the polyhedron.

    Future TODO: Currently we do Gaussian elimination with the variable as pivot ([Matrix.reduce_col]).
    This is fine for the affeq, but we do not want to blindly remove any slack variable info containing x
    from our info_map. Currently this happens, but refinement is needed in the future!
  *)
  let forget_vars (vars: Var.t list) (t: t) = 
    let new_affeq = List.fold_left rem_rows_containing_var t.affeq vars in
    let new_intervals = List.fold_left (flip VarMap.remove) t.intervals vars in
    let new_infos = List.fold_left rem_infos_containing_var t.infos vars in
      {affeq = new_affeq ; intervals = new_intervals ; infos = new_infos}
  
  (**
  [forget_var var t] forgets a single variable using [forget_vars].
  *)
  let forget_var (var : Var.t) (t: t) : t = 
    forget_vars [var] t
  
  (* HELPER-FUNCTIONS FOR DIMENSIONAL OPERATIONS *)
  let shift_index_add (old_index : Var.t) (occ_cols : (int * int) list) : Var.t = 
    (* find all entries that are less or equal to old_index in occ_cols, and count them (=k), then new_index = old_index + k , return new_index *)
    (let k = List.fold_left (fun acc (index, count) -> if index <= (Var.to_int old_index) then acc + count else acc) 0 occ_cols
    in let new_index = (Var.to_int old_index) + k 
    in Var.to_t new_index)

  let shift_index_remove (old_index : Var.t) (dim_list : int list) : Var.t = 
    (let k = List.fold_left (fun acc index -> if index < (Var.to_int old_index) then acc + 1 else acc) 0 dim_list
    in let new_index = (Var.to_int old_index) - k 
    in Var.to_t new_index)

    (** Replacements for new_slack_add and new_slack_remove for the new datatype: *)
  let new_infos_add (infos : info_map) occ_cols num_added : info_map = 
    VarMap.fold(fun var info acc ->
      let new_var = shift_index_add var occ_cols in
      let new_info = CoeffVector.insert_zero_at_indices info occ_cols num_added in
      VarMap.add new_var new_info acc) infos VarMap.empty
  let new_infos_remove (infos : info_map) dim_list : info_map = 
    VarMap.fold (fun var info acc ->
      let new_var = shift_index_remove var dim_list in
      let new_info = CoeffVector.remove_at_indices info dim_list in
      VarMap.add new_var new_info acc) infos VarMap.empty
  let new_intervals_add (intervals : interval_map) occ_cols : interval_map = 
    VarMap.fold( fun var interval acc ->
      let new_var = shift_index_add var occ_cols in
      VarMap.add new_var interval acc) intervals VarMap.empty
  let new_intervals_remove (intervals : interval_map) dim_list : interval_map =
    VarMap.fold( fun var interval acc ->
      let new_var = shift_index_remove var dim_list in
      VarMap.add new_var interval acc) intervals VarMap.empty

  let dim_add (ch: Apron.Dim.change) (t: t) = 
    let new_affeq = Matrix.dim_add ch t.affeq in
    let list = Array.to_list ch.dim in
    let grouped_indices = List.group Int.compare list in 
    let occ_cols = List.map (fun group -> ((List.hd group, List.length group))) grouped_indices in 
    (* Approach from listMatrix.ml: add_empty_columns; Example: cols_list = [1; 3; 3; 5] -> grouped_indices = [[1]; [3; 3]; [5]] -> occ_cols = [(1, 1); (3, 2); (5, 1)] *)
    let new_infos = new_infos_add t.infos occ_cols (Array.length ch.dim) in 
    let new_intervals = new_intervals_add t.intervals occ_cols in
    {affeq = new_affeq; infos = new_infos; intervals = new_intervals}

  let dim_remove (ch: Apron.Dim.change) (t: t) = 
    let new_affeq = Matrix.dim_remove ch t.affeq in
    let dim_list = Array.to_list ch.dim in
    let new_t = forget_vars (List.map Var.to_t dim_list) t in
    let dim_list = List.sort_uniq Int.compare dim_list in (* remove duplicates *)
    let new_infos = new_infos_remove new_t.infos dim_list in 
    let new_intervals = new_intervals_remove new_t.intervals dim_list in
    {affeq = new_affeq; infos = new_infos; intervals = new_intervals}

  let string_of_interval_map (m: interval_map) =
    VarMap.bindings m
    |> List.map (fun (var, interval) -> Var.string_of var ^ " -> " ^ I.show interval)
    |> String.concat "; "

  let string_of_info (e: info) =
      match CoeffVector.to_sparse_list e with
      | [] -> ""
      | terms ->
        terms
        |> List.map (fun (i, c) -> Mpqf.to_string c ^ "*" ^ Var.string_of (Var.to_t i))
        |> String.concat " + "
  let string_of_infos (infos: info_map) = 
    VarMap.bindings infos
      |> List.map (fun (var, info) -> Var.string_of var ^ " -> " ^ string_of_info info)
      |> String.concat "; "
  
  let string_of_interval (s: I.t) (i : info)=
    I.show s ^ "  (" ^ string_of_info i ^ ")"

  let string_of (t: t) =
    "{ affeq = " ^ Matrix.show t.affeq
    ^ "; intervals = [" ^ string_of_interval_map t.intervals ^ "]"
    ^ "; slacks = [" ^ string_of_infos t.infos ^ "] }"

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
