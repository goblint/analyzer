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

(** Internal representation of a consistent subpolyhedron.

    The state consists of three components over a common dimension space:
    - [affeq]: affine equalities, kept in rref. Row semantics for a row [r] of
      length [size + 1]: [sum_{i < size} r(i) * x_i = r(size)].
    - [intervals]: interval bounds for dimensions (program variables and slacks).
      A missing entry means top.
    - [infos]: for each slack dimension [beta], the linear form it was introduced
      for: [beta = sum iterms + iconst]. Invariant: this equality is implied by
      [affeq], and [iterms] only mentions program (non-slack) dimensions. *)
module SubPoly (Var : Var) (I : IntervalSig with type bound = Q.t) = struct
  (* Reuse the SparseVector and ListMatrix modules from the AffineEqualityDomain. *)
  include RatOps.ConvenienceOps (Mpqf)

  module Vector = SparseVector.SparseVector
  module CoeffVector = Vector(Mpqf)
  module Matrix =
    AffineEqualityDomain.AffineEqualityMatrix
      (Vector)
      (ListMatrix.ListMatrix)

  (* Map keyed by variables. *)
  module VarMap = Map.Make(Var)

  type affeq = Matrix.t [@@deriving eq, ord, hash] (*Our affine equality matrix.*)
  type interval_map = I.t VarMap.t [@@deriving eq, ord] (*Map from Var to Interval*)
  type info = {
    iterms: (Var.t * Mpqf.t) list; (* sorted by Var, nonzero coefficients, program dims only *)
    iconst: Mpqf.t;
  } [@@deriving eq, ord, hash]
  type info_map = info VarMap.t [@@deriving eq, ord] (*Map from slack Var to info*)

  let hash_interval_map (m: interval_map) =
    VarMap.fold (fun var interval acc ->
        Hashtbl.hash (Var.hash var, I.hash interval, acc)
      ) m 0

  let hash_info_map (m: info_map) =
    VarMap.fold (fun var info acc ->
        Hashtbl.hash (Var.hash var, hash_info info, acc)
      ) m 0

  type t = {
    affeq: affeq; (*Affine Equalities stored as sparse Matrix*)
    intervals: interval_map; (*Map of vars to intervals*)
    infos: info_map; (*Map of slack vars to info*)
  } [@@deriving eq, ord, hash]

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

  let get_iv (m: interval_map) (var: Var.t) =
    Option.default I.top (VarMap.find_opt var m)

  (* ---------------------------------------------------------------------- *)
  (* Rational helpers *)

  let q_of_mpqf (x: Mpqf.t) : Q.t =
    Q.make (Mpqf.get_num x) (Mpqf.get_den x)

  let is_integral (x: Mpqf.t) = Z.equal (Mpqf.get_den x) Z.one

  (* ---------------------------------------------------------------------- *)
  (* Row and vector helpers *)

  let rows (m: affeq) : CoeffVector.t list =
    List.init (Matrix.num_rows m) (Matrix.get_row m)

  (** Splits a vector of length [size + 1] into its term entries (index < size)
      and the entry at index [size] (RHS for matrix rows, constant for value-form
      vectors). *)
  let split_row (size: int) (r: CoeffVector.t) : (Var.t * Mpqf.t) list * Mpqf.t =
    let entries = CoeffVector.to_sparse_list r in
    let terms, last = List.partition (fun (i, _) -> i < size) entries in
    List.map (fun (i, c) -> (Var.to_t i, c)) terms,
    (match last with [] -> Mpqf.zero | (_, c) :: _ -> c)

  (** Builds a sparse vector of length [size + 1] from term entries and the last
      entry. Terms need not be sorted; zero coefficients are dropped. Duplicate
      variables are summed up. *)
  let vec_of_terms (size: int) (terms: (Var.t * Mpqf.t) list) (last: Mpqf.t) : CoeffVector.t =
    let merged = List.fold_left (fun acc (v, c) ->
        let i = Var.to_int v in
        VarMap.modify_def Mpqf.zero (Var.to_t i) ((+:) c) acc
      ) VarMap.empty terms
    in
    let entries =
      VarMap.fold (fun v c acc -> if c =: Mpqf.zero then acc else (Var.to_int v, c) :: acc) merged []
      |> List.sort (fun (i, _) (j, _) -> Int.compare i j)
    in
    let entries = if last =: Mpqf.zero then entries else entries @ [(size, last)] in
    CoeffVector.of_sparse_list (size + 1) entries

  (* ---------------------------------------------------------------------- *)
  (* Reduction: interval propagation through the equality rows.
     TODO: Implement simplex for better precision

     This is a cheap, sound reduction in the spirit of the Simplex-based
     reduction of the paper: for every row [sum a_i x_i = c] and every variable
     x_j of the row, the implied bound [(c - sum_{i<>j} a_i x_i) / a_j] is met
     into the interval of x_j, iterated until a fixpoint or an iteration cap is
     reached. Returns [None] if a contradiction (bottom) is detected. *)

  exception Bottom

  let scale_iv (c: Mpqf.t) (iv: I.t) = I.scale (q_of_mpqf c) iv

  let propagate ~(size: int) (t: t) : interval_map option =
    let row_data = List.map (split_row size) (rows t.affeq) in
    let refine_row (map, changed) (terms, rhs) = (* use (terms,rhs) to mutually refine occuring variables with their bounds *)
      match terms with
      | [] -> (map, changed) (* constant rows cannot occur in a consistent rref matrix *)
      | _ ->
        List.fold_left (fun (map, changed) (j, a) ->
            (* x_j = (rhs - sum_{i<>j} a_i * x_i) / a *)
            let others = List.fold_left (fun acc (i, ai) ->
                if Var.equal i j then acc
                else I.add acc (scale_iv ai (get_iv map i))
              ) (I.of_const Q.zero) terms
            in
            let cand = (* bounds deduced from other's bounds *)
              I.scale (Q.inv (q_of_mpqf a))
                (I.add (I.of_const (q_of_mpqf rhs)) (I.scale Q.minus_one others))
            in
            if I.is_top cand then (map, changed)
            else
              let old = get_iv map j in
              match I.meet old cand with (* compare candidate bounds to old bounds *)
              | None -> raise Bottom
              | Some res ->
                if I.equal res old then (map, changed)
                else (VarMap.add j res map, true) (* update if better *)
          ) (map, changed) terms
    in
    let rec go map k = (* repeatedly propagate interval bounds through equality rows *)
      if k <= 0 then map
      else
        let map', changed = List.fold_left refine_row (map, false) row_data in
        if changed then go map' (k - 1) else map'
    in
    let max_iter = max 2 (min 20 (List.length row_data + 1)) in (* adjust here for a tighter cap *)
    try Some (go t.intervals max_iter)
    with Bottom -> None

  (* ---------------------------------------------------------------------- *)
  (* Reduction: basis exploration (rho_BE, Algorithm 3 of the paper) with the
     linear explorer (delta_L).

     [propagate] only moves bounds along the rows the rref pivoting happened
     to produce; bounds that need a linear combination of rows (e.g. the
     half-sum of two rows) are invisible to it. 

     Basis exploration re-pivots the equality system (i.e. selects a different
     set of dimensions to use as pivots) onto a statically fixed sequence of bases
     and refines the interval of the basic variable of every row in each basis. 

     For example, with
     $     x0+x2+x3=1,            x1+x2-x3=0 $
     the current rows solve for x0 and x1. Re-pivoting onto {x2,x3} gives
     $     x2=(1-x0-x1)/2,        x3=(1-x0+x1)/2 $

     The linear explorer takes as bases all windows of [m] consecutive dimensions
     (cyclically, in dimension order) out of the [n] dimensions occurring in
     the matrix, where [m] is the number of rows; every pair of distinct
     dimensions then has some basis containing the one but not the other.

     For example: with dimensions [0; 1; 2; 3] and two rows, it tries 
     {0,1}, {1,2}, {2,3}, and {3,0}. 

     Cost: [n] Gauss-Jordan eliminations of the [m]-row matrix. *)

  (** One exploration pass over all bases of the linear explorer, starting
      from the interval map [map]. Refinements found in earlier bases feed the
      later ones. Returns [None] if a contradiction (bottom) is detected. *)
  let explore_linear ~(size: int) (t: t) (map: interval_map) : interval_map option =
    let rs = rows t.affeq in
    let m = List.length rs in
    if m = 0 then Some map
    else begin
      (* dimensions occurring in some row, in dimension order *)
      let cand =
        List.concat_map (fun r ->
            List.filter_map (fun (i, _) -> if i < size then Some i else None)
              (CoeffVector.to_sparse_list r)
          ) rs
        |> List.sort_uniq Int.compare
        |> Array.of_list
      in
      let n = Array.length cand in
      (* Gauss-Jordan elimination pivoting on the [basis] columns, in order;
         returns the pivoted rows keyed by their pivot column. A basis column
         that is linearly dependent on the earlier ones finds no pivot row and
         is skipped. *)
      let pivot_on (basis: int list) : (int * CoeffVector.t) list =
        let step (pivoted, rest) b =
          let rec pick acc = function
            | [] -> None
            | r :: rest when CoeffVector.nth r b <>: Mpqf.zero -> Some (r, List.rev_append acc rest)
            | r :: rest -> pick (r :: acc) rest
          in
          match pick [] rest with
          | None -> (pivoted, rest)
          | Some (r0, rest) ->
            let r0 = CoeffVector.apply_with_c_f_preserves_zero (/:) (CoeffVector.nth r0 b) r0 in
            let elim r =
              let c = CoeffVector.nth r b in
              if c =: Mpqf.zero then r
              else CoeffVector.map2_f_preserves_zero (fun x y -> x -: c *: y) r r0
            in
            ((b, r0) :: List.map (fun (b', r) -> (b', elim r)) pivoted, List.map elim rest)
        in
        fst (List.fold_left step ([], rs) basis)
      in
      (* the pivoted row has coefficient 1 on x_b: x_b = rhs - sum_{i<>b} a_i x_i *)
      let refine map (b, r) =
        let terms, rhs = split_row size r in
        let vb = Var.to_t b in
        let others = List.fold_left (fun acc (i, ai) ->
            if Var.equal i vb then acc
            else I.add acc (scale_iv ai (get_iv map i))
          ) (I.of_const Q.zero) terms
        in
        let cand = I.add (I.of_const (q_of_mpqf rhs)) (I.scale Q.minus_one others) in
        if I.is_top cand then map
        else
          let old = get_iv map vb in
          match I.meet old cand with
          | None -> raise Bottom
          | Some res -> if I.equal res old then map else VarMap.add vb res map
      in
      let explore map i0 =
        let basis = List.init (min m n) (fun k -> cand.((i0 + k) mod n)) in
        List.fold_left refine map (pivot_on basis)
      in
      let num_bases = if n <= m then 1 else n in
      try Some (List.fold_left explore map (List.init num_bases Fun.id))
      with Bottom -> None
    end

  (** Reduction entry point: fixpoint interval propagation, optionally
      followed by one basis-exploration pass and a final propagation pass
      spreading the newly found bounds. Returns [None] on contradiction. *)
  let reduce ~(size: int) ~(explore: bool) (t: t) : interval_map option =
    match propagate ~size t with
    | None -> None
    | Some map ->
      if not explore then Some map
      else
        match explore_linear ~size t map with
        | None -> None
        | Some map' ->
          if map' == map then Some map (* exploration refined nothing *)
          else propagate ~size { t with intervals = map' }

  (* ---------------------------------------------------------------------- *)
  (* Evaluation of linear expressions.

     Value-form vectors [e] of length [size + 1] denote the value
     [sum_{i < size} e(i) * x_i + e(size)]. *)

  let eval_value_vec ~(size: int) (map: interval_map) (e: CoeffVector.t) : I.t =
    let terms, const = split_row size e in
    List.fold_left (fun acc (i, a) -> I.add acc (scale_iv a (get_iv map i)))
      (I.of_const (q_of_mpqf const)) terms

  (** Reduces a value-form vector modulo the equality rows by eliminating the
      pivot variable of every row (Gaussian reduction). The result denotes the
      same value on every state satisfying the equalities. *)
  let reduce_value_vec ~(size: int) (m: affeq) (e: CoeffVector.t) : CoeffVector.t =
    List.fold_left (fun e r ->
        match CoeffVector.find_first_non_zero r with
        | Some (p, pv) when p < size ->
          let c = CoeffVector.nth e p in
          if c =: Mpqf.zero then e
          else
            (* zero-form of the row: sum r(i) * x_i - rhs = 0 *)
            let zrow = CoeffVector.set_nth r size (Mpqf.neg (CoeffVector.nth r size)) in
            let lambda = c /: pv in
            CoeffVector.map2_f_preserves_zero (fun x y -> x -: lambda *: y) e zrow
        | _ -> e
      ) e (rows m)

  (** Evaluates a value-form vector using the refined intervals, both directly
      and after Gaussian reduction modulo the equality rows, and meets the two
      results. *)
  let eval_vec ~(size: int) (t: t) (refined: interval_map) (e: CoeffVector.t) : I.t =
    let direct = eval_value_vec ~size refined e in
    let reduced = reduce_value_vec ~size t.affeq e in
    let via_rows = eval_value_vec ~size refined reduced in
    match I.meet direct via_rows with
    | Some iv -> iv
    | None -> direct (* only happens on bottom states; any sound answer is fine *)

  (* ---------------------------------------------------------------------- *)
  (* Slack handling 

     A slack variable \(\beta\) stands for a linear expression. Its defining row is the equality that connects the two.
     For example, if infos records
     $ \beta = 2x+y+3 $
     then the defining row stored in the equality matrix is
     $ 2x+y-\beta=-3 $

     This is distinct from a bound on the slack. To represent 2x+y+3 <= 5, the state can keep the defining equality 
     and the interval bound \beta <= 5. saturate re-establishes the defining equality when it is known from infos but 
     may have been lost from the matrix during a join or widen.

     Transfer functions must account for possible new values of variables e.g. during assignment:
     suppose $\beta=x+y$, then execute [[x := x + 1]]. The old value held by $\beta$ is now $x_{new}+y-1$. 
     In the invertible-assignment branch, the code substitutes through the equality matrix and the slack’s info, 
     then renames the slack to match its new defining expression.
     For a non-invertible assignment such as x := y, slacks whose definitions mention x are removed before the new 
     assignment equality is added. A slack that does not depend on the assigned variable can keep its defining relationship.

  *)

  (** Defining row of a slack: [beta = sum iterms + iconst], in RHS form
      [sum iterms - beta = -iconst]. *)
  let defining_row (size: int) (beta: Var.t) (info: info) : CoeffVector.t =
    vec_of_terms size ((beta, Mpqf.mone) :: info.iterms) (Mpqf.neg info.iconst)

  (** Establishes the defining rows of all slacks of [other_infos] in [t].
      Returns [None] if this exposes an inconsistency (i.e. [t] is bottom).

      Even when [t] already knows a slack, its defining row may have been
      dropped by a previous hull (join/widen keep info and interval of slacks
      whose row the hull discards); defining rows are universally valid, so
      unconditionally re-establishing them is sound and a no-op whenever the
      row is already implied. *)
  let saturate ~(size: int) (t: t) (other_infos: info_map) : t option =
    VarMap.fold (fun beta info acc ->
        Option.bind acc (fun t ->
            match Matrix.rref_vec t.affeq (defining_row size beta info) with
            | None -> None
            | Some m -> Some { t with affeq = m; infos = VarMap.add beta info t.infos }
          )
      ) other_infos (Some t)

  (** [row_implied_by m r]: the RHS-form row [r] is a linear combination of the
      rows of the rref matrix [m] (i.e. the equality [r] is implied by [m]). *)
  let row_implied_by (m: affeq) (r: CoeffVector.t) : bool =
    let r' = List.fold_left (fun r row ->
        match CoeffVector.find_first_non_zero row with
        | Some (p, pv) ->
          let c = CoeffVector.nth r p in
          if c =: Mpqf.zero then r
          else CoeffVector.map2_f_preserves_zero (fun x y -> x -: (c /: pv) *: y) r row
        | None -> r
      ) r (rows m)
    in
    CoeffVector.is_zero_vec r'

  (** [matrix_implied_by m m']: all rows of [m'] are implied by [m]. *)
  let matrix_implied_by (m: affeq) (m': affeq) : bool =
    List.for_all (row_implied_by m) (rows m')

  (** Rows of [m] that are not implied by [joined]. *)
  let dropped_rows (m: affeq) (joined: affeq) : CoeffVector.t list =
    List.filter (fun r -> not (row_implied_by joined r)) (rows m)

  (** Rewrites an RHS-form row into an equivalent one over program dimensions
      only, substituting every slack by its defining linear form. Only valid on
      states where the defining equalities hold. *)
  let subst_slacks ~(size: int) (infos: info_map) (r: CoeffVector.t) : CoeffVector.t =
    let rec go r =
      let entries = CoeffVector.to_sparse_list r in
      match List.find_opt (fun (i, _) -> i < size && VarMap.mem (Var.to_t i) infos) entries with
      | None -> r
      | Some (i, a) ->
        let beta = Var.to_t i in
        let info = VarMap.find beta infos in
        (* r + a * defining_row zeroes the beta entry, since the defining row has -1 there *)
        let d = defining_row size beta info in
        go (CoeffVector.map2_f_preserves_zero (fun x y -> x +: a *: y) r d)
    in
    go r

  (* ---------------------------------------------------------------------- *)
  (* Pointwise interval map operations *)

  let join_intervals (a: interval_map) (b: interval_map) : interval_map =
    VarMap.merge (fun _ x y ->
        match x, y with
        | Some x, Some y ->
          let j = I.join x y in
          if I.is_top j then None else Some j
        | _ -> None
      ) a b

  let widen_intervals ?thresholds (a: interval_map) (b: interval_map) : interval_map =
    let widen = match thresholds with
      | Some (lower, upper) -> I.widen_thresholds ~lower ~upper
      | None -> I.widen
    in
    VarMap.filter_map (fun k x ->
        let w = widen x (get_iv b k) in
        if I.is_top w then None else Some w
      ) a

  let narrow_intervals (a: interval_map) (b: interval_map) : interval_map =
    VarMap.merge (fun _ x y ->
        match x, y with
        | Some x, Some y -> Some (I.narrow x y)
        | Some x, None -> Some x
        | None, Some y -> Some y
        | None, None -> None
      ) a b

  let meet_intervals (a: interval_map) (b: interval_map) : interval_map option =
    try
      Some (VarMap.merge (fun _ x y ->
          match x, y with
          | Some x, Some y ->
            (match I.meet x y with
             | Some r -> Some r
             | None -> raise Bottom)
          | Some x, None -> Some x
          | None, Some y -> Some y
          | None, None -> None
        ) a b)
    with Bottom -> None

  (** [leq_intervals refined_a b]: all interval constraints of [b] are implied
      by the (already reduced) intervals of [a]. *)
  let leq_intervals (refined_a: interval_map) (b: interval_map) : bool =
    VarMap.for_all (fun k iv -> I.leq (get_iv refined_a k) iv) b

  (* ---------------------------------------------------------------------- *)
  (* Forgetting variables *)

  (**
     [rem_rows_containing_var affeq var] uses [Matrix.reduce_col] and [Matrix.remove_zero_rows]
     to existentially eliminate the variable from the matrix.
  *)
  let rem_rows_containing_var (affeq : affeq) (var : Var.t) : affeq =
    if Matrix.is_empty affeq then affeq
    else
      Matrix.remove_zero_rows @@ Matrix.reduce_col affeq (Var.to_int var)

  (**
     [forget_vars vars t] forgets a list of variables in the polyhedron:
     the variables are existentially eliminated from the equality rows and their
     interval bindings are dropped. Slacks whose info mentions a forgotten
     variable lose both their info and their interval (their remaining rows are
     still valid combinations). Callers that can change the environment should
     remove such dependent slack dimensions entirely.
  *)
  let forget_vars (vars: Var.t list) (t: t) =
    let dependent = VarMap.filter (fun _ info ->
        List.exists (fun (v, _) -> List.exists (Var.equal v) vars) info.iterms
      ) t.infos
    in
    let new_affeq = List.fold_left rem_rows_containing_var t.affeq vars in
    let new_intervals = List.fold_left (flip VarMap.remove) t.intervals vars in
    let new_intervals = VarMap.fold (fun beta _ acc -> VarMap.remove beta acc) dependent new_intervals in
    let new_infos = VarMap.filter (fun beta _ -> not (VarMap.mem beta dependent)) t.infos in
    let new_infos = List.fold_left (flip VarMap.remove) new_infos vars in
    { affeq = new_affeq; intervals = new_intervals; infos = new_infos }

  (**
     [forget_var var t] forgets a single variable using [forget_vars].
  *)
  let forget_var (var : Var.t) (t: t) : t =
    forget_vars [var] t

  (* ---------------------------------------------------------------------- *)
  (* Apron dimension changes *)

  let shift_info (f: Var.t -> Var.t) (info: info) : info =
    { info with iterms = List.map (fun (v, c) -> (f v, c)) info.iterms }

  (**
     [dim_add] Apron dimension change
  *)
  let dim_add (ch: Apron.Dim.change) (t: t) =
    let shift_index_add (old_index : Var.t) (occ_cols : (int * int) list) : Var.t =
      (* find all entries that are less or equal to old_index in occ_cols, and count them (=k), then new_index = old_index + k *)
      let k = List.fold_left (fun acc (index, count) -> if index <= (Var.to_int old_index) then acc + count else acc) 0 occ_cols in
      Var.to_t ((Var.to_int old_index) + k)
    in
    let new_affeq = Matrix.dim_add ch t.affeq in
    let list = Array.to_list ch.dim in
    let grouped_indices = List.group Int.compare list in
    let occ_cols = List.map (fun group -> ((List.hd group, List.length group))) grouped_indices in
    (* Approach from listMatrix.ml: add_empty_columns; Example: cols_list = [1; 3; 3; 5] -> grouped_indices = [[1]; [3; 3]; [5]] -> occ_cols = [(1, 1); (3, 2); (5, 1)] *)
    let shift v = shift_index_add v occ_cols in
    let new_infos =
      VarMap.fold (fun var info acc -> VarMap.add (shift var) (shift_info shift info) acc) t.infos VarMap.empty in
    let new_intervals =
      VarMap.fold (fun var interval acc -> VarMap.add (shift var) interval acc) t.intervals VarMap.empty in
    {affeq = new_affeq; infos = new_infos; intervals = new_intervals}

  (**
     [dim_remove] Apron dimension change
  *)
  let dim_remove (ch: Apron.Dim.change) (t: t) =
    let shift_index_remove (old_index : Var.t) (dim_list : int list) : Var.t =
      let k = List.fold_left (fun acc index -> if index < (Var.to_int old_index) then acc + 1 else acc) 0 dim_list in
      Var.to_t ((Var.to_int old_index) - k)
    in
    let new_affeq = Matrix.dim_remove ch t.affeq in
    let dim_list = Array.to_list ch.dim in
    let new_t = forget_vars (List.map Var.to_t dim_list) t in
    let dim_list = List.sort_uniq Int.compare dim_list in (* remove duplicates *)
    let shift v = shift_index_remove v dim_list in
    let new_infos =
      VarMap.fold (fun var info acc -> VarMap.add (shift var) (shift_info shift info) acc) new_t.infos VarMap.empty in
    let new_intervals =
      VarMap.fold (fun var interval acc -> VarMap.add (shift var) interval acc) new_t.intervals VarMap.empty in
    {affeq = new_affeq; infos = new_infos; intervals = new_intervals}

  (* ---------------------------------------------------------------------- *)
  (* Structural invariant checking (for tests and debugging) *)

  (** Checks the structural invariants of a state and returns a human-readable
      message per violation; an empty list means all invariants hold.
      [is_slack] classifies dimensions into slack and program dimensions.

      Checked invariants:
      - the matrix is in rref: rows of length [size + 1] without zero or
        contradiction rows, pivots are 1 with strictly increasing positions,
        and every pivot column is zero in all other rows;
      - infos are keyed by slack dimensions and mention only in-range program
        dimensions, with strictly sorted, nonzero terms;
      - the defining row of every slack is implied by the matrix;
      - intervals are keyed by in-range dimensions and are non-empty. *)
  let invariant_violations ~(size: int) ~(is_slack: Var.t -> bool) (t: t) : string list =
    let violations = ref [] in
    let add fmt = Printf.ksprintf (fun s -> violations := s :: !violations) fmt in
    let rs = rows t.affeq in
    List.iteri (fun i r ->
        if CoeffVector.length r <> size + 1 then
          add "row %d has length %d, expected %d" i (CoeffVector.length r) (size + 1)
      ) rs;
    let pivots = List.mapi (fun i r -> (i, CoeffVector.find_first_non_zero r)) rs in
    List.iter (fun (i, p) ->
        match p with
        | None -> add "row %d is a zero row" i
        | Some (p, pv) ->
          if p >= size then add "row %d is a contradiction row" i
          else if pv <>: Mpqf.one then add "row %d has pivot value %s, expected 1" i (Mpqf.to_string pv)
      ) pivots;
    let rec check_increasing = function
      | (i, Some (p, _)) :: (((_, Some (q, _)) :: _) as rest) ->
        if p >= q then add "pivot of row %d is not left of the next row's pivot" i;
        check_increasing rest
      | _ :: rest -> check_increasing rest
      | [] -> ()
    in
    check_increasing pivots;
    List.iter (fun (i, p) ->
        match p with
        | Some (p, _) when p < size ->
          List.iteri (fun j r ->
              if j <> i && CoeffVector.nth r p <>: Mpqf.zero then
                add "pivot column %d of row %d is nonzero in row %d" p i j
            ) rs
        | _ -> ()
      ) pivots;
    VarMap.iter (fun beta info ->
        let b = Var.string_of beta in
        if Var.to_int beta >= size then add "info key %s is out of range" b;
        if not (is_slack beta) then add "info key %s is not a slack dimension" b;
        if info.iterms = [] then add "info of %s has no terms" b;
        List.iter (fun (v, c) ->
            if Var.to_int v >= size then add "info of %s mentions out-of-range dimension %s" b (Var.string_of v);
            if is_slack v then add "info of %s mentions slack dimension %s" b (Var.string_of v);
            if c =: Mpqf.zero then add "info of %s has a zero coefficient for %s" b (Var.string_of v)
          ) info.iterms;
        let rec sorted = function
          | (v, _) :: (((w, _) :: _) as rest) -> Var.compare v w < 0 && sorted rest
          | _ -> true
        in
        if not (sorted info.iterms) then add "info terms of %s are not strictly sorted" b;
        if Var.to_int beta < size && not (row_implied_by t.affeq (defining_row size beta info)) then
          add "defining row of %s is not implied by the matrix" b
      ) t.infos;
    VarMap.iter (fun v iv ->
        if Var.to_int v >= size then add "interval on out-of-range dimension %s" (Var.string_of v)
        else
          match I.bounds iv with
          | Some l, Some u when Q.compare l u > 0 -> add "interval of %s is empty: %s" (Var.string_of v) (I.show iv)
          | _ -> ()
      ) t.intervals;
    List.rev !violations

  (* ---------------------------------------------------------------------- *)
  (* Printing *)

  let string_of_interval_map (m: interval_map) =
    VarMap.bindings m
    |> List.map (fun (var, interval) -> Var.string_of var ^ " -> " ^ I.show interval)
    |> String.concat "; "

  let string_of_info (e: info) =
    let terms =
      e.iterms
      |> List.map (fun (v, c) -> Mpqf.to_string c ^ "*" ^ Var.string_of v)
      |> String.concat " + "
    in
    if e.iconst =: Mpqf.zero then terms
    else terms ^ " + " ^ Mpqf.to_string e.iconst

  let string_of_infos (infos: info_map) =
    VarMap.bindings infos
    |> List.map (fun (var, info) -> Var.string_of var ^ " -> " ^ string_of_info info)
    |> String.concat "; "

  let string_of (t: t) =
    "{ affeq = " ^ GobPretty.sprint Matrix.pretty t.affeq
    ^ "; intervals = [" ^ string_of_interval_map t.intervals ^ "]"
    ^ "; slacks = [" ^ string_of_infos t.infos ^ "] }"
end
