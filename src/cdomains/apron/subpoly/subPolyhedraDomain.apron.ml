(** OCaml implementation of the subpolyhedra domain.

    Subpolyhedra is the reduced product of a linear-equality domain and an
    interval environment. Linear inequalities are represented with slack
    variables: for a constraint [e <= c] over program variables, a slack
    dimension [beta] with [beta = e] (a row of the equality component) and
    [beta ∈ [-inf, c]] (interval component) is introduced.

    Slack variables live in the Apron environment under canonical names derived
    from their normalized linear form, so the same constraint template maps to
    the same dimension in every state. Reduction between the two components is
    performed by interval propagation through the equality rows (a cheap, sound
    approximation of the Simplex-based reduction of the paper), optionally
    strengthened by basis exploration with the linear explorer (rho_BE with
    delta_L of the paper) when [ana.subpoly.basis-exploration] is enabled.

    @see <https://www.microsoft.com/en-us/research/wp-content/uploads/2011/06/subpolyhedra.pdf>  Subpolyhedra. *)

open GoblintCil
open Pretty
module M = Messages
open GobApron
open SubPolyhedraCore

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
    let to_int = Fun.id
    let to_t = Fun.id
  end
  module SubPolyDomain = SubPoly(Int)(RationalInterval)
  module P = SubPolyDomain
  module Vector = P.CoeffVector
  module Matrix = P.Matrix
  include SharedFunctions.VarManagementOps (SubPolyDomain)
  include RatOps.ConvenienceOps (Mpqf)

  let dim_add = SubPolyDomain.dim_add

  let size t = Environment.size t.env

  (** Whether reduction additionally explores bases with the linear explorer
      (rho_BE with delta_L of the paper). The config may be unavailable (e.g.
      in unit tests, which call the core directly instead); treat that as
      disabled. *)
  let basis_exploration_enabled =
    lazy (try GobConfig.get_bool "ana.subpoly.basis-exploration" with _ -> false)

  (** Reduction between the equality and interval components: fixpoint
      interval propagation, plus basis exploration when enabled. *)
  let reduce ~size (d: SubPolyDomain.t) =
    P.reduce ~size ~explore:(Lazy.force basis_exploration_enabled) d

  (* ---------------------------------------------------------------------- *)
  (* Slack variables.

     Slack variables are named canonically after the normalized linear form
     they represent, with a prefix that sorts after all program variables, so
     that rref pivoting prefers program dimensions. *)

  let slack_prefix = "~s"
  let tmp_prefix = "~tmp"

  let is_slack_var (v: Var.t) = String.starts_with (Var.to_string v) "~"

  (** Terms keyed by environment variables instead of dimensions; this
      representation survives environment changes. *)
  let vterms_of_dims env (terms: (int * Mpqf.t) list) : (Var.t * Mpqf.t) list =
    List.map (fun (d, c) -> (Environment.var_of_dim env d, c)) terms

  let dims_of_vterms env (ts: (Var.t * Mpqf.t) list) : (int * Mpqf.t) list =
    List.map (fun (v, c) -> (Environment.dim_of_var env v, c)) ts
    |> List.sort (fun (i, _) (j, _) -> Stdlib.compare i j)

  let slack_name (vts: (Var.t * Mpqf.t) list) (iconst: Mpqf.t) =
    let sorted = List.sort (fun (v1, _) (v2, _) -> String.compare (Var.to_string v1) (Var.to_string v2)) vts in
    let term_str (v, c) = Mpqf.to_string c ^ "*" ^ Var.to_string v in
    slack_prefix ^ "(" ^ String.concat "+" (List.map term_str sorted) ^ "|" ^ Mpqf.to_string iconst ^ ")"

  let eq_dterms (a: (int * Mpqf.t) list) (b: (int * Mpqf.t) list) =
    try List.for_all2 (fun (i, c) (j, d) -> i = j && Mpqf.equal c d) a b
    with Invalid_argument _ -> false

  (** Slack variables whose info mentions any of the given variables. *)
  let dependent_slack_vars (t: t) (vs: Var.t list) : Var.t list =
    match t.d with
    | None -> []
    | Some d ->
      let dims = List.filter_map (fun v ->
          if Environment.mem_var t.env v then Some (Environment.dim_of_var t.env v) else None
        ) vs
      in
      if dims = [] then []
      else
        P.VarMap.fold (fun beta (info: P.info) acc ->
            if List.exists (fun (v, _) -> List.mem v dims) info.P.iterms then
              Environment.var_of_dim t.env beta :: acc
            else acc
          ) d.P.infos []

  (* ---------------------------------------------------------------------- *)
  (* Conversion of Apron tree expressions to value-form coefficient vectors:
     a vector [v] of length [size + 1] denotes [sum v(i) * x_i + v(size)]. *)

  let to_constant_opt v = match Vector.find_first_non_zero v with
    | None -> Some Mpqf.zero
    | Some (i, value) when i = (Vector.length v) - 1 -> Some value
    | _ -> None

  let get_coeff_vec (t: t) texp =
    let open Apron.Texpr1 in
    let exception NotLinear in
    let zero_vec = Vector.zero_vec @@ Environment.size t.env + 1 in
    let neg v = Vector.map_f_preserves_zero Mpqf.neg v in
    let rec convert_texpr = function
      | Cst x ->
        let of_union = function
          | Coeff.Interval _ -> raise NotLinear
          | Coeff.Scalar (Scalar.Float x) -> Mpqf.of_float x
          | Coeff.Scalar (Scalar.Mpqf x) -> x
          | Coeff.Scalar (Scalar.Mpfrf x) -> Mpfr.to_mpq x
        in
        Vector.set_nth zero_vec ((Vector.length zero_vec) - 1) (of_union x)
      | Var x ->
        Vector.set_nth zero_vec (Environment.dim_of_var t.env x) Mpqf.one
      | Unop (Neg, e, _, _) -> neg @@ convert_texpr e
      | Unop (Cast, e, _, _) -> convert_texpr e (*Ignore since casts in apron are used for floating point nums and rounding in contrast to CIL casts*)
      | Unop (Sqrt, e, _, _) -> raise NotLinear
      | Binop (Add, e1, e2, _, _) ->
        Vector.map2_f_preserves_zero (+:) (convert_texpr e1) (convert_texpr e2)
      | Binop (Sub, e1, e2, _, _) ->
        Vector.map2_f_preserves_zero (+:) (convert_texpr e1) (neg @@ convert_texpr e2)
      | Binop (Mul, e1, e2, _, _) ->
        let v1 = convert_texpr e1 in
        let v2 = convert_texpr e2 in
        begin match to_constant_opt v1, to_constant_opt v2 with
          | _, Some c -> Vector.apply_with_c_f_preserves_zero ( *:) c v1
          | Some c, _ -> Vector.apply_with_c_f_preserves_zero ( *:) c v2
          | _, _ -> raise NotLinear
        end
      | Binop _ -> raise NotLinear
    in
    try
      Some (convert_texpr texp)
    with NotLinear -> None
end

module ExpressionBounds: (SharedFunctions.ConvBounds with type t = VarManagement.t) =
struct
  include VarManagement

  let bound_texpr (t: t) texpr =
    match t.d with
    | None -> (None, None)
    | Some d ->
      match get_coeff_vec t (Texpr1.to_expr texpr) with
      | None -> (None, None)
      | Some v ->
        let sz = size t in
        match reduce ~size:sz d with
        | None -> (None, None)
        | Some refined ->
          let iv = P.eval_vec ~size:sz d refined v in
          let l, u = RationalInterval.bounds iv in
          (Option.map (fun q -> Z.cdiv (Q.num q) (Q.den q)) l,
           Option.map (fun q -> Z.fdiv (Q.num q) (Q.den q)) u)

  let bound_texpr d texpr1 =
    let res = bound_texpr d texpr1 in
    (if M.tracing then
       match res with
       | Some min, Some max -> M.tracel "bounds" "min: %a max: %a" GobZ.pretty min GobZ.pretty max
       | _ -> ()
    );
    res
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

  type var = V.t

  let name () = "subpoly"

  let to_yojson _ = failwith "SubPolyhedraDomain.to_yojson: not implemented"

  (* ---------------------------------------------------------------------- *)
  (* Pretty printing *)

  let show (t: t) =
    match t.d with
    | None -> "⊥ (env: " ^ Environment.show t.env ^ ")"
    | Some d ->
      let var_name dim = Var.to_string (Environment.var_of_dim t.env dim) in
      let sz = size t in
      let term_str (dim, c) =
        (if Mpqf.equal c Mpqf.one then "" else if Mpqf.equal c Mpqf.mone then "-" else Mpqf.to_string c ^ "*")
        ^ var_name dim
      in
      let terms_str terms = String.concat " + " (List.map term_str terms) in
      let row_str r =
        let terms, rhs = P.split_row sz r in
        terms_str terms ^ " = " ^ Mpqf.to_string rhs
      in
      let rows_str = List.map row_str (P.rows d.P.affeq) in
      let iv_str (dim, iv) = var_name dim ^ " ∈ " ^ RationalInterval.show iv in
      let ivs_str = List.map iv_str (P.VarMap.bindings d.P.intervals) in
      let info_str (beta, (info: P.info)) =
        var_name beta ^ " := " ^ terms_str info.P.iterms
        ^ (if Mpqf.equal info.P.iconst Mpqf.zero then "" else " + " ^ Mpqf.to_string info.P.iconst)
      in
      let infos_str = List.map info_str (P.VarMap.bindings d.P.infos) in
      if rows_str = [] && ivs_str = [] && infos_str = [] then "⊤"
      else
        "{ rows: [" ^ String.concat "; " rows_str
        ^ "]; intervals: [" ^ String.concat "; " ivs_str
        ^ "]; slacks: [" ^ String.concat "; " infos_str ^ "] }"

  let pretty () (x: t) = text (show x)

  let pretty_diff () ((x, y): t * t) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  let printXml (f: _ BatInnerIO.output) (x: t) = BatPrintf.fprintf f "<value>\n%s</value>\n" (XmlUtil.escape (show x))

  (* ---------------------------------------------------------------------- *)
  (* Basic lattice elements *)

  let top () = { d = Some (P.empty ()); env = empty_env }
  let is_top (t: t) = GobOption.exists P.is_empty t.d
  let is_bot = is_bot_env

  (** Bottom check via reduction: detects interval/equality contradictions. *)
  let check_bot (t: t) : t =
    match t.d with
    | None -> t
    | Some d ->
      if Option.is_none (reduce ~size:(size t) d) then bot_env else t

  (** Reduction: materializes the interval refinement implied by the equality
      rows into the state, so the information survives the projection of the
      variable the constraint was stated on (e.g. [g#in] of a global read). *)
  let reduce_intervals (t: t) : t =
    match t.d with
    | None -> t
    | Some d ->
      match reduce ~size:(size t) d with
      | None -> bot_env
      | Some m -> { t with d = Some { d with P.intervals = m } }

  (* ---------------------------------------------------------------------- *)
  (* Structural invariant checking *)

  (** Structural invariant violations of a state; an empty list means the
      state is healthy. Combines the core checks (rref shape, defining rows
      implied, slack metadata well-formed, intervals non-empty) with the
      environment-level invariant that every slack dimension is canonically
      named after its info. Used by the unit tests, and asserted after every
      operation when [dbg.subpoly.check-invariants] is enabled. *)
  let check_invariants (t: t) : string list =
    match t.d with
    | None -> []
    | Some d ->
      let sz = size t in
      let is_slack dim = is_slack_var (Environment.var_of_dim t.env dim) in
      let core = P.invariant_violations ~size:sz ~is_slack d in
      let naming =
        P.VarMap.fold (fun beta (info: P.info) acc ->
            if List.exists (fun (v, _) -> v >= sz) info.P.iterms || beta >= sz then acc (* out of range, reported by core *)
            else
              let name = Var.to_string (Environment.var_of_dim t.env beta) in
              let expected = slack_name (vterms_of_dims t.env info.P.iterms) info.P.iconst in
              if String.equal name expected then acc
              else Printf.sprintf "slack %s is not canonically named (expected %s)" name expected :: acc
          ) d.P.infos []
      in
      core @ naming

  (** Whether to assert the structural invariants after every operation.
      The config may be unavailable (e.g. in unit tests, which call
      [check_invariants] directly instead); treat that as disabled. *)
  let invariant_checking_enabled =
    lazy (try GobConfig.get_bool "dbg.subpoly.check-invariants" with _ -> false)

  let verify (op: string) (res: t) : t =
    if Lazy.force invariant_checking_enabled then begin
      match check_invariants res with
      | [] -> ()
      | vs ->
        failwith (Printf.sprintf "subpoly invariant violation after %s: %s; state: %s"
                    op (String.concat "; " vs) (show res))
    end;
    res

  (* ---------------------------------------------------------------------- *)
  (* Threshold widening.

     Reuses the program-constant thresholds collected by [WideningThresholds],
     like the Apron domains, gated on the same option. The config may be
     unavailable (e.g. in unit tests); treat that as disabled. *)

  let widening_thresholds =
    ResettableLazy.from_fun (fun () ->
        let enabled = try GobConfig.get_bool "ana.apron.threshold_widening" with _ -> false in
        if not enabled then None
        else
          let lts = ResettableLazy.force WideningThresholds.lower_thresholds in
          let uts = ResettableLazy.force WideningThresholds.upper_thresholds in
          let lower b = Option.map Q.of_bigint (WideningThresholds.Thresholds.find_last_opt (fun x -> Q.geq b (Q.of_bigint x)) lts) in
          let upper b = Option.map Q.of_bigint (WideningThresholds.Thresholds.find_first_opt (fun x -> Q.leq b (Q.of_bigint x)) uts) in
          Some (lower, upper))

  (** [interval_widen old new]: interval widening, with thresholds if enabled. *)
  let interval_widen (old_iv: RationalInterval.t) (new_iv: RationalInterval.t) : RationalInterval.t =
    match ResettableLazy.force widening_thresholds with
    | Some (lower, upper) -> RationalInterval.widen_thresholds ~lower ~upper old_iv new_iv
    | None -> RationalInterval.widen old_iv new_iv

  (* ---------------------------------------------------------------------- *)
  (* Slack templates.

     [constrain_template t vts iv] constrains the normalized linear form
     [sum vts] to lie within [iv]: existing slacks for the same form are
     refined (their info constant is taken into account); otherwise a fresh
     slack with defining row [sum vts - beta = 0] is created. *)

  type template_combine =
    | TMeet  (* refine: meet, bottom on empty intersection *)
    | TWiden (* widening recovery: only ever grow the existing interval *)

  let constrain_template ?(create = true) (combine: template_combine) (t: t) (vts: (Var.t * Mpqf.t) list) (iv: RationalInterval.t) : t =
    match t.d with
    | None -> t
    | Some d ->
      let dterms = dims_of_vterms t.env vts in
      let matches = P.VarMap.filter (fun _ (info: P.info) -> eq_dterms info.P.iterms dterms) d.P.infos in
      if not (P.VarMap.is_empty matches) then begin
        let sz = size t in
        let exception Bot in
        try
          let d' = P.VarMap.fold (fun beta (info: P.info) d ->
              (* beta = sum vts + iconst, so sum vts ∈ iv iff beta ∈ iv + iconst *)
              let cand = RationalInterval.add_const (P.q_of_mpqf info.P.iconst) iv in
              let current = P.get_iv d.P.intervals beta in
              let new_iv = match combine with
                | TMeet ->
                  (match RationalInterval.meet current cand with
                   | Some r -> r
                   | None -> raise Bot)
                | TWiden ->
                  (* the candidate already covers both operands; widen against the
                     current interval (if any) so chains stay ascending *)
                  if RationalInterval.is_top current then cand
                  else interval_widen current cand
              in
              (* re-establish the defining row: a preceding hull may have dropped
                 it (e.g. the old operand of a widening did not contain the slack) *)
              match Matrix.rref_vec d.P.affeq (P.defining_row sz beta info) with
              | None -> raise Bot (* defining rows are semantically true, so the state is empty *)
              | Some m -> P.set_intv beta new_iv { d with P.affeq = m }
            ) matches d
          in
          { t with d = Some d' }
        with Bot -> bot_env
      end
      else if not create || RationalInterval.is_top iv then t
      else begin
        let sv = Var.of_string (slack_name vts Mpqf.zero) in
        if Environment.mem_var t.env sv then
          (* A stale dimension with this name exists (e.g. left behind by a
             dimension change); reusing it would be unsound, skipping the
             constraint is sound. *)
          t
        else
          let t1 = add_vars t [sv] in
          match t1.d with
          | None -> t1
          | Some d1 ->
            let sz = size t1 in
            let beta = Environment.dim_of_var t1.env sv in
            let dterms = dims_of_vterms t1.env vts in
            let row = P.vec_of_terms sz ((beta, Mpqf.mone) :: dterms) Mpqf.zero in
            match Matrix.rref_vec d1.P.affeq row with
            | None -> bot_env (* unreachable: beta is a fresh dimension *)
            | Some m ->
              let d1 = { d1 with P.affeq = m } in
              let d1 = P.set_intv beta iv d1 in
              let d1 = P.set_info beta { P.iterms = dterms; P.iconst = Mpqf.zero } d1 in
              { t1 with d = Some d1 }
      end

  (* ---------------------------------------------------------------------- *)
  (* Variable management with slack cleanup.

     Whenever a program variable disappears, all slack variables whose info
     mentions it must disappear with it: their interval would otherwise keep
     constraining a stale linear form under a canonical name. Before they are
     dropped, their constraint is re-expressed over surviving variables where
     the equality rows allow it (e.g. [total >= seed] surviving a function
     return as [RETURN >= seed#arg]). *)

  (** [rescue_dependent_slacks t removed dep] transfers the interval of every
      dependent slack in [dep] to a template over surviving variables, if the
      equality rows (projected onto the surviving dimensions) determine the
      slack as a linear form of surviving program variables. [removed] are the
      disappearing program variables; both lists still exist in [t]. *)
  let rescue_dependent_slacks (t: t) (removed: Var.t list) (dep: Var.t list) : t =
    match t.d with
    | None -> t
    | Some d ->
      if dep = [] then t
      else begin
        let sz = size t in
        let env = t.env in
        let dim_of v = Environment.dim_of_var env v in
        let removed_dims = List.filter_map (fun v ->
            if Environment.mem_var env v then Some (dim_of v) else None
          ) removed
        in
        let project m dims =
          List.fold_left (fun m x -> Matrix.remove_zero_rows (Matrix.reduce_col m x)) m dims
        in
        let m_base = project d.P.affeq removed_dims in
        (* collect candidates first: applying them adds dimensions, which would
           invalidate the dimension indices used here *)
        let candidates =
          List.concat_map (fun beta_v ->
              let beta = dim_of beta_v in
              let iv = P.get_iv d.P.intervals beta in
              if RationalInterval.is_top iv then []
              else begin
                (* project out all other slacks so any remaining row containing
                   [beta] defines it over surviving program variables only *)
                let other_slacks = List.filter_map (fun v ->
                    if is_slack_var v && dim_of v <> beta then Some (dim_of v) else None
                  ) (vars t)
                in
                let m = project m_base other_slacks in
                List.filter_map (fun r ->
                    let terms, rhs = P.split_row sz r in
                    match List.assoc_opt beta terms with
                    | None -> None
                    | Some c ->
                      match List.remove_assoc beta terms with
                      | [] -> None
                      | others ->
                        (* beta = (rhs - sum others)/c and beta ∈ iv, normalized
                           to leading coefficient 1 *)
                        let f_terms = List.map (fun (i, o) -> (i, Mpqf.neg o /: c)) others in
                        let f_iv = RationalInterval.add_const (Q.neg (P.q_of_mpqf (rhs /: c))) iv in
                        let (_, p) = List.hd f_terms in
                        let t0 = List.map (fun (i, ci) -> (i, ci /: p)) f_terms in
                        let iv0 = RationalInterval.scale (P.q_of_mpqf (Mpqf.one /: p)) f_iv in
                        Some (vterms_of_dims env t0, iv0)
                  ) (P.rows m)
              end
            ) dep
        in
        List.fold_left (fun t (vts, iv0) ->
            match vts, t.d with
            | _, None -> t
            | [(v, _)], Some d when Environment.mem_var t.env v ->
              let dim = Environment.dim_of_var t.env v in
              (match RationalInterval.meet (P.get_iv d.P.intervals dim) iv0 with
               | None -> bot_env
               | Some iv' -> { t with d = Some (P.set_intv dim iv' d) })
            | [_], Some _ -> t
            | _, Some _ -> constrain_template TMeet t vts iv0
          ) t candidates
      end

  let remove_vars (t: t) vars =
    let dep = dependent_slack_vars t vars in
    let t = rescue_dependent_slacks t vars dep in
    VarManagement.remove_vars t (dep @ vars)

  let remove_vars t vars = verify "remove_vars" (remove_vars t vars)

  let remove_vars_with (t: t) vars =
    let t' = remove_vars t vars in
    t.d <- t'.d;
    t.env <- t'.env

  let remove_filter (t: t) f =
    let to_remove = List.filter f (vars t) in
    remove_vars t to_remove

  let remove_filter_with (t: t) f =
    let t' = remove_filter t f in
    t.d <- t'.d;
    t.env <- t'.env

  let keep_filter (t: t) f =
    match t.d with
    | None -> t
    | Some _ ->
      let removed_prog = List.filter (fun v -> not (is_slack_var v) && not (f v)) (vars t) in
      let t = remove_vars t removed_prog in
      (* drop ghost slacks without info: their dimension cannot be interpreted *)
      match t.d with
      | None -> t
      | Some d ->
        let ghosts = List.filter (fun v ->
            is_slack_var v && not (P.VarMap.mem (Environment.dim_of_var t.env v) d.P.infos)
          ) (vars t)
        in
        if ghosts = [] then t else VarManagement.remove_vars t ghosts

  let keep_filter t f = verify "keep_filter" (keep_filter t f)

  let keep_vars (t: t) vs =
    keep_filter t (fun v -> List.mem v vs)

  let forget_vars (t: t) vars =
    if vars = [] || is_bot_env t then t
    else
      let dep = dependent_slack_vars t vars in
      (* the rescued templates only mention surviving variables, whose values
         a havoc of [vars] does not change *)
      let t = rescue_dependent_slacks t vars dep in
      let t = VarManagement.remove_vars t dep in
      match t.d with
      | None -> t
      | Some d ->
        let dims = List.filter_map (fun v ->
            if Environment.mem_var t.env v then Some (Environment.dim_of_var t.env v) else None
          ) vars
        in
        { t with d = Some (P.forget_vars dims d) }

  let forget_vars t vars =
    let res = forget_vars t vars in
    if M.tracing then M.tracel "ops" "forget_vars %s -> %s" (show t) (show res);
    verify "forget_vars" res

  let forget_var (t: t) (v: V.t) = forget_vars t [v]

  (** Removes slack dimensions that carry no interval information. *)
  let gc_slacks (t: t) =
    match t.d with
    | None -> t
    | Some d ->
      let to_remove = List.filter (fun v ->
          is_slack_var v &&
          (match P.VarMap.find_opt (Environment.dim_of_var t.env v) d.P.intervals with
           | None -> true
           | Some iv -> RationalInterval.is_top iv)
        ) (vars t)
      in
      if to_remove = [] then t else VarManagement.remove_vars t to_remove

  (** Re-establishes the defining rows of all slacks known to [t]. The hull in
      join/widen keeps interval and info of a slack even when it drops its
      defining row from the matrix; without the row the interval is dead weight
      for propagation and queries. Defining rows are universally valid, so
      re-adding them is sound (and a no-op when already implied). *)
  let restore_defining_rows (t: t) : t =
    match t.d with
    | None -> t
    | Some d ->
      match P.saturate ~size:(size t) d d.P.infos with
      | None -> bot_env
      | Some d -> { t with d = Some d }

  (* ---------------------------------------------------------------------- *)
  (* Lattice operations *)

  (** Union of two slack info maps; canonical naming guarantees that a shared
      key carries the same info on both sides. *)
  let union_infos i1 i2 = P.VarMap.union (fun _ x _ -> Some x) i1 i2

  (** Saturates a core state with the slack infos of the other operand and
      aligns it to [sup_env]. Returns [None] if the state turns out bottom. *)
  let saturated_pair (a: t) (b: t) =
    let sup_env = Environment.lce a.env b.env in
    let a = dimchange2_add a sup_env in
    let b = dimchange2_add b sup_env in
    let sz = Environment.size sup_env in
    let da = Option.get a.d in
    let db = Option.get b.d in
    (sup_env, sz, da, db)

  let meet (a: t) (b: t) =
    if is_bot_env a then a
    else if is_bot_env b then b
    else
      let (sup_env, sz, da, db) = saturated_pair a b in
      let all_infos = union_infos da.P.infos db.P.infos in
      match P.saturate ~size:sz da all_infos, P.saturate ~size:sz db all_infos with
      | None, _ | _, None -> bot_env
      | Some sa, Some sb ->
        match Matrix.rref_matrix sa.P.affeq sb.P.affeq with
        | None -> bot_env
        | Some m ->
          match P.meet_intervals sa.P.intervals sb.P.intervals with
          | None -> bot_env
          | Some ivs ->
            let infos = P.VarMap.union (fun _ x _ -> Some x) sa.P.infos sb.P.infos in
            check_bot { d = Some { P.affeq = m; P.intervals = ivs; P.infos = infos }; env = sup_env }

  let meet a b =
    let res = meet a b in
    if M.tracing then M.tracel "meet" "meet a: %s b: %s -> %s" (show a) (show b) (show res);
    res

  let meet a b = Timing.wrap "meet" (meet a) b

  let meet a b = verify "meet" (meet a b)

  let leq (a: t) (b: t) =
    if is_bot_env a then true
    else if is_bot_env b then false
    else if is_top b then true
    else
      let (_, sz, da, db) = saturated_pair a b in
      match P.saturate ~size:sz da (union_infos da.P.infos db.P.infos) with
      | None -> true (* a is bottom *)
      | Some sa ->
        match reduce ~size:sz sa with
        | None -> true (* a is bottom *)
        | Some ra ->
          P.matrix_implied_by sa.P.affeq db.P.affeq
          && P.leq_intervals ra db.P.intervals

  let leq a b =
    let res = leq a b in
    if M.tracing then M.tracel "leq" "leq a: %s b: %s -> %b" (show a) (show b) res;
    res

  let leq a b = Timing.wrap "leq" (leq a) b

  (** Candidates for recovery of dropped equalities: every row of [side] not
      implied by [joined] is rewritten over program dimensions (substituting
      slacks by their defining forms) and, if the resulting linear form is
      bounded in the opposite operand, yields a template constraint covering
      both operands.

      Returns the normalized form (environment-variable keyed, leading
      coefficient 1), the value the form has on [side] and its interval in the
      opposite operand. *)
  let recovery_candidates env sz (side: P.t) (opp: P.t) (refined_opp: P.interval_map) (joined: P.affeq) =
    P.dropped_rows side.P.affeq joined
    |> List.filter_map (fun r ->
        let r' = P.subst_slacks ~size:sz side.P.infos r in
        let terms, rhs = P.split_row sz r' in
        (* skip if any slack dimension remains (ghost slack without info) *)
        if List.exists (fun (dim, _) -> is_slack_var (Environment.var_of_dim env dim)) terms then None
        else
          match terms with
          | [] | [_] -> None (* single-variable information is covered by the interval component *)
          | (_, p) :: _ ->
            let t0 = List.map (fun (dim, c) -> (dim, c /: p)) terms in
            let rhs0 = rhs /: p in
            let vec = P.vec_of_terms sz t0 Mpqf.zero in
            let j = P.eval_vec ~size:sz opp refined_opp vec in
            if RationalInterval.is_top j then None
            else Some (vterms_of_dims env t0, P.q_of_mpqf rhs0, j))

  let join (a: t) (b: t) =
    if is_bot_env a then b
    else if is_bot_env b then a
    else
      let (sup_env, sz, da, db) = saturated_pair a b in
      if P.equal da db then { d = Some da; env = sup_env }
      else
        let all_infos = union_infos da.P.infos db.P.infos in
        match P.saturate ~size:sz da all_infos, P.saturate ~size:sz db all_infos with
        | None, _ -> { d = Some db; env = sup_env } (* a is bottom *)
        | _, None -> { d = Some da; env = sup_env } (* b is bottom *)
        | Some sa, Some sb ->
          match reduce ~size:sz sa, reduce ~size:sz sb with
          | None, _ -> { d = Some db; env = sup_env }
          | _, None -> { d = Some da; env = sup_env }
          | Some ra, Some rb ->
            let joined_m =
              if Matrix.is_empty sa.P.affeq || Matrix.is_empty sb.P.affeq then Matrix.empty ()
              else if Matrix.equal sa.P.affeq sb.P.affeq then sa.P.affeq
              else Matrix.affine_hull sa.P.affeq sb.P.affeq
            in
            let intervals = P.join_intervals ra rb in
            let infos = P.VarMap.union (fun _ x _ -> Some x) sa.P.infos sb.P.infos in
            let res = { d = Some { P.affeq = joined_m; P.intervals = intervals; P.infos = infos }; env = sup_env } in
            (* recovery of dropped equalities *)
            let recs =
              recovery_candidates sup_env sz sa sb rb joined_m
              @ recovery_candidates sup_env sz sb sa ra joined_m
            in
            let res = List.fold_left (fun res (vts, c, j) ->
                let iv = RationalInterval.join j (RationalInterval.of_const c) in
                constrain_template TMeet res vts iv
              ) res recs
            in
            gc_slacks (restore_defining_rows res)

  let join a b =
    let res = join a b in
    if M.tracing then M.tracel "join" "join a: %s b: %s -> %s" (show a) (show b) (show res);
    res

  let join a b = Timing.wrap "join" (join a) b

  let join a b = verify "join" (join a b)

  let widen (a: t) (b: t) =
    if is_bot_env a then b
    else if is_bot_env b then a
    else
      let (sup_env, sz, da, db) = saturated_pair a b in
      if P.equal da db then { d = Some da; env = sup_env }
      else
        (* Repair the old side with its own infos only (re-establishes defining
           rows a previous hull may have dropped); saturating it with the new
           side's infos would endanger termination. *)
        match P.saturate ~size:sz da da.P.infos with
        | None -> { d = Some db; env = sup_env } (* a is bottom *)
        | Some da ->
          match P.saturate ~size:sz db (union_infos da.P.infos db.P.infos) with
          | None -> { d = Some da; env = sup_env } (* b is bottom *)
          | Some sb ->
            match reduce ~size:sz sb with
            | None -> { d = Some da; env = sup_env }
            | Some rb ->
              let widened_m =
                if Matrix.is_empty da.P.affeq || Matrix.is_empty sb.P.affeq then Matrix.empty ()
                else if Matrix.equal da.P.affeq sb.P.affeq then da.P.affeq
                else Matrix.affine_hull da.P.affeq sb.P.affeq
              in
              let intervals = P.widen_intervals ?thresholds:(ResettableLazy.force widening_thresholds) da.P.intervals rb in
              let infos = P.VarMap.union (fun _ x _ -> Some x) da.P.infos sb.P.infos in
              let res = { d = Some { P.affeq = widened_m; P.intervals = intervals; P.infos = infos }; env = sup_env } in
              (* recovery of equalities dropped from the old state, with interval widening *)
              let recs = recovery_candidates sup_env sz da sb rb widened_m in
              let res = List.fold_left (fun res (vts, c, j) ->
                  let iv = interval_widen (RationalInterval.of_const c) j in
                  constrain_template TWiden res vts iv
                ) res recs
              in
              gc_slacks (restore_defining_rows res)

  let widen a b =
    let res = widen a b in
    if M.tracing then M.tracel "widen" "widen a: %s b: %s -> %s" (show a) (show b) (show res);
    verify "widen" res

  let narrow (a: t) (b: t) =
    if is_bot_env a then a
    else if is_bot_env b then b
    else
      let a_env = a.env in
      let (sup_env, _, da, db) = saturated_pair a b in
      let narrowed = { da with P.intervals = P.narrow_intervals da.P.intervals db.P.intervals } in
      let res = { d = Some narrowed; env = sup_env } in
      (* project back onto a's dimensions: the narrowed state keeps a's rows *)
      let extra = List.filter (fun v -> not (Environment.mem_var a_env v)) (Environment.ivars_only sup_env) in
      if extra = [] then res else VarManagement.remove_vars res extra

  let narrow a b = verify "narrow" (narrow a b)

  let unify a b =
    meet a b

  let unify a b =
    let res = unify a b in
    if M.tracing then M.tracel "ops" "unify: %s %s -> %s" (show a) (show b) (show res);
    res

  (* ---------------------------------------------------------------------- *)
  (* Assignment *)

  (** Moves a slack dimension to a new name (value-preserving): introduces the
      new dimension, equates it with the old one, transfers interval and info,
      and projects the old dimension out. *)
  let move_slack (t: t) (old_v: Var.t) (new_v: Var.t) (info_opt: P.info option) : t =
    let t1 = add_vars t [new_v] in
    match t1.d with
    | None -> t1
    | Some d ->
      let sz = size t1 in
      let d_old = Environment.dim_of_var t1.env old_v in
      let d_new = Environment.dim_of_var t1.env new_v in
      let row = P.vec_of_terms sz [(d_new, Mpqf.one); (d_old, Mpqf.mone)] Mpqf.zero in
      match Matrix.rref_vec d.P.affeq row with
      | None -> bot_env (* unreachable: new_v is fresh *)
      | Some m ->
        let d = { d with P.affeq = m } in
        let d = match P.VarMap.find_opt d_old d.P.intervals with
          | Some iv -> P.set_intv d_new iv d
          | None -> d
        in
        let d = match info_opt with
          | Some info -> P.set_info d_new info d
          | None -> d
        in
        VarManagement.remove_vars { t1 with d = Some d } [old_v]

  (** Substitutes the inverted assignment [x := b] (value-form, invertible in
      [x]) into a slack info: [x_old = (x - sum_{i<>x} b_i x_i - b_c) / b_x]. *)
  let subst_info (t: t) (b: Vector.t) (dim_x: int) (info: P.info) : ((Var.t * Mpqf.t) list * Mpqf.t) option =
    match List.assoc_opt dim_x info.P.iterms with
    | None -> None (* info does not mention x *)
    | Some cx ->
      let sz = size t in
      let b0 = Vector.nth b dim_x in
      let scale = cx /: b0 in
      let base = List.filter (fun (v, _) -> v <> dim_x) info.P.iterms in
      let subst_terms =
        Vector.to_sparse_list b
        |> List.filter_map (fun (i, bi) ->
            if i = dim_x || i = sz then None
            else Some (i, Mpqf.neg (scale *: bi)))
      in
      let bc = Vector.nth b sz in
      (* normalize through a vector roundtrip: merges duplicates, drops zeros, sorts *)
      let merged, _ = P.split_row sz (P.vec_of_terms sz (base @ [(dim_x, scale)] @ subst_terms) Mpqf.zero) in
      Some (vterms_of_dims t.env merged, info.P.iconst -: scale *: bc)

  let assign_texpr (t: t) var texp =
    match t.d with
    | None -> t
    | Some d ->
      match get_coeff_vec t texp with
      | None ->
        (* nonlinear: havoc the assigned variable *)
        forget_vars t [var]
      | Some v ->
        let sz = size t in
        let dim_x = Environment.dim_of_var t.env var in
        let rhs_iv =
          match reduce ~size:sz d with
          | None -> RationalInterval.top (* pre-state is bottom; detected below *)
          | Some refined -> P.eval_vec ~size:sz d refined v
        in
        if Vector.nth v dim_x <>: Mpqf.zero then begin
          (* invertible assignment: substitute x in rows and slack infos *)
          let a_j0 = Matrix.get_col_upper_triangular d.P.affeq dim_x in
          let b0 = Vector.nth v dim_x in
          let a_j0 = Vector.apply_with_c_f_preserves_zero (/:) b0 a_j0 in
          let recalc_entries m rd_a = Matrix.map2 (fun x y -> Vector.map2i (fun j z d ->
              if j = dim_x then y
              else if Vector.compare_length_with v (j + 1) > 0 then z -: y *: d
              else z +: y *: d) x v) m rd_a
          in
          match Matrix.normalize (recalc_entries d.P.affeq a_j0) with
          | None -> bot_env
          | Some m ->
            let d = { d with P.affeq = m } in
            let d =
              if RationalInterval.is_top rhs_iv then { d with P.intervals = P.VarMap.remove dim_x d.P.intervals }
              else P.set_intv dim_x rhs_iv d
            in
            let t = { t with d = Some d } in
            (* rename slacks whose info mentions x; two phases via temporary
               names, since the new canonical name of one slack can coincide
               with the stale name of another pending one *)
            let pending = P.VarMap.fold (fun beta info acc ->
                match subst_info t v dim_x info with
                | None -> acc
                | Some (vts, k) -> (Environment.var_of_dim t.env beta, vts, k) :: acc
              ) d.P.infos []
            in
            let t, moved = List.fold_left (fun (t, moved) (old_v, vts, k) ->
                if vts = [] then
                  (* degenerate: the form collapsed to a constant; drop the slack *)
                  (VarManagement.remove_vars t [old_v], moved)
                else
                  let tmp_v = Var.of_string (tmp_prefix ^ Var.to_string old_v) in
                  (move_slack t old_v tmp_v None, (tmp_v, vts, k) :: moved)
              ) (t, []) pending
            in
            List.fold_left (fun t (tmp_v, vts, k) ->
                match t.d with
                | None -> t
                | Some _ ->
                  let final_v = Var.of_string (slack_name vts k) in
                  let info_of t = { P.iterms = dims_of_vterms t.env vts; P.iconst = k } in
                  if Environment.mem_var t.env final_v then
                    (* cannot happen for renames among themselves (substitution is
                       injective), but a stale dimension may exist; drop the slack *)
                    VarManagement.remove_vars t [tmp_v]
                  else
                    let t' = move_slack t tmp_v final_v None in
                    (match t'.d with
                     | None -> t'
                     | Some d' ->
                       let beta = Environment.dim_of_var t'.env final_v in
                       { t' with d = Some (P.set_info beta (info_of t') d') })
              ) t moved
        end
        else begin
          (* non-invertible: slacks mentioning x lose their meaning *)
          let dep = dependent_slack_vars t [var] in
          let t1 = VarManagement.remove_vars t dep in
          match t1.d with
          | None -> t1
          | Some d1 ->
            let sz1 = size t1 in
            let dim_x1 = Environment.dim_of_var t1.env var in
            match get_coeff_vec t1 texp with
            | None -> forget_vars t1 [var]
            | Some v1 ->
              let m = Matrix.remove_zero_rows @@ Matrix.reduce_col d1.P.affeq dim_x1 in
              (* x - sum b_i x_i = b_c *)
              let terms, bc = P.split_row sz1 v1 in
              let row = P.vec_of_terms sz1 ((dim_x1, Mpqf.one) :: List.map (fun (i, c) -> (i, Mpqf.neg c)) terms) bc in
              match Matrix.rref_vec m row with
              | None -> bot_env (* unreachable: x's column was just cleared *)
              | Some m ->
                let d1 = { d1 with P.affeq = m } in
                let d1 =
                  if RationalInterval.is_top rhs_iv then { d1 with P.intervals = P.VarMap.remove dim_x1 d1.P.intervals }
                  else P.set_intv dim_x1 rhs_iv d1
                in
                { t1 with d = Some d1 }
        end

  let assign_texpr t var texp = Timing.wrap "assign_texpr" (assign_texpr t var) texp

  let assign_texpr t var texp = verify "assign_texpr" (assign_texpr t var texp)

  let assign_exp ask (t: VarManagement.t) var exp (no_ov: bool Lazy.t) : VarManagement.t =
    let t = if not @@ Environment.mem_var t.env var then add_vars t [var] else t in
    match Convert.texpr1_expr_of_cil_exp ask t t.env exp no_ov with
    | texp -> assign_texpr t var texp
    | exception Convert.Unsupported_CilExp _ -> forget_vars t [var]

  let assign_exp ask t var exp no_ov =
    let res = assign_exp ask t var exp no_ov in
    if M.tracing then M.tracel "ops" "assign_exp t:\n %s \n var: %a \n exp: %a\n no_ov: %b -> \n %s"
        (show t) Var.pretty var d_exp exp (Lazy.force no_ov) (show res);
    res

  let assign_var (t: VarManagement.t) v v' =
    let t = add_vars t [v; v'] in
    assign_texpr t v (Apron.Texpr1.Var v')

  let assign_var t v v' =
    let res = assign_var t v v' in
    if M.tracing then M.tracel "ops" "assign_var t:\n %s \n v: %a \n v': %a\n -> %s" (show t) Var.pretty v Var.pretty v' (show res);
    res

  let assign_var_parallel t vv's =
    let assigned_vars = List.map fst vv's in
    let t = add_vars t assigned_vars in
    let primed_vars = List.init (List.length assigned_vars) (fun i -> Var.of_string (string_of_int i ^ "'")) in
    let t_primed = add_vars t primed_vars in
    let multi_t = List.fold_left2 (fun t' v_prime (_,v') -> assign_var t' v_prime v') t_primed primed_vars vv's in
    match multi_t.d with
    | Some _ when not @@ is_top multi_t ->
      let switched_arr = List.fold_left2 (fun multi_t assigned_var primed_var -> assign_var multi_t assigned_var primed_var) multi_t assigned_vars primed_vars in
      remove_vars switched_arr primed_vars
    | _ -> t

  let assign_var_parallel t vv's = Timing.wrap "var_parallel" (assign_var_parallel t) vv's

  let assign_var_parallel_with t vv's =
    let t' = assign_var_parallel t vv's in
    t.d <- t'.d;
    t.env <- t'.env

  let assign_var_parallel' t vs1 vs2 =
    let vv's = List.combine vs1 vs2 in
    assign_var_parallel t vv's

  let cil_exp_of_lincons1 = Convert.cil_exp_of_lincons1

  (* ---------------------------------------------------------------------- *)
  (* Guards *)

  (** Meets a constraint given as a value-form coefficient vector [v]
      ([sum v(i) * x_i + v(size)] ⋈ 0) into the state. *)
  let meet_tcons (t: t) (tcons: Tcons1.t) =
    match t.d with
    | None -> t
    | Some d ->
      match get_coeff_vec t (Texpr1.to_expr @@ Tcons1.get_texpr1 tcons) with
      | None -> t
      | Some v ->
        let sz = size t in
        let terms, c = P.split_row sz v in
        match terms, Tcons1.get_typ tcons with
        | _, EQMOD _ -> t
        | [], typ ->
          let violated = match typ with
            | EQ -> c <>: Mpqf.zero
            | SUPEQ -> c <: Mpqf.zero
            | SUP -> c <=: Mpqf.zero
            | DISEQ -> c =: Mpqf.zero
            | EQMOD _ -> false
          in
          if violated then bot_env else t
        | _, DISEQ ->
          (match reduce ~size:sz d with
           | None -> bot_env
           | Some refined ->
             (match RationalInterval.bounds (P.eval_vec ~size:sz d refined v) with
              | Some l, Some u when Q.equal l u && Q.equal l Q.zero -> bot_env
              | _ -> t))
        | _, EQ ->
          (* sum terms = -c as an equality row *)
          let row = Vector.set_nth v sz (Mpqf.neg c) in
          (match Matrix.rref_vec d.P.affeq row with
           | None -> bot_env
           | Some m ->
             let t = { t with d = Some { d with P.affeq = m } } in
             let (_, p) = List.hd terms in
             let t0 = List.map (fun (i, ci) -> (i, ci /: p)) terms in
             let value = P.q_of_mpqf (Mpqf.neg c /: p) in
             let t =
               match t0 with
               | [(dim, _)] ->
                 (match t.d with
                  | None -> t
                  | Some d ->
                    (match RationalInterval.meet (P.get_iv d.P.intervals dim) (RationalInterval.of_const value) with
                     | None -> bot_env
                     | Some iv -> { t with d = Some (P.set_intv dim iv d) }))
               | _ ->
                 (* refine existing slacks for this template; no new slack needed,
                    the equality is fully represented in the matrix *)
                 constrain_template ~create:false TMeet t (vterms_of_dims t.env t0) (RationalInterval.of_const value)
             in
             check_bot t)
        | _, ((SUPEQ | SUP) as typ) ->
          (* sum terms >= -c (strict: > -c) *)
          let strict = typ = SUP in
          let integral = List.for_all (fun (_, ci) -> P.is_integral ci) terms && P.is_integral c in
          let bound = if strict && integral then Mpqf.one -: c else Mpqf.neg c in
          (* a strict constraint over non-integral coefficients is soundly
             weakened to its non-strict counterpart *)
          let (_, p) = List.hd terms in
          let t0 = List.map (fun (i, ci) -> (i, ci /: p)) terms in
          let b0 = P.q_of_mpqf (bound /: p) in
          let iv =
            if p >: Mpqf.zero then RationalInterval.of_bounds ~lower:(Some b0) ~upper:None
            else RationalInterval.of_bounds ~lower:None ~upper:(Some b0)
          in
          let t =
            match t0 with
            | [(dim, _)] ->
              (match RationalInterval.meet (P.get_iv d.P.intervals dim) iv with
               | None -> bot_env
               | Some iv' -> { t with d = Some (P.set_intv dim iv' d) })
            | _ ->
              constrain_template TMeet t (vterms_of_dims t.env t0) iv
          in
          check_bot t

  (* materialize the implied interval refinements: constraints are stated on
     short-lived [g#in]-style variables, whose bounds must reach the variables
     that survive *)
  let meet_tcons t tcons = reduce_intervals (meet_tcons t tcons)

  let meet_tcons t tcons = Timing.wrap "meet_tcons" (meet_tcons t) tcons

  let meet_tcons t tcons = verify "meet_tcons" (meet_tcons t tcons)

  (** Backwards semantics of [var := exp]. When [var] does not occur in [exp],
      this is: meet with [var = exp], then havoc [var]. Meeting first lets the
      removal machinery re-express slack constraints on [var] over the
      variables of [exp] before [var] is forgotten (e.g. argument substitution
      at function return). *)
  let substitute_exp ask (t: t) var exp no_ov =
    let t = if not @@ Environment.mem_var t.env var then add_vars t [var] else t in
    match Convert.texpr1_expr_of_cil_exp ask t t.env exp no_ov with
    | texp ->
      let var_in_exp =
        match get_coeff_vec t texp with
        | Some v -> Vector.nth v (Environment.dim_of_var t.env var) <>: Mpqf.zero
        | None -> true
      in
      if var_in_exp then
        (* sound fallback: drop all constraints on [var] *)
        forget_vars (assign_texpr t var texp) [var]
      else
        let texpr =
          Texpr1.of_expr t.env
            (Apron.Texpr1.Binop (Apron.Texpr1.Sub, Apron.Texpr1.Var var, texp, Apron.Texpr1.Int, Apron.Texpr1.Near))
        in
        let t = meet_tcons t (Tcons1.make texpr EQ) in
        forget_vars t [var]
    | exception Convert.Unsupported_CilExp _ -> forget_vars t [var]

  let substitute_exp ask t var exp no_ov =
    let res = substitute_exp ask t var exp no_ov in
    if M.tracing then M.tracel "ops" "Substitute_expr t: \n %s \n var: %a \n exp: %a \n -> \n %s" (show t) Var.pretty var d_exp exp (show res);
    verify "substitute_exp" res

  let assert_constraint ask (d: t) e negate (no_ov: bool Lazy.t) =
    match Convert.tcons1_of_cil_exp ask d d.env e negate no_ov with
    | tcons1 -> meet_tcons d tcons1
    | exception Convert.Unsupported_CilExp _ -> d

  let assert_constraint ask d e negate no_ov =
    let res = assert_constraint ask d e negate no_ov in
    if M.tracing then M.tracel "assert_constraint" "assert_constraint with expr: %a negate: %b -> %s" d_exp e negate (show res);
    res

  (* ---------------------------------------------------------------------- *)
  (* Queries *)

  let env t = t.env

  let eval_interval _ask = Bounds.bound_texpr

  (*TODO: Incomplete, it drops all ineqs and only outputs eqs.*)
  let invariant (t: t) =
    match t.d with
    | None -> []
    | Some d ->
      let sz = size t in
      P.rows d.P.affeq
      |> List.filter_map (fun r ->
          let terms, rhs = P.split_row sz r in
          if terms = [] || List.exists (fun (dim, _) -> is_slack_var (Environment.var_of_dim t.env dim)) terms then
            None
          else begin
            let e1 = Linexpr1.make t.env in
            Linexpr1.set_list e1
              (List.map (fun (dim, c) -> (Coeff.s_of_mpqf c, Environment.var_of_dim t.env dim)) terms)
              (Some (Coeff.s_of_mpqf (Mpqf.neg rhs)));
            Some (Lincons1.make e1 EQ)
          end)

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
