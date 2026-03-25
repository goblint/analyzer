(** Termination analysis for loops and [goto] statements ([termination]).

    @see <https://theses.hal.science/tel-00288805> Halbwachs, N. Détermination automatique de relations linéaires vérifiées par les variables d’un programme. PhD thesis. Section 8.3. *)

open Analyses
open GoblintCil
open TerminationPreprocessing
open ListMatrix
open SparseVector
open GobApron

(** Contains all loop counter variables (varinfo) and maps them to their corresponding loop statement. *)
let loop_counters : stmt VarToStmt.t ref = ref VarToStmt.empty

module V = struct
  type t = string
  let is_int _ = true
  let compare = String.compare
  let print fmt v = Format.fprintf fmt "%s" v
end

module Rat = struct
  include Mpqf
  let zero = of_int 0
  let one = of_int 1
  let m_one = of_int (-1)
  let is_zero x = cmp_int x 0 = 0
  let is_one x = cmp_int x 1 = 0
  let is_m_one x = cmp_int x (-1) = 0
  let is_int x = is_one (of_mpz (get_den x)) (* I assume the numbers to be normalized *)
  let min x y = if cmp x y <= 0 then x else y
  let floor x =
    let n = get_num x in
    let d = get_den x in
    of_mpz (Mpzf.fdiv_q n d)
  let ceiling x =
    let n = get_num x in
    let d = get_den x in
    of_mpz (Mpzf.cdiv_q n d)
  let minus = neg
  let sign = sgn
  let compare = cmp
  let mult = mul
  let hash _ = 0
  let get_den x = Z.of_string (Mpzf.to_string (get_den x))
  let get_num x = Z.of_string (Mpzf.to_string (get_num x))
end

module Ex = struct
  include Set.Make(String)

  let print fmt s = match elements s with
    | [] -> Format.fprintf fmt "()"
    | e::l ->
      Format.fprintf fmt "%s" e;
      List.iter (Format.fprintf fmt ", %s") l
end

module Sim = OcplibSimplex.Basic.Make (V) (Rat) (Ex)

module Matrix = ListMatrix (Rat) (SparseVector)

module SparseVec = SparseVector (Rat)

module StringSet = Set.Make (String)

let string_of_invariant (constraints: Invariant.t) =
  match constraints with
  | `Top -> "Top"
  | `Bot -> "Bot"
  | `Lifted constraints -> constraints |> Invariant.Exp.process |> List.map Invariant.Exp.show |> String.concat "; "

let exp_list_of_constraints (constraints : Invariant.t) =
  match constraints with
  | `Top -> []
  | `Bot -> failwith "Constraints are Bot"
  | `Lifted cs ->
    (* Split Eq into two Le's and mirror Ge into Le *)
    let to_leq acc = function
      | BinOp (Le, _, _, _) as e -> e :: acc
      | BinOp (Ge, e1, e2, t) -> (BinOp (Le, e2, e1, t)) :: acc
      | BinOp (Eq, e1, e2, t) -> (BinOp (Le, e1, e2, t)) :: (BinOp (Le, e2, e1, t)) :: acc
      | BinOp (Ne, _, _, _) -> acc
      | _ -> failwith "Found something else" in
    let cs =
      cs |> Invariant.Exp.process |> List.map Invariant.Exp.to_cil |> List.fold_left to_leq []
    in
    if M.tracing then
      (M.trace "termination" "Constraints: %s" (string_of_invariant constraints);
       M.trace "termination" "Number of constraints after conversion to Leq list: %i" (List.length cs));
    cs

let coeff_in_exp vname e =
  let rec inner = function
    | BinOp (PlusA, e1, e2, _) ->
      (match inner e1, inner e2 with
       | Some c1, Some c2 -> failwith "This shouldn't happen"
       | None, None -> None
       | c, None | None, c -> c)
    | BinOp (MinusA, e1, e2, _) ->
      failwith "found a Minus"
    | BinOp (Mult, Const (CInt (c, _, _)), Lval (Var v, _), _) -> if v.vname = vname then Some (c |> cilint_to_int |> Rat.of_int) else None
    | BinOp (Mult, _, _, _) -> failwith "found another Mult"
    | Const (CInt (c, _, _)) -> None
    | Lval (Var v, _) -> if v.vname = vname then Some Rat.one else None
    | BinOp (Mod, _, _, _) -> None
    | _ -> failwith "found something unexpected"
  in
  match e with
  | BinOp (Le, e1, e2, _) ->
    (match inner e1, inner e2 with
     | Some c1, Some c2 -> failwith "This shouldn't happen"
     | None, None -> Rat.zero
     | Some c, None -> c
     | None, Some c -> Rat.neg c)
  | _ -> failwith "This shouldn't happen"

let get_var vname env =
  let vars, _ = Environment.vars env in
  Array.find_map (fun v -> if Var.to_string v = vname then Some v else None) vars

let coeff_in_lincons1 vname (e: Lincons1.t) =
  match get_var vname e.env with
  | None -> Rat.zero
  | Some v ->
    match Lincons1.get_coeff e v with
    | Interval _ -> failwith "Lincons1 coeff is an interval, this shouldn't happen"
    | Scalar s ->
      s |> SharedFunctions.int_of_scalar |> Option.get |> Z.to_int |> Rat.of_int |> Rat.minus (* negate the coefficient because of SUPEQ typ *)

let const_in_exp e =
  let rec inner = function
    | BinOp (PlusA, e1, e2, _) ->
      (match inner e1, inner e2 with
       | Some c1, Some c2 -> failwith "This shouldn't happen"
       | None, None -> None
       | c, None | None, c -> c)
    | Const (CInt (c, _, _)) -> if Z.compare c Z.zero = 0 then None else Some (c |> cilint_to_int |> Rat.of_int)
    | BinOp (Mult, _, _, _) -> None
    | BinOp (Mod, _, _, _) -> None
    | Lval (Var v, _) -> None
    | _ -> failwith "found something else"
  in
  match e with
  | BinOp (Le, e1, e2, _) ->
    (match inner e1, inner e2 with
     | Some c1, Some c2 -> failwith "This shouldn't happen"
     | None, None -> Rat.zero
     | Some c, None -> Rat.neg c
     | None, Some c -> c)
  | _ -> failwith "This shouldn't happen"

let const_in_lincons1 (e: Lincons1.t) =
  match Lincons1.get_cst e with
  | Interval _ -> failwith "Lincons1 coeff is an interval, this shouldn't happen"
  | Scalar s ->
    match e.lincons0.typ with
    | SUPEQ -> s |> SharedFunctions.int_of_scalar |> Option.get |> Z.to_int |> Rat.of_int (* don't negate here because cst is on the correct side *)
    | _ -> failwith "wrong typ"

let transposed_matrices_of_exp_list id (cs : exp list) =
  let rec vars_from_exp acc = function
    | BinOp (_, e1, e2, _) -> StringSet.union (vars_from_exp acc e1) (vars_from_exp acc e2)
    | UnOp (_, e, _) -> vars_from_exp acc e
    | Lval (Var v, _) ->
      (match String.split_on_char '_' v.vname with
       | "id"::_::"old"::_ -> acc (* TODO: maybe I'm forgetting too much here, could keep old_vars with the correct id *)
       | _ -> StringSet.add v.vname acc)
    | CastE _ -> failwith "Encountered cast"
    | Question _ -> failwith "Encountered question"
    | _ -> acc
  in
  let vars = cs |> List.fold_left vars_from_exp StringSet.empty |> StringSet.elements in
  let old_vars = List.map (String.cat @@ "id_" ^ id ^ "_old_") vars in
  (* TODO: could this result in invalid expressions where some terms are missing? *)
  let atr = List.fold_left (fun m v -> cs |> List.map (coeff_in_exp v) |> SparseVec.of_list |> Matrix.append_row m) (Matrix.empty ()) old_vars in
  let a'tr = List.fold_left (fun m v -> cs |> List.map (coeff_in_exp v) |> SparseVec.of_list |> Matrix.append_row m) (Matrix.empty ()) vars in
  let b = cs |> List.map const_in_exp |> SparseVec.of_list in
  atr, a'tr, b

let transposed_matrices_of_lincons1set id (cs : Lincons1Set.t) =
  let to_supeq_set e a =
    match Lincons1.get_typ e with
    | SUPEQ -> Lincons1Set.add e a
    | EQ ->
      let pos_e = Lincons1.make (Lincons1.get_linexpr1 e) SUPEQ in
      let neg_e = Lincons1.make (Linexpr1.neg (Lincons1.get_linexpr1 e)) SUPEQ in
      a |> Lincons1Set.add pos_e |> Lincons1Set.add neg_e
    | _ -> failwith "Wrong typ in Lincons1Set"
  in
  let c = Lincons1Set.find_first (Fun.const true) cs in
  let remove_old v =
    match v |> Var.to_string |> String.split_on_char '_' with
    | "id"::_::"old"::_ -> false (* TODO: maybe I'm forgetting too much here, could keep old_vars with the correct id *)
    | _ -> true
  in
  let vars, _ = Environment.vars c.env in
  let vars = vars |> Array.to_list |> List.filter remove_old in
  let cs = Lincons1Set.fold to_supeq_set cs Lincons1Set.empty in
  let atr = List.fold_left
      (fun m v -> cs |> Lincons1Set.elements |>
        List.map (coeff_in_lincons1 (Printf.sprintf "id_%s_old_%s" id (Var.to_string v))) |> SparseVec.of_list |> Matrix.append_row m) (Matrix.empty ()) vars in
  let a'tr = List.fold_left (fun m v -> cs |> Lincons1Set.elements |> List.map (coeff_in_lincons1 (Var.to_string v)) |> SparseVec.of_list |> Matrix.append_row m) (Matrix.empty ()) vars in
  let b = cs |> Lincons1Set.elements |> List.map const_in_lincons1 |> SparseVec.of_list in
  atr, a'tr, b

let string_of_my_invariant (constraints : MyInvariant.t) =
  match constraints with
  | `Top -> "Top"
  | `Bot -> "Bot"
  | `Lifted cs -> cs |> Lincons1Set.elements |> List.map Lincons1.show |> String.concat "; "

let termination_provable a_transposed a'_transposed b =
  let num_constraints = Matrix.num_cols a_transposed in
  let zero, one, m_one = Sim.Core.R2.zero, Sim.Core.R2.of_r Rat.one, Sim.Core.R2.of_r Rat.m_one in
  let ass_l1_a'tr_eq_zero sim =
    let rec aux i sim =
      if i >= Matrix.num_rows a'_transposed then sim else
        let l1_ri = Matrix.get_row a'_transposed i
          |> SparseVec.to_sparse_list
          |> List.map (fun (idx, v) -> "x" ^ string_of_int idx, v)
        in
        if M.tracing then
          M.trace "termination" "l1_r%i = %s" i
            (l1_ri |> List.map (fun (var, value) -> (Printf.sprintf "(%s * %s)" (Rat.to_string value) var)) |> String.concat " + ");

        let sim = fst @@
          match l1_ri with
          | [] -> sim, false
          | [(var, _)] -> Sim.Assert.var sim var
                            ~min:{Sim.Core.bvalue = zero; explanation = Ex.singleton (var ^ ">=0")}
                            ~max:{Sim.Core.bvalue = zero; explanation = Ex.singleton (var ^ "<=0")}
          | _ ->
            let l1_ri = l1_ri |> Sim.Core.P.from_list in
            Sim.Assert.poly sim l1_ri ("l1_r" ^ string_of_int i)
              ~min:{Sim.Core.bvalue = zero; explanation = Ex.singleton ("l1_r" ^ string_of_int i ^ ">=0")}
              ~max:{Sim.Core.bvalue = zero; explanation = Ex.singleton ("l1_r" ^ string_of_int i ^ "<=0")}
        in aux (i + 1) sim
    in aux 0 sim
  in
  let ass_l1_minus_l2_atr_eq_zero sim =
    let rec aux i sim =
      if i >= Matrix.num_rows a_transposed then sim else
        let l1_m_l2_ri = Matrix.get_row a_transposed i
          |> SparseVec.to_sparse_list
          (* Example: (2, 0, -3) -> x0 * 2 + y0 * (-2) + x2 * (-3) + y2 * (-(-3))
             map doesn't work here because we double the number of list elements *)
          |> List.fold_left (fun l (idx, v) -> ("x" ^ string_of_int idx, v) :: ("y" ^ string_of_int idx, Rat.mul Rat.m_one v) :: l) []
        in
        if M.tracing then
          M.trace "termination" "l1_m_l2_r%i = %s" i
            (l1_m_l2_ri |> List.map (fun (var, value) -> (Printf.sprintf "(%s * %s)" (Rat.to_string value) var)) |> String.concat " + ");

        let sim = fst @@
          match l1_m_l2_ri with
          | [] -> sim, false
          | _ ->
            let l1_m_l2_ri = Sim.Core.P.from_list l1_m_l2_ri in
            Sim.Assert.poly sim l1_m_l2_ri ("l1_m_l2_r" ^ string_of_int i)
              ~min:{Sim.Core.bvalue = zero; explanation = Ex.singleton ("l1_m_l2_r" ^ string_of_int i ^ ">=0")}
              ~max:{Sim.Core.bvalue = zero; explanation = Ex.singleton ("l1_m_l2_r" ^ string_of_int i ^ "<=0")}
        in aux (i + 1) sim
    in aux 0 sim
  in    
  let ass_l2_atr_plus_a'tr_eq_zero sim =
    let m = Matrix.add a_transposed a'_transposed in
    let rec aux i sim =
      if i >= Matrix.num_rows m then sim else
        let l2_ri = Matrix.get_row m i
          |> SparseVec.to_sparse_list
          |> List.map (fun (idx, v) -> "y" ^ string_of_int idx, v)
        in
        if M.tracing then
          M.trace "termination" "l2_r%i = %s" i
            (l2_ri |> List.map (fun (var, value) -> (Printf.sprintf "(%s * %s)" (Rat.to_string value) var)) |> String.concat " + ");

        let sim = fst @@
          match l2_ri with
          | [] -> sim, false
          | [(var, _)] -> Sim.Assert.var sim var
                            ~min:{Sim.Core.bvalue = zero; explanation = Ex.singleton (var ^ ">=0")}
                            ~max:{Sim.Core.bvalue = zero; explanation = Ex.singleton (var ^ "<=0")}
          | _ -> let l2_ri = l2_ri |> Sim.Core.P.from_list in
            Sim.Assert.poly sim l2_ri ("l2_r" ^ string_of_int i)
              ~min:{Sim.Core.bvalue = zero; explanation = Ex.singleton ("l2_r" ^ string_of_int i ^ ">=0")}
              ~max:{Sim.Core.bvalue = zero; explanation = Ex.singleton ("l2_r" ^ string_of_int i ^ "<=0")}
        in aux (i + 1) sim
    in aux 0 sim
  in
  let ass_l2_b_lt_zero sim =
    let l2_b = b
      |> SparseVec.to_sparse_list
      |> List.map (fun (i, v) -> "y" ^ string_of_int i, v)
    in
    if M.tracing then
      M.trace "termination" "l2_b = %s"
        (l2_b |> List.map (fun (var, value) -> (Printf.sprintf "(%s * %s)" (Rat.to_string value) var)) |> String.concat " + ");

    fst @@
    match l2_b with
    | [] -> sim, false
    | [(var, value)] when Rat.compare value Rat.zero > 0 -> Sim.Assert.var sim var
                                                              ~max:{Sim.Core.bvalue = m_one; explanation = Ex.singleton (var ^ "<0")} (* creates a contradiction *)
    | [(var, value)] when Rat.compare value Rat.zero < 0 -> Sim.Assert.var sim var
                                                              ~min:{Sim.Core.bvalue = one; explanation = Ex.singleton (var ^ ">0")}
    | [(var, _)] -> failwith "This shouldn't happen"
    | _ -> let l2_b = Sim.Core.P.from_list l2_b in
      Sim.Assert.poly sim l2_b "l2_b"
        ~max:{Sim.Core.bvalue = m_one; explanation = Ex.singleton "l2_b<0"}
  in
  let ass_li_ge_zero sim =
    let f sim i =
      let sim', _ =
        Sim.Assert.var sim ("x" ^ string_of_int i)
          ~min:{Sim.Core.bvalue = zero; explanation = Ex.singleton ("x" ^ string_of_int i ^ ">=0")}
      in
      fst @@ Sim.Assert.var sim' ("y" ^ string_of_int i)
        ~min:{Sim.Core.bvalue = zero; explanation = Ex.singleton ("y" ^ string_of_int i ^ ">=0")}
    in
    List.fold_left f sim (List.init num_constraints Fun.id)
  in
  let sim = Sim.Core.empty ~is_int:true ~check_invs:true
    |> ass_l1_a'tr_eq_zero |> ass_l1_minus_l2_atr_eq_zero |> ass_l2_atr_plus_a'tr_eq_zero |> ass_l2_b_lt_zero |> ass_li_ge_zero
    |> Sim.Solve.solve in
  match sim.status with
  | UNSAT _ -> false
  | SAT -> true
  | UNK -> failwith "Simplex returned UNK, this shouldn't happen"

(** Checks whether a variable can be bounded. *)
let ask_bound man varinfo =
  let open IntDomain.IntDomTuple in
  let exp = Lval (Var varinfo, NoOffset) in
  match man.ask (EvalInt exp) with
  | `Top -> `Top
  | `Lifted v when is_top_of (ikind v) v -> `Top
  | `Lifted v -> `Lifted v
  | `Bot -> failwith "Loop counter variable is Bot."

(** The termination analysis considering loops and gotos *)
module Spec : Analyses.MCPSpec =
struct

  include Analyses.IdentitySpec

  let name () = "termination"

  module D = Lattice.Unit
  include Analyses.ValueContexts(D)

  module V = struct
    include UnitV
    let is_write_only _ = true
  end

  (** We want to record termination information of loops and use the loop statements for that. *)
  module G = MapDomain.MapBot (CilType.Stmt) (BoolDomain.MustBool)

  let startstate _ = ()
  let exitstate = startstate

  (** Recognizes a call of [__goblint_bounded] to check the EvalInt of the
   * respective loop counter variable at that position. *)
  let special man (lval : lval option) (f : varinfo) (arglist : exp list) =
    if !AnalysisState.postsolving then (
      match f.vname, arglist with
      | "__goblint_bounded", [Lval (Var loop_counter, NoOffset)] ->
        begin match VarToStmt.find_opt loop_counter !loop_counters with
          | Some loop_statement ->
            let bound = ask_bound man loop_counter in
            let is_bounded = bound <> `Top in
            man.sideg () (G.add loop_statement is_bounded (man.global ()));
            let loc = M.Location.CilLocation (Cilfacade.get_stmtLoc loop_statement) in
            begin match bound with
              | `Top ->
                M.warn ~category:Termination ~loc "The program might not terminate! (Loop analysis)"
              | `Lifted bound ->
                (* TODO: aggregate these per loop (if unrolled) and warn using WarnGlobal? *)
                if GobConfig.get_bool "dbg.termination-bounds" then
                  M.success ~category:Termination ~loc "Loop terminates: bounded by %a iteration(s)" IntDomain.IntDomTuple.pretty bound;
            end
          | None ->
            (*failwith "Encountered a call to __goblint_bounded with an unknown loop counter variable."*)
            let loop_id = loop_counter.vname |> String.split_on_char '_' |> (function _::id::_ -> id | _ -> "-1")
            in
            (*let atr, a'tr, b = man.ask (Queries.Invariant Invariant.default_context) |> exp_list_of_constraints |> transposed_matrices_of_exp_list loop_id in*)

            let my_inv = man.ask (Queries.MyInvariant MyInvariant.default_context) in
            let lincons1set =
              match my_inv with
              | `Lifted cs -> cs
              | `Top | `Bot -> Lincons1Set.empty
            in
            let atr, a'tr, b = transposed_matrices_of_lincons1set loop_id lincons1set in

            let term_provable = termination_provable atr a'tr b in
            if term_provable then ((*if GobConfig.get_bool "dbg.termination-bounds" then*) M.success ~category:Termination "Loop terminates")
            else (M.warn ~category:Termination "The program might not terminate!")

        end
      | "__goblint_bounded", _ ->
        failwith "__goblint_bounded call unexpected arguments"
      | _ -> ()
    )

  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MustTermLoop loop_statement ->
      let multithreaded = man.ask Queries.IsEverMultiThreaded in
      (not multithreaded)
      && (BatOption.default false (G.find_opt loop_statement (man.global ())))
    | Queries.MustTermAllLoops ->
      let multithreaded = man.ask Queries.IsEverMultiThreaded in
      if multithreaded then (
        M.warn ~category:Termination "The program might not terminate! (Multithreaded)";
        false)
      else
        G.for_all (fun _ term_info -> term_info) (man.global ())
    | _ -> Queries.Result.top q

end

let () =
  Cilfacade.register_preprocess (Spec.name ()) (new loopCounterVisitor loop_counters);
  MCP.register_analysis (module Spec : MCPSpec)
