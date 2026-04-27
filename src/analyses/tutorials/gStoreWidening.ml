open GoblintCil
open SimplifiedAnalysis
open GStoreWideningHelper

(**
    There are two regression tests for this analysis, which you can run by calling:
   -  ./regtest.sh 99 05
   -  ./regtest.sh 99 06

    Running these scripts also produces a visualization of the analysis results as a HTML file in the folder
    result.

    You can access these by spinning up a HTTP server, e.g., by calling `python3 -m http.server`

    First fix the TODO: 1) to ensure unreachable code is marked as dead.
    Then, tackle TODO: 2) to change this analysis so it tracks global variables

    After modifying things, don't forget to compile by running `make`
*)


module Analysis: SimplifiedSpec = struct
  let name = "gStoreWidening"

  module I = GStoreWideningHelper.Intervals

  module D = MapDomain.MapBot (Basetype.Variables) (I)
  module C = Printable.Unit

  (** TODO: 2) Modify so that we store values for globals instead of always assuming they are top *)
  module V = Printable.Unit
  module G = Lattice.Unit

  let startstate = D.bot ()
  let startcontext = ()

  (* Evaluate a single variable given a local state *)
  let eval_varinfo man state v =
    if v.vglob then
      (** TODO: 2) Modify so that we store values for globals *)
      top_of_var v
    else
      D.find v state

  (* evaluate an expression given a local state, can remain unmodified *)
  let rec eval man (state: D.t) (e: exp) =
    try
      match e with
      | Const (CInt (i, ik, _)) ->
        const_int ik i
      | Lval (Var v, NoOffset) when GStoreWideningHelper.is_tracked_var v ->
        eval_varinfo man state v
      | CastE (_, t, e) ->
        cast_to_typ t (eval man state e)
      | UnOp (Neg, e, t) ->
        I.neg (cast_to_typ t (eval man state e))
      | UnOp (BNot, e, t) ->
        I.lognot (cast_to_typ t (eval man state e))
      | UnOp (LNot, e, t) ->
        begin match I.to_bool (eval man state e) with
          | Some b -> I.of_bool (ikind_of_typ t) (not b)
          | None -> top_of_typ t
        end
      | BinOp (op, e1, e2, t) ->
        eval_binop man state op e1 e2 t
      | _ ->
        top_of_exp e
    with
    | IntDomain.ArithmeticOnIntegerBot _
    | IntDomain.IncompatibleIKinds _
    | Cilfacade.TypeOfError _ ->
      top_of_exp e

  (* evaluation of binary operators, can remain unmodified *)
  and eval_binop man state op e1 e2 t =
    let ik = ikind_of_typ t in
    let v1 = cast_to_typ t (eval man state e1) in
    let v2 = cast_to_typ t (eval man state e2) in
    match op with
    | PlusA | PlusPI | IndexPI ->
      I.add v1 v2
    | MinusA | MinusPI | MinusPP ->
      I.sub v1 v2
    | Mult ->
      I.mul v1 v2
    | Div ->
      I.div v1 v2
    | Mod ->
      I.rem v1 v2
    | BAnd ->
      I.logand v1 v2
    | BOr ->
      I.logor v1 v2
    | BXor ->
      I.logxor v1 v2
    | Shiftlt ->
      I.shift_left v1 v2
    | Shiftrt ->
      I.shift_right v1 v2
    | Lt | Gt | Le | Ge | Eq | Ne ->
      let cmp =
        match op with
        | Lt -> I.lt v1 v2
        | Gt -> I.gt v1 v2
        | Le -> I.le v1 v2
        | Ge -> I.ge v1 v2
        | Eq -> I.eq v1 v2
        | Ne -> I.ne v1 v2
        | _ -> None
      in
      begin match cmp with
        | Some b -> I.of_bool ik b
        | None -> I.top_of ik
      end
    | LAnd | LOr ->
      begin match I.to_bool v1, I.to_bool v2 with
        | Some b1, Some b2 ->
          let b = if op = LAnd then b1 && b2 else b1 || b2 in
          I.of_bool ik b
        | _ -> I.top_of ik
      end

  let query man state (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.EvalInt e ->
      let ik = ikind_of_exp e in
      let v = eval man state e in
      begin match I.minimal v, I.maximal v with
        | Some l, Some u -> Queries.ID.of_interval ik (l, u)
        | _ -> Queries.Result.top q
      end
    | _ ->
      Queries.Result.top q

  let assign man state lval rval =
    match is_tracked_lval lval with
    | Some v ->
      if not v.vglob then
        D.add v (cast_to_typ v.vtype (eval man state rval)) state
      else
        (** TODO: 2) Modify so that we store values for globals *)
        state
    | None ->
      state

  (** TODO: 1) raise Analyses.Deadcode if we branch on a condition that is known-to-be false *)
  (* Returns the state resulting when the expression `e` evaluates to `tv` *)
  let branch man state e tv =
    let e_evaluated_to_bool = I.to_bool (eval man state e) in
    state


  (* Glue code, does not need to be modified for this tutorial *)
  let set_lval_top state = function
    | Some (Var v, NoOffset) when is_tracked_var v && not v.vglob ->
      D.add v (I.top_of (ikind_of_typ v.vtype)) state
    | _ -> state

  let return _ state _ _ =
    state

  let body _ state f =
    List.fold_left (fun acc v ->
        if is_tracked_var v then
          D.add v (I.top_of (ikind_of_typ v.vtype)) acc
        else
          acc
      ) state f.slocals

  let enter man state _ f args =
    List.fold_left2 (fun acc formal actual ->
        if is_tracked_var formal then
          D.add formal (cast_to_typ formal.vtype (eval man state actual)) acc
        else
          acc
      ) (D.bot ()) f.sformals args

  let combine _ state _ lval _ _ =
    set_lval_top state lval

  let special man state lval _ _ =
    set_lval_top state lval

  let context _ (_, c) _ _ =
    c

  let threadenter _ _ f _ =
    List.fold_left (fun acc v ->
        if is_tracked_var v then
          D.add v (I.top_of (ikind_of_typ v.vtype)) acc
        else
          acc
      ) (D.bot ()) f.sformals
end

let _ =
  MCPRegistry.registered_simplified_analysis (module Analysis:SimplifiedSpec)
