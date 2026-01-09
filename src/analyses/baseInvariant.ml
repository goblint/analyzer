(** {!Analyses.Spec.branch} refinement for {!Base} analysis. *)

open GoblintCil

module M = Messages
module VD = BaseDomain.VD
module ID = ValueDomain.ID
module FD = ValueDomain.FD
module AD = ValueDomain.AD

module type Eval =
sig
  module D: Lattice.S
  module V: Analyses.SpecSysVar
  module G: Lattice.S

  val unop_ID: Cil.unop -> ID.t -> ID.t
  val unop_FD: Cil.unop -> FD.t -> VD.t

  val eval_rv: man:(D.t, G.t, _, V.t) Analyses.man -> D.t -> exp -> VD.t
  val eval_rv_address: man:(D.t, G.t, _, V.t) Analyses.man -> D.t -> exp -> VD.t
  val eval_lv: man:(D.t, G.t, _, V.t) Analyses.man -> D.t -> lval -> AD.t
  val convert_offset: man:(D.t, G.t, _, V.t) Analyses.man -> D.t -> offset -> ID.t Offset.t

  val get_var: man:(D.t, G.t, _, V.t) Analyses.man -> D.t -> varinfo -> VD.t
  val get: man:(D.t, G.t, _, V.t) Analyses.man -> D.t -> AD.t -> exp option -> VD.t
  val set: man:(D.t, G.t, _, V.t) Analyses.man -> D.t -> AD.t -> typ -> ?lval_raw:lval -> VD.t -> D.t

  val refine_entire_var: bool
  val map_oldval: VD.t -> typ -> VD.t
  val eval_rv_lval_refine: man:(D.t, G.t, _, V.t) Analyses.man -> D.t -> exp -> lval -> VD.t

  val id_meet_down: old:ID.t -> c:ID.t -> ID.t
  val fd_meet_down: old:FD.t -> c:FD.t -> FD.t

  (** Handle contradiction.

      Normal branch refinement just raises {!Analyses.Deadcode}.
      Unassume leaves unchanged. *)
  val contra: D.t -> D.t
end

module Make (Eval: Eval) =
struct
  open Eval

  let is_some_bot (x:VD.t) =
    match x with
    | Bot -> false (* HACK: bot is here due to typing conflict (we do not cast appropriately) *)
    | _ -> VD.is_bot_value x

  let apply_invariant ~old_val ~new_val =
    try
      VD.meet old_val new_val
    with Lattice.Uncomparable -> old_val

  let refine_lv_fallback man st lval value tv =
    if M.tracing then M.tracec "invariant" "Restricting %a with %a" d_lval lval VD.pretty value;
    let addr = eval_lv ~man st lval in
    if (AD.is_top addr) then st
    else
      let old_val = get ~man st addr None in (* None is ok here, we could try to get more precise, but this is ok (reading at unknown position in array) *)
      let t_lval = Cilfacade.typeOfLval lval in
      let old_val = map_oldval old_val t_lval in
      let old_val =
        if is_some_bot old_val then (
          if M.tracing then M.tracec "invariant" "%a is bot! This should not happen. Will continue with top!" d_lval lval;
          VD.top ()
        )
        else
          old_val
      in
      let state_with_excluded = set st addr t_lval value ~man in
      let value =  get ~man state_with_excluded addr None in
      let new_val = apply_invariant ~old_val ~new_val:value in
      if M.tracing then M.traceu "invariant" "New value is %a" VD.pretty new_val;
      (* make that address meet the invariant, i.e exclusion sets will be joined *)
      if is_some_bot new_val then (
        if M.tracing then M.tracel "branch" "C The branch %B is dead!" tv;
        contra st
      )
      else if VD.is_bot new_val
      then set st addr t_lval value ~man (* no *_raw because this is not a real assignment *)
      else set st addr t_lval new_val ~man (* no *_raw because this is not a real assignment *)

  let refine_lv man st c x c' pretty exp =
    let set' lval v st = set st (eval_lv ~man st lval) (Cilfacade.typeOfLval lval) ~lval_raw:lval v ~man in
    let default () =
      (* For accesses via pointers in complicated case, no refinement yet *)
      let old_val = eval_rv_lval_refine ~man st exp x in
      let old_val = map_oldval old_val (Cilfacade.typeOfLval x) in
      let v = apply_invariant ~old_val ~new_val:c' in
      if is_some_bot v then contra st
      else (
        if M.tracing then M.tracel "inv" "improve lval %a from %a to %a (c = %a, c' = %a)" d_lval x VD.pretty old_val VD.pretty v pretty c VD.pretty c';
        set' x v st
      )
    in
    match x with
    | Var var, o when refine_entire_var ->
      (* For variables, this is done at to the level of entire variables to benefit e.g. from disjunctive struct domains *)
      let old_val = get_var ~man st var in
      let old_val = map_oldval old_val var.vtype in
      let offs = convert_offset ~man st o in
      let new_val = VD.update_offset (Queries.to_value_domain_ask (Analyses.ask_of_man man)) old_val offs c' (Some exp) x (var.vtype) in
      let v = apply_invariant ~old_val ~new_val in
      if is_some_bot v then contra st
      else (
        if M.tracing then M.tracel "inv" "improve variable %a from %a to %a (c = %a, c' = %a)" CilType.Varinfo.pretty var VD.pretty old_val VD.pretty v pretty c VD.pretty c';
        let r = set' (Cil.var var) v st in
        if M.tracing then M.tracel "inv" "st from %a to %a" D.pretty st D.pretty r;
        r
      )
    | Mem (Lval lv), off ->
      (* Underlying lvals (may have offsets themselves), e.g., for struct members NOT including any offset appended to outer Mem *)
      let lvals = eval_lv ~man st (Mem (Lval lv), NoOffset) in
      (* Additional offset of value being refined in Addr Offset type *)
      let original_offset = convert_offset ~man st off in
      let res = AD.fold (fun base_a acc ->
          Option.bind acc (fun acc ->
              match base_a with
              | Addr _ ->
                let (lval_a:VD.t) = Address (AD.singleton base_a) in
                if M.tracing then M.tracel "inv" "Consider case of lval %a = %a" d_lval lv VD.pretty lval_a;
                let st = set' lv lval_a st in
                let orig = AD.Addr.add_offset base_a original_offset in
                let old_val = get ~man st (AD.singleton orig) None in
                let old_val = VD.cast ~kind:Internal (Cilfacade.typeOfLval x) old_val in (* needed as the type of this pointer may be different *) (* TODO: proper castkind *)
                (* this what I would originally have liked to do, but eval_rv_lval_refine uses queries and thus stale values *)
                (* let old_val = eval_rv_lval_refine ~man st exp x in *)
                let old_val = map_oldval old_val (Cilfacade.typeOfLval x) in
                let v = apply_invariant ~old_val ~new_val:c' in
                if is_some_bot v then
                  Some (D.join acc (try contra st with Analyses.Deadcode -> D.bot ()))
                else (
                  if M.tracing then M.tracel "inv" "improve lval %a from %a to %a (c = %a, c' = %a)" d_lval x VD.pretty old_val VD.pretty v pretty c VD.pretty c';
                  Some (D.join acc (set' x v st))
                )
              | _ -> None
            )
        ) lvals (Some (D.bot ()))
      in
      BatOption.map_default_delayed (fun d -> if D.is_bot d then raise Analyses.Deadcode else d) default res
    | Var _, _
    | Mem _, _ ->
      default ()


  let invariant_fallback man st exp tv =
    (* We use a recursive helper function so that x != 0 is false can be handled
     * as x == 0 is true etc *)
    let rec helper (op: binop) (lval: lval) (value: VD.t) (tv: bool): [> `Refine of lval * VD.t | `NotUnderstood] =
      match (op, lval, value, tv) with
      (* The true-branch where x == value: *)
      | Eq, x, value, true ->
        if M.tracing then M.tracec "invariant" "Yes, %a equals %a" d_lval x VD.pretty value;
        (match value with
         | Int n ->
           let ikind = Cilfacade.get_ikind_exp (Lval lval) in
           `Refine (x, Int (ID.cast_to ~kind:Internal ikind n)) (* TODO: proper castkind *)
         | _ -> `Refine (x, value))
      (* The false-branch for x == value: *)
      | Eq, x, value, false -> begin
          match value with
          | Int n -> begin
              match ID.to_int n with
              | Some n ->
                (* When x != n, we can return a singleton exclusion set *)
                if M.tracing then M.tracec "invariant" "Yes, %a is not %a" d_lval x GobZ.pretty n;
                let ikind = Cilfacade.get_ikind_exp (Lval lval) in
                `Refine (x, Int (ID.of_excl_list ikind [n]))
              | None -> `NotUnderstood
            end
          | Address n -> begin
              if M.tracing then M.tracec "invariant" "Yes, %a is not %a" d_lval x AD.pretty n;
              match eval_rv_address ~man st (Lval x) with
              | Address a when AD.is_definite n ->
                `Refine (x, Address (AD.diff a n))
              | Top when AD.is_null n ->
                `Refine (x, Address AD.not_null)
              | v ->
                if M.tracing then M.tracec "invariant" "No address invariant for: %a != %a" VD.pretty v AD.pretty n;
                `NotUnderstood
            end
          (* | Address a -> `Refine (x, value) *)
          | _ ->
            (* We can't say anything else, exclusion sets are finite, so not
             * being in one means an infinite number of values *)
            if M.tracing then M.tracec "invariant" "Failed! (not a definite value)";
            `NotUnderstood
        end
      | Ne, x, value, _ -> helper Eq x value (not tv)
      | Lt, x, value, _ -> begin
          match value with
          | Int n -> begin
              let ikind = Cilfacade.get_ikind_exp (Lval lval) in
              let n = ID.cast_to ~kind:Internal ikind n in (* TODO: proper castkind *)
              let range_from x = if tv then ID.ending ikind (Z.pred x) else ID.starting ikind x in
              let limit_from = if tv then ID.maximal else ID.minimal in
              match limit_from n with
              | Some n ->
                if M.tracing then M.tracec "invariant" "Yes, success! %a is not %a" d_lval x GobZ.pretty n;
                `Refine (x, Int (range_from n))
              | None -> `NotUnderstood
            end
          | _ -> `NotUnderstood
        end
      | Le, x, value, _ -> begin
          match value with
          | Int n -> begin
              let ikind = Cilfacade.get_ikind_exp (Lval lval) in
              let n = ID.cast_to ~kind:Internal ikind n in (* TODO: proper castkind *)
              let range_from x = if tv then ID.ending ikind x else ID.starting ikind (Z.succ x) in
              let limit_from = if tv then ID.maximal else ID.minimal in
              match limit_from n with
              | Some n ->
                if M.tracing then M.tracec "invariant" "Yes, success! %a is not %a" d_lval x GobZ.pretty n;
                `Refine (x, Int (range_from n))
              | None -> `NotUnderstood
            end
          | _ -> `NotUnderstood
        end
      | Gt, x, value, _ -> helper Le x value (not tv)
      | Ge, x, value, _ -> helper Lt x value (not tv)
      | _ ->
        if M.tracing then M.trace "invariant" "Failed! (operation not supported)";
        `NotUnderstood
    in
    if M.tracing then M.traceli "invariant" "assume expression %a is %B" d_exp exp tv;
    let null_val (typ:typ):VD.t =
      match Cil.unrollType typ with
      | TPtr _                    -> Address AD.null_ptr
      | TEnum({ekind=_;_},_)
      | _                         -> Int (ID.of_int (Cilfacade.get_ikind typ) Z.zero)
    in
    let rec derived_invariant exp tv: [`Refine of lval * VD.t | `NothingToRefine | `NotUnderstood] =
      let switchedOp = function Lt -> Gt | Gt -> Lt | Le -> Ge | Ge -> Le | x -> x in (* a op b <=> b (switchedOp op) b *)
      match exp with
      (* Since we handle not only equalities, the order is important *)
      | BinOp(op, Lval x, rval, typ) ->
        let v = eval_rv ~man st rval in
        let x_type = Cilfacade.typeOfLval x in
        if VD.is_dynamically_safe_cast x_type (Cilfacade.typeOf rval) v then
          helper op x (VD.cast ~kind:Internal x_type v) tv (* TODO: proper castkind *)
        else
          `NotUnderstood
      | BinOp(op, rval, Lval x, typ) -> derived_invariant (BinOp(switchedOp op, Lval x, rval, typ)) tv
      | BinOp(op, CastE (_, t1, c1), CastE (_, t2, c2), t) when (op = Eq || op = Ne) && typeSig t1 = typeSig t2 && VD.is_statically_safe_cast t1 (Cilfacade.typeOf c1) && VD.is_statically_safe_cast t2 (Cilfacade.typeOf c2)
        -> derived_invariant (BinOp (op, c1, c2, t)) tv
      | BinOp(op, CastE (_, t1, Lval x), rval, typ) when Cil.isIntegralType t1 ->
        begin match eval_rv ~man st (Lval x) with
          | Int v ->
            if VD.is_dynamically_safe_cast t1 (Cilfacade.typeOfLval x) (Int v) then
              derived_invariant (BinOp (op, Lval x, rval, typ)) tv
            else
              `NotUnderstood
          | _ -> `NotUnderstood
        end
      | BinOp(op, rval, CastE (k, ti, Lval x), typ) when Cil.isIntegralType ti ->
        derived_invariant (BinOp (switchedOp op, CastE(k, ti, Lval x), rval, typ)) tv
      | BinOp(op, (Const _ | AddrOf _), rval, typ) ->
        (* This is last such that we never reach here with rval being Lval (it is swapped around). *)
        `NothingToRefine
      (* Cases like if (x) are treated like if (x != 0) *)
      | Lval x ->
        (* There are two correct ways of doing it: "if ((int)x != 0)" or "if (x != (typeof(x))0))"
         * Because we try to avoid casts (and use a more precise address domain) we use the latter *)
        helper Ne x (null_val (Cilfacade.typeOf exp)) tv
      | UnOp (LNot,uexp,typ) -> derived_invariant uexp (not tv)
      | _ ->
        if M.tracing then M.tracec "invariant" "Failed! (expression %a not understood)" d_plainexp exp;
        `NotUnderstood
    in
    match derived_invariant exp tv with
    | `Refine (lval, value) ->
      refine_lv_fallback man st lval value tv
    | `NothingToRefine ->
      if M.tracing then M.traceu "invariant" "Nothing to refine.";
      st
    | `NotUnderstood ->
      if M.tracing then M.traceu "invariant" "Doing nothing.";
      M.debug ~category:Analyzer "Invariant failed: expression \"%a\" not understood." d_exp exp;
      st

  let invariant man st exp tv: D.t =
    let fallback reason st =
      if M.tracing then M.tracel "inv" "Can't handle %a.\n%t" d_plainexp exp reason;
      invariant_fallback man st exp tv
    in
    (* inverse values for binary operation a `op` b == c *)
    (* ikind is the type of a for limiting ranges of the operands a, b. The only binops which can have different types for a, b are Shiftlt, Shiftrt (not handled below; don't use ikind to limit b there). *)
    let inv_bin_int (a, b) ikind c op =
      let warn_and_top_on_zero x =
        if GobOption.exists (Z.equal Z.zero) (ID.to_int x) then
          (M.error ~category:M.Category.Integer.div_by_zero ~tags:[CWE 369] "Must Undefined Behavior: Second argument of div or mod is 0, continuing with top";
           Checks.error Checks.Category.DivisionByZero "Must Undefined Behavior: Second argument of div or mod is 0, continuing with top";
           ID.top_of ikind)
        else (
          Checks.safe Checks.Category.DivisionByZero;
          x)
      in
      let meet_bin a' b'  = id_meet_down ~old:a ~c:a', id_meet_down ~old:b ~c:b' in
      let meet_com oi = (* commutative *)
        try
          meet_bin (oi c b) (oi c a)
        with
          IntDomain.ArithmeticOnIntegerBot _ -> raise Analyses.Deadcode in
      let meet_non oi oo = (* non-commutative *)
        try
          meet_bin (oi c b) (oo a c)
        with IntDomain.ArithmeticOnIntegerBot _ -> raise Analyses.Deadcode in
      match op with
      | PlusA  -> meet_com ID.sub
      | Mult   ->
        (* Only multiplication with odd numbers is an invertible operation in (mod 2^n) *)
        (* refine x by information about y, using x * y == c *)
        let refine_by x y = (match ID.to_int y with
            | None -> x
            | Some v when Z.equal (Z.rem v (Z.of_int 2)) Z.zero (* v % 2 = 0 *) -> x (* A refinement would still be possible here, but has to take non-injectivity into account. *)
            | Some v (* when Int64.rem v 2L = 1L *) -> id_meet_down ~old:x ~c:(ID.div c y)) (* Div is ok here, c must be divisible by a and b *)
        in
        (refine_by a b, refine_by b a)
      | MinusA -> meet_non ID.add ID.sub
      | Div    ->
        (* If b must be zero, we have must UB *)
        let b = warn_and_top_on_zero b in
        (* Integer division means we need to add the remainder, so instead of just `a = c*b` we have `a = c*b + a%b`.
         * However, a%b will give [-b+1, b-1] for a=top, but we only want the positive/negative side depending on the sign of c*b.
         * If c*b = 0 or it can be positive or negative, we need the full range for the remainder. *)
        let rem =
          let is_pos = ID.to_bool @@ ID.gt (ID.mul b c) (ID.of_int ikind Z.zero) = Some true in
          let is_neg = ID.to_bool @@ ID.lt (ID.mul b c) (ID.of_int ikind Z.zero) = Some true in
          let full = ID.rem a b in
          if is_pos then ID.meet (ID.starting ikind Z.zero) full
          else if is_neg then ID.meet (ID.ending ikind Z.zero) full
          else full
        in
        meet_bin (ID.add (ID.mul b c) rem) (ID.div (ID.sub a rem) c)
      | Mod    -> (* a % b == c *)
        (* If b must be zero, we have must UB *)
        let b = warn_and_top_on_zero b in
        (* a' = a/b*b + c and derived from it b' = (a-c)/(a/b)
         * The idea is to formulate a' as quotient * divisor + remainder. *)
        let a' = ID.add (ID.mul (ID.div a b) b) c in
        let b' = ID.div (ID.sub a c) (ID.div a b) in
        (* However, for [2,4]%2 == 1 this only gives [3,4].
         * If the upper bound of a is divisible by b, we can also meet with the result of a/b*b - c to get the precise [3,3].
         * If b is negative we have to look at the lower bound. *)
        let is_divisible bound =
          GobOption.exists (fun ba -> ID.rem (ID.of_int ikind ba) b |> ID.to_int = Some Z.zero) (bound a)
        in
        let max_pos = match ID.maximal b with None -> true | Some x -> Z.compare x Z.zero >= 0 in
        let min_neg = match ID.minimal b with None -> true | Some x -> Z.compare x Z.zero < 0 in
        let implies a b = not a || b in
        let a'' =
          if implies max_pos (is_divisible ID.maximal) && implies min_neg (is_divisible ID.minimal) then
            ID.meet a' (ID.sub (ID.mul (ID.div a b) b) c)
          else a'
        in
        let a''' =
          (* if both b and c are definite, we can get a precise value in the congruence domain *)
          match ID.to_int b, ID.to_int c with
          | Some b, Some c ->
            (* a%b == c  -> a: c+bℤ *)
            let t = ID.of_congruence ikind (c, b) in
            ID.meet a'' t
          | _, _ -> a''
        in
        let a,b = meet_bin a''' b' in
        (* Special handling for case a % 2 != c *)
        let callerFundec = Node.find_fundec man.node in
        let a = if PrecisionUtil.(is_congruence_active (int_precision_from_fundec_or_config callerFundec)) then
            let two = Z.of_int 2 in
            match ID.to_int b, ID.to_excl_list c with
            | Some b, Some ([v], _) when Z.equal b two ->
              let k = if Z.equal (Z.abs (Z.rem v two)) Z.zero then Z.one else Z.zero in
              ID.meet (ID.of_congruence ikind (k, b)) a
            | _, _ -> a
          else a
        in
        a, b
      | Eq | Ne as op ->
        begin match op, ID.to_bool c with
          | Eq, Some true
          | Ne, Some false -> (* def. equal: if they compare equal, both values must be from the meet *)
            (id_meet_down ~old:a ~c:b, id_meet_down ~old:b ~c:a)
          | Eq, Some false
          | Ne, Some true -> (* def. unequal *)
            (* Both values can not be in the meet together, but it's not sound to exclude the meet from both.
             * e.g. a=[0,1], b=[1,2], meet a b = [1,1], but (a != b) does not imply a=[0,0], b=[2,2] since others are possible: a=[1,1], b=[2,2]
             * Only if a is a definite value, we can exclude it from b: *)
            (* Used to cause inconsistent results:
               interval not sufficiently refined:
                 inv_bin_int: unequal: (Unknown int([-31,31]),[0,1]) and (0,[0,0]); ikind: int; a': (Not {0}([-31,31]),[-2147483648,2147483647]), b': (0,[0,0])
                 binop: m == 0, a': (Not {0}([-31,31]),[0,1]), b': (0,[0,0]) *)
            let excl a b =
              match ID.to_int a with
              | Some x ->
                let ex1 = ID.of_excl_list ikind [x] in
                let ex2 =
                  (* Fix previously inconsistent results by excluding interval bounds. *)
                  let top_ik = ID.top_of ikind in
                  match ID.minimal b, ID.maximal b with
                  | Some lb, Some ub ->
                    let starting = if Z.equal lb x then ID.starting ikind (Z.succ lb) else top_ik in
                    let ending = if Z.equal ub x then ID.ending ikind (Z.pred ub) else top_ik in
                    ID.meet starting ending
                  | _ ->
                    top_ik
                in
                ID.meet ex1 ex2
              | None -> b
            in
            let a' = excl b a in
            let b' = excl a b in
            if M.tracing then M.tracel "inv" "inv_bin_int: unequal: %a and %a; ikind: %a; a': %a, b': %a" ID.pretty a ID.pretty b d_ikind ikind ID.pretty a' ID.pretty b';
            meet_bin a' b'
          | _, _ -> a, b
        end
      | Lt | Le | Ge | Gt as op ->
        (match ID.minimal a, ID.maximal a, ID.minimal b, ID.maximal b with
         | Some l1, Some u1, Some l2, Some u2 ->
           (* if M.tracing then M.tracel "inv" "Op: %s, l1: %Ld, u1: %Ld, l2: %Ld, u2: %Ld" (show_binop op) l1 u1 l2 u2; *)
           (* TODO: This causes inconsistent results:
              def_exc and interval in conflict:
                binop m < 0 with (Not {-1}([-31,31]),[-1,0]) < (0,[0,0]) == (1,[1,1])
                binop: m < 0, a': (Not {-1, 0}([-31,31]),[-1,-1]), b': (0,[0,0]) *)
           (match op, ID.to_bool c with
            | Le, Some true
            | Gt, Some false -> meet_bin (ID.ending ikind u2) (ID.starting ikind l1)
            | Ge, Some true
            | Lt, Some false -> meet_bin (ID.starting ikind l2) (ID.ending ikind u1)
            | Lt, Some true
            | Ge, Some false -> meet_bin (ID.ending ikind (Z.pred u2)) (ID.starting ikind (Z.succ l1))
            | Gt, Some true
            | Le, Some false -> meet_bin (ID.starting ikind (Z.succ l2)) (ID.ending ikind (Z.pred u1))
            | _, _ -> a, b)
         | _ -> a, b)
      | BOr ->
        (* Be careful: inv_exp performs a meet on both arguments of the BOr / BXor. *)
        if PrecisionUtil.get_bitfield () then
          (* refinement based on the following idea: bit set to one in c and set to zero in b must be one in a and bit set to zero in c must be zero in a too (analogously for b) *)
          let ((az, ao), (bz, bo)) = BitfieldDomain.Bitfield.refine_bor (ID.to_bitfield ikind a) (ID.to_bitfield ikind b) (ID.to_bitfield ikind c) in
          ID.meet a (ID.of_bitfield ikind (az, ao)), ID.meet b (ID.of_bitfield ikind (bz, bo))
        else
          (if M.tracing then M.tracel "inv" "Unhandled operator %a" d_binop op;
           (* Be careful: inv_exp performs a meet on both arguments of the BOr / BXor. *)
           (a, b)
          )
      | BXor ->
        (* Be careful: inv_exp performs a meet on both arguments of the BOr / BXor. *)
        meet_com ID.logxor
      | LAnd ->
        if ID.to_bool c = Some true then
          meet_bin c c
        else
          a, b
      | BAnd ->
        (* we only attempt to refine a here *)
        let b_int = ID.to_int b in
        let a =
          match b_int with
          | Some x when Z.equal x Z.one ->
            (match ID.to_bool c with
             | Some true -> ID.meet a (ID.of_congruence ikind (Z.one, Z.of_int 2))
             | Some false -> ID.meet a (ID.of_congruence ikind (Z.zero, Z.of_int 2))
             | None -> a)
          | _ -> a
        in
        if PrecisionUtil.get_bitfield () then
          (* refinement based on the following idea: bit set to zero in c and set to one in b must be zero in a and bit set to one in c must be one in a too (analogously for b) *)
          let ((az, ao), (bz, bo)) = BitfieldDomain.Bitfield.refine_band (ID.to_bitfield ikind a) (ID.to_bitfield ikind b) (ID.to_bitfield ikind c) in
          ID.meet a (ID.of_bitfield ikind (az, ao)), ID.meet b (ID.of_bitfield ikind (bz, bo))
        else if b_int = None then
          (if M.tracing then M.tracel "inv" "Unhandled operator %a" d_binop op;
           (a, b)
          )
        else a, b
      | op ->
        if M.tracing then M.tracel "inv" "Unhandled operator %a" d_binop op;
        a, b
    in
    let inv_bin_float (a, b) c op =
      let open Stdlib in
      let meet_bin a' b'  = fd_meet_down ~old:a ~c:a', fd_meet_down ~old:b ~c:b' in
      (* Refining the abstract values based on branching is roughly based on the idea in [Symbolic execution of floating-point computations](https://hal.inria.fr/inria-00540299/document)
         However, their approach is only applicable to the "nearest" rounding mode. Here we use a more general approach covering all possible rounding modes and therefore
         use the actual `pred c_min`/`succ c_max` for the outer-bounds instead of the middles between `c_min` and `pred c_min`/`c_max` and `succ c_max` as suggested in the paper.
         This also removes the necessity of computing those expressions with higher precise than in the concrete.
      *)
      try
        match op with
        | PlusA  ->
          (* A + B = C, \forall a \in A. a + b_min > pred c_min \land a + b_max < succ c_max
              \land a + b_max > pred c_min \land a + b_min < succ c_max
             \rightarrow A = [min(pred c_min - b_min, pred c_min - b_max), max(succ c_max - b_max, succ c_max - b_min)]
             \rightarrow A = [pred c_min - b_max, succ c_max - b_min]
          *)
          let reverse_add v v' = (match FD.minimal c, FD.maximal c, FD.minimal v, FD.maximal v with
              | Some c_min, Some c_max, Some v_min, Some v_max when Float.is_finite (Float.pred c_min) && Float.is_finite (Float.succ c_max) ->
                let l = Float.pred c_min -. v_max in
                let h =  Float.succ c_max -. v_min in
                FD.of_interval (FD.get_fkind c) (l, h)
              | _ -> v') in
          meet_bin (reverse_add b a) (reverse_add a b)
        | MinusA ->
          (* A - B = C \ forall a \in A. a - b_max > pred c_min \land a - b_min < succ c_max
              \land a - b_min > pred c_min \land a - b_max < succ c_max
             \rightarrow A = [min(pred c_min + b_max, pred c_min + b_min), max(succ c_max + b_max, succ c_max + b_max)]
             \rightarrow A = [pred c_min + b_min, succ c_max + b_max]
          *)
          let a' = (match FD.minimal c, FD.maximal c, FD.minimal b, FD.maximal b with
              | Some c_min, Some c_max, Some b_min, Some b_max when Float.is_finite (Float.pred c_min) && Float.is_finite (Float.succ c_max) ->
                let l = Float.pred c_min +. b_min in
                let h =  Float.succ c_max +. b_max in
                FD.of_interval (FD.get_fkind c) (l, h)
              | _ -> a) in
          (* A - B = C \ forall b \in B. a_min - b > pred c_min \land a_max - b < succ c_max
              \land a_max - b > pred c_min \land a_min - b < succ c_max
             \rightarrow B = [min(a_max - succ c_max, a_min - succ c_max), max(a_min - pred c_min, a_max - pred c_min)]
             \rightarrow B = [a_min - succ c_max, a_max - pred c_min]
          *)
          let b' = (match FD.minimal c, FD.maximal c, FD.minimal a, FD.maximal a with
              | Some c_min, Some c_max, Some a_min, Some a_max when Float.is_finite (Float.pred c_min) && Float.is_finite (Float.succ c_max) ->
                let l = a_min -. Float.succ c_max in
                let h =  a_max -. Float.pred c_min in
                FD.of_interval (FD.get_fkind c) (l, h)
              | _ -> b) in
          meet_bin a'  b'
        | Mult   ->
          (* A * B = C \forall a \in A, a > 0. a * b_min > pred c_min \land a * b_max < succ c_max
             A * B = C \forall a \in A, a < 0. a * b_max > pred c_min \land a * b_min < succ c_max
             (with negative b reversed <>)
             \rightarrow A = [min(pred c_min / b_min, pred c_min / b_max, succ c_max / b_min, succ c_max /b_max),
                              max(succ c_max / b_min, succ c_max /b_max, pred c_min / b_min, pred c_min / b_max)]
          *)
          let reverse_mul v v' = (match FD.minimal c, FD.maximal c, FD.minimal v, FD.maximal v with
              | Some c_min, Some c_max, Some v_min, Some v_max when Float.is_finite (Float.pred c_min) && Float.is_finite (Float.succ c_max) ->
                let v1, v2, v3, v4 = (Float.pred c_min /. v_min), (Float.pred c_min /. v_max), (Float.succ c_max /. v_min), (Float.succ c_max /. v_max) in
                let l = Float.min (Float.min v1 v2) (Float.min v3 v4) in
                let h =  Float.max (Float.max v1 v2) (Float.max v3 v4) in
                FD.of_interval (FD.get_fkind c) (l, h)
              | _ -> v') in
          meet_bin (reverse_mul b a) (reverse_mul a b)
        | Div ->
          (* A / B = C \forall a \in A, a > 0, b_min > 1. a / b_max > pred c_min \land a / b_min < succ c_max
             A / B = C \forall a \in A, a < 0, b_min > 1. a / b_min > pred c_min \land a / b_max < succ c_max
             A / B = C \forall a \in A, a > 0, 0 < b_min, b_max < 1. a / b_max > pred c_min \land a / b_min < succ c_max
             A / B = C \forall a \in A, a < 0, 0 < b_min, b_max < 1. a / b_min > pred c_min \land a / b_max < succ c_max
             ... same for negative b
             \rightarrow A = [min(b_max * pred c_min, b_min * pred c_min, b_min * succ c_max, b_max * succ c_max),
                              max(b_max * succ c_max, b_min * succ c_max, b_max * pred c_min, b_min * pred c_min)]
          *)
          let a' = (match FD.minimal c, FD.maximal c, FD.minimal b, FD.maximal b with
              | Some c_min, Some c_max, Some b_min, Some b_max when Float.is_finite (Float.pred c_min) && Float.is_finite (Float.succ c_max) ->
                let v1, v2, v3, v4 = (Float.pred c_min *. b_max), (Float.pred c_min *. b_min), (Float.succ c_max *. b_max), (Float.succ c_max *. b_min) in
                let l = Float.min (Float.min v1 v2) (Float.min v3 v4) in
                let h =  Float.max (Float.max v1 v2) (Float.max v3 v4) in
                FD.of_interval (FD.get_fkind c) (l, h)
              | _ -> a) in
          (* A / B = C \forall b \in B, b > 0, a_min / b > pred c_min \land a_min / b < succ c_max
              \land a_max / b > pred c_min \land a_max / b < succ c_max
             A / B = C \forall b \in B, b < 0, a_min / b > pred c_min \land a_min / b < succ c_max
              \land a_max / b > pred c_min \land a_max / b < succ c_max
             \rightarrow (b != 0) B = [min(a_min / succ c_max, a_max / succ c_max, a_min / pred c_min, a_max / pred c_min),
                                      max(a_min / pred c_min, a_max / pred c_min, a_min / succ c_max, a_max / succ c_max)]
          *)
          let b' = (match FD.minimal c, FD.maximal c, FD.minimal a, FD.maximal a with
              | Some c_min, Some c_max, Some a_min, Some a_max when Float.is_finite (Float.pred c_min) && Float.is_finite (Float.succ c_max) ->
                let zero_not_in_a = a_min > 0. || a_max < 0. in
                let zero_not_in_c = c_min > 0. || c_max < 0. in
                if zero_not_in_a && zero_not_in_c then
                  let v1, v2, v3, v4 = (a_min /. Float.pred c_min), (a_max /. Float.pred c_min), (a_min /. Float.succ c_max), (a_max /. Float.succ c_max) in
                  let l = Float.min (Float.min v1 v2) (Float.min v3 v4) in
                  let h =  Float.max (Float.max v1 v2) (Float.max v3 v4) in
                  FD.of_interval (FD.get_fkind c) (l, h)
                else
                  b
              | _ -> b) in
          if M.tracing then M.trace "inv_float" "Div: (%a,%a) = %a   yields (%a,%a)" FD.pretty a FD.pretty b FD.pretty c FD.pretty a' FD.pretty b';
          meet_bin a' b'
        | Eq | Ne as op ->
          let both x = x, x in
          (match op, ID.to_bool (FD.to_int IBool c) with
           | Eq, Some true
           | Ne, Some false -> both (FD.meet a b) (* def. equal: if they compare equal, both values must be from the meet *)
           | Eq, Some false
           | Ne, Some true -> (* def. unequal *)
             (* M.debug ~category:Analyzer "Can't use unequal information about float value in expression \"%a\"." d_plainexp exp; *)
             a, b (* TODO: no meet_bin? *)
           | _, _ -> a, b
          )
        | Lt | Le | Ge | Gt as op ->
          (match FD.minimal a, FD.maximal a, FD.minimal b, FD.maximal b with
           | Some l1, Some u1, Some l2, Some u2 ->
             (match op, ID.to_bool (FD.to_int IBool c) with
              | Le, Some true
              | Gt, Some false -> meet_bin (FD.ending (FD.get_fkind a) u2) (FD.starting (FD.get_fkind b) l1)
              | Ge, Some true
              | Lt, Some false -> meet_bin (FD.starting (FD.get_fkind a) l2) (FD.ending (FD.get_fkind b) u1)
              | Lt, Some true
              | Ge, Some false -> meet_bin (FD.ending_before (FD.get_fkind a) u2) (FD.starting_after (FD.get_fkind b) l1)
              | Gt, Some true
              | Le, Some false -> meet_bin (FD.starting_after (FD.get_fkind a) l2) (FD.ending_before (FD.get_fkind b) u1)
              | _, _ -> a, b)
           | _ -> a, b)
        | op ->
          if M.tracing then M.tracel "inv" "Unhandled operator %a" d_binop op;
          a, b
      with FloatDomain.ArithmeticOnFloatBot _ -> raise Analyses.Deadcode
    in
    let eval e st = eval_rv ~man st e in
    let eval_bool e st = match eval e st with Int i -> ID.to_bool i | _ -> None in
    let unroll_fk_of_exp e =
      match unrollType (Cilfacade.typeOf e) with
      | TFloat (fk, _) -> fk
      | _ -> failwith "value which was expected to be a float is of different type?!"
    in
    let rec inv_exp c_typed exp (st:D.t): D.t =
      (* trying to improve variables in an expression so it is bottom means dead code *)
      if VD.is_bot_value c_typed then contra st
      else
        match exp, c_typed with
        | UnOp (LNot, e, _), Int c ->
          (match Cil.unrollType (Cilfacade.typeOf e) with
           | TInt  _ | TEnum _ | TPtr _ ->
             let ikind = Cilfacade.get_ikind_exp e in
             let c' =
               match ID.to_bool (unop_ID LNot c) with
               | Some true ->
                 (* i.e. e should evaluate to [1,1] *)
                 (* LNot x is 0 for any x != 0 *)
                 ID.of_excl_list ikind [Z.zero]
               | Some false -> ID.of_bool ikind false
               | _ -> ID.top_of ikind
             in
             inv_exp (Int c') e st
           | TFloat(fkind, _) when ID.to_bool (unop_ID LNot c) = Some false ->
             (* C99 §6.5.3.3/5 *)
             (* The result of the logical negation operator ! is 0 if the value of its operand compares *)
             (* unequal to 0, 1 if the value of its operand compares equal to 0. The result has type int. *)
             (* The expression !E is equivalent to (0==E). *)
             (* NaN compares unequal to 0 so no problems *)
             (* We do not have exclusions for floats, so we do not bother here with the other case *)
             let zero_float = FD.of_const fkind 0. in
             inv_exp (Float zero_float) e st
           | _ -> st
          )
        | UnOp (Neg, e, _), Float c -> inv_exp (unop_FD Neg c) e st
        | UnOp ((BNot|Neg) as op, e, _), Int c -> inv_exp (Int (unop_ID op c)) e st
        (* no equivalent for Float, as VD.is_statically_safe_cast fails for all float types anyways *)
        | BinOp((Eq | Ne) as op, CastE (_, t1, e1), CastE (_, t2, e2), t), Int c when typeSig (Cilfacade.typeOf e1) = typeSig (Cilfacade.typeOf e2) && VD.is_statically_safe_cast t1 (Cilfacade.typeOf e1) && VD.is_statically_safe_cast t2 (Cilfacade.typeOf e2) ->
          inv_exp (Int c) (BinOp (op, e1, e2, t)) st
        | BinOp (LOr, arg1, arg2, typ) as exp, Int c ->
          (* copied & modified from eval_rv_base... *)
          let (let*) = Option.bind in
          (* split nested LOr Eqs to equality pairs, if possible *)
          let rec split = function
            (* copied from above to support pointer equalities with implicit casts inserted *)
            | BinOp (Eq, CastE (_, t1, e1), CastE (_, t2, e2), typ) when typeSig (Cilfacade.typeOf e1) = typeSig (Cilfacade.typeOf e2) && VD.is_statically_safe_cast t1 (Cilfacade.typeOf e1) && VD.is_statically_safe_cast t2 (Cilfacade.typeOf e2) -> (* slightly different from eval_rv_base... *)
              Some [(e1, e2)]
            | BinOp (Eq, arg1, arg2, _) ->
              Some [(arg1, arg2)]
            | BinOp (LOr, arg1, arg2, _) ->
              let* s1 = split arg1 in
              let* s2 = split arg2 in
              Some (s1 @ s2)
            | _ ->
              None
          in
          (* find common exp from all equality pairs and list of other sides, if possible *)
          let find_common = function
            | [] -> assert false
            | (e1, e2) :: eqs ->
              let eqs_for_all_mem e = List.for_all (fun (e1, e2) -> CilType.Exp.(equal e1 e || equal e2 e)) eqs in
              let eqs_map_remove e = List.map (fun (e1, e2) -> if CilType.Exp.equal e1 e then e2 else e1) eqs in
              if eqs_for_all_mem e1 then
                Some (e1, e2 :: eqs_map_remove e1)
              else if eqs_for_all_mem e2 then
                Some (e2, e1 :: eqs_map_remove e2)
              else
                None
          in
          let eqs_st =
            let* eqs = split exp in
            let* (e, es) = find_common eqs in
            let v = eval e st in (* value of common exp *)
            let vs = List.map (fun e -> eval e st) es in (* values of other sides *)
            match v with
            | Address _ ->
              (* get definite addrs from vs *)
              let rec to_definite_ad = function
                | [] -> AD.empty ()
                | VD.Address a :: vs when AD.is_definite a ->
                  AD.union a (to_definite_ad vs)
                | _ :: vs ->
                  AD.top ()
              in
              let definite_ad = to_definite_ad vs in
              let c' = VD.Address definite_ad in
              Some (inv_exp c' e st)
            | Int i ->
              let ik = ID.ikind i in
              let module BISet = IntDomain.BISet in
              (* get definite ints from vs *)
              let rec to_int_id = function
                | [] -> ID.bot_of ik
                | VD.Int i :: vs ->
                  begin match ID.to_int i with
                    | Some i' -> ID.join i (to_int_id vs)
                    | None -> ID.top_of ik
                  end
                | _ :: vs ->
                  ID.top_of ik
              in
              let int_id = to_int_id vs in
              let c' = VD.Int int_id in
              Some (inv_exp c' e st)
            | _ ->
              None
          in
          begin match eqs_st with
            | Some st -> st
            | None when ID.to_bool c = Some true ->
              begin match inv_exp (Int c) arg1 st with
                | st1 ->
                  begin match inv_exp (Int c) arg2 st with
                    | st2 -> D.join st1 st2
                    | exception Analyses.Deadcode -> st1
                  end
                | exception Analyses.Deadcode -> inv_exp (Int c) arg2 st (* Deadcode falls through *)
              end
            | None ->
              st (* TODO: not bothering to fall back, no other case can refine LOr anyway *)
          end
        | (BinOp (op, e1, e2, _) as e, Float _)
        | (BinOp (op, e1, e2, _) as e, Int _) ->
          let invert_binary_op c pretty c_int c_float =
            if M.tracing then M.tracel "inv" "binop %a with %a %a %a == %a" d_exp e VD.pretty (eval e1 st) d_binop op VD.pretty (eval e2 st) pretty c;
            (match eval e1 st, eval e2 st with
             | Int a, Int b ->
               let ikind = Cilfacade.get_ikind_exp e1 in (* both operands have the same type (except for Shiftlt, Shiftrt)! *)
               let ikres = Cilfacade.get_ikind_exp e in (* might be different from argument types, e.g. for LT, GT, EQ, ... *)
               let a', b' = inv_bin_int (a, b) ikind (c_int ikres) op in
               if M.tracing then M.tracel "inv" "binop: %a, c: %a, a': %a, b': %a" d_exp e ID.pretty (c_int ikind) ID.pretty a' ID.pretty b';
               let st' = inv_exp (Int a') e1 st in
               let st'' = inv_exp (Int b') e2 st' in
               st''
             | Float a, Float b ->
               let fkind = Cilfacade.get_fkind_exp e1 in (* both operands have the same type *)
               let a', b' = inv_bin_float (a, b) (c_float fkind) op in
               if M.tracing then M.tracel "inv" "binop: %a, c: %a, a': %a, b': %a" d_exp e FD.pretty (c_float fkind) FD.pretty a' FD.pretty b';
               let st' = inv_exp (Float a') e1 st in
               let st'' = inv_exp (Float b') e2 st' in
               st''
             (* Mixed Float and Int cases should never happen, as there are no binary operators with one float and one int parameter ?!*)
             | Int _, Float _ | Float _, Int _ -> failwith "ill-typed program";
               (* | Address a, Address b -> ... *)
             | a1, a2 -> fallback (fun () -> Pretty.dprintf "binop: got abstract values that are not Int: %a and %a" VD.pretty a1 VD.pretty a2) st)
            (* use closures to avoid unused casts *)
          in (match c_typed with
              | Int c -> invert_binary_op c ID.pretty (fun ik -> ID.cast_to ~kind:Internal ik c) (fun fk -> FD.of_int fk c) (* TODO: proper castkind *)
              | Float c -> invert_binary_op c FD.pretty (fun ik -> FD.to_int ik c) (fun fk -> FD.cast_to fk c)
              | _ -> failwith "unreachable")
        | Lval x, (Int _ | Float _ | Address _) -> (* meet x with c *)
          let update_lval c x c' pretty = refine_lv man st c x c' pretty exp in
          let t = Cil.unrollType (Cilfacade.typeOfLval x) in  (* unroll type to deal with TNamed *)
          if M.tracing then M.trace "invSpecial" "invariant with Lval %a, c_typed %a, type %a" d_lval x VD.pretty c_typed d_type t;
          begin match c_typed with
            | Int c ->
              let c' = match t with
                | TPtr _ -> VD.Address (AD.of_int c)
                | TInt (ik, _)
                | TEnum ({ekind = ik; _}, _) -> Int (ID.cast_to ~kind:Internal ik c) (* TODO: proper castkind *)
                | TFloat (fk, _) -> Float (FD.of_int fk c)
                | _ -> Int c
              in
              (* handle special calls *)
              let default () = update_lval c x c' ID.pretty in
              begin match x, t with
                | (Var v, offs), TInt (ik, _) ->
                  let tmpSpecial = man.ask (Queries.TmpSpecial (v, Offset.Exp.of_cil offs)) in
                  if M.tracing then M.trace "invSpecial" "qry Result: %a" Queries.ML.pretty tmpSpecial;
                  begin match tmpSpecial with
                    | `Lifted (Abs (ik, xInt)) ->
                      let c' = ID.cast_to ~kind:Internal ik c in (* different ik! *) (* TODO: proper castkind *)
                      inv_exp (Int (ID.join c' (ID.neg c'))) xInt st
                    | tmpSpecial ->
                      BatOption.map_default_delayed (fun tv ->
                          match tmpSpecial with
                          | `Lifted (Isfinite xFloat) when tv -> inv_exp (Float (FD.finite (unroll_fk_of_exp xFloat))) xFloat st
                          | `Lifted (Isnan xFloat) when tv -> inv_exp (Float (FD.nan_of (unroll_fk_of_exp xFloat))) xFloat st
                          (* should be correct according to C99 standard*)
                          (* The following do to_bool and of_bool to convert Not{0} into 1 for downstream float inversions *)
                          | `Lifted (Isgreater (xFloat, yFloat)) -> inv_exp (Int (ID.of_bool ik tv)) (BinOp (Gt, xFloat, yFloat, (typeOf xFloat))) st
                          | `Lifted (Isgreaterequal (xFloat, yFloat)) -> inv_exp (Int (ID.of_bool ik tv)) (BinOp (Ge, xFloat, yFloat, (typeOf xFloat))) st
                          | `Lifted (Isless (xFloat, yFloat)) -> inv_exp (Int (ID.of_bool ik tv)) (BinOp (Lt, xFloat, yFloat, (typeOf xFloat))) st
                          | `Lifted (Islessequal (xFloat, yFloat)) -> inv_exp (Int (ID.of_bool ik tv)) (BinOp (Le, xFloat, yFloat, (typeOf xFloat))) st
                          | `Lifted (Islessgreater (xFloat, yFloat)) -> inv_exp (Int (ID.of_bool ik tv)) (BinOp (LOr, (BinOp (Lt, xFloat, yFloat, (typeOf xFloat))), (BinOp (Gt, xFloat, yFloat, (typeOf xFloat))), (TInt (IBool, [])))) st
                          | _ -> default ()
                        ) default (ID.to_bool c)
                  end
                | _, _ -> default ()
              end
            | Float c ->
              let c' = match t with
                (* | TPtr _ -> ..., pointer conversion from/to float is not supported *)
                | TInt (ik, _) -> VD.Int (FD.to_int ik c)
                (* this is theoretically possible and should be handled correctly, however i can't imagine an actual piece of c code producing this?! *)
                | TEnum ({ekind = ik; _}, _) -> Int (FD.to_int ik c)
                | TFloat (fk, _) -> Float (FD.cast_to fk c)
                | _ -> Float c
              in
              (* handle special calls *)
              begin match x, t with
                | (Var v, offs), TFloat (fk, _) ->
                  let tmpSpecial = man.ask (Queries.TmpSpecial (v, Offset.Exp.of_cil offs)) in
                  if M.tracing then M.trace "invSpecial" "qry Result: %a" Queries.ML.pretty tmpSpecial;
                  begin match tmpSpecial with
                    | `Lifted (Ceil (ret_fk, xFloat)) -> inv_exp (Float (FD.inv_ceil (FD.cast_to ret_fk c))) xFloat st
                    | `Lifted (Floor (ret_fk, xFloat)) -> inv_exp (Float (FD.inv_floor (FD.cast_to ret_fk c))) xFloat st
                    | `Lifted (Fabs (ret_fk, xFloat)) ->
                      let inv = FD.inv_fabs (FD.cast_to ret_fk c) in
                      if FD.is_bot inv then
                        raise Analyses.Deadcode
                      else
                        inv_exp (Float inv) xFloat st
                    | _ -> update_lval c x c' FD.pretty
                  end
                | _ -> update_lval c x c' FD.pretty
              end
            | Address c ->
              let c' = c_typed in (* TODO: need any of the type-matching nonsense? *)
              update_lval c x c' AD.pretty
            | _ -> assert false
          end
        | Const _ , _ -> st (* nothing to do *)
        | CastE (k, t, e), c_typed ->
          begin match Cil.unrollType t, c_typed with
            | TFloat (_, _), Float c ->
              (match unrollType (Cilfacade.typeOf e), FD.get_fkind c with
               | TFloat (FLongDouble as fk, _), FFloat
               | TFloat (FDouble as fk, _), FFloat
               | TFloat (FLongDouble as fk, _), FDouble
               | TFloat (fk, _), FLongDouble
               | TFloat (FDouble as fk, _), FDouble
               | TFloat (FFloat as fk, _), FFloat -> inv_exp (Float (FD.cast_to fk c)) e st
               | _ -> fallback (fun () -> Pretty.text "CastE: incompatible types") st)
            | (TInt (ik, _) as t), Int c
            | (TEnum ({ekind = ik; _ }, _) as t), Int c -> (* Can only meet the t part of an Lval in e with c (unless we meet with all overflow possibilities)! Since there is no good way to do this, we only continue if e has no values outside of t. *)
              (match eval e st with
               | Int i ->
                 (match unrollType (Cilfacade.typeOf e) with
                  | (TInt(ik_e, _) as t')
                  | (TEnum ({ekind = ik_e; _ }, _) as t') ->
                    if VD.is_dynamically_safe_cast t t' (Int i) then
                      (* let c' = ID.cast_to ik_e c in *)
                      (* Suppressing overflow warnings as this is not a computation that comes from the program *)
                      let res_range = (ID.cast_to ~suppress_ovwarn:true ~kind:Internal ik (ID.top_of ik_e)) in (* TODO: proper castkind *)
                      let c' = ID.cast_to ~kind:Internal ik_e (ID.meet c res_range) in (* TODO: cast without overflow, is this right for normal invariant? *) (* TODO: proper castkind *)
                      if M.tracing then M.tracel "inv" "cast: %a from %a to %a: i = %a; cast c = %a to %a = %a" d_exp e d_ikind ik_e d_ikind ik ID.pretty i ID.pretty c d_ikind ik_e ID.pretty c';
                      inv_exp (Int c') e st
                    else
                      fallback (fun () -> Pretty.dprintf "CastE: %a evaluates to %a which is bigger than the type it is cast to which is %a" d_plainexp e ID.pretty i CilType.Typ.pretty t) st
                  | x -> fallback (fun () -> Pretty.dprintf "CastE: e did evaluate to Int, but the type did not match %a" CilType.Typ.pretty t) st)
               | v -> fallback (fun () -> Pretty.dprintf "CastE: e did not evaluate to Int, but %a" VD.pretty v) st)
            | _, _ -> fallback (fun () -> Pretty.dprintf "CastE: %a not implemented" d_plainexp (CastE (k, t, e))) st
          end
        | e, _ -> fallback (fun () -> Pretty.dprintf "%a not implemented" d_plainexp e) st
    in
    if eval_bool exp st = Some (not tv) then contra st (* we already know that the branch is dead *)
    else
      match Cilfacade.get_ikind_exp exp with
      | ik ->
        let itv = if not tv || Cilfacade.exp_is_boolean exp then (* false is 0, but true can be anything that is not 0, except for comparisons which yield 1 *)
            ID.of_bool ik tv (* this will give 1 for true which is only ok for comparisons *)
          else
            ID.of_excl_list ik [Z.zero] (* Lvals, Casts, arithmetic operations etc. should work with true = non_zero *)
        in
        inv_exp (Int itv) exp st
      | exception Invalid_argument _ ->
        let fk = Cilfacade.get_fkind_exp exp in
        let ftv = if not tv then (* false is 0, but true can be anything that is not 0, except for comparisons which yield 1 *)
            FD.of_const fk 0.
          else
            FD.top_of fk
        in
        inv_exp (Float ftv) exp st

  let invariant man st exp tv =
    (* The computations that happen here are not computations that happen in the programs *)
    (* Any overflow during the forward evaluation will already have been flagged here *)
    GobRef.wrap AnalysisState.executing_speculative_computations true
    @@ fun () -> invariant man st exp tv
end
