open Apron

module ElinaImplementation = struct
  (*
  This is supposed to implement the same functionality as Abstract1.substitute_texpr_with
  which for some reason doesn't seem to work correctly in elina?
  
  We do not use the last argument for substitute_texpr_with, so it is ignored.

  This is achieved with the following steps
  1) Create a new expression "old val-new val"
  2) Create a new constraint "old val-new val=0" (equal to "old val = new val")
  3) Intersect, ending up with "old val" being replaced by "new val"
  *)
  let substitute_texpr_with_alt man av ov nv _ =
    (* Create expressions which are just the original and new value *)
    let oe = Texpr1.Var ov in
    let ne = Texpr1.to_expr nv in
    (* Create a new expression which is "old value - new value" *)
    let oe_min_ne = Texpr1.Binop (Texpr1.Sub, oe, ne, Texpr1.Real, Texpr1.Rnd) in
    let oe_min_ne_expr = Texpr1.of_expr (Abstract1.env av) oe_min_ne in
    (* Create the new constraint *)
    let sub_constraint = Tcons1.make oe_min_ne_expr Tcons1.EQ in

    (* Create a constraint array to be used by meet_tcons_array_with *)
    let tcons_ar = Tcons1.array_make (Abstract1.env av) 1 in
    (* Add our new constraint to the array *)
    Tcons1.array_set tcons_ar 0 sub_constraint;
    (* Intersect? *)
    Abstract1.meet_tcons_array_with man av tcons_ar

  (* Oct *)
  type t = Elina_oct.t
  let to_oct = Elina_oct.Abstract1.to_opt_oct
  let of_oct = Elina_oct.Abstract1.of_opt_oct
  let widening_thresholds = Elina_oct.elina_abstract0_opt_oct_widening_thresholds
  let substitute_texpr_with = substitute_texpr_with_alt
  let manager_is_oct = Elina_oct.manager_is_opt_oct
  let manager_to_oct = Elina_oct.manager_to_opt_oct
  let narrowing = Elina_oct.elina_abstract0_opt_oct_narrowing
  let manager_alloc = Elina_oct.manager_alloc

  (* Poly *)
  type pt = Elina_poly.loose Elina_poly.t
  let manager_alloc_loose = Elina_poly.manager_alloc_loose

  (* Other *)
  (*
  Using Abstract1.hash like in the apron interface leads to segmentation faults,
  so this is used as a workaround
  *)
  let hash _ x = Environment.hash (Abstract1.env x)
  let impl () = "Elina"


  (***************)
  (* Bound Texpr *)
  (***************)

  (* Avoid Mpqf x/0 error *)
  let is_den_zero mpqf =
    let _, den = Mpqf.to_mpzf2 mpqf in
    (Mpzf.to_float den) = 0.0

  (* Int bounds for float **)
  let ibf fl =
    if fl > 2147483647.0 then 2147483647.0 else if fl < -2147483648.0 then -2147483648.0 else fl

  (* Scalar to float *)
  let stf scal =
    match scal with
    | Scalar.Float fl -> ibf fl
    | Scalar.Mpqf mpqf -> if is_den_zero mpqf then 2147483647.0 else ibf (Mpqf.to_float mpqf)
    | Scalar.Mpfrf mpfrf -> ibf (Mpfrf.to_float mpfrf)

  (* Float to scalar *)
  (* Creates an interval with ints from two passed float values *)
  let fts fl1 fl2 =
    let fl1b = ibf fl1 in
    let fl2b = ibf fl2 in
    Interval.of_scalar (Scalar.of_int (Float.to_int fl1b)) (Scalar.of_int (Float.to_int fl2b))

  (* Binop/Mod case *)
  let bound_texpr_mod i1 _ s1 s2 = 
    (* There seem to be SO many cases for mod, I dont quite get it *)
    let inf1 = stf i1 in  
    let sup1 = stf s1 in
    let sup2 = stf s2 in
    (* If dividend is always positive the range is [0;S2-1] or possibly a smaller range *)
    if inf1 >= 0.0 && sup1 >= 0.0 then
      fts 0.0 (sup2 -. 1.0)
    (* If dividend is always negative the range is [-S2+1;0] or possibly a smaller range *)
    else if inf1 <= 0.0 && sup1 <= 0.0 then
      fts (0.0 -. (sup2 -. 1.0)) 0.0
    (* If dividen could be both negative or positive the range could be [-S2+1;S2-1]*)
    else
      fts (0.0 -. (sup2 -. 1.0)) (sup2 -. 1.0)
    (* This fails to consider many subcases such as the fact that [1;2]%[3;4]
    would be in range [1;2] not [1;3], etc. Also I'm not even sure I understand the negative
    result thing correctly*)

  (* Workaround for Abstract1.bound_texpr *)
  let rec bound_texpr_alt mgr d exprt1 orig =
    match exprt1 with
    (* Constant *)
    | Texpr1.Cst cst ->
      (match cst with
      | Coeff.Scalar coeff -> Interval.of_scalar coeff coeff
      | Coeff.Interval intv -> intv
      )
    (* Variable *)
    | Texpr1.Var var -> Abstract1.bound_variable mgr d var
    (* Unary *)
    | Texpr1.Unop (unop,expr,typ,round) ->
      let bounds = bound_texpr_alt mgr d expr orig in
      (match unop with
      | Texpr1.Neg -> Interval.of_scalar (Scalar.neg bounds.inf) (Scalar.neg bounds.sup)
      | Texpr1.Cast -> bounds (* Unsure? *)
      | Texpr1.Sqrt -> fts (Float.sqrt (stf bounds.inf)) (Float.sqrt (stf bounds.sup))
      )
    (* Binary *)
    | Texpr1.Binop (binop,expr1,expr2,typ,round) ->
      let bounds1 = bound_texpr_alt mgr d expr1 orig in
      let bounds2 = bound_texpr_alt mgr d expr2 orig in
      (match binop with
      | Texpr1.Add -> fts ((stf bounds1.inf) +. (stf bounds2.inf)) ((stf bounds1.sup) +. (stf bounds2.sup))
      | Texpr1.Sub -> fts ((stf bounds1.inf) -. (stf bounds2.sup)) ((stf bounds1.sup) -. (stf bounds2.inf))
      | Texpr1.Mul -> fts ((stf bounds1.inf) *. (stf bounds2.inf)) ((stf bounds1.sup) *. (stf bounds2.sup))
      | Texpr1.Div -> fts ((stf bounds1.inf) /. (stf bounds2.sup)) ((stf bounds1.sup) /. (stf bounds2.inf))
      | Texpr1.Mod -> bound_texpr_mod bounds1.inf bounds2.inf bounds1.sup bounds2.sup
      | Texpr1.Pow -> fts ((stf bounds1.inf) ** (stf bounds2.sup)) ((stf bounds1.sup) ** (stf bounds2.inf))
      )

    let bound_texpr mgr name d texpr1 =
      if name = "Polyhedra" then
        bound_texpr_alt mgr d (Texpr1.to_expr texpr1) texpr1
      else 
        Abstract1.bound_texpr mgr d texpr1
end