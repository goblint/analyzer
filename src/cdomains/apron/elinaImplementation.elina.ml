open Apron

module ElinaImplementation = struct
  (*  This is supposed to implement the same functionality as Abstract1.substitute_texpr_with
      which doesn't work correctly in elina - it leads to segmentation faults
      The relevant github issue can be found here:
      https://github.com/eth-sri/ELINA/issues/97
  
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

  (* Functions with different names in Elina for Octagon Domain *)
  type t = Elina_oct.t
  let to_oct = Elina_oct.Abstract1.to_opt_oct
  let of_oct = Elina_oct.Abstract1.of_opt_oct
  let widening_thresholds = Elina_oct.elina_abstract0_opt_oct_widening_thresholds
  let substitute_texpr_with = substitute_texpr_with_alt
  let manager_is_oct = Elina_oct.manager_is_opt_oct
  let manager_to_oct = Elina_oct.manager_to_opt_oct
  let narrowing = Elina_oct.elina_abstract0_opt_oct_narrowing
  let manager_alloc = Elina_oct.manager_alloc

  (* Functions with different names in Elina for Polyhedra Domain *)
  type pt = Elina_poly.loose Elina_poly.t
  let manager_alloc_loose = Elina_poly.manager_alloc_loose

  (*  Using Abstract1.hash like in the apron interface leads to segmentation faults,
      so this is used as a workaround.
      The relevant github issue can be found here:
      https://github.com/eth-sri/ELINA/issues/99
  *)
  let hash _ x = Environment.hash (Abstract1.env x)

  (***************)
  (* Bound Texpr *)
  (***************)

  (* Scalar to Mpqf - Converts a scalar which may take several types to a Mpqf *)
  let scalar_to_mpqf scalar =
    match scalar with
    | Scalar.Float fl -> Mpqf.of_float fl
    | Scalar.Mpqf mpqf -> mpqf
    | Scalar.Mpfrf mpfrf -> Mpfrf.to_mpqf mpfrf

  (* Mpqf to scalar interval - Creates an interval from two passed Mpqf values *)
  let mpqfs_to_interval m1 m2 =
    Interval.of_scalar (Scalar.of_mpqf m1) (Scalar.of_mpqf m2)

  (* Binop/Mod case *)
  let bound_texpr_mod i1 _ s1 s2 = 
    let inf1 = scalar_to_mpqf i1 in  
    let sup1 = scalar_to_mpqf s1 in
    let sup2 = scalar_to_mpqf s2 in
    let zero = Mpqf.of_int 0 in
    let one = Mpqf.of_int 1 in
    let sup2mo = Mpqf.sub sup2 one in
    (* If dividend is always positive the range is [0;S2-1] or possibly a smaller range *)
    if Mpqf.sgn inf1 = 1 && Mpqf.sgn sup1 = 1 then
      mpqfs_to_interval zero sup2mo
    (* If dividend is always negative the range is [-S2+1;0] or possibly a smaller range *)
    else if Mpqf.sgn inf1 = -1 && Mpqf.sgn sup1 = -1 then
      mpqfs_to_interval (Mpqf.neg sup2mo) zero
    (* If dividend could be both negative or positive the range could be [-S2+1;S2-1]*)
    else
      mpqfs_to_interval (Mpqf.neg sup2mo) sup2mo

  (* Binop/Pow case *)
  (* Internal recursive functions *)
  let rec bound_texpr_pow_rec a b = 
    let one = Mpqf.of_int 1 in
    if Mpqf.sgn b = 1 then 
      Mpqf.mul a (bound_texpr_pow_rec a (Mpqf.sub b one))
    else if Mpqf.sgn b = -1 then
      Mpqf.div one (bound_texpr_pow_rec a (Mpqf.neg b))
    else
      one
    
  (* Main pow function which is called by bound_texpr_alt *)
  let bound_texpr_pow inf1 inf2 sup1 sup2 =
    let inf_pow = (bound_texpr_pow_rec (scalar_to_mpqf inf1) (scalar_to_mpqf inf2)) in
    let sup_pow = (bound_texpr_pow_rec (scalar_to_mpqf sup1) (scalar_to_mpqf sup2)) in
    mpqfs_to_interval inf_pow sup_pow

  (* Unop/Sqrt case *)
  let bound_texpr_sqrt inf sup =
    let zero = (Mpqf.of_int 0) in
    let one = (Mpqf.of_int 1) in
    let two = (Mpqf.of_int 2) in
    (* Approximate sqrt by returning a range of [0;(sup/2)+1] *)
    let sup_div2 = (Mpqf.div (scalar_to_mpqf sup) two) in
    mpqfs_to_interval zero (Mpqf.add sup_div2 one)

  (*  Workaround for Abstract1.bound_texpr
      This creates an approximation based on the bounds
      of the component variables which is not as precise
      as the actual bound_texpr could be 
  *)
  let rec bound_texpr_alt mgr d exprt1 =
    match exprt1 with
    (* Constant *)
    | Texpr1.Cst cst ->
      (match cst with
      (* Scalar - Return interval [cst;cst] *)
      | Coeff.Scalar coeff ->
        Interval.of_scalar coeff coeff
      (* Interval - Return interval directly *)
      | Coeff.Interval intv ->
        intv
      )
    (* Variable - Use Abstract1.bound_variable function *)
    | Texpr1.Var var ->
      Abstract1.bound_variable mgr d var
    (* Unary Operation - Apply operation on the bound returned by the component expression *)
    | Texpr1.Unop (unop,expr,typ,round) ->
      let bounds = bound_texpr_alt mgr d expr in
      (match unop with
      (* Negation: [-sup,-inf]*)
      | Texpr1.Neg ->
        Interval.of_scalar (Scalar.neg bounds.sup) (Scalar.neg bounds.inf)
      (* Cast: [inf,sup] *)
      | Texpr1.Cast ->
        bounds
      (* Square Root: [0;(sup2/2)+1] *)
      | Texpr1.Sqrt ->
        bound_texpr_sqrt bounds.inf bounds.sup
      )
    (* Binary Operation - Apply operation on the two bounds returned by the component expressions *)
    | Texpr1.Binop (binop,expr1,expr2,typ,round) ->
      let bounds1 = bound_texpr_alt mgr d expr1 in
      let bounds2 = bound_texpr_alt mgr d expr2 in
      (match binop with
      (* Addition: [inf1+inf2;sup1+sup2] *)
      | Texpr1.Add ->
        let inf = (Mpqf.add (scalar_to_mpqf bounds1.inf) (scalar_to_mpqf bounds2.inf)) in
        let sup = (Mpqf.add (scalar_to_mpqf bounds1.sup) (scalar_to_mpqf bounds2.sup)) in
        mpqfs_to_interval inf sup
      (* Substraction: [inf1-sup2;sup1-inf2] *)
      | Texpr1.Sub ->
        let inf = (Mpqf.sub (scalar_to_mpqf bounds1.inf) (scalar_to_mpqf bounds2.sup)) in
        let sup = (Mpqf.sub (scalar_to_mpqf bounds1.sup) (scalar_to_mpqf bounds2.inf)) in
        mpqfs_to_interval inf sup
      (* Multiplication: [inf1*inf2;sup1*sup2] *)
      | Texpr1.Mul ->
        let inf = (Mpqf.mul (scalar_to_mpqf bounds1.inf) (scalar_to_mpqf bounds2.inf)) in
        let sup = (Mpqf.mul (scalar_to_mpqf bounds1.sup) (scalar_to_mpqf bounds2.sup)) in
        mpqfs_to_interval inf sup
      (* Division: [inf1/sup2;sup1/inf2] *)
      | Texpr1.Div ->
        let inf = (Mpqf.div (scalar_to_mpqf bounds1.inf) (scalar_to_mpqf bounds2.sup)) in
        let sup = (Mpqf.div (scalar_to_mpqf bounds1.sup) (scalar_to_mpqf bounds2.inf)) in
        mpqfs_to_interval inf sup
      (* Modulo: if(inf1>0&&sup1>0)[0;sup2-1] elseif(inf1<0&&sup2<0)[-sup2+1;0] else[sup2+1;-sup2-1] *)
      | Texpr1.Mod ->
        bound_texpr_mod bounds1.inf bounds2.inf bounds1.sup bounds2.sup
      (* Power: [inf1^inf2;sup1^sup2] *)
      | Texpr1.Pow ->
        bound_texpr_pow bounds1.inf bounds2.inf bounds1.sup bounds2.sup
      )

    (*  The Abstract1.bound_texpr does not work for the Polyhedra Domain in Elina 
        as a workaround, bound_texpr_alt is used when using a Polyhedra Domain manager.
        The relevant issue for this can be found here:
        https://github.com/eth-sri/ELINA/issues/94
    *)
    let bound_texpr mgr name d texpr1 =
      if name = "Polyhedra" then
        bound_texpr_alt mgr d (Texpr1.to_expr texpr1)
      else 
        Abstract1.bound_texpr mgr d texpr1
end
