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
end