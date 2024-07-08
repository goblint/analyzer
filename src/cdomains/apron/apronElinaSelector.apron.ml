open Apron

(* Selects between apron and elina functions *)
module ApronElinaSelector = struct
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

    (* Generic interface, set to true for elina and false for apron *)
    let substitute_texpr_with =
    if true then
        substitute_texpr_with_alt
    else
        Abstract1.substitute_texpr_with

end