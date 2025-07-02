(** Implementation of the pentagon domain (pntg)
    @see <https://doi.org/10.1016/j.scico.2009.04.004> 
    "Pentagons: A weakly relational abstract domain for the efficient validation of array accesses"
    -- Francesco Logozzo, Manuel Fähndrich (2010) *)

open Batteries
open GoblintCil
module M = Messages
open GobApron
open BatList
open Pretty

(* Pentagon specific modules *)
open StringUtils
open ZExt
open Intv
open Boxes
open Subs
open Pntg



module Rhs = struct
  (* Rhs represents coefficient*var_i + offset / divisor
     depending on whether coefficient is 0, the monomial term may disappear completely, not refering to any var_i, thus:
     (Some (coefficient, i), offset, divisor )         with coefficient != 0    , or
     (None                 , offset, divisor ) *)
  type t = ((GobZ.t * int) option *  GobZ.t * GobZ.t) [@@deriving eq, ord, hash]
  let var_zero i = (Some (Z.one,i), Z.zero, Z.one)
  let show_coeff c =
    if Z.equal c Z.one then ""
    else if Z.equal c Z.minus_one then "-"
    else (Z.to_string c) ^"·"
  let show_rhs_formatted formatter = let ztostring n = (if Z.(geq n zero) then "+" else "") ^ Z.to_string n in
    function
    | (Some (coeff,v), o,_) when Z.equal o Z.zero -> Printf.sprintf "%s%s" (show_coeff coeff) (formatter v)
    | (Some (coeff,v), o,_) -> Printf.sprintf "%s%s %s" (show_coeff coeff) (formatter v) (ztostring o)
    | (None,   o,_) -> Printf.sprintf "%s" (Z.to_string o)
  let show (v,o,d) =
    let rhs=show_rhs_formatted (Printf.sprintf "var_%d") (v,o,d) in
    if not (Z.equal d Z.one) then "(" ^ rhs ^ ")/" ^ (Z.to_string d) else rhs

  (** factor out gcd from all terms, i.e. ax=by+c with a positive is the canonical form for adx+bdy+cd *)
  let canonicalize (v,o,d) =
    let gcd = Z.gcd o d in (* gcd of coefficients *)
    let gcd = Option.map_default (fun (c,_) -> Z.gcd c gcd) gcd v in (* include monomial in gcd computation *)
    let commondivisor = if Z.(lt d zero) then Z.neg gcd else gcd in (* canonical form dictates d being positive *)
    (BatOption.map (fun (coeff,i) -> (Z.div coeff commondivisor,i)) v, Z.div o commondivisor, Z.div d commondivisor)

  (** Substitute rhs for varx in rhs' *)
  let subt rhs varx rhs' =
    match rhs,rhs' with
    | (monom, o, d), (Some (c', x'), o', d') when x'=varx -> canonicalize (Option.map (fun (c,x) -> (Z.mul c c',x)) monom, Z.((o*c')+(d*o')), Z.mul d d')
    | _ -> rhs'

end


module type Tracked =
sig
  val type_tracked: typ -> bool
  val varinfo_tracked: varinfo -> bool
end



(** [VarManagement] defines the type t of the affine equality domain (a record that contains an optional matrix and an apron environment) and provides the functions needed for handling variables (which are defined by [RelationDomain.D2]) such as [add_vars], [remove_vars].
    Furthermore, it provides the function [simplified_monomials_from_texp] that converts an apron expression into a list of monomials of reference variables and a constant offset *)
module VarManagement =
struct
  include SharedFunctions.VarManagementOps (Pntg)
end

(* Helper functions *)
let z_ext_of_scalar (s: Scalar.t) = 
  match s with
  | Float(f) -> ZExt.of_float f
  | Mpqf(mpqf) -> ZExt.of_float (Mpqf.to_float mpqf)
  | Mpfrf(mpfrf) -> ZExt.of_float (Mpfrf.to_float mpfrf)

let eval_texpr_to_intv (t: VarManagement.t) (texpr:Texpr1.expr) : Intv.t = 
  let get_dim var = Environment.dim_of_var t.env var in
  let d = Option.get t.d in
  let boxes = d.boxes in
  let rec aux texpr =
    match texpr with
    | Texpr1.Cst (Interval inv) -> (z_ext_of_scalar inv.inf, z_ext_of_scalar inv.sup)
    | Cst (Scalar s) -> let s = z_ext_of_scalar s in (s, s)
    | Var y -> List.at boxes (get_dim y) 
    | Unop  (Cast, e, _, _) -> aux e
    | Unop  (Sqrt, e, _, _) -> failwith "Do the sqrt. :)"
    | Unop  (Neg,  e, _, _) -> Intv.neg (aux e)
    | Binop (Add, e1, e2, _, _) -> Intv.add (aux e1) (aux e2)
    | Binop (Sub, e1, e2, _, _) -> Intv.sub (aux e1) (aux e2)
    | Binop (Mul, e1, e2, _, _) -> Intv.mul (aux e1) (aux e2)
    | Binop (Div, e1, e2, _, _) -> Intv.div (aux e1) (aux e2)
    | Binop (Mod, e1, e2, _, _)  -> Intv.rem (aux e1) (aux e2)
    | Binop (Pow, e1, e2, _, _) -> Intv.pow (aux e1) (aux e2)
  in
  aux texpr;;

(* We assume that the divisor is always 1, so we omit it and that t is not bottom. *)
let rec eval_monoms_to_intv boxes (terms, (constant, _)) =
  match terms with
  | [] -> Intv.create_const_of_z constant
  | (coeff, index, _)::terms -> (
      let intv_coeff = Intv.create_const_of_z coeff in
      Intv.add (Intv.mul intv_coeff (Boxes.get_value index boxes)) (eval_monoms_to_intv boxes (terms, (constant, Z.one)))
    );;  

module ExpressionBounds: (SharedFunctions.ConvBounds with type t = VarManagement.t) =
struct
  include VarManagement

  let bound_texpr t texpr =
    match (eval_texpr_to_intv t (Texpr1.to_expr texpr)) with
    | Arb s1, Arb s2 -> Some(s1), Some(s2)
    | Arb s1, _ -> Some(s1), None
    | _, Arb s2 -> None, Some(s2)
    | _, _ -> None, None

  let bound_texpr d texpr1 = Timing.wrap "bounds calculation" (bound_texpr d) texpr1
end

module D =
struct
  include ZExtOps
  include Printable.Std
  include VarManagement
  module Bounds = ExpressionBounds
  module V = RelationDomain.V
  module Arg = struct
    let allow_global = true
  end

  module Convert = SharedFunctions.Convert (V) (Bounds) (Arg) (SharedFunctions.Tracked)

  type t = VarManagement.t

  type var = V.t
  let name () = "pentagon"

  let is_bot t = 
    match t.d with
    | None -> true
    | Some d -> Boxes.is_bot d.boxes || Subs.is_bot d.subs

  let is_top t = 
    match t.d with
    | None -> false
    | Some d -> Boxes.is_top d.boxes && Subs.is_top d.subs

  let to_string t =
    if is_bot t then
      "⊥"
    else if is_top t then
      "⊤"
    else
      (* d = None should have been handled by is_bot. *)
      let d = Option.get t.d in
      let vars = Array.map (StringUtils.string_of_var) (fst (Environment.vars t.env)) in
      let res = Pntg.to_string d in
      let key_re = Str.regexp {|\([0-9]+\)->|} in
      let subs_re = Str.regexp {|\([0-9]+\)#|} in
      let varname_and_append = fun postfix m -> (
          let idx = int_of_string (Str.matched_group 1 m) in
          if idx < Array.length vars then
            vars.(idx) ^ postfix
          else
            failwith "D.to_string hit unknown variable!"
        ) in
      (* First pass substitutes the variable names for the keys left to the arrow. *)
      Str.global_substitute key_re (varname_and_append "->") res |>
      (* Second pass adjusts the variable name for the subs sets. *)
      Str.global_substitute subs_re (varname_and_append "");;

  let show = to_string

  let equal t1 t2 =
    Environment.equal t1.env t2.env
    &&
    match t1.d, t2.d with
    | None, None -> true
    | None, _ -> false
    | _ , None -> false
    | Some(d1), Some(d2) -> Pntg.equal d1 d2

  let pretty () (t:t) = text (show t)


  let pretty_diff () (x, y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y


  let printXml f (t:t) =  BatPrintf.fprintf f "<value>\n<map>\n<key>\npntg\n</key>\n<value>\n%s</value>\n<key>\nenv\n</key>\n<value>\n%a</value>\n</map>\n</value>\n" (XmlUtil.escape (show t)) Environment.printXml t.env


  let to_yojson t = failwith "TODO"

  (**
     Bottom creation does not make sense if we do not know anything about our variables.
     We assume no variables have been encountered when this funciton is called.
     It therefore holds that: bot = top.
  *)
  let bot () = {d = None; env = empty_env}

  let bot_of_env env = ({ d = None; env = env }:t)


  (**
     Top creation does not make sense if we do not know anything about our variables.
     We assume no variables have been encountered when this funciton is called.
     It therefore holds that: top = bot.
  *)
  let top () = {d = Some {boxes = []; subs = []}; env = empty_env}

  let top_of_env env = dimchange2_add (top ()) env

  let meet t1 t2 =
    let sup_env = Environment.lce t1.env t2.env in
    let t1 = dimchange2_add t1 sup_env in
    let t2 = dimchange2_add t2 sup_env in
    match t1.d, t2.d with
    | Some d1', Some d2' ->
      ({d = Some {boxes = Boxes.meet d1'.boxes d2'.boxes; subs = Subs.meet d1'.subs d2'.subs}; env = sup_env}: t)
    | _ -> {d = None; env = sup_env}

  let meet t1 t2 = 
    let res = meet t1 t2 in
    if M.tracing then M.trace "pntg" "D.meet:\nt1:\t%s\nt2:\t%s\nres:\t%s\n\n" (show t1) (show t2) (show res);
    res

  let meet t1 t2 = Timing.wrap "pntg" (meet t1) t2

  let leq t1 t2 = 
    let sup_env = Environment.lce t1.env t2.env in
    let t1 = dimchange2_add t1 sup_env in
    let t2 = dimchange2_add t2 sup_env in
    match t1.d, t2.d with
    | Some d1', Some d2' ->
      let boxes_1, boxes_2 = d1'.boxes, d2'.boxes in
      let sub1, sub2 = d1'.subs, d2'.subs in
      let for_all_i f lst =
        List.for_all (fun (i, x) -> f i x) (List.mapi (fun i x -> (i, x)) lst) in
      let bool1 = Boxes.leq boxes_1 boxes_2 in
      let bool2 = for_all_i(fun x s2x -> 
          Subs.VarSet.for_all(fun y -> 
              let s1x = Subs.VarList.at sub1 x in
              let b1x = BatList.at boxes_1 x in
              let b1y = BatList.at boxes_1 y in
              Subs.VarSet.mem y s1x ||
              Intv.sup b1x <* Intv.inf b1y
            ) s2x
        ) sub2 in
      bool1 && bool2
    | Some d1', None -> Boxes.is_bot d1'.boxes || Subs.is_bot d1'.subs
    | _ -> true

  let leq t1 t2 =
    let res = leq t1 t2 in
    if M.tracing then M.trace "pntg" "D.leq:\nt1:\t%s\nt2:\t%s\nres:\t%b\n\n" (show t1) (show t2) res;
    res

  let leq a b = Timing.wrap "leq" (leq a) b

  let join t1 t2 =
    let sup_env = Environment.lce t1.env t2.env in
    let t1 = dimchange2_add t1 sup_env in
    let t2 = dimchange2_add t2 sup_env in

    match t1.d, t2.d with
    | Some d1', Some d2' ->
      let joined_boxes = Boxes.join d1'.boxes d2'.boxes in
      let s' x s1x = Subs.VarSet.inter s1x (List.at d2'.subs x) in
      let s'' x s1x = Subs.VarSet.filter (fun y -> Intv.sup (List.at d2'.boxes x) <* Intv.inf (List.at d2'.boxes y)) s1x in
      let s''' x = Subs.VarSet.filter (fun y -> Intv.sup (List.at d1'.boxes x) <* Intv.inf (List.at d1'.boxes y)) (List.at d2'.subs x) in
      let joined_subs = List.mapi (fun x s1x -> Subs.VarSet.union (s' x s1x) (Subs.VarSet.union (s'' x s1x) (s''' x))) d1'.subs in
      ({d = Some {boxes = joined_boxes; subs = joined_subs}; env = sup_env}: t)
    | Some d1', None -> {d = Some d1'; env = sup_env}
    | None, Some d2' -> {d = Some d2'; env = sup_env}
    | _ -> {d = None; env = sup_env}

  let join a b = Timing.wrap "join" (join a) b

  let join a b =
    let res = join a b in
    if M.tracing then M.trace "pntg" "D.join:\nt1:\t%s\nt2:\t%s\nres:\t%s\n\n" (show a) (show b) (show res) ;
    res

  let widen t1 t2 = 
    let sup_env = Environment.lce t1.env t2.env in
    let t1 = dimchange2_add t1 sup_env in
    let t2 = dimchange2_add t2 sup_env in
    match t1.d, t2.d with
    | Some d1', Some d2' ->
      ({d = Some {boxes = Boxes.widen d1'.boxes d2'.boxes; subs = Subs.widen d1'.subs d2'.subs}; env = sup_env}: t)
    | _ -> {d = None; env = sup_env}

  let widen a b =
    let res = widen a b in
    if M.tracing then M.trace "pntg" "D.widen:\nt1:\t%s\nt2:\t%s\nres:\t%s\n\n" (show a) (show b) (show res) ;
    res

  let narrow t1 t2 = meet t1 t2

  let narrow a b =
    let res = narrow a b in
    if M.tracing then M.trace "pntg" "D.narrow:\nt1:\t%s\nt2:\t%s\nres:\t%s\n\n" (show a) (show b) (show res) ;
    res

  (* S2 Specific functions of RelationDomain *)
  let is_bot_env t = t.d = None

  let forget_vars t vars = 
    if is_bot t || is_bot_env t || vars = [] then t
    else 
      let (pntg: Pntg.t) = Option.get t.d in
      let int_vars = List.map (fun v -> Environment.dim_of_var t.env v) vars in
      {d = Some({boxes = Boxes.forget_vars int_vars pntg.boxes; subs = Subs.forget_vars int_vars pntg.subs}); env=t.env};;

  (** Taken from lin2var and modified for our domain. *)
  (** Parses a Texpr to obtain a Rhs.t list to repr. a sum of a variables that have a coefficient. If variable is None, the coefficient represents a constant offset. *)
  let monomials_from_texp env texp =
    let open Apron.Texpr1 in
    let exception NotLinearExpr in
    let exception ScalarIsInfinity in
    let negate coeff_var_list =
      List.map (fun (monom, offs, divi) -> Z.(BatOption.map (fun (coeff,i) -> (neg coeff, i)) monom, neg offs, divi)) coeff_var_list in
    let multiply_with_Q dividend divisor coeff_var_list =
      List.map (fun (monom, offs, divi) -> Rhs.canonicalize Z.(BatOption.map (fun (coeff,i) -> (dividend*coeff,i)) monom, dividend*offs, divi*divisor) ) coeff_var_list in
    let multiply a b =
      (* if one of them is a constant, then multiply. Otherwise, the expression is not linear *)
      match a, b with
      | [(None,coeff, divi)], c
      | c, [(None,coeff, divi)] -> multiply_with_Q coeff divi c
      | _ -> raise NotLinearExpr
    in
    let rec convert_texpr texp =
      begin match texp with
        | Cst (Interval _) -> failwith "constant was an interval; this is not supported"
        | Cst (Scalar x) ->
          begin match SharedFunctions.int_of_scalar ?round:None x with
            | Some x -> [(None,x,Z.one)]
            | None -> raise ScalarIsInfinity end
        | Var x ->
          let var_dim = Environment.dim_of_var env x in
          [(Some (Z.one,var_dim),Z.zero,Z.one)]
        | Unop  (Neg,  e, _, _) -> negate (convert_texpr e)
        | Unop  (Cast, e, _, _) -> convert_texpr e (* Ignore since casts in apron are used for floating point nums and rounding in contrast to CIL casts *)
        | Unop  (Sqrt, e, _, _) -> raise NotLinearExpr
        | Binop (Add, e1, e2, _, _) -> convert_texpr e1 @ convert_texpr e2
        | Binop (Sub, e1, e2, _, _) -> convert_texpr e1 @ negate (convert_texpr e2)
        | Binop (Mul, e1, e2, _, _) -> multiply (convert_texpr e1) (convert_texpr e2)
        | Binop _  -> raise NotLinearExpr end
    in match convert_texpr texp with
    | exception NotLinearExpr -> None
    | exception ScalarIsInfinity -> None
    | x -> Some(x)
  ;;

  (** Taken from lin2var and modified for our domain. *)
  (** convert and simplify (wrt. reference variables) a texpr into a tuple of a list of monomials (coeff,varidx,divi) and a (constant/divi) *)
  let simplified_monomials_from_texp env texp =
    BatOption.bind (monomials_from_texp env texp)
      (fun monomiallist ->
         let module IMap = BatMap.Make(Int) in
         let accumulate_constants (exprcache,(aconst,adiv)) (v,offs,divi) = match v with
           | None -> let gcdee = Z.gcd adiv divi in exprcache,(Z.(aconst*divi/gcdee + offs*adiv/gcdee),Z.lcm adiv divi)
           | Some (coeff,idx) -> let (somevar,someoffs,somedivi)= v,offs,divi in (* normalize! *)
             let newcache = Option.map_default (fun (coef,ter) -> IMap.add ter Q.((IMap.find_default zero ter exprcache) + make coef somedivi) exprcache) exprcache somevar in
             let gcdee = Z.gcd adiv divi in
             (newcache,(Z.(aconst*divi/gcdee + offs*adiv/gcdee),Z.lcm adiv divi))
         in
         let (expr,constant) = List.fold_left accumulate_constants (IMap.empty,(Z.zero,Z.one)) monomiallist in (* abstract simplification of the guard wrt. reference variables *)
         Some (IMap.fold (fun v c acc -> if Q.equal c Q.zero then acc else (Q.num c,v,Q.den c)::acc) expr [], constant) )
  ;;

  (*
  Rename var to x
  *)
  let assign_texpr (t: t) x (texp: Texpr1.expr) : t =
    let wrap b s = {d=Some({boxes = b; subs = s}); env=t.env} in
    let dim_x = Environment.dim_of_var t.env x in 
    match t.d with
    | None -> t 
    | Some d ->
      match simplified_monomials_from_texp t.env texp with
      | None -> (
          match texp with
          (* x:= u % d *)
          | Binop(Mod, e1, e2, _, _) as rem_op -> (
              let rem_intv = eval_texpr_to_intv t rem_op in
              let boxes = Boxes.set_value dim_x rem_intv d.boxes in

              let subs: Subs.t =
                (* We remove any subs of x. *)
                Subs.forget_vars [dim_x] d.subs |>
                (match e2 with
                 | Var(d) -> (

                     let d_intv = eval_texpr_to_intv t e2 in
                     let dim_d = Environment.dim_of_var t.env d in

                     (* Check if all i ∈ d. i >= 0.*)
                     if (Intv.inf d_intv) >* ZExt.zero then
                       (* We can add x < d *)
                       Subs.set_value dim_x (Subs.VarSet.singleton dim_d)
                     else if Intv.sup d_intv <* ZExt.zero then
                       (* We can add d < x *)
                       Subs.set_value dim_d (Subs.VarSet.singleton dim_x)
                     else
                       (* We cannot add any subs. Potentially div-by-zero. *)
                       Fun.id
                   )
                 | _ -> Fun.id)
              in
              wrap boxes subs
            )
          (* Non-Linear cases. We only do interval analysis and forget any sub information. *)
          | expr -> (
              let boxes = (Boxes.set_value dim_x (eval_texpr_to_intv t expr) d.boxes) in
              let subs = (Subs.forget_vars [dim_x] d.subs) in
              wrap boxes subs
            )  
        )
      | Some(sum_of_terms, (constant,divisor)) ->
        if divisor <> Z.one then failwith "assign_texpr: DIVISOR WAS NOT ONE"
        else
          (* intv_acc: interval analysation of constant + all linear terms from indices 1 to i-1 *)
          let rec aux sum_of_terms intv_acc (subs_acc : Subs.VarSet.t) (slbs_acc : Subs.VarSet.t) delete_subs_flag delete_slbs_flag =
            match sum_of_terms with
            | (_, _, div) :: _ when div <> Z.one -> failwith "assign_texpr: DIVISOR WAS NOT ONE"
            | [] ->
              let intv_x = Boxes.get_value dim_x d.boxes in
              let delete_subs_flag = delete_subs_flag && Intv.sup intv_acc >* Intv.inf intv_x in
              let delete_slbs_flag = delete_slbs_flag && Intv.inf intv_acc <* Intv.sup intv_x in

              let new_subs = Subs.VarSet.remove dim_x subs_acc in
              let new_slbs = slbs_acc in

              let update_subs i subs_i =
                if i = dim_x then (* i = x ==> update Subs(x) *)
                  if delete_subs_flag then
                    new_subs
                  else
                    Subs.VarSet.union subs_i new_subs
                else (* i <> x ==> update Subs(i) *)
                if Subs.VarSet.mem i new_slbs then 
                  Subs.VarSet.add dim_x subs_i else
                if delete_slbs_flag then
                  Subs.VarSet.remove dim_x subs_i
                else
                  subs_i
              in
              wrap (Boxes.set_value dim_x intv_acc d.boxes) (List.mapi update_subs d.subs)

            | (coefficient, y, _) :: rem_terms when y = dim_x -> (* x' := ax + ... *)
              let rem_terms_intv = eval_monoms_to_intv d.boxes (rem_terms, (Z.zero, Z.one)) in
              let all_except_i = Intv.add intv_acc rem_terms_intv in
              (* We analyse 0 >< (a-1)x + b because it is more precise than x >< ax + b *)
              let a_decr_x = ([(Z.(-) coefficient Z.one, y, Z.one)], (Z.zero, Z.one)) in
              let (cmp_lb, cmp_ub) = Intv.add (eval_monoms_to_intv d.boxes a_decr_x) all_except_i in
              let delete_subs_flag = delete_subs_flag && cmp_ub >* ZExt.zero in (* x could have got greater *)
              let delete_slbs_flag = delete_slbs_flag && cmp_lb <* ZExt.zero (* x could have got lower *)
              in

              let this_intv = eval_monoms_to_intv d.boxes ([(coefficient, y, Z.one)], (Z.zero, Z.one)) in
              aux rem_terms (Intv.add intv_acc this_intv) subs_acc slbs_acc delete_subs_flag delete_slbs_flag

            | (coefficient, y, _) :: rem_terms -> (* x' := a1x1 + a2x2 + ... + ay + ... *)
        (*
            x < x'
            <==> x < ay + b
            <==> 0 < -x + ay + b


            analysiere 0 >< -x + ay + b, hier kann man auch vorherige SUBs-Infos verwenden
            x < y ==> -x + ay + b >= (a-1)y + b + 1 ==> wenn (a-1)y + b >= 0 möglich, dann x' > x möglich, also delete SUBs(x)
            x > y ==> -x + ay + b <= (a-1)y + b - 1 ==> wenn (a-1)y + b <= 0 möglich, dann x' < x möglich, also delete SLBs(x)

            FRAGE: Sind die modifizierten Terme immer genauer als die direkte Version, also für den ersten Fall:
            Gilt immer sup(-x + ay + b) >= sup ((a-1)y + b + 1)?

            x' < x möglich: delete SLBs(x)
            x' > x möglich: delete SUBs(x)*)

              (*let intv_x = Boxes.get_value dim_var d.boxes in
                let intv_x' = eval_monoms_to_intv d.boxes monoms in*)

              let rem_terms_intv = eval_monoms_to_intv d.boxes (rem_terms, (Z.zero, Z.one)) in
              let all_except_i = Intv.add intv_acc rem_terms_intv in
              (* We analyse 0 >< (a-1)y + b because it is more precise than y >< ay + b *)
              let a_decr_y = ([(Z.(-) coefficient Z.one, y, Z.one)], (Z.zero, Z.one)) in
              let (cmp_lb, cmp_ub) = Intv.add (eval_monoms_to_intv d.boxes a_decr_y) all_except_i in

              let delete_subs_flag = (* x could have got greater / we can't rule out x' > x *)
                delete_subs_flag &&
                (*Intv.sup intv_x' >* Intv.inf intv_x &&*)
                if Subs.lt d.subs dim_x y (* x < y ==> -x + ay + b >= (a-1)y + b + 1 ==> if (a-1)y + b >= 0 is possible, x' > x is possible *)
                then cmp_ub >=* ZExt.zero
                else true in
              let delete_slbs_flag = (* x could have got lower / we can't rule out x' < x *)
                delete_slbs_flag &&
                (*Intv.inf intv_x' <* Intv.sup intv_x &&*)
                if Subs.gt d.subs dim_x y
                then cmp_lb <=* ZExt.zero
                else true in

        (*
            analysiere 0 >< (a-1)y + b
            x' < y sicher:  SUBs(x') := SUBs(x') u SUBs(y) u {y}
            x' <= y sicher: SUBs(x') := SUBs(x') u SUBs(y)
            x' > y sicher:  SUBs(y) := SUBs(y) u {x'}

        *)
              let subs_y = Subs.get_value y d.subs in
              (* Caution: New subs/slbs can contain the old x. This is not a contradiction, it just has to be deleted afterwards. *)
              let new_subs =
                if cmp_ub <* ZExt.zero then
                  Subs.VarSet.union subs_y (Subs.VarSet.singleton y)
                else if cmp_ub =* ZExt.zero then
                  subs_y
                else
                  Subs.VarSet.empty
              in
              let new_slbs =
                if cmp_lb >* ZExt.zero then
                  (Subs.VarSet.singleton y)
                else
                  Subs.VarSet.empty
              in
              let subs_acc = Subs.VarSet.union subs_acc new_subs in
              let slbs_acc = Subs.VarSet.union slbs_acc new_slbs in
              let this_intv = eval_monoms_to_intv d.boxes ([(coefficient, y, Z.one)], (Z.zero, Z.zero))
              in
              aux rem_terms (Intv.add intv_acc this_intv) subs_acc slbs_acc delete_subs_flag delete_slbs_flag

          in aux sum_of_terms ((Arb constant, Arb constant) : Intv.t) Subs.VarSet.empty Subs.VarSet.empty true true

        (*
         (** 
            Implemented as described by the paper mention at the beginning of this file.
            Refer to 6.2.2 Remainder.
         *)
         | Binop (Mod, e1, e2, _, _)  ->
           let (boxes_1, sub_1) = aux e1 t in
           let (boxes_2, sub_2) = aux e2 t in

           let i2 = BatList.at boxes_2 dim in
           let intv = BatList.modify_at dim (
               fun i1 -> 
                 Intv.rem i1 i2
             ) boxes_1 in

           let sub = 
             match e2 with
             | Var divisor -> (
                 let dim_divisor = Environment.dim_of_var t.env divisor in 
                 let intv_divisor = BatList.at boxes_2 dim_divisor
                 in
                 if (Intv.inf intv_divisor) <* ZExt.zero then 
                   sub_without_var
                 else
                   BatList.modify_at dim (fun _ -> Subs.VarSet.singleton dim_divisor) sub_without_var
               )
             | _ -> sub_without_var
           in
           (intv, sub)
           *)

  ;;

  let assign_texpr t x texp =
    let res = assign_texpr t x texp in
    if M.tracing then M.trace "pntg" "D.assign_texpr:\nassign:\t%s := %s\nt:\t%s\nres:\t%s\n\n" (StringUtils.string_of_var x) (StringUtils.string_of_texpr1 texp) (show t) (show res);
    res


  let assign_exp ask (t: VarManagement.t) var exp (no_ov: bool Lazy.t) = 
    let t = if not @@ Environment.mem_var t.env var then add_vars t [var] else t in
    match Convert.texpr1_expr_of_cil_exp ask t t.env exp no_ov with
    | texp -> assign_texpr t var texp
    | exception Convert.Unsupported_CilExp _ -> forget_vars t [var]

  let assign_var t v v' = 
    let t = add_vars t [v; v'] in
    assign_texpr t v (Var v');;


  let assign_var_parallel (t: t) (var_tuples: (var *  var) list) : t = 
    let assigned_vars = List.map fst var_tuples in
    let t = add_vars t assigned_vars in
    let primed_vars = List.init (List.length assigned_vars) (fun i -> Var.of_string (Int.to_string i  ^"'")) in
    let t_primed = add_vars t primed_vars in
    let multi_t = List.fold_left2 (fun t' v_prime (_,v') -> assign_var t' v_prime v') t_primed primed_vars var_tuples in
    match multi_t.d with
    | Some m when not @@ is_top multi_t ->
      let switched_arr = List.fold_left2 (fun multi_t assigned_var primed_var-> assign_var multi_t assigned_var primed_var) multi_t assigned_vars primed_vars in
      remove_vars switched_arr primed_vars
    | _ -> t


  (**
     Combines two var lists into a list of tuples and runs assign_var_parallel
  *)
  let assign_var_parallel' t vs1 vs2 =
    let var_tuples = List.combine vs1 vs2 in
    assign_var_parallel t var_tuples

  let assign_var_parallel_with t (var_tuples: (var * var) list) : unit =  
    let t' = assign_var_parallel t var_tuples in
    t.d <- t'.d;
    t.env <- t'.env;;

  (**
      Taken from Lin2Var.
  *)
  let assert_constraint ask t e negate (no_ov: bool Lazy.t) =
    (*let wrap b s = {d=Some({boxes = b; sub=s}); env=t.env} in
      let texp = (
      match Convert.tcons1_of_cil_exp ask t t.env e negate no_ov with
      | exception Convert.Unsupported_CilExp _ -> failwith "Failed to convert cil expression\n"
      | tcons1 -> Texpr1.to_expr @@ Tcons1.get_texpr1 tcons1
      ) 
      in
      match t.d with
      | None -> t 
      | Some d ->
      (* TODO: Modulo support is lost after linearisation, may be quite important for us, as it is mentioned in the paper *)
      let monoms = simplified_monomials_from_texp t.env texp in
      match monoms with
      | None -> t
      | Some(sum_of_terms, (constant,divisor)) ->
        let monoms = Option.get monoms in
        match sum_of_terms with
        | _ when divisor <> Z.one -> failwith "assign_texpr: DIVISOR WAS NOT ONE"
        | [] ->  t
        | [(coefficient, y, _)] -> t
        | _ -> t*)
    let zero = ZExt.zero in 
    (** Checks if the constraining interval violates the assertion. *)
    let interval_helper ((lb, ub): ZExt.t * ZExt.t) (tcons_typ: Tcons1.typ) =
      match tcons_typ with
      | EQ when lb <=* zero && ub >=* zero -> t
      | SUPEQ when ub >=* zero -> t
      | SUP when ub >* zero -> t
      | DISEQ when ub <>* zero || lb <>* zero -> t
      | EQMOD (s) -> (
          let s = z_ext_of_scalar s in
          let ( - ) = ZExt.sub in
          if (ub - lb) <= (s - ZExt.of_int 2) && lb <>* zero && (ZExt.rem_add lb s) <=* (ZExt.rem_add ub s) then
            bot_of_env t.env
          else
            t
        )
      | _ -> bot_of_env t.env
    in
    let var_intv_meet constraining_interval dim_x : t = (
      (* Already matched to be not None. *)
      let d = Option.get t.d in
      let intv_x = List.at d.boxes dim_x in
      let intersected_intv_x = Intv.inter intv_x constraining_interval in
      if Intv.is_bot intersected_intv_x then
        bot_of_env t.env
      else 
        let boxes = List.modify_at dim_x (fun _ -> intersected_intv_x) d.boxes in
        { d = Some({boxes = boxes; subs = d.subs}); env=t.env}
    ) in
    match t.d with 
    | None -> t
    | Some d ->
      match Convert.tcons1_of_cil_exp ask t t.env e negate (Lazy.from_val true) with
      | exception Convert.Unsupported_CilExp exn -> t
      | tcons1 -> 
        let tcons_typ = Tcons1.get_typ tcons1 in
        let texpr = (Texpr1.to_expr @@ Tcons1.get_texpr1 tcons1) in
        let rec assert_constraint t (texpr: Texpr1.expr) =
          match texpr with 
          | Cst (Interval inv) -> 
            interval_helper (z_ext_of_scalar inv.inf, z_ext_of_scalar inv.sup) tcons_typ
          | Cst (Scalar s) -> 
            interval_helper (z_ext_of_scalar s, z_ext_of_scalar s) tcons_typ
          | Var x -> (
              (** We ignore sub-information for now. *)
              let dim_x = Environment.dim_of_var t.env x in
              let intv_x = List.at d.boxes dim_x in
              let (lb, ub) = intv_x in
              let zero = ZExt.zero in
              match tcons_typ with
              | EQ -> var_intv_meet (zero, zero) dim_x
              | SUPEQ -> var_intv_meet (zero, PosInfty) dim_x
              | SUP -> var_intv_meet (ZExt.of_int 1, PosInfty) dim_x
              | DISEQ ->
                if lb <>* zero && ub <>* zero then
                  t
                else if lb =* zero && ub >* zero then
                  var_intv_meet (ZExt.of_int 1, PosInfty) dim_x
                else if lb <* zero && ub =* zero then
                  var_intv_meet (NegInfty, ZExt.of_int (-1)) dim_x
                else 
                  bot_of_env t.env
              | EQMOD (s) -> (
                  let t = interval_helper intv_x tcons_typ in
                  let s = z_ext_of_scalar s in
                  match t.d with
                  | None -> t
                  | Some(pntg) -> (
                      let corrected_intv = (
                        let ( - ) = ZExt.sub in
                        let ( + ) = ZExt.add in
                        let tmp_lb = lb - ZExt.rem_add lb s in
                        let lb = if tmp_lb <* lb then (tmp_lb) + s else tmp_lb in
                        let ub = ub - ZExt.rem_add ub s in
                        (lb, ub)
                      )
                      in
                      let boxes = List.modify_at dim_x (fun _ -> corrected_intv) d.boxes in
                      ({ d = Some({boxes=boxes; subs=d.subs}); env=t.env})
                    )
                )
            )
          | Binop (Sub, e1, e2, t0, r) -> (
              match e1, e2 with
              | Var x, Cst(Scalar s) when (z_ext_of_scalar s) =* zero -> assert_constraint t e1

              | Var x, e -> (
                  let inf = Intv.inf in
                  let intv = eval_texpr_to_intv t (e) in
                  let dim_x = Environment.dim_of_var t.env x in

                  match tcons_typ with
                  | EQ -> var_intv_meet intv dim_x
                  | SUPEQ -> var_intv_meet (Intv.inf intv, PosInfty) dim_x
                  | SUP -> var_intv_meet (ZExt.add (Intv.inf intv) (ZExt.of_int 1), PosInfty) dim_x
                  | DISEQ when Intv.inf intv = Intv.sup intv -> (
                      let c = inf intv in
                      let (lb_x, ub_x) = List.at d.boxes dim_x in

                      if c = lb_x && c = ub_x then
                        bot_of_env t.env
                      else if c = lb_x then
                        var_intv_meet (ZExt.add c (ZExt.of_int 1), PosInfty) dim_x
                      else if c = ub_x then
                        var_intv_meet (NegInfty ,ZExt.sub c (ZExt.of_int 1)) dim_x
                      else
                        t

                    )
                  | DISEQ -> t 
                  | EQMOD(s) ->
                    (* still missing: bot handling (like everywhere) *)
                    let s = z_ext_of_scalar s in
                    let (-) = ZExt.sub in
                    let (+) = ZExt.add in
                    let (lb_x, ub_x) = List.at d.boxes dim_x in
                    let intv_mod_s = Intv.rem intv (s, s) in
                    let (lb_intv_mod_s, ub_intv_mod_s) = intv_mod_s in
                    let (lb_x, ub_x) = if ZExt.sign ub_intv_mod_s < 0 then Intv.inter (lb_x, ub_x) (NegInfty, ZExt.of_int (-1)) else
                      if ZExt.sign lb_intv_mod_s > 0 then Intv.inter (lb_x, ub_x) (ZExt.of_int 1, PosInfty) else (lb_x, ub_x) in
                    let lb = if lb_intv_mod_s = NegInfty || lb_x = NegInfty || Intv.mem (ZExt.rem lb_x s) intv_mod_s then lb_x else
                        (* greatest_zero_leq is the greatest number n <= lb_x equivalent to 0 (mod s) *)
                        let greatest_zero_leq = lb_x - ZExt.rem_add lb_x s in
                        let tmp_lb = greatest_zero_leq + lb_intv_mod_s in
                        if tmp_lb >* lb_x then tmp_lb else
                          let tmp_lb = tmp_lb + s in
                          if tmp_lb >* lb_x then tmp_lb else
                            let tmp_lb = tmp_lb + s in
                            if tmp_lb >* lb_x then tmp_lb else
                              failwith "EQMOD: this shouldn't happen"
                    in
                    let ub = if ub_intv_mod_s = PosInfty || ub_x = PosInfty || Intv.mem (ZExt.rem lb_x s) intv_mod_s then ub_x else
                        let greatest_zero_leq = ub_x - ZExt.rem_add ub_x s in
                        let tmp_ub = greatest_zero_leq + ub_intv_mod_s in
                        if tmp_ub <* ub_x then tmp_ub else
                          let tmp_ub = tmp_ub - s in
                          if tmp_ub <* ub_x then tmp_ub else
                            failwith "EQMOD: this shouldn't happen"

                    in
                    let boxes = List.modify_at dim_x (fun _ -> (lb, ub)) d.boxes in
                    ({d = Some({boxes=boxes; subs=d.subs}); env=t.env})
                )

              | Cst(Scalar s), Var x when (z_ext_of_scalar s) =* zero && (tcons_typ = DISEQ || tcons_typ = EQ) -> assert_constraint t e2

              | e, Var x -> (let inf = Intv.inf in
                             let intv = eval_texpr_to_intv t (e) in
                             let dim_x = Environment.dim_of_var t.env x in

                             match tcons_typ with
                             | EQ -> var_intv_meet intv dim_x
                             | SUPEQ -> var_intv_meet (NegInfty, Intv.sup intv) dim_x
                             | SUP -> var_intv_meet (NegInfty, ZExt.sub (Intv.sup intv) (ZExt.of_int 1)) dim_x
                             | DISEQ when Intv.inf intv = Intv.sup intv -> (
                                 let c = inf intv in
                                 let (lb_x, ub_x) = List.at d.boxes dim_x in

                                 if c = lb_x && c = ub_x then
                                   bot_of_env t.env
                                 else if c = lb_x then
                                   var_intv_meet (ZExt.add c (ZExt.of_int 1), PosInfty) dim_x
                                 else if c = ub_x then
                                   var_intv_meet (NegInfty, ZExt.sub c (ZExt.of_int 1)) dim_x
                                 else
                                   t

                               )
                             | DISEQ -> t 
                             | EQMOD(s) -> t
                            )

              | _ -> let intv = eval_texpr_to_intv t (Binop (Sub, e1, e2, t0, r)) in interval_helper intv tcons_typ
            )
          | _ -> t
        in
        assert_constraint t texpr


  let assert_constraint ask t e negate no_ov ~trace =
    let res = assert_constraint ask t e negate no_ov in
    if M.tracing && trace then (
      let tcons_str = 
        match Convert.tcons1_of_cil_exp ask t t.env e negate (Lazy.from_val true) with
        | exception Convert.Unsupported_CilExp exn -> (
            Printf.sprintf 
              "Failed to convert cil expression: exception %s"
              (SharedFunctions.show_unsupported_cilExp exn)
          )
        | tcons1 -> StringUtils.string_of_tcons1 tcons1
      in
      M.trace "pntg" "D.assert_constraint:\ntcons:\t%s\nt:\t%s\nres:\t%s\n\n" tcons_str (show t) (show res));
    res

  let assert_constraint ask t e negate no_ov =
    match e with
    (* 
      We remove clutter here: 
      Do not trace interger bound checking assertions. 
    *)

    | BinOp(Le,Lval(_),Const(CInt(i, _, _)),_) when Z.equal i (Z.of_int (Int32.to_int Int32.max_int)) -> assert_constraint ask t e negate no_ov ~trace:false
    | BinOp(Ge,Lval(_),Const(CInt(i, _, _)),_) when Z.equal i (Z.of_int (Int32.to_int Int32.min_int)) ->  assert_constraint ask t e negate no_ov ~trace:false
    | _ -> assert_constraint ask t e negate no_ov ~trace:true

  let invariant t : Lincons1Set.elt list = []

  (** Taken from lin2var. *)
  let substitute_exp ask (t:t) var exp no_ov = 
    let t = if not @@ Environment.mem_var t.env var then add_vars t [var] else t in 
    let res = assign_exp ask t var exp no_ov in
    forget_vars res [var]
  ;;

  (** Taken from lin2var.  *)
  let unify pntg1 pntg2 = meet pntg1 pntg2

  type marshal = t
  let marshal t = t
  let unmarshal t = t

  let relift t = t

  let cil_exp_of_lincons1 = Convert.cil_exp_of_lincons1

  let env t = t.env

  let eval_interval (ask) = Bounds.bound_texpr

  let to_string t = 
    if is_bot t then
      "⊥"
    else if is_top t then
      "⊤"
    else
      match t.d with
      | None -> failwith "is_bot should take care of that"
      | Some(d) -> Pntg.to_string d;;

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
