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


type intv_infty_list = ZExt.t * Int.t list
type ext_intv = intv_infty_list * intv_infty_list

(* We assume that the divisor is always 1, so we omit it and that t is not bottom. *)
(*
TODO: Remove duplicate code.
TODO: Rename to eval_monoms_to_ext_intv
*)
let rec eval_monoms_to_intv_infty_list boxes (terms, (constant, _)) : ext_intv =
  match terms with
  | [] -> ((ZExt.Arb constant, []), (ZExt.Arb constant, []))
  | (coeff, x, _)::terms -> (
      let terms_intv_infty_list = eval_monoms_to_intv_infty_list boxes (terms, (constant, Z.one)) in
      let x_intv = Boxes.get_value x boxes in

      let ub = match snd terms_intv_infty_list with
        | (NegInfty, _) -> failwith "This should not happen :)"
        | (PosInfty, l ) -> (ZExt.PosInfty, l)
        | (c, l) -> 


          let coeff_x_ub = 
            if Z.sign coeff > 0 then
              ZExt.mul (Arb coeff) (Intv.sup x_intv)
            else
              ZExt.mul (Arb coeff) (Intv.inf x_intv)
          in

          match coeff_x_ub with
          | ZExt.NegInfty -> failwith "This should not happen :)"
          | ZExt.PosInfty -> (
              if Z.abs coeff = Z.one then
                if List.length l >= 2 then
                  (ZExt.PosInfty, [])
                else
                  c, (x :: l)
              else 
                PosInfty, l
            )
          | ub -> ZExt.add c ub, l
      in

      let lb = match fst terms_intv_infty_list with
        | (PosInfty, _) -> failwith "This should not happen :)"
        | (NegInfty, l ) -> (ZExt.NegInfty, l)
        | (c, l) -> 


          let coeff_x_lb = 
            if Z.sign coeff > 0 then
              ZExt.mul (Arb coeff) (Intv.inf x_intv)
            else
              ZExt.mul (Arb coeff) (Intv.sup x_intv)
          in

          match coeff_x_lb with
          | ZExt.PosInfty -> failwith "This should not happen :)"
          | ZExt.NegInfty -> (
              if Z.abs coeff = Z.one then
                if List.length l >= 2 then
                  (ZExt.NegInfty, [])
                else
                  c, (x :: l)
              else 
                NegInfty, l
            )
          | lb -> ZExt.add c lb, l
      in


      (lb, ub)

    );;

let ext_intv_to_intv (((c_lb, infty_list_lb), (c_ub, infty_list_ub)) : ext_intv) : Intv.t =
  let lb = ZExt.add c_lb (if infty_list_lb = [] then ZExt.zero else NegInfty) in
  let ub = ZExt.add c_ub (if infty_list_ub = [] then ZExt.zero else PosInfty) in
  (lb, ub)


let ext_intv_plus_x (coeff_x, x, x_intv) (ext_intv: ext_intv) =
  let (const_lb, lb_list), (const_ub, ub_list) = ext_intv in

  let lb_correction_term = if Z.sign coeff_x < 0 then Intv.sup x_intv else Intv.inf x_intv in
  let ub_correction_term = if Z.sign coeff_x < 0 then Intv.inf x_intv else Intv.sup x_intv in

  let ext_lb = 
    if const_lb = NegInfty then ZExt.NegInfty, [] else
      match lb_list with
      | [] -> (ZExt.add const_lb lb_correction_term, [])
      | x'::l when x = x'-> 
        if lb_correction_term = PosInfty then (const_lb, l) else (ZExt.add const_lb lb_correction_term, l)
      | [y; x'] when x = x' -> 
        if lb_correction_term = PosInfty then (const_lb, [y]) else (ZExt.add const_lb lb_correction_term, [y])
      | l -> (
          if lb_correction_term = PosInfty then
            failwith "This should not happen."
          else
            (ZExt.add const_lb lb_correction_term, l)
        )
  in

  let ext_ub = 
    if const_ub = PosInfty then ZExt.PosInfty, [] else 
      match ub_list with
      | [] -> (ZExt.add const_ub ub_correction_term, [])
      | x'::l when x = x'-> 
        if ub_correction_term = NegInfty then (const_ub, l) else (ZExt.add const_ub ub_correction_term, l)
      | [y; x'] when x = x' -> 
        if ub_correction_term = NegInfty then (const_ub, [y]) else (ZExt.add const_ub ub_correction_term, [y])
      | l -> (
          if ub_correction_term = NegInfty then
            failwith "This should not happen."
          else
            (ZExt.add const_ub ub_correction_term, l)
        )
  in
  (ext_lb, ext_ub)

(*
First argument: Values of a variable corresponding to a term in the associated expr of the extended intv.
Second argument: The evaluation of an expression as an extended intv. 
*)
let eval_ext_intv_plus_x x ext_intv =
  ext_intv_to_intv (ext_intv_plus_x x ext_intv);;

let eval_ext_intv_minus_x (coeff_x, x, x_intv) (ext_intv: ext_intv) = 
  eval_ext_intv_plus_x (Z.neg coeff_x, x, Intv.neg x_intv) (ext_intv);;

let neg_ext_intv ((c1, lst1), (c2, lst2)) = (ZExt.neg c2, lst2), (ZExt.neg c1, lst1)

let string_of_infty_list (c, infty_list : intv_infty_list) =
  let list_string = String.concat " +- " (List.map (fun i -> "∞" ^ (string_of_int i)) infty_list) in
  ZExt.to_string c ^ " + " ^ list_string 

let string_of_ext_intv (ext_intv : intv_infty_list * intv_infty_list) =
  "[" ^ string_of_infty_list (fst ext_intv) ^ ", " ^ string_of_infty_list (snd ext_intv) ^ "]"

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


  (*
  TODO:
  *)
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
  TODO: discuss whether to implement the subtraction like the pentagon paper
     (special case? different eval_monoms_to_intv that also uses relational information?) *)
  let assign_texpr (t: t) x (texp: Texpr1.expr) : t =
    let wrap b s = {d=Some({boxes = b; subs = s}); env=t.env} in
    let x = Environment.dim_of_var t.env x in 
    match t.d with
    | None -> t 
    | Some d ->
      match simplified_monomials_from_texp t.env texp with
      | None -> (
          match texp with
          (* x:= u % div
             Implemented like the paper suggests: If the divisor is positive, it is a strict upper bound for x'. *)
          | Binop(Mod, e1, Var(d'), _, _) as rem_op -> (
              let rem_intv = eval_texpr_to_intv t rem_op in
              let boxes = Boxes.set_value x rem_intv d.boxes in

              let subs: Subs.t =
                (* We remove any subs of x.
                   TODO: We could add a special case for x' := x % d, x >= 0 ==> x' <= x ==> SUBs(x) can stay *)
                Subs.forget_vars [x] d.subs |>
                let dim_d = Environment.dim_of_var t.env d' in
                let d_intv = Boxes.get_value dim_d d.boxes in

                (* Check if all possible divisors are positive. *)
                if (Intv.inf d_intv) >* ZExt.zero then
                  (* We can set SUBs(x) := {d} *)
                  Subs.set_value x (Subs.VarSet.singleton dim_d)
                else if Intv.sup d_intv <* ZExt.zero then
                  (* We can set SUBs(d) := SUBs(d) u {x} *)
                  let subs_d = Subs.get_value dim_d d.subs in
                  Subs.set_value dim_d (Subs.VarSet.add x subs_d) (* BUG WAS HERE *)
                else
                  (* We cannot add any subs. Potentially div-by-zero. *)
                  Fun.id
              in
              wrap boxes subs
            )
          (* Non-Linear cases. We only do interval analysis and forget all relational information. *)
          | expr ->
            let boxes = (Boxes.set_value x (eval_texpr_to_intv t expr) d.boxes) in
            let subs = (Subs.forget_vars [x] d.subs) in
            wrap boxes subs
        )
      | Some(sum_of_terms, (constant,divisor)) as monoms  ->

        if divisor <> Z.one then failwith "assign_texpr: DIVISOR WAS NOT ONE"
        else
          let monoms = Option.get monoms in
          let expr_intv = eval_monoms_to_intv d.boxes monoms in
          let expr_ext_intv = eval_monoms_to_intv_infty_list d.boxes monoms in

          if expr_intv <> ext_intv_to_intv expr_ext_intv then
            failwith "ext_intv <> expr_intv"
          else

            let x_intv = Boxes.get_value x d.boxes in

            let rec aux sum_of_terms (subs_acc : Subs.VarSet.t) (slbs_acc : Subs.VarSet.t) delete_subs_flag delete_slbs_flag =
              match sum_of_terms with
              | (_, _, div) :: _ when div <> Z.one -> failwith "assign_texpr: DIVISOR WAS NOT ONE"

              (* We finished recursion through the linear expression. Time to set the subs and boxes for our pentagon. *)
              | [] ->

                (* If x after the assignment can be greater than the value of x before the assignment, we must delete its subs. *)
                let delete_subs_flag = delete_subs_flag && Intv.sup expr_intv >* Intv.inf x_intv in

                (* If x after the assignment can be lower than the value of x before the assignment, we must delete its slbs. *)
                let delete_slbs_flag = delete_slbs_flag && Intv.inf expr_intv <* Intv.sup x_intv in

                (* Variables which are the upper bound of x. Remove x which might be wrongfully added. *)
                let new_subs = Subs.VarSet.remove x subs_acc in 

                (* Variables where x is the upper bound. *)
                let new_slbs = slbs_acc in 

                let update_subs y subs_y =
                  if y = x then (* y = x ==> update Subs(x) *)
                    if delete_subs_flag then
                      new_subs
                    else
                      Subs.VarSet.union subs_y new_subs
                  else (* y <> x ==> possibly add x to / delete x from Subs(y) *)
                  if Subs.VarSet.mem y new_slbs then 
                    Subs.VarSet.add x subs_y else
                  if delete_slbs_flag then
                    Subs.VarSet.remove x subs_y
                  else
                    subs_y
                in
                wrap (Boxes.set_value x expr_intv d.boxes) (List.mapi update_subs d.subs)

              | (coefficient, x', _) :: rem_terms when x' = x -> (* x' := ax + ... *)
                (* We analyze 0 >< (a-1)x + [c,d] because it is more precise than x >< ax + [c,d] *)
                let (cmp_lb, cmp_ub) = 
                  eval_ext_intv_minus_x (coefficient, x, x_intv) expr_ext_intv 
                in
                (* x could have got greater *)
                let delete_subs_flag = delete_subs_flag && cmp_ub >* ZExt.zero in 
                (* x could have got lower *)
                let delete_slbs_flag = delete_slbs_flag && cmp_lb <* ZExt.zero 
                in
                aux rem_terms subs_acc slbs_acc delete_subs_flag delete_slbs_flag

              | (coefficient, y, _) :: rem_terms -> (* x' := ay + ... *)
                let y_intv = Boxes.get_value y d.boxes in (* BUG WAS HERE *)

                (* We analyze 0 >< (a-1)y + [c,d] because it is more precise than y >< ay + [c,d] *)
                let (cmp_lb, cmp_ub) = 
                  eval_ext_intv_minus_x (coefficient, y, y_intv) expr_ext_intv 
                in

              (*
                x >= y + 1 (by Subs)
                y + 1 >= x' (by cmp)
                ==> x >= x' ==> we can keep Subs(x)
                *)
                let keep_subs_flag =
                  Subs.gt x y d.subs && ZExt.of_int 1 >=* cmp_ub in
                let delete_subs_flag =
                  (* x could have got greater / we can't rule out x' > x *)
                  delete_subs_flag && not keep_subs_flag in
                (* If x' is guaranteed to be less then / equal to x, we can keep the subs.
                   We want to know x' > x? 

                   In this case x' := ay + [c,d], therefore we get:
                   ay + [c, d] > x <===> -x + ay + [c,d] > 0

                   From x < y (y \in Subs(x)) we can derive 
                   -x + ay + [c,d] > (a-1)y + [c,d]

                   So if (a-1)y + [c,d] >= 0 is possible, x' > x is possible. *)

                (*if Subs.lt d.subs x y
                  then cmp_ub >=* ZExt.zero
                  else true in*)

                let keep_slbs_flag =
                  Subs.lt x y d.subs && ZExt.of_int (-1) <=* cmp_ub in
                let delete_slbs_flag = 
                  (* x could have got lower / we can't rule out x' < x *)
                  delete_slbs_flag && not keep_slbs_flag in
                (*if Subs.gt d.subs x y
                  then cmp_lb <=* ZExt.zero
                  else true in*)
        (*
            analyze 0 >< (a-1)y + [c,d]
            x' < y known:  SUBs(x') := SUBs(x') u SUBs(y) u {y}
            x' <= y known: SUBs(x') := SUBs(x') u SUBs(y)
            x' > y known:  SLBs(x') := SLBs(x') u {y}

        *)
                let subs_y = Subs.get_value y d.subs in
              (*
              Caution: New subs can contain the old x.
              This is not a contradiction, it just has to be deleted afterwards.
              *)
                let new_subs =
                  if cmp_ub <* ZExt.zero then
                    Subs.VarSet.union subs_y (Subs.VarSet.singleton y)
                  else if cmp_ub =* ZExt.zero then
                    subs_y
                  else
                    Subs.VarSet.empty
                in
                let subs_acc = Subs.VarSet.union subs_acc new_subs in
                let slbs_acc = if cmp_lb >* ZExt.zero then Subs.VarSet.add y slbs_acc else slbs_acc in
                aux rem_terms subs_acc slbs_acc delete_subs_flag delete_slbs_flag

            in aux sum_of_terms Subs.VarSet.empty Subs.VarSet.empty true true
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

  let rec assert_constraint ask t tcons negate (no_ov: bool Lazy.t) =
    let wrap b s = {d=Some({boxes = b; subs=s}); env=t.env} in
    match t.d with
    | None -> t
    | Some d ->
      match Tcons1.get_typ tcons with
      (* Reduction of all Tcons1 comparison operators to SUPEQ, analogous to Miné dissertation, section 3.6.3 *)
      | EQ -> (
          let e = Texpr1.to_expr @@ Tcons1.get_texpr1 tcons in
          let e = Texpr1.of_expr t.env (Texpr1.Unop(Texpr1.Neg, e, Texpr1.Int, Texpr1.Near)) in
          let tcons1 = Tcons1.make e Tcons1.SUPEQ in

          let expr2 = Tcons1.get_texpr1 tcons in
          let tcons2 = Tcons1.make expr2 Tcons1.SUPEQ in
          let t1 = assert_constraint ask t tcons1 negate no_ov in
          let t2 = assert_constraint ask t tcons2 negate no_ov in
          meet t1 t2
        ) 
      | SUP -> (
          let e = Texpr1.to_expr @@ Tcons1.get_texpr1 tcons in
          let e = Texpr1.of_expr t.env (Texpr1.Binop(Texpr1.Sub, e, Cst(Scalar(Scalar.of_int 1)) ,Texpr1.Int, Texpr1.Near)) in
          let tcons1 = Tcons1.make e Tcons1.SUPEQ in
          assert_constraint ask t tcons1 negate no_ov)
      | DISEQ -> (
          let e = Texpr1.to_expr @@ Tcons1.get_texpr1 tcons in
          let e = Texpr1.of_expr t.env (Texpr1.Unop(Texpr1.Neg, e, Texpr1.Int, Texpr1.Near)) in
          let tcons1 = Tcons1.make e Tcons1.SUP in

          let expr2 = Tcons1.get_texpr1 tcons in
          let tcons2 = Tcons1.make expr2 Tcons1.SUP in

          let t1 = assert_constraint ask t tcons1 negate no_ov in
          let t2 = assert_constraint ask t tcons2 negate no_ov in
          join t1 t2
        )
      | EQMOD (s) -> t
      (* Base case :) *)
      | SUPEQ -> (
          (* e >= 0 *)
          let monoms = simplified_monomials_from_texp t.env (Texpr1.to_expr @@ Tcons1.get_texpr1 tcons) in
          match monoms with
          | None -> t
          | Some (sum_of_terms, (constant,_)) -> (
              let monoms = Option.get monoms in 

              let expr_intv = eval_monoms_to_intv d.boxes monoms in
              let neg_expr_intv = Intv.neg expr_intv in

              let expr_ext_intv = eval_monoms_to_intv_infty_list d.boxes monoms in
              let neg_expr_ext_intv = neg_ext_intv expr_ext_intv in


              if expr_intv <> ext_intv_to_intv expr_ext_intv ||
                 neg_expr_intv <> ext_intv_to_intv neg_expr_ext_intv then
                failwith "ext_intv <> expr_intv"
              else

                let rec aux sum_of_terms subs_acc boxes =

                  match sum_of_terms with
                  | [] -> (* no reference variables in the guard, so check if we can rule out expr_intv >= 0 *)

                    if Intv.sup expr_intv <* ZExt.zero (* added bot check *)
                    then bot_of_env t.env
                    else
                      wrap boxes subs_acc

                  | (coeff_x, x, _) :: rem_terms ->

                    let x_intv = Boxes.get_value x d.boxes in
                    let (lb_x, ub_x) = x_intv in

                    (* We receive x from the outer recursion. It stays fixed. *)
                    let rec inner x_term_intv sum_of_terms subs_acc =

                      match sum_of_terms with 
                      | [] -> Some(subs_acc)

                      | (coeff_y, y, _) :: rem_terms -> 

                        let y_intv = Boxes.get_value y d.boxes in
                        let y_term_intv = (coeff_y, y, y_intv) in

                        let helper (coeff_x, x, x_intv) (coeff_y, y, y_intv) subs_acc_opt = 
                          match subs_acc_opt with
                          | None -> None
                          | Some(subs_acc) ->

                            let neg_expr_plus_x_ext_intv = ext_intv_plus_x (Z.neg coeff_x, x, x_intv) neg_expr_ext_intv in

                            let (constraint_lb,_) = eval_ext_intv_minus_x (Z.neg coeff_y, y, y_intv) neg_expr_plus_x_ext_intv in

                            let (_, numeric_ub) = Intv.sub x_intv y_intv in

                            (* Checking for contradictions first. *)
                            if numeric_ub <* constraint_lb then
                              None (* Contradiction! *)
                            else if (ZExt.sign constraint_lb) >= 0 && Subs.lt x y subs_acc then
                              None (* Contradiction! *)
                            else if (ZExt.sign constraint_lb) > 0 then
                              Some(Subs.add_gt x y subs_acc)
                            else
                              Some(subs_acc)
                        in

                        let subs = 
                          helper x_term_intv y_term_intv (Some subs_acc) |> 
                          helper y_term_intv x_term_intv
                        in
                        match subs with
                        | None -> None
                        | Some(subs) -> 
                          inner x_term_intv rem_terms subs
                    in


                    (* Setting BOXES *)
                    (* ax + [c,d] >= 0 ==> x >= x - (ax + [c,d]) = (1-a)x - [c,d] >= (1-a)x - d
                                            x <= x + (ax + [c,d]) = (1+a)x + [c,d] <= (1+a)x + d *)
                    let constraint_lb_x, _ =
                      eval_ext_intv_plus_x (Z.neg coeff_x, x, x_intv) neg_expr_ext_intv
                    in

                    let _, constraint_ub_x =
                      eval_ext_intv_plus_x (coeff_x, x, x_intv) expr_ext_intv
                    in

                    let intv' = (ZExt.max lb_x constraint_lb_x, ZExt.min ub_x constraint_ub_x) in
                    (*Printf.printf "constraint: %s, constraint': %s\n" (Intv.to_string (a, b)) (Intv.to_string (constraint_lb_x, constraint_ub_x));*)
                    if Intv.is_bot intv' then 
                      bot_of_env t.env

                    else
                      let boxes = Boxes.set_value x intv' boxes in

                      (* Setting SUBS *)
                      match inner (coeff_x, x, intv') rem_terms subs_acc with
                      | None -> (
                          bot_of_env t.env
                        )
                      | Some(subs) ->
                        aux rem_terms subs boxes
                in
                aux sum_of_terms d.subs d.boxes
            )
        )


  let assert_constraint ask t e negate no_ov ~trace =
    let (str, res) = 
      match Convert.tcons1_of_cil_exp ask t t.env e negate no_ov with 
      | exception Convert.Unsupported_CilExp exn -> (
          (
            Printf.sprintf 
              "Failed to convert cil expression: exception %s"
              (SharedFunctions.show_unsupported_cilExp exn),
            t
          )
        )
      | tcons -> (
          (
            StringUtils.string_of_tcons1 tcons,
            assert_constraint ask t tcons negate no_ov
          )
        )
    in
    if M.tracing && trace then (
      M.trace "pntg" "D.assert_constraint:\ntcons:\t%s\nt:\t%s\nres:\t%s\n\n" str (show t) (show res));
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


  (* 
  This function returns linear constraints of form: 
  y - x > 0 for all x in t.env and y in SUBS(x)
  and
  z - a >= 0, -z + b >= 0 for all z in t.env with z in [a, b]
  *)
  let invariant (t:t) : GobApron.Lincons1Set.elt list = 
    (* It`s safe to assume that t.d <> None. *)
    let ({boxes; subs}: Pntg.t) = Option.get t.d in
    let s_of_int i: Coeff.union_5 = (GobApron.Coeff.s_of_int i) in
    let s_of_zext (z: ZExt.t): Coeff.union_5 = 
      match z with
      | NegInfty -> Scalar (Scalar.of_infty (-1))
      | PosInfty -> Scalar (Scalar.of_infty (1))
      | Arb z -> Scalar (Scalar.of_z z)
    in
    (* 
        Creates a lincons of the form: 
          0*x_1 + ... + coeff * var + ... + 0*x_n + cst >= 0
      *)
    let create_supeq_lincons (var: Var.t) ~coeff ~cst= 
      (* Create a "empty" lincons: 0*x_1 + ... + 0 * var + ... + 0*x_n + 0 >= 0 *)
      let lincons = Lincons1.make (Linexpr1.make t.env) Lincons1.SUPEQ in
      (* Set the coefficient for the given x to 1. *)
      Lincons1.set_coeff lincons var (s_of_int coeff);
      (* Add the constant. *)
      Lincons1.set_cst lincons (s_of_zext cst);
      (* Return the side-effected lincons. *)
      lincons 
    in

    (* 
        Creates a lincons of the form: 
          0*x_1 + ... + (-1) * var1 + 1 * var2 ... + 0*x_n + 0 > 0
      *)
    let create_sup_lincons (var1: Var.t) (var2: Var.t) = 
      (* Create a "empty" lincons: 0*x_1 + ... + 0 * var + ... + 0*x_n + 0 >= 0 *)
      let lincons = Lincons1.make (Linexpr1.make t.env) Lincons1.SUP in
      (* Set the coefficient for the given var1 to -1. *)
      Lincons1.set_coeff lincons var1 (s_of_int (-1));
      (* Set the coefficient for the given var2 to 1. *)
      Lincons1.set_coeff lincons var2 (s_of_int 1);
      (* Return the side-effected lincons. *)
      lincons 
    in

    (** Create a list of constraints x >= lb, x <= ub, i.e. x - lb >= 0, -x + ub >= 0. *)
    let constraints_of_intv var intv = 
      let lb, ub = intv in
      [
        create_supeq_lincons var ~coeff:(1) ~cst:(ZExt.neg lb);
        create_supeq_lincons var ~coeff:(-1) ~cst:(lb) 
      ]
    in

    let constraints_of_subs x subs =
      (* Creation of constraints y > x, i.e. y - x > 0 forall y in SUBS(x). *)
      List.map (create_sup_lincons x) subs
    in
    (* Zip variables, intervals and subs into one list and transform the subs into variables. *)
    List.map2i (fun i intv subs -> (
          Tuple3.make
            (Environment.var_of_dim t.env i)
            intv
            (List.map (Environment.var_of_dim t.env) (Subs.VarSet.to_list subs))

        )) boxes subs |>
    List.fold_left (
      fun acc (var,intv,subs) -> 
        acc @ (constraints_of_intv var intv) @ (constraints_of_subs var subs)
    ) []
  ;;


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
