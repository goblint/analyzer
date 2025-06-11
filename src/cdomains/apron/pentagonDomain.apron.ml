(** Implementation of the pentagon domain (pntg)
    @see <https://doi.org/10.1016/j.scico.2009.04.004> 
    "Pentagons: A weakly relational abstract domain for the efficient validation of array accesses"
    -- Francesco Logozzo, Manuel Fähndrich (2010) *)

open Batteries
open GoblintCil
module M = Messages
open GobApron
open BatList
open Intv
open Sub
open ZExt

module type Tracked =
sig
  val type_tracked: typ -> bool
  val varinfo_tracked: varinfo -> bool
end

module PNTG =
struct
  type t = { intv: Intv.t; sub: Sub.t } [@@deriving eq, ord]

  let hash : (t -> int)  = fun _ -> failwith "TODO"
  let equal pntg1 pntg2  = Intv.equal pntg1.intv pntg2.intv && Sub.equal pntg1.sub pntg2.sub;;
  let copy (x: t) = x
  let empty () = { intv = []; sub = [] }
  let is_empty pntg =
    match pntg.intv, pntg.sub with
    | [], [] -> true
    | _ -> false

  (**
     See https://antoinemine.github.io/Apron/doc/api/ocaml/Dim.html
     for the semantic of Dim.change
  *)
  let dim_add (dim_change: Apron.Dim.change) pntg =
    if dim_change.realdim != 0 then
      failwith "Pentagons are defined over integers: \
                extension with real domain is nonsensical"
    else 
      let intv, sub = 
        Intv.dim_add dim_change pntg.intv,
        Sub.dim_add dim_change pntg.sub 
      in
      ({intv = intv; sub = sub}: t)

  (** 
     See https://antoinemine.github.io/Apron/doc/api/ocaml/Dim.html
     for the semantic of Dim.change 
  *)
  let dim_remove (dim_change: Apron.Dim.change) pntg  =
    if dim_change.realdim != 0 then
      failwith "Pentagons are defined over integers: \
                extension with real domain is nonsensical"
    else 
      let intv, sub = 
        Intv.dim_remove dim_change pntg.intv,
        Sub.dim_remove dim_change pntg.sub 
      in
      ({intv = intv; sub = sub}: t)
end

(** [VarManagement] defines the type t of the affine equality domain (a record that contains an optional matrix and an apron environment) and provides the functions needed for handling variables (which are defined by [RelationDomain.D2]) such as [add_vars], [remove_vars].
    Furthermore, it provides the function [simplified_monomials_from_texp] that converts an apron expression into a list of monomials of reference variables and a constant offset *)
module VarManagement =
struct
  include SharedFunctions.VarManagementOps (PNTG)
end


module ExpressionBounds: (SharedFunctions.ConvBounds with type t = VarManagement.t) =
struct
  include VarManagement

  let bound_texpr t texpr = failwith "TODO"

  let bound_texpr d texpr1 = Timing.wrap "bounds calculation" (bound_texpr d) texpr1
end


module D =
struct
  include Printable.Std
  include VarManagement
  module Bounds = ExpressionBounds
  module V = RelationDomain.V
  module Arg = struct
    let allow_global = true
  end

  module Convert = SharedFunctions.Convert (V) (Bounds) (Arg) (SharedFunctions.Tracked)
  (**
     TODO: module Tracked
  *)
  module Tracked = struct let varinfo_tracked _ = failwith "TODO Tracked";; let type_tracked _ = failwith "TODO Tracked";; end

  type t = VarManagement.t [@@deriving eq]

  type var = V.t

  let pretty_diff () (x, y) = failwith "TODO pretty_diff"

  let show varM = failwith "TODO"

  let pretty () (x:t) = failwith "TODO"

  let printXml f x = failwith "TODO"

  let name () = "pentagon"

  let to_yojson _ = failwith "TODO"

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
  let top () = {d = Some {intv = []; sub = []}; env = empty_env}

  let top_of_env env = dimchange2_add (top ()) env

  let is_bot t = 
    match t.d with
    | None -> true
    | Some d -> Intv.is_bot d.intv || Sub.is_bot d.sub

  let is_top t = 
    match t.d with
    | None -> false
    | Some d -> Intv.is_top d.intv && Sub.is_top d.sub


  let meet t1 t2 =
    let sup_env = Environment.lce t1.env t2.env in
    let t1 = dimchange2_add t1 sup_env in
    let t2 = dimchange2_add t2 sup_env in
    match t1.d, t2.d with
    | Some d1', Some d2' ->
      ({d = Some {intv = Intv.meet d1'.intv d2'.intv; sub = Sub.meet d1'.sub d2'.sub}; env = sup_env}: t)
    | _ -> {d = None; env = sup_env}

  let meet t1 t2 = 
    let res = meet t1 t2 in
    if M.tracing then M.tracel "meet" "meet a: %s\n U  \n b: %s \n -> %s" (show t1) (show t2) (show res) ;
    res

  let meet t1 t2 = Timing.wrap "meet" (meet t1) t2

  let leq t1 t2 = 
    let sup_env = Environment.lce t1.env t2.env in
    let t1 = dimchange2_add t1 sup_env in
    let t2 = dimchange2_add t2 sup_env in
    match t1.d, t2.d with
    | Some d1', Some d2' ->
      let interval1, interval2 = d1'.intv, d2'.intv in
      let sub1, sub2 = d1'.sub, d2'.sub in
      let for_all_i f lst =
        List.for_all (fun (i, x) -> f i x) (List.mapi (fun i x -> (i, x)) lst) in
      let bool1 = Intv.leq interval1 interval2 in
      let bool2 = for_all_i(fun x s2x -> 
          Sub.VarSet.for_all(fun y -> 
              let s1x = Sub.VarList.at sub1 x in
              let b1x = BatList.at interval1 x in
              let b1y = BatList.at interval1 y in
              Sub.VarSet.mem y s1x ||
              Interval.sup b1x < Interval.inf b1y
            ) s2x
        ) sub2 in
      bool1 && bool2
    | Some d1', None -> Intv.is_bot d1'.intv || Sub.is_bot d1'.sub
    | _ -> true

  let leq a b = Timing.wrap "leq" (leq a) b

  let leq t1 t2 =
    let res = leq t1 t2 in
    if M.tracing then M.tracel "leq" "leq a: %s b: %s -> %b" (show t1) (show t2) res ;
    res

  let join t1 t2 =
    let sup_env = Environment.lce t1.env t2.env in
    let t1 = dimchange2_add t1 sup_env in
    let t2 = dimchange2_add t2 sup_env in
    match t1.d, t2.d with
    | Some d1', Some d2' ->
      let intv_join = Intv.join d1'.intv d1'.intv in
      let s' x s1x = Sub.VarSet.inter s1x (List.at d2'.sub x) in
      let s'' x s1x = Sub.VarSet.filter (fun y -> Interval.sup (List.at d2'.intv x) < Interval.inf (List.at d2'.intv y)) s1x in
      let s''' x = Sub.VarSet.filter (fun y -> Interval.sup (List.at d1'.intv x) < Interval.inf (List.at d1'.intv y)) (List.at d2'.sub x) in
      let sub_join = List.mapi (fun x s1x -> Sub.VarSet.union (s' x s1x) (Sub.VarSet.union (s'' x s1x) (s''' x))) d1'.sub in

      ({d = Some {intv = intv_join; sub = sub_join}; env = sup_env}: t)
    | Some d1', None -> {d = Some d1'; env = sup_env}
    | None, Some d2' -> {d = Some d2'; env = sup_env}
    | _ -> {d = None; env = sup_env}

  let join a b = Timing.wrap "join" (join a) b

  let join a b =
    let res = join a b in
    if M.tracing then M.tracel "join" "join a: %s b: %s -> %s" (show a) (show b) (show res) ;
    res

  let widen t1 t2 = 
    let sup_env = Environment.lce t1.env t2.env in
    let t1 = dimchange2_add t1 sup_env in
    let t2 = dimchange2_add t2 sup_env in
    match t1.d, t2.d with
    | Some d1', Some d2' ->
      ({d = Some {intv = Intv.widen d1'.intv d2'.intv; sub = Sub.widen d1'.sub d2'.sub}; env = sup_env}: t)
    | _ -> {d = None; env = sup_env}

  let widen a b =
    let res = widen a b in
    if M.tracing then M.tracel "widen" "widen a: %s b: %s -> %s" (show a) (show b) (show res) ;
    res

  let narrow t1 t2 = meet t1 t2

  let narrow a b =
    let res = narrow a b in
    if M.tracing then M.tracel "narrow" "narrow a: %s b: %s -> %s" (show a) (show b) (show res) ;
    res

  let to_string pntg1 =
    match pntg1.d with
    | None -> "bot"
    | Some d ->
      let intv_str = Intv.to_string d.intv in
      let sub_str = Sub.to_string d.sub in
      Printf.sprintf "Pentagon: %s, %s" intv_str sub_str



  (* S2 Specific functions of RelationDomain *)
  let is_bot_env t = t.d = None

  let forget_vars t vars = 
    if is_bot t || is_bot_env t || vars = [] then t
    else 
      let (pntg: PNTG.t) = Option.get t.d in
      let int_vars = List.map (fun v -> Environment.dim_of_var t.env v) vars in
      {d = Some({intv = Intv.forget_vars int_vars pntg.intv; sub = Sub.forget_vars int_vars pntg.sub}); env=t.env};;


  let z_ext_of_scalar (s: Scalar.t) = 
    match s with
    | Float(f) -> ZExt.of_float f
    | Mpqf(mpqf) -> ZExt.of_float (Mpqf.to_float mpqf)
    | Mpfrf(mpfrf) -> ZExt.of_float (Mpfrf.to_float mpfrf)



  let assign_texpr (t: t) var (texp: Texpr1.expr) =

    let dim = Environment.dim_of_var t.env var in

    let rec convert_texpr (texp: Texpr1.expr) t : Intv.t * Sub.t =
      match t.d with
      | None -> ([], []) (** Bot *)
      | Some d ->
        let intv, sub = d.intv, d.sub in

        let sub_without_var = Sub.forget_vars [dim] sub in
        let intv_without_var = Intv.forget_vars [dim] intv in
        (match texp with
         (** Case: x := [inv.inf, inv.sup] *)
         | Cst (Interval inv) ->
           let intv = BatList.modify_at dim (fun _ -> (z_ext_of_scalar inv.inf, z_ext_of_scalar inv.sup)) intv
           in
           (intv, sub)

         (** Case: x := s *)
         | Cst (Scalar s) -> 
           let intv = BatList.modify_at dim (fun _ -> (z_ext_of_scalar s, z_ext_of_scalar s)) intv
           in
           (intv, sub)

         (** Case: x := y *)
         | Var y ->
           let dim_y = Environment.dim_of_var t.env y in
           let intv = BatList.modify_at dim (fun _ -> BatList.at intv dim_y) intv in
           let sub = sub |>
                     Sub.forget_vars [dim] |>
                     (* x = y ==> if z < y then also z < x *)
                     Sub.VarList.map (
                       fun set ->
                         if Sub.VarSet.mem dim_y set then
                           Sub.VarSet.add dim set
                         else 
                           set
                     ) |>
                     (* Subs of x := Subs of y *)
                     BatList.modify_at dim (fun _ -> BatList.at sub dim_y)
           in
           (intv, sub)

         | Unop  (Neg,  e, _, _) -> 
           let (intv, sub) = convert_texpr e t in

           let intv = BatList.modify_at dim (
               fun intv ->
                 (ZExt.neg (Interval.sup intv), ZExt.neg (Interval.inf intv))
             ) intv
           in
           (**
              We do not add redundant information in Subs. 
              Later checks can derive inequalities by looking at intv.
           *)
           (intv, sub_without_var)

         | Unop  (Cast, e, _, _) -> convert_texpr e t

         | Unop  (Sqrt, e, _, _) ->
           (** 
              TODO: What is the semantics of Sqrt. May we still support this? 
           *)
           (intv_without_var, sub_without_var)
         | Binop (Add, e1, e2, _, _) -> 
           let (intv_1, sub_1) = convert_texpr e1 t in
           let (intv_2, sub_2) = convert_texpr e2 t in

           let i2 = BatList.at intv_2 dim in
           let intv = BatList.modify_at dim (
               fun i1 ->
                 Interval.add i1 i2
             ) intv_1
           in
           (intv, sub_without_var)

         | Binop (Sub, e1, e2, t0, r) ->
           convert_texpr (Binop (Add, e1, Unop (Neg, e2, t0, r), t0, r)) t

         | Binop (Mul, e1, e2, _, _) ->
           let (intv_1, sub_1) = convert_texpr e1 t in
           let (intv_2, sub_2) = convert_texpr e2 t in

           let i2 = BatList.at intv_2 dim in
           let intv = BatList.modify_at dim (
               fun i1 -> Interval.mul i1 i2 ) intv_1 in
           (intv, sub_without_var)

         | Binop (Div, e1, e2, _, _) ->
           let (intv_1, sub_1) = convert_texpr e1 t in
           let (intv_2, sub_2) = convert_texpr e2 t in

           let i2 = BatList.at intv_2 dim in
           let intv = BatList.modify_at dim (
               fun i1 -> Interval.div i1 i2 ) intv_1 in
           (intv, sub_without_var)

         (** 
            Implemented as described by the paper mention at the beginning of this file.
            Refer to 6.2.2 Remainder.
         *)
         | Binop (Mod, e1, e2, _, _)  ->
           let (intv_1, sub_1) = convert_texpr e1 t in
           let (intv_2, sub_2) = convert_texpr e2 t in

           let i2 = BatList.at intv_2 dim in
           let intv = BatList.modify_at dim (
               fun i1 -> 
                 Interval.rem i1 i2
             ) intv_1 in

           let sub = 
             match e2 with
             | Var divisor -> (
                 let dim_divisor = Environment.dim_of_var t.env divisor in 
                 let intv_divisor = BatList.at intv_2 dim_divisor
                 in
                 if (Interval.inf intv_divisor) < ZExt.zero then 
                   sub_without_var
                 else
                   BatList.modify_at dim (fun _ -> Sub.VarSet.singleton dim_divisor) sub_without_var
               )
             | _ -> sub_without_var
           in
           (intv, sub)


         (** e1 ^ e2 *)
         | Binop (Pow, e1, e2, _, _) -> 
           let (intv_1, sub_1) = convert_texpr e1 t in
           let (intv_2, sub_2) = convert_texpr e2 t in

           let i2 = BatList.at intv_2 dim in
           let intv = BatList.modify_at dim (
               fun i1 -> 
                 Interval.pow i1 i2
             ) intv_1 in

           (intv, sub_without_var)
        ) in

    let (intv, sub) = convert_texpr texp t in
    match intv, sub with
    | [], [] -> { d= None; env=t.env }
    | _ ->
      { d=Some({ intv = intv; sub = sub }); env = t.env }
  ;;


  let assign_texpr t var texp = Timing.wrap "assign_texpr" (assign_texpr t var) texp

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
    let interval_helper ((lb, ub): ZExt.t * ZExt.t) tcons =
      let zero = ZExt.of_int 0 in 
      match Tcons1.get_typ tcons with
      | EQ when lb <= zero && ub >= zero -> t
      | SUPEQ when ub >= zero -> t
      | SUP when ub >= zero -> t
      | DISEQ when ub <> zero || lb <> zero -> t
      | EQMOD (s) -> (
          let s = z_ext_of_scalar s in
          let ( - ) = ZExt.sub in
          if (ub - lb) <= (s - ZExt.of_int 2) && (ZExt.rem_add lb s) <= (ZExt.rem_add ub s) then
            t 
          else
            bot_of_env t.env
        )
      | _ -> bot_of_env t.env
    in
    match t.d with 
    | None -> t
    | Some d -> 
      match Convert.tcons1_of_cil_exp ask t t.env e negate no_ov with
      | exception Convert.Unsupported_CilExp _ -> t
      | tcons1 -> 
        match (Texpr1.to_expr @@ Tcons1.get_texpr1 tcons1) with 
        | Cst (Interval inv) -> 
          interval_helper (z_ext_of_scalar inv.inf, z_ext_of_scalar inv.sup) tcons1
        | Cst (Scalar s) -> 
          interval_helper (z_ext_of_scalar s, z_ext_of_scalar s) tcons1
        | Var y -> (
            (** We ignore sub-information for now. *)
            let dim_y = Environment.dim_of_var t.env y in
            let intv_y = List.at d.intv dim_y in
            interval_helper intv_y tcons1
          )
        | Unop  (Neg,  e, _, _) -> failwith "TODO"
        | Unop  (Cast, e, _, _) -> failwith "TODO"
        | Unop  (Sqrt, e, _, _) ->failwith "TODO"
        | Binop (Add, e1, e2, _, _) -> failwith "TODO"
        | Binop (Sub, e1, e2, t0, r) ->failwith "TODO"
        | Binop (Mul, e1, e2, _, _) ->failwith "TODO"
        | Binop (Div, e1, e2, _, _) ->failwith "TODO"
        | Binop (Mod, e1, e2, _, _)  -> failwith "TODO"
        | Binop (Pow, e1, e2, _, _) -> failwith "TODO"

  let invariant t : Lincons1Set.elt list = failwith "TODO invariant"

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

  let to_string pntg = 
    if is_bot pntg then
      "bot"
    else if is_top pntg then
      "top"
    else
      match pntg.d with
      | None -> failwith "is_bot should take care of that"
      | Some(d) -> Intv.to_string d.intv ^ " " ^ Sub.to_string d.sub;;

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
