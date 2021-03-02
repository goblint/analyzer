(** Analysis using Apron for integer variables. *)

open Prelude.Ana
open Analyses
open Apron

open ApronDomain

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  let name () = "poly"

  module D = D
  module G = Lattice.Unit
  module C = D

  let val_of x = x
  let context x = if GobConfig.get_bool "exp.full-context" then x else D.bot ()

  let threadenter ctx lval f args = D.top ()
  let threadspawn ctx lval f args fctx = D.bot ()
  let exitstate  _ = D.top ()
  let startstate _ = D.top ()

  let enter ctx r f args =
    if D.is_bot ctx.local then [ctx.local, D.bot ()] else
      let f = Cilfacade.getdec f in
      let is, fs = D.typesort f.sformals in
      let is = is @ List.map (fun x -> x^"'") is in
      let fs = fs @ List.map (fun x -> x^"'") fs in
      let newd = D.add_vars ctx.local (is,fs) in
      let formargs = List.map2 (fun x y -> x,y) f.sformals args in
      let arith_formals = List.filter (fun (x,_) -> isArithmeticType x.vtype) formargs in
      List.iter (fun (v, e) -> D.assign_var_with newd (v.vname^"'") e) arith_formals;
      D.forget_all_with newd (List.map (fun (x,_) -> x.vname) arith_formals);
      List.iter  (fun (v,_)   -> D.assign_var_eq_with newd v.vname (v.vname^"'")) arith_formals;
      D.remove_all_but_with newd (is@fs);
      [ctx.local, newd]


  let combine ctx r fe f args fc d =
    if D.is_bot ctx.local || D.is_bot d then D.bot () else
      let f = Cilfacade.getdec f in
      match r with
      | Some (Var v, NoOffset) when isArithmeticType v.vtype && (not v.vglob) ->
        let nd = D.forget_all ctx.local [v.vname] in
        let fis,ffs = D.get_vars ctx.local in
        let fis = List.map Var.to_string fis in
        let ffs = List.map Var.to_string ffs in
        let nd' = D.add_vars d (fis,ffs) in
        let formargs = List.map2 (fun x y -> x,y) f.sformals args in
        let arith_formals = List.filter (fun (x,_) -> isArithmeticType x.vtype) formargs in
        List.iter (fun (v, e) -> D.substitute_var_with nd' (v.vname^"'") e) arith_formals;
        let vars = List.map (fun (x,_) -> x.vname^"'") arith_formals in
        D.remove_all_with nd' vars;
        D.forget_all_with nd' [v.vname];
        D.substitute_var_eq_with nd' "#ret" v.vname;
        D.remove_all_with nd' ["#ret"];
        A.unify Man.mgr nd nd'
      | _ -> D.topE (A.env ctx.local)

  let special ctx r f args =
    if D.is_bot ctx.local then D.bot () else
      begin
        match LibraryFunctions.classify f.vname args with
        | `Assert expression -> D.assert_inv ctx.local expression false
        | `Unknown "printf" -> ctx.local
        | _ -> D.topE (A.env ctx.local)
      end

  let branch ctx e b =
    if D.is_bot ctx.local then D.bot () else
      D.assert_inv ctx.local e (not b)

  let return ctx e f =
    if D.is_bot ctx.local then D.bot () else
      match e with
      | Some e when isArithmeticType (typeOf e) ->
        let nd =
          if isIntegralType (typeOf e) then
            D.add_vars ctx.local (["#ret"],[])
          else
            D.add_vars ctx.local (["#ret"],[])
        in
        D.assign_var_with nd "#ret" e;
        let vars = List.filter (fun x -> isArithmeticType x.vtype) (f.slocals @ f.sformals) in
        let vars = List.map (fun x -> x.vname) vars in
        D.remove_all_with nd vars;
        nd
      | Some e -> ctx.local
      | None -> D.topE (A.env ctx.local)

  let body ctx f =
    if D.is_bot ctx.local then D.bot () else
      let vars = D.typesort f.slocals in
      D.add_vars ctx.local vars

  let assign ctx (lv:lval) e =
    if D.is_bot ctx.local then D.bot () else
      match lv with
      | Var v, NoOffset when isArithmeticType v.vtype && (not v.vglob) -> D.assign_var ctx.local v.vname e
      | _ -> D.topE (A.env ctx.local)

  let query ctx (q:Queries.t) : Queries.Result.t =
    let open Queries in
    let d = ctx.local in
    match q with
    | EvalInt e ->
      begin
        match D.get_int_val_for_cil_exp d e with
        | Some i -> `Int i
        | _ -> `Top
      end
   (* | EvalIntSet e ->
      begin
        match D.get_int_interval_for_cil_exp d e with
        | Some i, Some s ->
          if (Int64.compare i s) <= 0 then
            `IntSet (IntDomain.Enums.of_interval (i,s))
          else Result.bot ()
        | _ -> Result.top ()
      end 
    | EvalInterval e ->
      begin
        match D.get_int_interval_for_cil_exp d e with
        | Some i, Some s ->
          if (Int64.compare i s) <= 0 then
            `Interval (IntDomain.Interval.of_interval (i,s))
          else Result.bot ()
        | Some i, _ ->  `Interval (IntDomain.Interval.starting i)
        | _, Some s -> `Interval (IntDomain.Interval.ending s)
        | _ -> `Top
      end *)
    | MustBeEqual (e1, e2) ->
      if D.cil_exp_equals d e1 e2 then `MustBool true
      else `MustBool false
    | _ -> Result.top ()
end

let _ =
  MCP.register_analysis (module Spec : Spec)