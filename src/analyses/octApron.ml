(** Analysis using Apron for integer variables. *)

open Prelude.Ana
open Analyses
open Apron
open OctApronDomain

module SpecFunctor (Priv: OctApronPriv.S) : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let name () = "octApron"

  module D = OctApronComponents (Priv.D)
  module G = Priv.G
  module C = D

  module AD = OctApronDomain.D

  let val_of x = x
  let context x = if GobConfig.get_bool "exp.full-context" then x else D.bot ()


  let rec print_list_exp myList = match myList with
    | [] -> print_endline "End!"
    | head::body ->
      AD.print_expression head;
      print_list_exp body

  let rec get_vnames_list exp = match exp with
    | Lval lval ->
      let lhost, offset = lval in
      begin match lhost with
        | Var vinfo -> [vinfo.vname]
        | _ -> []
      end
    | UnOp (unop, e, typ) -> get_vnames_list e
    | BinOp (binop, e1, e2, typ) -> (get_vnames_list e1) @ (get_vnames_list e2)
    | AddrOf lval -> get_vnames_list (Lval(lval))
    | CastE(_, e) -> get_vnames_list e
    | _ -> []

  let invalidate oct (exps: exp list) =
    if Messages.tracing && exps <> [] then Messages.tracel "invalidate" "Will invalidate expressions [%a]\n" (d_list ", " d_plainexp) exps;
    let l = List.flatten (List.map get_vnames_list exps) in
    AD.forget_all_with oct l

  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = let st = ctx.local in (invalidate st.oct args); st
  let exitstate  _ = D.top ()
  let startstate _ =  D.top ()

  let enter ctx r f args =
    let st = ctx.local in
    if AD.is_bot st.oct then [st, D.bot ()] else
      let f = Cilfacade.getdec f in
      let is, fs = AD.typesort f.sformals in
      let is = is @ List.map (fun x -> x^"'") is in
      let fs = fs @ List.map (fun x -> x^"'") fs in
      let newd = AD.add_vars st.oct (is,fs) in
      let formargs = Goblintutil.zip f.sformals args in
      let arith_formals = List.filter (fun (x,_) -> isArithmeticType x.vtype) formargs in
      List.iter (fun (v, e) -> AD.assign_var_with newd (v.vname^"'") e) arith_formals;
      AD.forget_all_with newd (List.map (fun (x,_) -> x.vname) arith_formals);
      List.iter  (fun (v,_)   -> AD.assign_var_eq_with newd v.vname (v.vname^"'")) arith_formals;
      AD.remove_all_but_with newd (is@fs);
      [st, {st with oct = newd}]


  let combine ctx r fe f args fc fun_st =
    let st = ctx.local in
    if AD.is_bot st.oct then D.bot () else
      let f = Cilfacade.getdec f in
      match r with
      | Some (Var v, NoOffset) when isArithmeticType v.vtype && (not v.vglob) ->
        let nd = AD.forget_all st.oct [v.vname] in
        let fis,ffs = AD.get_vars st.oct in
        let fis = List.map Var.to_string fis in
        let ffs = List.map Var.to_string ffs in
        let nd' = AD.add_vars fun_st.oct (fis,ffs) in
        let formargs = Goblintutil.zip f.sformals args in
        let arith_formals = List.filter (fun (x,_) -> isArithmeticType x.vtype) formargs in
        List.iter (fun (v, e) -> AD.substitute_var_with nd' (v.vname^"'") e) arith_formals;
        let vars = List.map (fun (x,_) -> x.vname^"'") arith_formals in
        AD.remove_all_with nd' vars;
        AD.forget_all_with nd' [v.vname];
        AD.substitute_var_eq_with nd' "#ret" v.vname;
        AD.remove_all_with nd' ["#ret"];
        {fun_st with oct = A.unify Man.mgr nd nd'}
      | _ -> {fun_st with oct = AD.topE (A.env st.oct)}

  let special ctx r f args =
    let st = ctx.local in
    if AD.is_bot st.oct then D.bot () else
      begin
        match LibraryFunctions.classify f.vname args with
        | `Assert expression -> st
        | `Unknown "printf" -> st
        | `Unknown "__goblint_check" -> st
        | `Unknown "__goblint_commit" -> st
        | `Unknown "__goblint_assert" -> st
        | `Malloc size ->
          begin match r with
            | Some lv ->
              {st with oct = AD.remove_all st.oct [f.vname]}
            | _ -> st
          end
        | `Calloc (n, size) ->
          begin match r with
            | Some lv ->
              {st with oct = AD.remove_all st.oct [f.vname]}
            | _ -> st
          end
        | `ThreadJoin (id,ret_var) ->
          let nd = st.oct in
          invalidate nd [ret_var];
          st
        | `ThreadCreate _ -> st
        | _ ->
          begin
            let st =
              match LibraryFunctions.get_invalidate_action f.vname with
              | Some fnc -> let () = invalidate st.oct (fnc `Write  args) in st
              | None -> {st with oct = AD.topE (A.env st.oct)}
            in
            st
          end
      end

  let branch ctx e b =
    let st = ctx.local in
    if AD.is_bot st.oct then
      D.bot ()
    else
      let res = AD.assert_inv st.oct e (not b) in
      if AD.is_bot res then raise Deadcode;
      {st with oct = res}

  let return ctx e f =
    let st = ctx.local in
    if AD.is_bot st.oct then D.bot () else

      let nd = match e with
        | Some e when isArithmeticType (typeOf e) ->
          let nd = AD.add_vars st.oct (["#ret"],[]) in
          let () = AD.assign_var_with nd "#ret" e in
          nd
        | None -> AD.topE (A.env st.oct)
        | _ -> AD.add_vars st.oct (["#ret"],[])
      in
      let vars = List.filter (fun x -> isArithmeticType x.vtype) (f.slocals @ f.sformals) in
      let vars = List.map (fun x -> x.vname) vars in
      AD.remove_all_with nd vars;
      {st with oct = nd}

  let body ctx f =
    let st = ctx.local in
    if AD.is_bot st.oct then D.bot () else
      let vars = AD.typesort f.slocals in
      {st with oct = AD.add_vars st.oct vars}

  let assign ctx (lv:lval) e =
    let st = ctx.local in
    if AD.is_bot st.oct then D.bot () else
      match lv with
      (* Lvals which are numbers, have no offset and their address wasn't taken *)
      | Var v, NoOffset when isArithmeticType v.vtype && not v.vaddrof ->
        if not v.vglob then
          {st with oct = AD.assign_var_handling_underflow_overflow st.oct v e}
        else (
          let v_out = Goblintutil.create_var @@ makeVarinfo false (v.vname ^ "#out") v.vtype in (* temporary local g#out for global g *)
          let oct' = AD.assign_var_handling_underflow_overflow st.oct v_out e in (* g#out = e; *)
          let st' = Priv.write_global (Analyses.ask_of_ctx ctx) ctx.global ctx.sideg {st with oct = oct'} v v_out in (* g = g#out; *)
          let oct'' = AD.remove_all st'.oct [v_out.vname] in (* remove temporary g#out *)
          {st' with oct = oct''}
        )
      (* Ignoring all other assigns *)
      | _ -> st

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    let st = ctx.local in
    match q with
    | Assert e ->
      begin match AD.check_assert e st.oct with
        | `Top -> `Top
        | `True -> `Lifted true
        | `False -> `Lifted false
        | _ -> `Bot
      end
    | EvalInt e ->
      begin
        match AD.get_int_val_for_cil_exp st.oct e with
        | Some i -> ID.of_int i
        | _ -> `Top
      end
    | _ -> Result.top q
end

(* TODO: make dynamically configurable *)
module rec Spec: MCPSpec = SpecFunctor (OctApronPriv.WriteCenteredPriv)

let _ =
  MCP.register_analysis (module Spec : MCPSpec)