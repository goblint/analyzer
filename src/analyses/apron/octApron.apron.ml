(** Analysis using Apron for integer variables. *)

open Prelude.Ana
open Analyses
open Apron
open OctApronDomain

module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let name () = "octApron"

  module D = OctApronDomain.D
  module G = Lattice.Unit
  module C = D

  let val_of x = x
  let context x = if GobConfig.get_bool "exp.full-context" then x else D.bot ()


  let rec print_list_exp myList = match myList with
    | [] -> print_endline "End!"
    | head::body ->
    begin
      D.print_expression head;
      print_list_exp body
    end

  let rec get_vnames_list exp = match exp with
  | Lval lval ->
    let lhost, offset = lval in
    (match lhost with
      | Var vinfo -> [vinfo.vname]
      | _ -> [])
  | UnOp (unop, e, typ) -> get_vnames_list e
  | BinOp (binop, e1, e2, typ) -> (get_vnames_list e1) @ (get_vnames_list e2)
  | AddrOf lval -> get_vnames_list (Lval(lval))
  | CastE(_, e) -> get_vnames_list e
  | _ -> []

  let invalidate oct (exps: exp list) =
    if Messages.tracing && exps <> [] then Messages.tracel "invalidate" "Will invalidate expressions [%a]\n" (d_list ", " d_plainexp) exps;
    let l = List.flatten (List.map get_vnames_list exps) in
    D.forget_all oct l

  let threadenter ctx lval f args = [D.top ()] (* TODO: correct env? *)
  let threadspawn ctx lval f args fctx =
    invalidate ctx.local args
  let exitstate  _ = D.top () (* TODO: correct env? *)
  let startstate _ =  D.top () (* TODO: correct env? *)

  let enter ctx r f args =
    if D.is_bot ctx.local then [ctx.local, D.bot ()] else (
      if Messages.tracing then Messages.tracel "combine" "apron enter f: %a\n" d_varinfo f.svar;
      if Messages.tracing then Messages.tracel "combine" "apron enter formals: %a\n" (d_list "," d_varinfo) f.sformals;
      if Messages.tracing then Messages.tracel "combine" "apron enter local: %a\n" D.pretty ctx.local;
      let is = D.typesort f.sformals in
      let is = is @ List.map (fun x -> x^"'") is in
      let newd = D.add_vars ctx.local is in
      let formargs = Goblintutil.zip f.sformals args in
      let arith_formals = List.filter (fun (x,_) -> isIntegralType x.vtype) formargs in
      List.iter (fun (v, e) -> D.assign_var_with newd (v.vname^"'") e) arith_formals;
      D.forget_all_with newd (List.map (fun (x,_) -> x.vname) arith_formals);
      List.iter  (fun (v,_)   -> D.assign_var_eq_with newd v.vname (v.vname^"'")) arith_formals;
      D.remove_all_but_with newd (is);
      if Messages.tracing then Messages.tracel "combine" "apron enter newd: %a\n" D.pretty newd;
      [ctx.local, newd]
    )


  let combine ctx r fe f args fc d =
    if D.is_bot ctx.local || D.is_bot d then D.bot () else (
      if Messages.tracing then Messages.tracel "combine" "apron f: %a\n" d_varinfo f.svar;
      if Messages.tracing then Messages.tracel "combine" "apron formals: %a\n" (d_list "," d_varinfo) f.sformals;
      match r with
      | Some (Var v, NoOffset) when isIntegralType v.vtype && (not v.vglob) ->
        let nd = D.forget_all ctx.local [v.vname] in
        let fis = D.get_vars ctx.local in
        let fis = List.map Var.to_string fis in
        let nd' = D.add_vars d fis in
        if Messages.tracing then Messages.tracel "combine" "apron args: %a\n" (d_list "," d_exp) args;
        let formargs = Goblintutil.zip f.sformals args in
        let arith_formals = List.filter (fun (x,_) -> isIntegralType x.vtype) formargs in
        List.iter (fun (v, e) -> D.substitute_var_with nd' (v.vname^"'") e) arith_formals;
        let vars = List.map (fun (x,_) -> x.vname^"'") arith_formals in
        if Messages.tracing then Messages.tracel "combine" "apron remove vars: %a\n" (docList Pretty.text) vars;
        D.remove_all_with nd' vars;
        D.forget_all_with nd' [v.vname];
        D.substitute_var_eq_with nd' "#ret" v.vname;
        D.remove_all_with nd' ["#ret"];
        let r = A.unify Man.mgr nd nd' in
        if Messages.tracing then Messages.tracel "combine" "apron unifying %a %a = %a\n" D.pretty nd D.pretty nd' D.pretty r;
        r
      | _ ->
        (* TODO: don't go to top, but just forget r *)
        D.top_env (A.env ctx.local)
    )

  let special ctx r f args =
    if D.is_bot ctx.local then D.bot () else
      begin
        match LibraryFunctions.classify f.vname args with
        | `Assert expression -> ctx.local
        | `Unknown "printf" -> ctx.local
        | `Unknown "__goblint_check" -> ctx.local
        | `Unknown "__goblint_commit" -> ctx.local
        | `Unknown "__goblint_assert" -> ctx.local
        | `Malloc size ->
          (match r with
            | Some lv ->
              D.remove_all ctx.local [f.vname]
            | _ -> ctx.local)
        | `Calloc (n, size) ->
          (match r with
            | Some lv ->
              D.remove_all ctx.local [f.vname]
            | _ -> ctx.local)
        | `ThreadJoin (id,ret_var) ->
          invalidate ctx.local [ret_var]
        | `ThreadCreate _ -> ctx.local
        | _ ->
          begin
            let st =
              match LibraryFunctions.get_invalidate_action f.vname with
              | Some fnc -> invalidate ctx.local (fnc `Write  args)
              | None -> D.top_env (A.env ctx.local)
            in
              st
          end
      end

  let branch ctx e b =
    if D.is_bot ctx.local then
      D.bot ()
    else
      let res = D.assert_inv ctx.local e (not b) in
      if D.is_bot_env res then raise Deadcode;
      res

  let return ctx e f =
    if D.is_bot ctx.local then D.bot () else

      let nd = match e with
        | Some e when isIntegralType (typeOf e) ->
          let nd = D.add_vars ctx.local ["#ret"] in
          let () = D.assign_var_with nd "#ret" e in
          nd
        | None -> D.top_env (A.env ctx.local)
        | _ -> D.add_vars ctx.local ["#ret"]
      in
      let vars = List.filter (fun x -> isIntegralType x.vtype) (f.slocals @ f.sformals) in
      let vars = List.map (fun x -> x.vname) vars in
      D.remove_all_with nd vars;
      nd

  let body ctx f =
    if D.is_bot ctx.local then D.bot () else
      let vars = D.typesort f.slocals in
      D.add_vars ctx.local vars

  let assign ctx (lv:lval) e =
    if D.is_bot ctx.local then D.bot () else
      match lv with
      (* Locals which are numbers, have no offset and their address wasn't taken *)
      | Var v, NoOffset when isIntegralType v.vtype && (not v.vglob) && (not v.vaddrof)->
          D.assign_var_handling_underflow_overflow ctx.local v e
      (* Ignoring all other assigns *)
      | _ -> ctx.local

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    let d = ctx.local in
    match q with
    | Assert e ->
      begin match D.check_assert e ctx.local with
        | `Top -> `Top
        | `True -> `Lifted true
        | `False -> `Lifted false
        | _ -> `Bot
      end
    | EvalInt e ->
      begin
        match D.get_int_val_for_cil_exp d e with
        | Some i -> ID.of_int i
        | _ -> `Top
      end
    | _ -> Result.top q
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)

let () =
  Printexc.register_printer
    (function
      | Apron.Manager.Error e ->
        let () = Apron.Manager.print_exclog Format.str_formatter e in
        Some(Printf.sprintf "Apron.Manager.Error\n %s" (Format.flush_str_formatter ()))
      | _ -> None (* for other exceptions *)
    )
