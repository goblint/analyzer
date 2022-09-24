open Prelude.Ana
open Analyses

module M = Messages

module ID = Queries.ID

module Spec : Analyses.MCPSpec =
struct
  let name () = "expsplit"

  module D = MapDomain.MapBot (Basetype.CilExp) (ID)
  module C = D

  let startstate v = D.bot ()
  let exitstate = startstate

  include Analyses.IdentitySpec (* TODO: implement others correctly instead of identity *)

  (* after IdentitySpec, because that would override... *)
  let should_join = D.equal

  let emit_splits ctx d =
    D.iter (fun e _ ->
        ctx.emit (UpdateExpSplit e)
      ) d;
    d

  let emit_splits_ctx ctx =
    emit_splits ctx ctx.local

  let assign ctx (lval:lval) (rval:exp) =
    emit_splits_ctx ctx

  let vdecl ctx (var:varinfo) =
    emit_splits_ctx ctx

  let vdecl ctx (var:varinfo) =
    emit_splits_ctx ctx

  let branch ctx (exp:exp) (tv:bool) =
    emit_splits_ctx ctx

  (* TODO: should splits go to enter? *)

  let body ctx (f:fundec) =
    emit_splits_ctx ctx

  let return ctx (exp:exp option) (f:fundec) =
    emit_splits_ctx ctx

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc au =
    emit_splits_ctx ctx (* TODO: how to combine if splits also from au? *)

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) =
    let d = match f.vname with
      | "__goblint_split_begin" ->
        let exp = List.hd arglist in
        let ik = Cilfacade.get_ikind_exp exp in
        (* TODO: something different for pointers, currently casts pointers to ints and loses precision (other than NULL) *)
        D.add exp (ID.top_of ik) ctx.local (* split immediately follows *)
      | "__goblint_split_end" ->
        let exp = List.hd arglist in
        D.remove exp ctx.local
      | _ ->
        ctx.local
    in
    emit_splits ctx d

  let threadspawn ctx lval f args fctx =
    emit_splits_ctx ctx

  let event ctx (event: Events.t) octx =
    match event with
    | UpdateExpSplit exp ->
      let value = ctx.ask (EvalInt exp) in
      D.add exp value ctx.local
    | _ ->
      ctx.local
end

let () =
  MCP.register_analysis (module Spec : MCPSpec)
