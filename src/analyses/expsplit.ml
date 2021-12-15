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

  (* TODO: emit update splits from all transfer functions *)
  let assign ctx (lval:lval) (rval:exp) =
    D.iter (fun e _ ->
        ctx.emit (UpdateExpSplit e)
      ) ctx.local;
    ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) =
    (* TODO: emit update splits *)
    match f.vname with
    | "__goblint_split_begin" ->
      let exp = List.hd arglist in
      let ik = Cilfacade.get_ikind_exp exp in
      (* TODO: something different for pointers, currently casts pointers to ints and loses precision (other than NULL) *)
      D.add exp (ID.top_of ik) ctx.local (* TODO: immediately split *)
      (* TODO: emit update splits _after_ adding expression, then doesn't have to eval for something better than top here (?) *)
    | "__goblint_split_end" ->
      let exp = List.hd arglist in
      D.remove exp ctx.local
    | _ ->
      ctx.local (* TODO: not identity *)

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
