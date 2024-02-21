(** Semantic loop unrolling ([loopunroll]). *)

open Batteries
open GoblintCil
open Analyses

module M = Messages

module ID = Queries.ID

module Spec : Analyses.MCPSpec =
struct
  let name () = "loopunroll"

  module CountParam =
  struct
    let n () = GobConfig.get_int "exp.unrolling-factor" + 1
    let names = string_of_int
  end
  module Count = Lattice.Chain (CountParam)

  module D = MapDomain.MapBot (Node) (Count)
  module C = D

  let startstate v = D.bot ()
  let exitstate = startstate

  include Analyses.DefaultSpec
  module P = IdentityP (D)

  let emit_splits ctx d =
    match CfgTools.is_loop_head ctx.node with
    | Some s ->
      let cur = D.find ctx.node d in
      let next =
        if cur = CountParam.n () then
          cur
        else
          cur + 1
      in
      D.add ctx.node next d
    | None ->
      d

  let emit_splits_ctx ctx =
    emit_splits ctx ctx.local

  let assign ctx (lval:lval) (rval:exp) =
    emit_splits_ctx ctx

  let vdecl ctx (var:varinfo) =
    emit_splits_ctx ctx

  let branch ctx (exp:exp) (tv:bool) =
    let d = ctx.local in
    let d' =
      match CfgTools.is_loop_head ctx.prev_node with
      | Some s when tv = false ->
        D.remove ctx.prev_node d
      | _ ->
        d
    in
    emit_splits ctx d'

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) =
    [ctx.local, ctx.local]

  let body ctx (f:fundec) =
    emit_splits_ctx ctx

  let return ctx (exp:exp option) (f:fundec) =
    emit_splits_ctx ctx

  let combine_env ctx lval fexp f args fc au f_ask =
    let d = D.join ctx.local au in
    emit_splits ctx d (* Update/preserve splits for globals in combined environment. *)

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc au (f_ask: Queries.ask) =
    emit_splits_ctx ctx (* Update/preserve splits over assigned variable. *)

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) =
    emit_splits_ctx ctx

  let threadenter ctx ~multiple lval f args = [ctx.local]

  let threadspawn ctx ~multiple lval f args fctx =
    emit_splits_ctx ctx
end

let () =
  MCP.register_analysis (module Spec : MCPSpec)
