open Batteries
open GoblintCil
open Analyses

(* module Spec : Analyses.MCPSpec with module D = Lattice.Unit and module C = Lattice.Unit and type marshal = unit = *)
(* No signature so others can override module G *)
module Spec (S: MCPSpec) : MCPSpec =
struct
  include S

  let name () = S.name ()
  module D = S.D
  module C = S.C

  let trace text =
    M.tracel (name ()) text

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    let res = S.assign ctx lval rval in
    if M.tracing then trace "assign: %a\n" D.pretty res;
    res

  let branch ctx (exp:exp) (tv:bool) : D.t =
    let res = S.branch ctx exp tv in
    if M.tracing then trace "branch: %a\n" D.pretty res;
    res

  let body ctx (f:fundec) : D.t =
    let res = S.body ctx f in
    if M.tracing then trace "body: %a\n" D.pretty res;
    res

  let return ctx (exp:exp option) (f:fundec) : D.t =
    let res = S.return ctx exp f in
    if M.tracing then trace "return: %a\n" D.pretty res;
    res

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let res = S.enter ctx lval f args in
    if M.tracing then trace "threadenter %a\n" (GoblintCil.Pretty.d_list ", " D.pretty) (List.map Tuple2.second res);
    res

  let combine_env ctx lval fexp f args fc au f_ask =
    let res = S.combine_env ctx lval fexp f args fc au f_ask in
    if M.tracing then trace "combine_env: %a\n" D.pretty res;
    res

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    let res = S.combine_assign ctx lval fexp f args fc au f_ask in
    if M.tracing then trace "combine_assign %a\n" D.pretty res;
    res

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let res = S.special ctx lval f arglist in
    if M.tracing then trace "special %a\n" D.pretty res;
    res

  let startstate v =
    let res = S.startstate v in
    if M.tracing then trace "startstate %a\n" D.pretty res;
    res

  let threadenter ctx ~multiple lval f args =
    let res = S.threadenter ctx ~multiple lval f args in
    if M.tracing then trace "threadenter %a\n" (GoblintCil.Pretty.d_list ", " D.pretty) res;
    res

  let threadspawn ctx ~multiple lval f args fctx =
    let res = S.threadspawn ctx ~multiple lval f args fctx in
    if M.tracing then trace "threadspawn %a\n" D.pretty res;
    res

  let exitstate v =
    let res = S.exitstate v in
    if M.tracing then trace "exitstate %a\n" D.pretty res;
    res

end