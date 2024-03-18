(** Wrapper to time an analysis *)

open GoblintCil
open Analyses

module Spec (S: MCPSpec) : MCPSpec =
struct
  include S

  let name () = S.name ()

  let time s f x = Timing.wrap (name () ^ "."^ s) f x

  let query ctx (type a) (q: a Queries.t) : a Queries.result =
    time "query" (query ctx) q

  let sync ctx reason =
    time "sync" (sync ctx) reason

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    time "assign" (S.assign ctx lval) rval

  let branch ctx (exp:exp) (tv:bool) : D.t =
    time "branch" (S.branch ctx exp) tv

  let body ctx (f:fundec) : D.t =
    time "body" (body ctx) f

  let return ctx (exp:exp option) (f:fundec) : D.t =
    time "return" (return ctx exp) f

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    time "enter" (enter ctx lval f) args

  let combine_env ctx lval fexp f args fc au f_ask =
    time "combine_env" (combine_env ctx lval fexp f args fc au) f_ask

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    time "combine_assign" (combine_assign ctx lval fexp f args fc au) f_ask

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    time "special" (special ctx lval f) arglist

  let startstate v =
    time "startstate" startstate v
  let threadenter ctx ~multiple lval f args =
    time "threadenter" (threadenter ~multiple ctx lval f) args

  let threadspawn ctx ~multiple lval f args fctx =
    time "threadspawn" (threadspawn ~multiple ctx lval f args) fctx

  let exitstate v =
    time "exitstate" exitstate v
end

(* let _ =
  MCP.register_analysis (module Spec : MCPSpec) *)
