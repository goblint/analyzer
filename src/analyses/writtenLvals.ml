(** An analysis specification for didactic purposes. *)

open Prelude.Ana
open Analyses

module Q = Queries

module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let name () = "writtenLvals"
  module D = Q.LS
  module G = Lattice.Unit
  module C = Q.LS

  (* transfer functions *)
  let add_written_lval ctx (lval:lval): D.t =
    let query e = ctx.ask (Q.MayPointTo e) in
    match lval with
      | Mem e, NoOffset
      | Mem e, Index _ ->
        (match query e with
          | `LvalSet s -> D.union ctx.local s
          | _ -> ctx.local
        )
      | Mem e, Field (finfo, offs) ->
        (match query e with
          | `LvalSet s -> D.union ctx.local (Q.LS.map (fun (v, offset) -> (v, `Field (finfo, offset))) s)
          | _ -> ctx.local
        )
      | _, _ -> ctx.local

  let add_written_option_lval ctx (lval: lval option): D.t =
    match lval with
    | Some lval -> add_written_lval ctx lval
    | None -> ctx.local

  let assign ctx (lval:lval) (rval:exp) : D.t = add_written_lval ctx lval

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local, Q.LS.bot ()]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t =
    add_written_option_lval ctx lval

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    add_written_option_lval ctx lval

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = D.bot ()
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
