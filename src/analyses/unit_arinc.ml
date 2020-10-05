(** An analysis specification for didactic purposes. *)

open Prelude.Ana
open Analyses_arinc

module Spec : Analyses_arinc.ArincSpec =
struct
  include Analyses_arinc.DefaultSpec

  let name () = "unit"
  module D = Lattice.Unit
  module G = Lattice.Unit
  module C = Lattice.Unit

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local, ctx.local]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let arinc_edge ctx e =
    ctx.local

  let arinc_start ctx e =
    ctx.local

  let startstate v = D.bot ()
  let otherstate v = D.top ()
  let exitstate  v = D.top ()
end

let _ =
  MCP_arinc.register_analysis (module Spec : ArincSpec)
