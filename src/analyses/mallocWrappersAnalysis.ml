(** An analysis that handles the case when malloc is called from a wrapper function all over the code. *)

open Prelude.Ana
open Analyses

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  let name () = "mallocWrapper"
  module D = Lattice.Flat (Basetype.ProgLines) (struct 
      let top_name = "Unknown line"
      let bot_name = "Unreachable line" 
    end)
  module G = Lattice.Unit
  module C = D

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
    let interestingfunctions = ["myalloc"; "myalloc2"; "ldv_malloc"] in
    let calleofinterest = List.mem f.vname interestingfunctions in
    let callectx = if calleofinterest then
       if ctx.local = `Top then
        `Lifted (MyCFG.getLoc ctx.node) 
        else ctx.local
      else D.top () in     
    [(ctx.local, callectx)]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t =
    ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.bot ()
  let otherstate v = D.top ()
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : Spec)
