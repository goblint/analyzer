(** An analysis specification for didactic purposes. *)

open Prelude.Ana
open Analyses

let is_sink varinfo = false
let is_source varinfo = false


module Spec : Analyses.MCPSpec with module D = SetDomain.Make(CilType.Varinfo) and module C = Lattice.Unit =
struct
  include Analyses.DefaultSpec

  let name () = "taint"
  module D = SetDomain.Make(CilType.Varinfo) (* Change such that you have a fitting local domain, and update also in the signature above *)
  module C = Lattice.Unit

  (* We are context insensitive in this analysis *)
  let context _ _ = ()

  let rec is_exp_tainted (e:Cil.exp) (state:D.t) =
    (* Recurse over the structure in the expression, retruning true if any varinfo appearing in the expression is tainted *)
    false

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    (* Nothing needs to be done here, as the (non-formals) locals are initally untainted *)
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    (* TODO: Fake variable angeben, wenn exp tainted ist, dann diese hinzufügen *)
    ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    (* first component is state of caller, second component is state of callee *)
    [ctx.local, ctx.local]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) : D.t =
    let caller_local = ctx.local in
    (* combine the local states of caller and callee appropriately to get the local state after the call *)
    (* taking extra care to consider the fake global we introduced in return *)
    callee_local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    (* here you should check if f is a sink / source and handle it appropriately *)
    ctx.local

  (* You may leave these alone *)
  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
