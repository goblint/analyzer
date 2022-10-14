(** An analysis checking whether a function only returns if its only argument has a non-zero value. *)

open Prelude.Ana
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "abortUnless"
  module D = BoolDomain.MustBool
  module C = Lattice.Unit

  let context _ _ = ()

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    false

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    if ctx.local then
      match f.sformals with
      | [arg] when isIntegralType arg.vtype ->
        (match ctx.ask (EvalInt (Lval (Var arg, NoOffset))) with
         | v when Queries.ID.is_bot v -> false
         | v ->
           match Queries.ID.to_bool v with
           | Some b -> b
           | None -> false)
      | _ ->
        (* should not happen, ctx.local should always be false in this case *)
        false
    else
      false

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let candidate = match f.sformals with
      | [arg] when isIntegralType arg.vtype -> true
      | _ -> false
    in
    [false, candidate]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_aks: Queries.ask) : D.t =
    if au && lval = None then (
      (* Assert happens after evaluation of call, so if variables in `arg` are assigned to, asserting might unsoundly yield bot *)
      (* See test 62/03 *)
      match args with
      | [arg] -> ctx.emit (Events.Assert arg)
      | _ -> ()
    );
    false

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    false

  let startstate v = false
  let threadenter ctx lval f args = [false]
  let threadspawn ctx lval f args fctx = false
  let exitstate v = false
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
