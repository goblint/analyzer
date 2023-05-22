(** Analysis that tracks whether the current function is to be analyzed modularly. *)
open GoblintCil
open Analyses

(* module Spec : Analyses.MCPSpec with module D = Lattice.Unit and module C = Lattice.Unit and type marshal = unit = *)
(* No signature so others can override module G *)
module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "is_modular"
  module D = BoolDomain.MustBool
  module C = Lattice.Unit

  let context f d = ()

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let callee_modular = ctx.local || ModularUtil0.is_modular_fun f.svar in
    [ctx.local, callee_modular]

  let combine_env ctx lval fexp f args fc au f_ask =
    ctx.local

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = false
  let threadenter ctx lval f args =
    let is_modular = ModularUtil0.is_modular_fun f in
    [is_modular]

  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = false

  let query ctx (type a) (q: a Queries.t): a Queries.result = match q with
    | IsModular ->
      let result: bool = ctx.local in (* type annotation required *)
      result
    | _ -> Queries.Result.top q

  let modular_support () = Both
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
