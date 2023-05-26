(** Wrapper analysis to answer EvalFunvar query using Cil's pointer analysis. *)

open GoblintCil
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "ptranal"

  module D = Lattice.Unit
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

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [ctx.local, ctx.local]

  let combine_env ctx lval fexp f args fc au f_ask =
    au

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.EvalFunvar (Lval (Mem e, _)) -> 
      let funs = Ptranal.resolve_exp e in
      List.fold_left (fun xs f -> Queries.LS.add (f, `NoOffset) xs) (Queries.LS.empty ()) funs
    | _ -> Queries.Result.top q

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()

  let init _: unit =
    Ptranal.analyze_file !Cilfacade.current_file;
    Ptranal.compute_results false

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
