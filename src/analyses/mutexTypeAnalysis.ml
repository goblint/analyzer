(** An analysis tracking the type of a mutex. *)

open Prelude.Ana
open Analyses

module MAttr = ValueDomain.MutexAttr
module LF = LibraryFunctions

module Spec : Analyses.MCPSpec with module D = Lattice.Unit and module C = Lattice.Unit =
struct
  include Analyses.DefaultSpec
  module V = VarinfoV

  let name () = "pthreadMutexType"
  module D = Lattice.Unit
  module C = Lattice.Unit
  module G = MAttr

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    match lval with
    | Var v, Field ({fname = "__data"; _}, Field ({fname = "__kind"; _}, NoOffset)) when ValueDomain.Compound.is_mutex_type v.vtype ->
      let kind =
        (match Cil.constFold true rval with
         | Const (CInt (c, _, _)) -> MAttr.of_int c
         | _ -> `Top)
      in
      ctx.sideg v kind;
      ctx.local
    | _ -> ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [ctx.local, ctx.local]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask:Queries.ask) : D.t =
    au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LF.find f in
    match desc.special arglist with
    | MutexInit {mutex = mutex; attr = attr} ->
      let mutexes = ctx.ask (Queries.MayPointTo mutex) in
      let attr = ctx.ask (Queries.EvalMutexAttr attr) in
      Queries.LS.iter (function (v, _) -> ctx.sideg v attr) mutexes;
      ctx.local
    | _ -> ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.IsRecursiveMutex v -> ctx.global v = `Lifted (MAttr.MutexKind.Recursive)
    | _ -> Queries.Result.top q
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
