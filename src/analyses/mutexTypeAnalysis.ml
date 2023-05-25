(** An analysis tracking the type of a mutex. *)

open GoblintCil
open Analyses

module MAttr = ValueDomain.MutexAttr
module LF = LibraryFunctions

(* Removing indexes here avoids complicated lookups inside the map for a varinfo, at the price that different types of mutexes in arrays are not dinstinguished *)
let rec offs_no_index o =
  match o with
  | `NoOffset -> `NoOffset
  | `Field (f,o) -> `Field (f, offs_no_index o)
  | `Index (i,o) -> `Index (Lval.any_index_exp, offs_no_index o)

module Spec : Analyses.MCPSpec with module D = Lattice.Unit and module C = Lattice.Unit =
struct
  include Analyses.DefaultSpec
  module V = VarinfoV

  let name () = "pthreadMutexType"
  module D = Lattice.Unit
  module C = Lattice.Unit

  module G = MapDomain.MapBot_LiftTop (Lval.CilLval) (MAttr)

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    match lval with
    | Var v, Field ({fname = "__data"; _}, Field ({fname = "__kind"; _}, NoOffset)) when ValueDomain.Compound.is_mutex_type v.vtype ->
      let kind =
        (match Cil.constFold true rval with
         | Const (CInt (c, _, _)) -> MAttr.of_int c
         | _ -> `Top)
      in
      let r = G.singleton ((v,`NoOffset)) kind in
      ctx.sideg v r;
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

  let combine_env ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask:Queries.ask) : D.t =
    au

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LF.find f in
    match desc.special arglist with
    | MutexInit {mutex = mutex; attr = attr} ->
      let mutexes = ctx.ask (Queries.MayPointTo mutex) in
      let attr = ctx.ask (Queries.EvalMutexAttr attr) in
      (* It is correct to iter over these sets here, as mutexes need to be intialized before being used, and an analysis that detects usage before initialization is a different analysis. *)
      Queries.LS.iter (function (v, o) ->
          let r = G.singleton (v, offs_no_index o) attr in
          ctx.sideg v r) mutexes;
      ctx.local
    | _ -> ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MutexType ((v,o):Lval.CilLval.t) -> let r = ctx.global v in (G.find (v,o) r:MutexAttrDomain.t)
    | _ -> Queries.Result.top q
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
