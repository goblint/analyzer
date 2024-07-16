(** An analysis tracking the type of a mutex ([pthreadMutexType]). *)

open GoblintCil
open Analyses

module MAttr = ValueDomain.MutexAttr
module LF = LibraryFunctions

module Spec : Analyses.MCPSpec with module D = Lattice.Unit and module C = Printable.Unit =
struct
  include UnitAnalysis.Spec

  let name () = "pthreadMutexType"

  module V =
  struct
    (* Removing indexes here avoids complicated lookups and allows to have the LVals as vars here, at the price that different types of mutexes in arrays are not dinstinguished *)
    include Mval.Unit
    let is_write_only _ = false
  end

  module G = MAttr

  module O = Offset.Unit

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    match lval with
    | (Var v, o) ->
      (* There's no way to use the PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP etc for accesses via pointers *)
      let rec helper o t = function
        | Field ({fname = "__data"; _}, Field ({fname = "__kind"; _}, NoOffset)) when ValueDomain.Compound.is_mutex_type t ->
          let kind =
            (match Cil.constFold true rval with
             | Const (CInt (c, _, _)) -> MAttr.of_int c
             | _ -> `Top)
          in
          ctx.sideg (v,o) kind;
          ctx.local
        | Field ({fname = "__sig"; _}, NoOffset) when ValueDomain.Compound.is_mutex_type t -> (* OSX *)
          let kind: MAttr.t = match Cil.constFold true rval with
            | Const (CInt (c, _, _)) ->
              begin match Z.to_int c with (* magic constants from https://opensource.apple.com/source/libpthread/libpthread-301.30.1/pthread/pthread_impl.h.auto.html *)
                | 0x32AAABA7 -> `Lifted NonRec (* _PTHREAD_MUTEX_SIG_init *)
                | 0x32AAABA1 -> `Lifted NonRec (* _PTHREAD_ERRORCHECK_MUTEX_SIG_init *)
                | 0x32AAABA2 -> `Lifted Recursive (* _PTHREAD_RECURSIVE_MUTEX_SIG_init *)
                | _ -> `Top
              end
            | _ -> `Top
          in
          ctx.sideg (v,o) kind;
          ctx.local
        | Index (i,o') ->
          let o'' = O.of_offs (`Index (i, `NoOffset)) in
          helper (O.add_offset o o'') (Cilfacade.typeOffset t (Index (i,NoOffset))) o'
        | Field (f,o') ->
          let o'' = O.of_offs (`Field (f, `NoOffset)) in
          helper (O.add_offset o o'')  (Cilfacade.typeOffset t (Field (f,NoOffset))) o'
        | NoOffset -> ctx.local
      in
      helper `NoOffset v.vtype o
    | _ -> ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LF.find f in
    match desc.special arglist with
    | MutexInit {mutex = mutex; attr = attr} ->
      let attr = ctx.ask (Queries.EvalMutexAttr attr) in
      let mutexes = ctx.ask (Queries.MayPointTo mutex) in
      (* It is correct to iter over these sets here, as mutexes need to be intialized before being used, and an analysis that detects usage before initialization is a different analysis. *)
      Queries.AD.iter (function addr ->
        match addr with
        | Queries.AD.Addr.Addr (v,o) -> ctx.sideg (v,O.of_offs o) attr
        | _ -> ()
        ) mutexes;
      ctx.local
    | _ -> ctx.local

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MutexType (v,o) -> (ctx.global (v,o):MutexAttrDomain.t)
    | _ -> Queries.Result.top q
end

let must_be_recursive ctx (v,o) =
  ctx.ask (Queries.MutexType (v, Offset.Unit.of_offs o)) = `Lifted MutexAttrDomain.MutexKind.Recursive

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
