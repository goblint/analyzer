(** An analysis for the detection of memory leaks ([memLeak]). *)

open GoblintCil
open Analyses
open MessageCategory

module ToppedVarInfoSet = SetDomain.ToppedSet(CilType.Varinfo)(struct let topname = "All Heap Variables" end)

module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let name () = "memLeak"

  module D = ToppedVarInfoSet
  module C = Lattice.Unit

  let context _ _ = ()

  (* HELPER FUNCTIONS *)
  let warn_for_multi_threaded ctx =
    if not (ctx.ask (Queries.MustBeSingleThreaded { since_start = true })) then
      M.warn ~category:(Behavior (Undefined MemoryLeak)) ~tags:[CWE 401] "Program isn't running in single-threaded mode. A memory leak might occur due to multi-threading"

  (* TRANSFER FUNCTIONS *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    let state = ctx.local in
    (* TODO: Is this too hacky of a solution? *)
    if f.svar.vname = "main" && not @@ D.is_empty state then
      M.warn ~category:(Behavior (Undefined MemoryLeak)) ~tags:[CWE 401] "Memory leak from function \"%s\": %a\n" f.svar.vname D.pretty state;
    state

  let enter ctx (lval:lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [ctx.local, ctx.local]

  let combine_env ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) (f_ask:Queries.ask) : D.t =
    callee_local

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) (f_ask: Queries.ask): D.t =
    ctx.local

  let special ctx (lval:lval option) (f:varinfo) (arglist:exp list) : D.t =
    let state = ctx.local in
    let desc = LibraryFunctions.find f in
    match desc.special arglist with
    | Malloc _
    | Calloc _
    | Realloc _ ->
      (* Warn about multi-threaded programs as soon as we encounter a dynamic memory allocation function *)
      warn_for_multi_threaded ctx;
      begin match ctx.ask Queries.HeapVar with
        | `Lifted var -> D.add var state
        | _ -> state
      end
    | Free ptr ->
      begin match ctx.ask (Queries.MayPointTo ptr) with
        | a when not (Queries.LS.is_top a) && not (Queries.LS.mem (dummyFunDec.svar, `NoOffset) a) ->
          (* TODO: Need to always set "ana.malloc.unique_address_count" to smth > 0 *)
          let unique_pointed_to_heap_vars =
            Queries.LS.filter (fun (v, _) -> ctx.ask (Queries.IsHeapVar v) && not @@ ctx.ask (Queries.IsMultiple v)) a
            |> Queries.LS.elements
            |> List.map fst
            |> D.of_list
          in
          D.diff state unique_pointed_to_heap_vars
        | _ -> state
      end
    | _ -> state

  let threadenter ctx lval f args = [ctx.local]
  let threadspawn ctx lval f args fctx = ctx.local

  let startstate v = D.bot ()
  let exitstate v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)