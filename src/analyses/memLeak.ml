(** An analysis for the detection of memory leaks ([memLeak]). *)

open GoblintCil
open Analyses
open MessageCategory
open AnalysisStateUtil

module ToppedVarInfoSet = SetDomain.ToppedSet(CilType.Varinfo)(struct let topname = "All Heap Variables" end)
module WasMallocCalled = BoolDomain.MayBool
module Spec : Analyses.MCPSpec =
struct
  include Analyses.IdentitySpec

  let name () = "memLeak"

  module D = ToppedVarInfoSet
  module C = D
  module P = IdentityP (D)

  module V = UnitV
  module G = WasMallocCalled

  let context _ d = d

  (* HELPER FUNCTIONS *)
  let warn_for_multi_threaded_due_to_abort ctx =
    let malloc_called = ctx.global () in
    if not (ctx.ask (Queries.MustBeSingleThreaded { since_start = true })) && malloc_called then (
      set_mem_safety_flag InvalidMemTrack;
      set_mem_safety_flag InvalidMemcleanup;
      M.warn ~category:(Behavior (Undefined MemoryLeak)) ~tags:[CWE 401] "Program aborted while running in multi-threaded mode. A memory leak might occur"
    )

  (* If [is_return] is set to [true], then a thread return occurred, else a thread exit *)
  let warn_for_thread_return_or_exit ctx is_return =
    if not (ToppedVarInfoSet.is_empty ctx.local) then (
      set_mem_safety_flag InvalidMemTrack;
      set_mem_safety_flag InvalidMemcleanup;
      let current_thread = ctx.ask (Queries.CurrentThreadId) in
      M.warn ~category:(Behavior (Undefined MemoryLeak)) ~tags:[CWE 401] "Memory may be leaked at thread %s for thread %a" (if is_return then "return" else "exit") ThreadIdDomain.ThreadLifted.pretty current_thread
    )

  let check_for_mem_leak ?(assert_exp_imprecise = false) ?(exp = None) ctx =
    if not (ToppedVarInfoSet.is_empty ctx.local) then
      match assert_exp_imprecise, exp with
      | true, Some exp ->
        set_mem_safety_flag InvalidMemTrack;
        set_mem_safety_flag InvalidMemcleanup;
        M.warn ~category:(Behavior (Undefined MemoryLeak)) ~tags:[CWE 401] "assert expression %a is unknown. Memory leak might possibly occur for heap variables: %a" d_exp exp D.pretty ctx.local
      | _ ->
        set_mem_safety_flag InvalidMemTrack;
        set_mem_safety_flag InvalidMemcleanup;
        M.warn ~category:(Behavior (Undefined MemoryLeak)) ~tags:[CWE 401] "Memory leak detected for heap variables"

  (* TRANSFER FUNCTIONS *)
  let return ctx (exp:exp option) (f:fundec) : D.t =
    (* Check for a valid-memcleanup and memtrack violation in a multi-threaded setting *)
    (* The check for multi-threadedness is to ensure that valid-memtrack and valid-memclenaup are treated separately for single-threaded programs *)
    if (ctx.ask (Queries.MayBeThreadReturn) &&  not (ctx.ask (Queries.MustBeSingleThreaded { since_start = true }))) then (
        warn_for_thread_return_or_exit ctx true
    );
    (* Returning from "main" is one possible program exit => need to check for memory leaks *)
    if f.svar.vname = "main" then check_for_mem_leak ctx;
    ctx.local

  let special ctx (lval:lval option) (f:varinfo) (arglist:exp list) : D.t =
    let state = ctx.local in
    let desc = LibraryFunctions.find f in
    match desc.special arglist with
    | Malloc _
    | Calloc _
    | Realloc _ ->
      (ctx.sideg () true;
      begin match ctx.ask (Queries.AllocVar {on_stack = false}) with
        | `Lifted var ->
          ToppedVarInfoSet.add var state
        | _ -> state
      end)
    | Free ptr ->
      begin match ctx.ask (Queries.MayPointTo ptr) with
        | ad when (not (Queries.AD.is_top ad)) && Queries.AD.cardinal ad = 1 ->
          (* Note: Need to always set "ana.malloc.unique_address_count" to a value > 0 *)
          begin match Queries.AD.choose ad with
            | Queries.AD.Addr.Addr (v,_) when ctx.ask (Queries.IsAllocVar v) && ctx.ask (Queries.IsHeapVar v) && not @@ ctx.ask (Queries.IsMultiple v) ->
              ToppedVarInfoSet.remove v ctx.local
            | _ -> ctx.local
          end
        | _ -> ctx.local
      end
    | Abort ->
      check_for_mem_leak ctx;
      (* Upon a call to the "Abort" special function in the multi-threaded case, we give up and conservatively warn *)
      warn_for_multi_threaded_due_to_abort ctx;
      state
    | Assert { exp; _ } ->
      let warn_for_assert_exp =
        match ctx.ask (Queries.EvalInt exp) with
        | a when Queries.ID.is_bot a -> M.warn ~category:Assert "assert expression %a is bottom" d_exp exp
        | a ->
          begin match Queries.ID.to_bool a with
            | Some b -> (
              (* If we know for sure that the expression in "assert" is false => need to check for memory leaks *)
                if b = false then (
                  warn_for_multi_threaded_due_to_abort ctx;
                  check_for_mem_leak ctx
                )
                else ())
            | None ->
              (warn_for_multi_threaded_due_to_abort ctx;
               check_for_mem_leak ctx ~assert_exp_imprecise:true ~exp:(Some exp))
          end
      in
      warn_for_assert_exp;
      state
    | ThreadExit _ ->
      begin match ctx.ask (Queries.CurrentThreadId) with
        | `Lifted tid ->
          warn_for_thread_return_or_exit ctx false
        | _ -> ()
      end;
      state
    | _ -> state

  let startstate v = D.bot ()
  let exitstate v = D.top ()

  let threadenter ctx ~multiple lval f args = [D.bot ()]
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
