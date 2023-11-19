(** An analysis for the detection of memory leaks ([memLeak]). *)

open GoblintCil
open Analyses
open MessageCategory
open AnalysisStateUtil

module ToppedVarInfoSet = SetDomain.ToppedSet(CilType.Varinfo)(struct let topname = "All Heap Variables" end)
module ThreadsToHeapVarsMap = MapDomain.MapBot(ThreadIdDomain.Thread)(ToppedVarInfoSet)
module WasMallocCalled = BoolDomain.MayBool
module Spec : Analyses.MCPSpec =
struct
  include Analyses.IdentitySpec

  let name () = "memLeak"

  module D = Lattice.Prod(ThreadsToHeapVarsMap)(WasMallocCalled)
  module C = D
  module P = IdentityP (D)

  let context _ d = d

  (* HELPER FUNCTIONS *)
  let warn_for_multi_threaded_due_to_abort ctx =
    let state = ctx.local in
    if not (ctx.ask (Queries.MustBeSingleThreaded { since_start = true })) && snd state then (
      set_mem_safety_flag InvalidMemTrack;
      set_mem_safety_flag InvalidMemcleanup;
      M.warn ~category:(Behavior (Undefined MemoryLeak)) ~tags:[CWE 401] "Program aborted while running in multi-threaded mode. A memory leak might occur"
    )

  (* If [is_return] is set to [true], then a thread return occurred, else a thread exit *)
  let warn_for_thread_return_or_exit current_thread ctx is_return =
    let state = ctx.local in
    let heap_vars_of_curr_tid = ThreadsToHeapVarsMap.find current_thread (fst state) in
    if not (ToppedVarInfoSet.is_empty heap_vars_of_curr_tid) then (
      set_mem_safety_flag InvalidMemcleanup;
      M.warn ~category:(Behavior (Undefined MemoryLeak)) ~tags:[CWE 401] "Memory may be leaked at thread %s for thread %a" (if is_return then "return" else "exit") ThreadIdDomain.Thread.pretty current_thread
    )

  let check_for_mem_leak ?(assert_exp_imprecise = false) ?(exp = None) ctx =
    let state = ctx.local in
    if not (ThreadsToHeapVarsMap.for_all (fun tid heap_vars -> ToppedVarInfoSet.is_empty heap_vars) (fst state)) then
      match assert_exp_imprecise, exp with
      | true, Some exp ->
        set_mem_safety_flag InvalidMemTrack;
        set_mem_safety_flag InvalidMemcleanup;
        M.warn ~category:(Behavior (Undefined MemoryLeak)) ~tags:[CWE 401] "assert expression %a is unknown. Memory leak might possibly occur for heap variables: %a" d_exp exp D.pretty state
      | _ ->
        set_mem_safety_flag InvalidMemTrack;
        set_mem_safety_flag InvalidMemcleanup;
        M.warn ~category:(Behavior (Undefined MemoryLeak)) ~tags:[CWE 401] "Memory leak detected for heap variables"

  (* TRANSFER FUNCTIONS *)
  let return ctx (exp:exp option) (f:fundec) : D.t =
    (* Check for a valid-memcleanup violation in a multi-threaded setting *)
    if (ctx.ask (Queries.MayBeThreadReturn)) then (
      match ctx.ask (Queries.CurrentThreadId) with
      | `Lifted tid ->
        warn_for_thread_return_or_exit tid ctx true
      | _ -> ()
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
      begin match ctx.ask (Queries.AllocVar {on_stack = false}) with
        | `Lifted var ->
          begin match ctx.ask (Queries.CurrentThreadId) with
            | `Lifted tid ->
              ((ThreadsToHeapVarsMap.add tid (ToppedVarInfoSet.singleton var) (fst state)), true)
            | _ -> (fst state, true)
          end
        | _ -> (fst state, true)
      end
    | Free ptr ->
      begin match ctx.ask (Queries.MayPointTo ptr) with
        (* TODO: The cardinality of 1 seems to lead to the situation where only free() calls in main() are detected here (affects 76/08 and 76/09) *)
        (* | ad when not (Queries.AD.is_top ad) && Queries.AD.cardinal ad = 1 -> *)
        | ad when not (Queries.AD.is_top ad) ->
          (* Note: Need to always set "ana.malloc.unique_address_count" to a value > 0 *)
          begin match Queries.AD.choose ad with
            | Queries.AD.Addr.Addr (v,_) when ctx.ask (Queries.IsAllocVar v) && ctx.ask (Queries.IsHeapVar v) && not @@ ctx.ask (Queries.IsMultiple v) ->
              begin match ctx.ask (Queries.CurrentThreadId) with
                | `Lifted tid ->
                  let heap_vars_of_tid = ThreadsToHeapVarsMap.find tid (fst state) in
                  let heap_vars_of_tid_without_v = ToppedVarInfoSet.remove v heap_vars_of_tid in
                  let new_fst_state = ThreadsToHeapVarsMap.add tid heap_vars_of_tid_without_v (fst state) in
                  (new_fst_state, snd state)
                | _ -> state
              end
            | _ -> state
          end
        | _ -> state
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
            | Some b ->
              (* If we know for sure that the expression in "assert" is false => need to check for memory leaks *)
              if b = false then
                check_for_mem_leak ctx
              else ()
            | None -> check_for_mem_leak ctx ~assert_exp_imprecise:true ~exp:(Some exp)
          end
      in
      warn_for_assert_exp;
      state
    | ThreadExit _ ->
      begin match ctx.ask (Queries.CurrentThreadId) with
        | `Lifted tid ->
          warn_for_thread_return_or_exit tid ctx false
        | _ -> ()
      end;
      state
    | _ -> state

  let startstate v = D.bot ()
  let exitstate v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
