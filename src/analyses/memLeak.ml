(** An analysis for the detection of memory leaks ([memLeak]). *)

open GoblintCil
open Analyses
open MessageCategory
open AnalysisStateUtil

module ToppedVarInfoSet = SetDomain.ToppedSet(CilType.Varinfo)(struct let topname = "All Heap Variables" end)
module WasMallocCalled = BoolDomain.MustBool
module Spec : Analyses.MCPSpec =
struct
  include Analyses.IdentitySpec

  let name () = "memLeak"

  (* module D = ToppedVarInfoSet *)
  module D = Lattice.Prod(ToppedVarInfoSet)(WasMallocCalled)
  module C = D
  module P = IdentityP (D)
  module G = ToppedVarInfoSet
  module V =
  struct
    include ThreadIdDomain.Thread
    include StdV
  end

  let context _ d = d

  (* HELPER FUNCTIONS *)
  let warn_for_multi_threaded_due_to_abort ctx =
    let state = ctx.local in
    if not (ctx.ask (Queries.MustBeSingleThreaded { since_start = true })) && snd state then (
      set_mem_safety_flag InvalidMemTrack;
      set_mem_safety_flag InvalidMemcleanup;
      M.warn ~category:(Behavior (Undefined MemoryLeak)) ~tags:[CWE 401] "Program aborted while running in multi-threaded mode. A memory leak might occur"
    )

  (* If [is_return] is set to [true], then a thread return occurred, else a thread join *)
  let warn_for_thread_return_or_exit current_thread ctx is_return =
    let global_state = ctx.global current_thread in
    if not (G.is_empty global_state) then (
      set_mem_safety_flag InvalidMemcleanup;
      M.warn ~category:(Behavior (Undefined MemoryLeak)) ~tags:[CWE 401] "Memory may be leaked at thread %s" (if is_return then "return" else "join")
    )
  (* if not (ToppedVarInfoSet.is_empty (fst state)) && snd state then (
      set_mem_safety_flag InvalidMemcleanup;
      M.warn ~category:(Behavior (Undefined MemoryLeak)) ~tags:[CWE 401] "Memory may be leaked at thread %s" (if is_return then "return" else "join")
      ) *)

  let check_for_mem_leak ?(assert_exp_imprecise = false) ?(exp = None) ctx =
    let state = ctx.local in
    if not (ToppedVarInfoSet.is_empty (fst state)) then
      match assert_exp_imprecise, exp with
      | true, Some exp ->
        set_mem_safety_flag InvalidMemTrack;
        set_mem_safety_flag InvalidMemcleanup;
        M.warn ~category:(Behavior (Undefined MemoryLeak)) ~tags:[CWE 401] "assert expression %a is unknown. Memory leak might possibly occur for heap variables: %a" d_exp exp D.pretty state
      | _ ->
        set_mem_safety_flag InvalidMemTrack;
        set_mem_safety_flag InvalidMemcleanup;
        M.warn ~category:(Behavior (Undefined MemoryLeak)) ~tags:[CWE 401] "Memory leak detected for heap variables: %a" D.pretty state

  (* TRANSFER FUNCTIONS *)
  let return ctx (exp:exp option) (f:fundec) : D.t =
    (* Check for a valid-memcleanup violation in a multi-threaded setting *)
    if (ctx.ask (Queries.MayBeThreadReturn)) then (
      match ctx.ask (Queries.CurrentThreadId) with
      | `Lifted tid -> warn_for_thread_return_or_exit tid ctx true
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
              let current_globals = ctx.global tid in
              let globals_to_side_effect = G.add var current_globals in
              ctx.sideg tid globals_to_side_effect;
            | _ -> ()
          end;
          (ToppedVarInfoSet.add var (fst state), true)
        | _ -> (fst state, true)
      end
    | Free ptr ->
      begin match ctx.ask (Queries.MayPointTo ptr) with
        | ad when not (Queries.AD.is_top ad) && Queries.AD.cardinal ad = 1 ->
          (* Note: Need to always set "ana.malloc.unique_address_count" to a value > 0 *)
          begin match Queries.AD.choose ad with
            | Queries.AD.Addr.Addr (v,_) when ctx.ask (Queries.IsAllocVar v) && ctx.ask (Queries.IsHeapVar v) && not @@ ctx.ask (Queries.IsMultiple v) ->
              begin match ctx.ask (Queries.CurrentThreadId) with
                | `Lifted tid ->
                  let current_globals = ctx.global tid in
                  let globals_to_side_effect = G.remove v current_globals in
                  ctx.sideg tid globals_to_side_effect
                | _ -> ()
              end;
              (ToppedVarInfoSet.remove v (fst state), snd state) (* Unique pointed to heap vars *)
            | _ -> state
          end
        | _ -> state
      end
    | Abort ->
      (* Upon a call to the "Abort" special function, we give up and conservatively warn *)
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
        | `Lifted tid -> warn_for_thread_return_or_exit tid ctx false
        | _ -> ()
      end;
      state
    | _ -> state

  let startstate v = D.bot ()
  let exitstate v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)