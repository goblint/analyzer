(** An analysis for the detection of use-after-free vulnerabilities ([useAfterFree]). *)

open GoblintCil
open Analyses
open MessageCategory
open AnalysisStateUtil

module AllocaVars = SetDomain.ToppedSet(CilType.Varinfo)(struct let topname = "All alloca() Variables" end)
module InvalidVars = SetDomain.ToppedSet(CilType.Varinfo)(struct let topname = "All invalid memory variables" end)

(* Live alloca() regions owned by the current frame; regions whose storage may
   have ended through free() or return. *)
module StackAndInvalidVars = Lattice.Prod(AllocaVars)(InvalidVars)

module ThreadIdToJoinedThreadsMap = MapDomain.MapBot(ThreadIdDomain.ThreadLifted)(ConcDomain.FiniteMustThreadSet)

module Spec : Analyses.MCPSpec =
struct
  include Analyses.IdentityUnitContextsSpec

  let name () = "useAfterFree"

  module D = StackAndInvalidVars
  module G = ThreadIdToJoinedThreadsMap
  module V = VarinfoV

  let addressed_automatic_vars fundec =
    let automatics =
      fundec.sformals @ List.filter (fun v -> v.vstorage <> Static && v.vstorage <> Extern) fundec.slocals
    in
    List.filter (fun v -> v.vaddrof) automatics

  (* HELPER FUNCTIONS *)

  let get_current_threadid man =
    man.ask Queries.CurrentThreadId

  let get_joined_threads man =
    man.ask Queries.MustJoinedThreads

  let warn_for_multi_threaded_access man ?(is_free = false) (memory_var:varinfo) behavior cwe_number =
    let freeing_threads = man.global memory_var in
    (* If we're single-threaded or there are no threads freeing the memory, we have nothing to WARN about *)
    if not (ThreadFlag.has_ever_been_multi (Analyses.ask_of_man man)) || G.is_empty freeing_threads then ()
    else begin
      let other_possibly_started current tid joined_threads =
        match tid with
        | `Lifted tid when (ThreadId.Thread.equal current tid && ThreadId.Thread.is_unique current) ->
          (* if our own (unique) thread is started here, that is not a problem *)
          false
        | `Lifted tid ->
          let created_threads = man.ask Queries.CreatedThreads in
          let not_started = MHP.definitely_not_started (current, created_threads) tid in
          let possibly_started = not not_started in
          (* If [current] is possibly running together with [tid], but is also joined before the free() in [tid], then no need to WARN *)
          possibly_started && not (ConcDomain.FiniteMustThreadSet.mem_lifted current joined_threads)
        | `Top -> true
        | `Bot -> false
      in
      let equal_current current tid joined_threads =
        match tid with
        | `Lifted tid ->
          ThreadId.Thread.equal current tid
        | `Top -> true
        | `Bot -> false
      in
      let bug_name = if is_free then "Double Free" else "Use After Free" in
      match get_current_threadid man with
      | `Lifted current ->
        let possibly_started = G.exists (other_possibly_started current) freeing_threads in
        if possibly_started then begin
          if is_free then set_mem_safety_flag InvalidFree else set_mem_safety_flag InvalidDeref;
          M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "There's a thread that's been started in parallel with the memory-freeing threads for memory region %a. %s might occur" CilType.Varinfo.pretty memory_var bug_name
        end
        else begin
          let current_is_unique = ThreadId.Thread.is_unique current in
          let any_equal_current threads = G.exists (equal_current current) threads in
          if not current_is_unique && any_equal_current freeing_threads then begin
            if is_free then set_mem_safety_flag InvalidFree else set_mem_safety_flag InvalidDeref;
            M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Current thread is not unique and a %s might occur for memory region %a" bug_name CilType.Varinfo.pretty memory_var
          end
          else if InvalidVars.mem memory_var (snd man.local) then begin
            if is_free then set_mem_safety_flag InvalidFree else set_mem_safety_flag InvalidDeref;
            M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "%s might occur in current unique thread %a for memory region %a" bug_name ThreadIdDomain.Thread.pretty current CilType.Varinfo.pretty memory_var
          end
        end
      | `Top ->
        if is_free then set_mem_safety_flag InvalidFree else set_mem_safety_flag InvalidDeref;
        M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "CurrentThreadId is top. %s might occur for memory region %a" bug_name CilType.Varinfo.pretty memory_var
      | `Bot ->
        M.warn ~category:MessageCategory.Analyzer "CurrentThreadId is bottom"
    end

  let publish_invalidation man invalidated_vars threadid joined_threads =
    let side_effect_global memory_var =
      let current_globals = man.global memory_var in
      let globals_to_side_effect = G.add threadid joined_threads current_globals in
      man.sideg memory_var globals_to_side_effect
    in
    InvalidVars.iter side_effect_global invalidated_vars

  (* TRANSFER FUNCTIONS *)

  let enter man (lval:lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let caller_state = man.local in
    (* TODO: The 2nd component of the callee state needs to contain only the heap vars from the caller state which are reachable from: *)
    (* * Global program variables *)
    (* * The callee arguments *)
    [caller_state, (AllocaVars.empty (), snd caller_state)]

  let combine_env man (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) (f_ask:Queries.ask) : D.t =
    let (caller_stack_state, caller_heap_state) = man.local in
    let callee_heap_state = snd callee_local in
    (* The callee's return has already invalidated its alloca regions. *)
    (caller_stack_state, InvalidVars.join caller_heap_state callee_heap_state)

  let return man _ fundec =
    match man.edge with
    | MyCFG.Ret (_, returned_fundec) when CilType.Fundec.equal fundec returned_fundec ->
      let stack_allocations, invalid = man.local in
      let expired = List.fold_left (fun expired v -> InvalidVars.add v expired) stack_allocations
          (addressed_automatic_vars fundec) in
      publish_invalidation man expired (get_current_threadid man) (get_joined_threads man);
      let invalid = InvalidVars.join expired invalid in
      AllocaVars.empty (), invalid
    | _ -> man.local

  let special man (lval:lval option) (f:varinfo) (arglist:exp list) : D.t =
    let state = man.local in
    let desc = LibraryFunctions.find f in
    match desc.special arglist with
    | Free ptr ->
      (* TODO: do this using Free Access events? *)
      begin match man.ask (Queries.MayPointTo ptr) with
        | ad when not (Queries.AD.is_top ad) ->
          let pointed_to_heap_vars =
            Queries.AD.fold (fun addr state ->
                match addr with
                | Queries.AD.Addr.Addr (var,_) when man.ask (Queries.IsAllocVar var) && man.ask (Queries.IsHeapVar var) -> InvalidVars.add var state
                | _ -> state
              ) ad (InvalidVars.empty ())
          in
          (* Side-effect the tid that's freeing all the heap vars collected here *)
          publish_invalidation man pointed_to_heap_vars (get_current_threadid man) (get_joined_threads man);
          (* Add all heap vars, which ptr points to, to the state *)
          (fst state, InvalidVars.join (snd state) pointed_to_heap_vars)
        | _ -> state
      end
    | Alloca _ ->
      (* Create fresh heap var for the alloca() call *)
      begin match man.ask (Queries.AllocVar Stack) with
        | `Lifted v -> (AllocaVars.add v (fst state), snd state)
        | _ -> state
      end
    | _ -> state

  let startstate v = D.bot ()
  let exitstate v = D.top ()

  let event man e oman =
    match e with
    | Events.Access {exp; ad; kind; reach} ->
      (* must use original (pre-assign, etc) man queries *)
      let is_free = kind = Free in
      let invalid_vars = snd oman.local in
      let undefined_behavior = if is_free then Undefined DoubleFree else Undefined UseAfterFree in
      let cwe_number = if is_free then 415 else 416 in
      let warn_for_invalid_var v =
        if InvalidVars.mem v invalid_vars then begin
          if is_free then (
            set_mem_safety_flag InvalidFree;
            Checks.warn Checks.Category.DoubleFree "lval (%s) points to a maybe freed memory region" v.vname)
          else (
            set_mem_safety_flag InvalidDeref;
            Checks.warn Checks.Category.InvalidMemoryAccess "lval (%s) points to a maybe freed memory region" v.vname
          );
          M.warn ~category:(Behavior undefined_behavior) ~tags:[CWE cwe_number] "lval (%s) points to a maybe freed memory region" v.vname;
        end else if is_free then
          Checks.safe Checks.Category.DoubleFree
        else
          Checks.safe Checks.Category.InvalidMemoryAccess
      in
      if not (Queries.AD.is_top ad) then begin
        let directly_accessed v =
          match exp with
          | AddrOf (Var direct, _) | StartOf (Var direct, _) -> CilType.Varinfo.equal v direct
          | _ -> false
        in
        let pointed_to_invalid_vars =
          Queries.AD.fold (fun addr vars ->
              match addr with
              | Queries.AD.Addr.Addr (v,_)
                when oman.ask (Queries.IsAllocVar v)
                     || ((InvalidVars.mem v invalid_vars || not (G.is_empty (oman.global v)))
                         && not (directly_accessed v)) -> v :: vars
              | _ -> vars
            ) ad []
        in
        List.iter warn_for_invalid_var pointed_to_invalid_vars;
        List.iter (fun memory_var -> warn_for_multi_threaded_access oman ~is_free memory_var undefined_behavior cwe_number) pointed_to_invalid_vars
      end;
      man.local
    | _ ->
      man.local
end

let _ =
  MCP.register_analysis ~dep:["access"] (module Spec : MCPSpec)
