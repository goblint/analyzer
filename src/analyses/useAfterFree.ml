(** An analysis for the detection of use-after-free vulnerabilities ([useAfterFree]). *)

open GoblintCil
open Analyses
open MessageCategory
open AnalysisStateUtil

module AllocaVars = SetDomain.ToppedSet(CilType.Varinfo)(struct let topname = "All alloca() Variables" end)
module InvalidVars = SetDomain.ToppedSet(CilType.Varinfo)(struct let topname = "All invalid memory variables" end)

(* Live alloca() regions owned by the current frame; regions whose storage may
   have ended through free() or return. *)
module UnknownEscapes = SetDomain.ToppedSet(CilType.Varinfo)(struct let topname = "All variables" end)
module Invalid = Lattice.Prod3 (InvalidVars) (BoolDomain.MayBool) (UnknownEscapes)
module StackAndInvalidVars = Lattice.Prod(AllocaVars)(Invalid)

module ThreadIdToJoinedThreadsMap = MapDomain.MapBot(ThreadIdDomain.ThreadLifted)(ConcDomain.FiniteMustThreadSet)

module Spec : Analyses.MCPSpec =
struct
  include Analyses.IdentityUnitContextsSpec

  let name () = "useAfterFree"

  module D = StackAndInvalidVars

  module G = Lattice.Prod (ThreadIdToJoinedThreadsMap) (UnknownEscapes)
  (* Named objects, all invalidations, and invalidations through an unknown pointer. *)
  module V = struct
    include Printable.Either (VarinfoV) (BoolDomain.MayBool)
    let is_write_only _ = false
  end

  let invalid_vars (_, (invalid, _, _)) = invalid
  let unknown_invalidated (_, (_, unknown, _)) = unknown
  let freeing_threads man key = fst (man.global key)
  let publish_threads man key threads = man.sideg key (threads, UnknownEscapes.bot ())

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

  let warn_for_multi_threaded_access man ?(is_free = false) ~threads ~locally_invalid (memory_var:varinfo) behavior cwe_number =
    let freeing_threads = threads in
    (* If we're single-threaded or there are no threads freeing the memory, we have nothing to WARN about *)
    if not (ThreadFlag.has_ever_been_multi (Analyses.ask_of_man man)) || ThreadIdToJoinedThreadsMap.is_empty freeing_threads then ()
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
        let possibly_started = ThreadIdToJoinedThreadsMap.exists (other_possibly_started current) freeing_threads in
        if possibly_started then begin
          if is_free then set_mem_safety_flag InvalidFree else set_mem_safety_flag InvalidDeref;
          M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "There's a thread that's been started in parallel with the memory-freeing threads for memory region %a. %s might occur" CilType.Varinfo.pretty memory_var bug_name
        end
        else begin
          let current_is_unique = ThreadId.Thread.is_unique current in
          let any_equal_current threads = ThreadIdToJoinedThreadsMap.exists (equal_current current) threads in
          if not current_is_unique && any_equal_current freeing_threads then begin
            if is_free then set_mem_safety_flag InvalidFree else set_mem_safety_flag InvalidDeref;
            M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Current thread is not unique and a %s might occur for memory region %a" bug_name CilType.Varinfo.pretty memory_var
          end
          else if locally_invalid then begin
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
    let publish key =
      publish_threads man key (ThreadIdToJoinedThreadsMap.add threadid joined_threads (freeing_threads man key))
    in
    InvalidVars.iter (fun v -> publish (`Left v)) invalidated_vars;
    if not (InvalidVars.is_empty invalidated_vars) then publish (`Right false)

  let record_unknown_invalidation man =
    let threads = ThreadIdToJoinedThreadsMap.add (get_current_threadid man) (get_joined_threads man)
        (freeing_threads man (`Right true)) in
    publish_threads man (`Right true) threads;
    publish_threads man (`Right false) threads

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
    (caller_stack_state, Invalid.join caller_heap_state callee_heap_state)

  let return man _ fundec =
    match man.edge with
    | MyCFG.Ret (_, returned_fundec) when CilType.Fundec.equal fundec returned_fundec ->
      let stack_allocations, (invalid, unknown, escapes) = man.local in
      let expired = List.fold_left (fun expired v -> InvalidVars.add v expired) stack_allocations
          (addressed_automatic_vars fundec) in
      publish_invalidation man expired (get_current_threadid man) (get_joined_threads man);
      let invalid = InvalidVars.join expired invalid in
      AllocaVars.empty (), (invalid, unknown, escapes)
    | _ -> man.local

  let special man (lval:lval option) (f:varinfo) (arglist:exp list) : D.t =
    let state = man.local in
    let desc = LibraryFunctions.find f in
    match desc.special arglist with
    | Free ptr ->
      (* TODO: do this using Free Access events? *)
      let ad = man.ask (Queries.MayPointTo ptr) in
      let pointed_to_heap_vars = Queries.AD.fold (fun addr vars ->
          match addr with
          | Queries.AD.Addr.Addr (v, _) when man.ask (Queries.IsAllocVar v) && man.ask (Queries.IsHeapVar v) -> InvalidVars.add v vars
          | _ -> vars
        ) ad (InvalidVars.empty ()) in
      publish_invalidation man pointed_to_heap_vars (get_current_threadid man) (get_joined_threads man);
      let stack, (invalid, unknown, escapes) = state in
      let unknown = unknown || Queries.AD.may_be_unknown ad in
      if Queries.AD.may_be_unknown ad then record_unknown_invalidation man;
      stack, (InvalidVars.join invalid pointed_to_heap_vars, unknown, escapes)
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
      let invalid_vars = invalid_vars oman.local in
      let undefined_behavior = if is_free then Undefined DoubleFree else Undefined UseAfterFree in
      let cwe_number = if is_free then 415 else 416 in
      let warn_for_invalid_var v =
        if InvalidVars.mem v invalid_vars || (unknown_invalidated oman.local && oman.ask (Queries.IsAllocVar v) && oman.ask (Queries.IsHeapVar v)) then begin
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
                     || ((InvalidVars.mem v invalid_vars || not (ThreadIdToJoinedThreadsMap.is_empty (freeing_threads oman (`Left v))))
                         && not (directly_accessed v)) -> v :: vars
              | _ -> vars
            ) ad []
        in
        List.iter warn_for_invalid_var pointed_to_invalid_vars;
        List.iter (fun memory_var ->
            let heap = oman.ask (Queries.IsAllocVar memory_var) && oman.ask (Queries.IsHeapVar memory_var) in
            let threads = freeing_threads oman (`Left memory_var) in
            let threads = if heap then ThreadIdToJoinedThreadsMap.join threads (freeing_threads oman (`Right true)) else threads in
            warn_for_multi_threaded_access oman ~is_free ~threads
              ~locally_invalid:(InvalidVars.mem memory_var invalid_vars || (heap && unknown_invalidated oman.local))
              memory_var undefined_behavior cwe_number
          ) pointed_to_invalid_vars
      end;
      if Queries.AD.may_be_unknown ad then begin
        if not (InvalidVars.is_empty invalid_vars) || unknown_invalidated oman.local then begin
          if is_free then set_mem_safety_flag InvalidFree else set_mem_safety_flag InvalidDeref;
          M.warn ~category:(Behavior undefined_behavior) ~tags:[CWE cwe_number]
            "Access through an unknown pointer may reach invalid storage"
        end;
        let fd = Node.find_fundec oman.node in
        warn_for_multi_threaded_access oman ~is_free ~threads:(freeing_threads oman (`Right false))
          ~locally_invalid:false {fd.svar with vname = "unknown storage"} undefined_behavior cwe_number
      end;
      man.local
    | _ ->
      man.local
end

let _ =
  MCP.register_analysis ~dep:["access"] (module Spec : MCPSpec)
