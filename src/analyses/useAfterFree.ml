(** An analysis for the detection of use-after-free vulnerabilities ([useAfterFree]). *)

open GoblintCil
open Analyses
open MessageCategory
open AnalysisStateUtil

module AllocaVars = SetDomain.ToppedSet(CilType.Varinfo)(struct let topname = "All alloca() Variables" end)
module HeapVars = SetDomain.ToppedSet(CilType.Varinfo)(struct let topname = "All Heap Variables" end)

(* Heap vars created by alloca() and deallocated at function exit * Heap vars deallocated by free() *)
module StackAndHeapVars = Lattice.Prod(AllocaVars)(HeapVars)

module ThreadIdToJoinedThreadsMap = MapDomain.MapBot(ThreadIdDomain.ThreadLifted)(ConcDomain.MustThreadSet)

module Spec : Analyses.MCPSpec =
struct
  include Analyses.IdentityUnitContextsSpec

  let name () = "useAfterFree"

  module D = StackAndHeapVars
  module G = ThreadIdToJoinedThreadsMap
  module V = VarinfoV

  (* HELPER FUNCTIONS *)

  let get_current_threadid man =
    man.ask Queries.CurrentThreadId

  let get_joined_threads man =
    man.ask Queries.MustJoinedThreads

  let warn_for_multi_threaded_access man ?(is_double_free = false) (heap_var:varinfo) behavior cwe_number =
    let freeing_threads = man.global heap_var in
    (* If we're single-threaded or there are no threads freeing the memory, we have nothing to WARN about *)
    if man.ask (Queries.MustBeSingleThreaded { since_start = true }) || G.is_empty freeing_threads then ()
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
          let current_joined_before_free = ConcDomain.MustThreadSet.mem current joined_threads in
          possibly_started && not current_joined_before_free
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
      let bug_name = if is_double_free then "Double Free" else "Use After Free" in
      match get_current_threadid man with
      | `Lifted current ->
        let possibly_started = G.exists (other_possibly_started current) freeing_threads in
        if possibly_started then begin
          if is_double_free then set_mem_safety_flag InvalidFree else set_mem_safety_flag InvalidDeref;
          M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "There's a thread that's been started in parallel with the memory-freeing threads for heap variable %a. %s might occur" CilType.Varinfo.pretty heap_var bug_name
        end
        else begin
          let current_is_unique = ThreadId.Thread.is_unique current in
          let any_equal_current threads = G.exists (equal_current current) threads in
          if not current_is_unique && any_equal_current freeing_threads then begin
            if is_double_free then set_mem_safety_flag InvalidFree else set_mem_safety_flag InvalidDeref;
            M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Current thread is not unique and a %s might occur for heap variable %a" bug_name CilType.Varinfo.pretty heap_var
          end
          else if HeapVars.mem heap_var (snd man.local) then begin
            if is_double_free then set_mem_safety_flag InvalidFree else set_mem_safety_flag InvalidDeref;
            M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "%s might occur in current unique thread %a for heap variable %a" bug_name ThreadIdDomain.Thread.pretty current CilType.Varinfo.pretty heap_var
          end
        end
      | `Top ->
        if is_double_free then set_mem_safety_flag InvalidFree else set_mem_safety_flag InvalidDeref;
        M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "CurrentThreadId is top. %s might occur for heap variable %a" bug_name CilType.Varinfo.pretty heap_var
      | `Bot ->
        M.warn ~category:MessageCategory.Analyzer "CurrentThreadId is bottom"
    end

  let rec warn_lval_might_contain_freed ?(is_implicitly_derefed = false) ?(is_double_free = false) (transfer_fn_name:string) man (lval:lval) =
    match is_implicitly_derefed, is_double_free, lval with
    (* If we're not checking for a double-free and there's no deref happening, then there's no need to check for an invalid deref or an invalid free *)
    | false, false, (Var _, NoOffset) -> ()
    | _ ->
      let state = man.local in
      let undefined_behavior = if is_double_free then Undefined DoubleFree else Undefined UseAfterFree in
      let cwe_number = if is_double_free then 415 else 416 in
      let rec offset_might_contain_freed offset =
        match offset with
        | NoOffset -> ()
        | Field (f, o) -> offset_might_contain_freed o
        | Index (e, o) -> warn_exp_might_contain_freed transfer_fn_name man e; offset_might_contain_freed o
      in
      let (lval_host, o) = lval in offset_might_contain_freed o; (* Check the lval's offset *)
      let lval_to_query =
        match lval_host with
        | Var _ -> Lval lval
        | Mem _ -> mkAddrOf lval (* Take the lval's address if its lhost is of the form *p, where p is a ptr *)
      in
      begin match man.ask (Queries.MayPointTo lval_to_query) with
        | ad when not (Queries.AD.is_top ad) ->
          let warn_for_heap_var v =
            if HeapVars.mem v (snd state) then begin
              if is_double_free then set_mem_safety_flag InvalidFree else set_mem_safety_flag InvalidDeref;
              M.warn ~category:(Behavior undefined_behavior) ~tags:[CWE cwe_number] "lval (%s) in \"%s\" points to a maybe freed memory region" v.vname transfer_fn_name
            end
          in
          let pointed_to_heap_vars =
            Queries.AD.fold (fun addr vars ->
                match addr with
                | Queries.AD.Addr.Addr (v,_) when man.ask (Queries.IsAllocVar v) -> v :: vars
                | _ -> vars
              ) ad []
          in
          (* Warn for all heap vars that the lval possibly points to *)
          List.iter warn_for_heap_var pointed_to_heap_vars;
          (* Warn for a potential multi-threaded UAF for all heap vars that the lval possibly points to *)
          List.iter (fun heap_var -> warn_for_multi_threaded_access man ~is_double_free heap_var undefined_behavior cwe_number) pointed_to_heap_vars
        | _ -> ()
      end

  and warn_exp_might_contain_freed ?(is_implicitly_derefed = false) ?(is_double_free = false) (transfer_fn_name:string) man (exp:exp) =
    match exp with
    (* Base recursion cases *)
    | Const _
    | SizeOf _
    | SizeOfStr _
    | AlignOf _
    | AddrOfLabel _ -> ()
    (* Non-base cases *)
    | Real e
    | Imag e
    | SizeOfE e
    | AlignOfE e
    | UnOp (_, e, _)
    | CastE (_, e) -> warn_exp_might_contain_freed ~is_implicitly_derefed ~is_double_free transfer_fn_name man e
    | BinOp (_, e1, e2, _) ->
      warn_exp_might_contain_freed ~is_implicitly_derefed ~is_double_free transfer_fn_name man e1;
      warn_exp_might_contain_freed ~is_implicitly_derefed ~is_double_free transfer_fn_name man e2
    | Question (e1, e2, e3, _) ->
      warn_exp_might_contain_freed ~is_implicitly_derefed ~is_double_free transfer_fn_name man e1;
      warn_exp_might_contain_freed ~is_implicitly_derefed ~is_double_free transfer_fn_name man e2;
      warn_exp_might_contain_freed ~is_implicitly_derefed ~is_double_free transfer_fn_name man e3
    (* Lval cases (need [warn_lval_might_contain_freed] for them) *)
    | Lval lval
    | StartOf lval
    | AddrOf lval -> warn_lval_might_contain_freed ~is_implicitly_derefed ~is_double_free transfer_fn_name man lval

  let side_effect_mem_free man freed_heap_vars threadid joined_threads =
    let side_effect_globals_to_heap_var heap_var =
      let current_globals = man.global heap_var in
      let globals_to_side_effect = G.add threadid joined_threads current_globals in
      man.sideg heap_var globals_to_side_effect
    in
    HeapVars.iter side_effect_globals_to_heap_var freed_heap_vars


  (* TRANSFER FUNCTIONS *)

  let assign man (lval:lval) (rval:exp) : D.t =
    warn_lval_might_contain_freed "assign" man lval;
    warn_exp_might_contain_freed "assign" man rval;
    man.local

  let branch man (exp:exp) (tv:bool) : D.t =
    warn_exp_might_contain_freed "branch" man exp;
    man.local

  let return man (exp:exp option) (f:fundec) : D.t =
    Option.iter (fun x -> warn_exp_might_contain_freed "return" man x) exp;
    man.local

  let enter man (lval:lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let caller_state = man.local in
    List.iter (fun arg -> warn_exp_might_contain_freed "enter" man arg) args;
    (* TODO: The 2nd component of the callee state needs to contain only the heap vars from the caller state which are reachable from: *)
    (* * Global program variables *)
    (* * The callee arguments *)
    [caller_state, (AllocaVars.empty (), snd caller_state)]

  let combine_env man (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) (f_ask:Queries.ask) : D.t =
    let (caller_stack_state, caller_heap_state) = man.local in
    let callee_stack_state = fst callee_local in
    let callee_heap_state = snd callee_local in
    (* Put all alloca()-vars together with all freed() vars in the caller's second component *)
    (* Don't change caller's first component => caller hasn't exited yet *)
    let callee_combined_state = HeapVars.join callee_stack_state callee_heap_state in
    (caller_stack_state, HeapVars.join caller_heap_state callee_combined_state)

  let combine_assign man (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) (f_ask: Queries.ask): D.t =
    Option.iter (fun x -> warn_lval_might_contain_freed "enter" man x) lval;
    man.local

  let special man (lval:lval option) (f:varinfo) (arglist:exp list) : D.t =
    let state = man.local in
    let desc = LibraryFunctions.find f in
    let is_arg_implicitly_derefed arg =
      let read_shallow_args = LibraryDesc.Accesses.find desc.accs { kind = Read; deep = false } arglist in
      let read_deep_args = LibraryDesc.Accesses.find desc.accs { kind = Read; deep = true } arglist in
      let write_shallow_args = LibraryDesc.Accesses.find desc.accs { kind = Write; deep = false } arglist in
      let write_deep_args = LibraryDesc.Accesses.find desc.accs { kind = Write; deep = true } arglist in
      List.mem arg read_shallow_args || List.mem arg read_deep_args || List.mem arg write_shallow_args || List.mem arg write_deep_args
    in
    Option.iter (fun x -> warn_lval_might_contain_freed ("special: " ^ f.vname) man x) lval;
    List.iter (fun arg -> warn_exp_might_contain_freed ~is_implicitly_derefed:(is_arg_implicitly_derefed arg) ~is_double_free:(match desc.special arglist with Free _ -> true | _ -> false) ("special: " ^ f.vname) man arg) arglist;
    match desc.special arglist with
    | Free ptr ->
      begin match man.ask (Queries.MayPointTo ptr) with
        | ad when not (Queries.AD.is_top ad) ->
          let pointed_to_heap_vars =
            Queries.AD.fold (fun addr state ->
                match addr with
                | Queries.AD.Addr.Addr (var,_) when man.ask (Queries.IsAllocVar var) && man.ask (Queries.IsHeapVar var) -> HeapVars.add var state
                | _ -> state
              ) ad (HeapVars.empty ())
          in
          (* Side-effect the tid that's freeing all the heap vars collected here *)
          side_effect_mem_free man pointed_to_heap_vars (get_current_threadid man) (get_joined_threads man);
          (* Add all heap vars, which ptr points to, to the state *)
          (fst state, HeapVars.join (snd state) pointed_to_heap_vars)
        | _ -> state
      end
    | Alloca _ ->
      (* Create fresh heap var for the alloca() call *)
      begin match man.ask (Queries.AllocVar {on_stack = true}) with
        | `Lifted v -> (AllocaVars.add v (fst state), snd state)
        | _ -> state
      end
    | _ -> state

  let startstate v = D.bot ()
  let exitstate v = D.top ()

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
