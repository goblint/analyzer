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

  (* Assumed by [unknown_call_frees] at a call of a function with neither a
     definition nor a library specification, when an earlier such call was
     given the address of a local of a frame other than the calling one. *)
  let unknown_frees_through_other_frames =
    "use-after-free.unknown-frees-through-other-frames: a function with neither a definition nor a library specification frees no heap object that is reachable only through a local of another frame whose address was given to an earlier such call or stored by the program at an address the pointer analysis does not know"

  (* Assumed by [unknown_call_frees] at a call of a function with neither a
     definition nor a library specification that, as far as the pointer
     analysis can tell, reaches no address it does not know. *)
  let unknown_functions_free_only_reachable_objects =
    "use-after-free.unknown-functions-free-only-reachable-objects: a function with neither a definition nor a library specification frees no heap object that the program reaches only through a pointer converted to an integer, or through a pointer that library code keeps where the program cannot name it (as pthread_setspecific, hsearch and putenv keep their arguments, or in a global a header declares and the program never names); every other heap object it can reach is counted as possibly freed"

  (* The expressions whose [Queries.ReachableFrom] is what a function given
     [e] as argument, or a store of [e], can reach: [e] as written and [e]
     with its casts stripped, since a cast to an integer type hides the
     pointer from the pointer analysis; for a value of struct or union type,
     its address instead. *)
  let reach_roots e =
    match Cil.stripCasts e with
    | Lval lval when (match Cil.unrollType (Cilfacade.typeOfLval lval) with TComp _ -> true | _ -> false) -> [AddrOf lval]
    | e' when CilType.Exp.equal e e' -> [e]
    | e' -> [e; e']

  module VarSet = Set.Make (CilType.Varinfo)

  (* The global variables the program's own code names, in a function body
     or in a global's initializer, for [unknown_call_frees]. *)
  let named_globals =
    let cache = ref None in
    fun () ->
      let file = !Cilfacade.current_file in
      match !cache with
      | Some (f, vars) when f == file -> vars
      | _ ->
        let vars = ref VarSet.empty in
        let visitor = object
          inherit nopCilVisitor
          method! vvrbl v =
            if v.vglob then vars := VarSet.add v !vars;
            SkipChildren
        end
        in
        Cil.iterGlobals file (function
            | GFun (fd, _) -> ignore (Cil.visitCilFunction visitor fd)
            | GVar (v, {init = Some i}, _) -> ignore (Cil.visitCilInit visitor v NoOffset i)
            | _ -> ()
          );
        cache := Some (file, !vars);
        !vars

  (* What a call of a function with neither a definition nor a library
     specification, given [args], may free, on a path whose [UnknownEscapes]
     are [escapes]: [(heap_vars, unknown, escapes')], where [heap_vars] are
     the heap variables of which it may free an object, [unknown] is true if
     some address it can reach is not known to the pointer analysis, and
     [escapes'] are [escapes] with the variables it can reach added. Such a
     function may free any heap object it can reach: one reachable from an
     argument, from a global, or from a variable of [escapes], whose address
     an earlier such call may have kept. A [static] global counts too: the
     function may call a function of the program, such as a callback it was
     given, that reads it. Once the
     program may have more than one thread, [escapes] also takes in what a
     call on any path of any thread could reach, which
     [OrderKey.UnknownEscapes] collects. An argument of struct or union type
     is followed from its address, which also counts the object holding it
     when that is a heap object. A global the program declares but never
     names, such as one a C library header declares, is not followed. Where
     [unknown] is false, [unknown_functions_free_only_reachable_objects] is
     assumed: what the pointer analysis does not follow, such as a pointer
     converted to an integer, is not counted. *)
  let unknown_call_frees man args escapes =
    let named = named_globals () in
    let globals =
      foldGlobals !Cilfacade.current_file (fun roots -> function
          | GVar (v, _, _) when not (Cil.isFunctionType v.vtype) ->
            AddrOf (Var v, NoOffset) :: roots
          | GVarDecl (v, _) when not (Cil.isFunctionType v.vtype)
                              && (VarSet.mem v named || StandardStreams.of_extern_variable v <> None) ->
            AddrOf (Var v, NoOffset) :: roots
          | _ -> roots
        ) []
    in
    let escapes =
      if ThreadFlag.has_ever_been_multi (Analyses.ask_of_man man) then
        let _, everywhere = man.global (`Right false) in
        UnknownEscapes.join escapes everywhere
      else escapes
    in
    let escaped_roots =
      if UnknownEscapes.is_top escapes then []
      else UnknownEscapes.fold (fun v roots -> AddrOf (Var v, NoOffset) :: roots) escapes []
    in
    (* The pointer analysis has no value here for a local of another frame,
       so what it reaches is not known: [unknown_frees_through_other_frames]
       is assumed. *)
    if not (UnknownEscapes.is_top escapes) then begin
      let fd = Node.find_fundec man.node in
      let own v =
        let v = v in
        List.exists (CilType.Varinfo.equal v) fd.sformals || List.exists (CilType.Varinfo.equal v) fd.slocals
      in
      let others = UnknownEscapes.filter (fun v -> not v.vglob && not (own v) && not (man.ask (Queries.IsAllocVar v))) escapes in
      if not (UnknownEscapes.is_empty others) then
        Assumptions.add ~final:("Used assumption " ^ unknown_frees_through_other_frames) "%s; relied on here for the escaped locals %s"
          unknown_frees_through_other_frames
          (String.concat ", " (List.map (fun (v: varinfo) -> v.vname) (UnknownEscapes.elements others)))
    end;
    (* A standard stream's variable, which the program does not assign, and
       its object hold the unknown pointer only for what library code, or a
       function such as this one, stored there: an object such code can
       reach anyway, or a buffer [setvbuf] was given at an unknown address,
       where [StandardStreams.buffer_lifetime] is assumed. So the unknown
       pointer they hold is not counted, and the named objects they point to
       are followed as other roots. *)
    let stream_var v =
      StandardStreams.is_object v
      || (match StandardStreams.of_extern_variable v with Some s -> not (StandardStreams.assigned s) | None -> false)
    in
    let rec reach (reached, unknown) e =
      match e with
      | AddrOf (Var v, NoOffset) when stream_var v ->
        if UnknownEscapes.mem v reached then reached, unknown
        else
          Queries.AD.fold (fun addr acc -> match addr with
              | Queries.AD.Addr.Addr (w, _) -> reach acc (AddrOf (Var w, NoOffset))
              | _ -> acc
            ) (man.ask (Queries.MayPointTo (Lval (Var v, NoOffset)))) (UnknownEscapes.add v reached, unknown)
      | _ ->
        let ad = man.ask (Queries.ReachableFrom e) in
        Queries.AD.fold (fun addr reached -> match addr with
            | Queries.AD.Addr.Addr (v, _) -> UnknownEscapes.add v reached
            | _ -> reached
          ) ad reached,
        unknown || Queries.AD.is_top ad
    in
    let reached, unknown =
      List.fold_left reach (UnknownEscapes.empty (), UnknownEscapes.is_top escapes) (List.concat_map reach_roots args @ globals @ escaped_roots)
    in
    if not unknown then
      Assumptions.add ~final:("Used assumption " ^ unknown_functions_free_only_reachable_objects) "%s"
        unknown_functions_free_only_reachable_objects;
    man.sideg (`Right false) (ThreadIdToJoinedThreadsMap.bot (), reached);
    let heap_vars = UnknownEscapes.fold (fun v heap_vars ->
        if man.ask (Queries.IsAllocVar v) && man.ask (Queries.IsHeapVar v) then InvalidVars.add v heap_vars else heap_vars
      ) reached (InvalidVars.empty ())
    in
    heap_vars, unknown, UnknownEscapes.join escapes reached

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
    | Unknown when not (LibraryFunctions.is_specified f) ->
      let stack, (invalid, unknown, escapes) = state in
      let vars, reaches_unknown, escapes = unknown_call_frees man arglist escapes in
      publish_invalidation man vars (get_current_threadid man) (get_joined_threads man);
      if reaches_unknown then record_unknown_invalidation man;
      stack, (InvalidVars.join invalid vars, unknown || reaches_unknown, escapes)
    | Alloca _ ->
      (* Create fresh heap var for the alloca() call *)
      begin match man.ask (Queries.AllocVar Stack) with
        | `Lifted v -> (AllocaVars.add v (fst state), snd state)
        | _ -> state
      end
    | _ -> state

  let assign man lval rval =
    match lval with
    | Mem _, _ when Queries.AD.may_be_unknown (man.ask (Queries.MayPointTo (AddrOf lval))) ->
      let reached = List.fold_left (fun vars root ->
          Queries.AD.fold (fun addr vars -> match addr with
              | Queries.AD.Addr.Addr (v, _) -> UnknownEscapes.add v vars
              | _ -> vars
            ) (man.ask (Queries.ReachableFrom root)) vars
        ) (UnknownEscapes.empty ()) (reach_roots rval) in
      man.sideg (`Right false) (ThreadIdToJoinedThreadsMap.bot (), reached);
      let stack, (invalid, unknown, escapes) = man.local in
      stack, (invalid, unknown, UnknownEscapes.join escapes reached)
    | _ -> man.local

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
