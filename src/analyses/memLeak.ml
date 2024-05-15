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

  let context ctx _ d = d

  let must_be_single_threaded ~since_start ctx =
    ctx.ask (Queries.MustBeSingleThreaded { since_start })

  let was_malloc_called ctx =
    ctx.global ()

  (* HELPER FUNCTIONS *)
  let get_global_vars () =
    List.filter_map (function GVar (v, _, _) | GVarDecl (v, _) -> Some v | _ -> None) !Cilfacade.current_file.globals

  let get_global_struct_ptr_vars () =
    get_global_vars ()
    |> List.filter (fun v ->
        match unrollType v.vtype with
        | TPtr (TComp (ci,_), _)
        | TPtr ((TNamed ({ttype = TComp (ci, _); _}, _)), _) -> ci.cstruct
        | TComp (_, _)
        | (TNamed ({ttype = TComp _; _}, _)) -> false
        | _ -> false)

  let get_global_struct_non_ptr_vars () =
    get_global_vars ()
    |> List.filter (fun v ->
        match unrollType v.vtype with
        | TComp (ci, _)
        | (TNamed ({ttype = TComp (ci,_); _}, _)) -> ci.cstruct
        | _ -> false)

  let get_reachable_mem_from_globals (global_vars:varinfo list) ctx =
    global_vars
    |> List.map (fun v -> Lval (Var v, NoOffset))
    |> List.filter_map (fun exp ->
        match ctx.ask (Queries.MayPointTo exp) with
        | a when not (Queries.AD.is_top a) && Queries.AD.cardinal a = 1  ->
          begin match List.hd @@ Queries.AD.elements a with
            | Queries.AD.Addr.Addr (v, _) when (ctx.ask (Queries.IsHeapVar v)) && not (ctx.ask (Queries.IsMultiple v)) -> Some v
            | _ -> None
          end
        | _ -> None)

  let rec get_reachable_mem_from_str_ptr_globals (global_struct_ptr_vars:varinfo list) ctx =
    let eval_value_of_heap_var heap_var =
      match ctx.ask (Queries.EvalValue (Lval (Var heap_var, NoOffset))) with
      | a when not (Queries.VD.is_top a) ->
        begin match a with
          | Struct s ->
            List.fold_left (fun acc f ->
                if isPointerType f.ftype then
                  begin match ValueDomain.Structs.get s f with
                    | Queries.VD.Address a when not (Queries.AD.is_top a) && Queries.AD.cardinal a = 1 ->
                      let reachable_from_addr_set =
                        Queries.AD.fold (fun addr acc ->
                            match addr with
                            | Queries.AD.Addr.Addr (v, _) -> (v :: get_reachable_mem_from_str_ptr_globals [v] ctx) @ acc
                            | _ -> acc
                          ) a []
                      in
                      reachable_from_addr_set @ acc
                    | _ -> acc
                  end
                else acc
              ) [] (ValueDomain.Structs.keys s)
          | _ -> []
        end
      | _ -> []
    in
    let get_pts_of_non_heap_ptr_var var =
      match ctx.ask (Queries.MayPointTo (Lval (Var var, NoOffset))) with
      | a when not (Queries.AD.is_top a) && Queries.AD.cardinal a = 1  ->
        begin match List.hd @@ Queries.AD.elements a with
          | Queries.AD.Addr.Addr (v, _) when (ctx.ask (Queries.IsHeapVar v)) && not (ctx.ask (Queries.IsMultiple v)) -> v :: (eval_value_of_heap_var v)
          | Queries.AD.Addr.Addr (v, _) when not (ctx.ask (Queries.IsAllocVar v)) && isPointerType v.vtype -> get_reachable_mem_from_str_ptr_globals [v] ctx
          | _ -> []
        end
      | _ -> []
    in
    global_struct_ptr_vars
    |> List.fold_left (fun acc var ->
        if ctx.ask (Queries.IsHeapVar var) then (eval_value_of_heap_var var) @ acc
        else if not (ctx.ask (Queries.IsAllocVar var)) && isPointerType var.vtype then (get_pts_of_non_heap_ptr_var var) @ acc
        else acc
      ) []

  let get_reachable_mem_from_str_non_ptr_globals (global_struct_non_ptr_vars:varinfo list) ctx =
    global_struct_non_ptr_vars
    (* Filter out global struct vars that don't have pointer fields *)
    |> List.filter_map (fun v ->
        match ctx.ask (Queries.EvalValue (Lval (Var v, NoOffset))) with
        | a when not (Queries.VD.is_top a) ->
          begin match a with
            | Queries.VD.Struct s ->
              let struct_fields = ValueDomain.Structs.keys s in
              let ptr_struct_fields = List.filter (fun f -> isPointerType f.ftype) struct_fields in
              if ptr_struct_fields = [] then None else Some (s, ptr_struct_fields)
            | _ -> None
          end
        | _ -> None
      )
    |> List.fold_left (fun acc_struct (s, fields) ->
        let reachable_from_fields =
          List.fold_left (fun acc_field field ->
              match ValueDomain.Structs.get s field with
              | Queries.VD.Address a ->
                let reachable_from_addr_set =
                  Queries.AD.fold (fun addr acc_addr ->
                      match addr with
                      | Queries.AD.Addr.Addr (v, _) ->
                        let reachable_from_v = Queries.AD.of_list (List.map (fun v -> Queries.AD.Addr.Addr (v, `NoOffset)) (get_reachable_mem_from_str_ptr_globals [v] ctx)) in
                        Queries.AD.join (Queries.AD.add addr reachable_from_v) acc_addr
                      | _ -> acc_addr
                    ) a (Queries.AD.empty ())
                in (Queries.AD.to_var_may reachable_from_addr_set) @ acc_field
              | _ -> acc_field
            ) [] fields
        in
        reachable_from_fields @ acc_struct
      ) []

  let warn_for_multi_threaded_due_to_abort ctx =
    let malloc_called = was_malloc_called ctx in
    if not (must_be_single_threaded ctx ~since_start:true) && malloc_called then (
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
    let allocated_mem = ctx.local in
    if not (D.is_empty allocated_mem) then
      let reachable_mem_from_non_struct_globals = D.of_list (get_reachable_mem_from_globals (get_global_vars ()) ctx) in
      let reachable_mem_from_struct_ptr_globals = D.of_list (get_reachable_mem_from_str_ptr_globals (get_global_struct_ptr_vars ()) ctx) in
      let reachable_mem_from_struct_non_ptr_globals = D.of_list (get_reachable_mem_from_str_non_ptr_globals (get_global_struct_non_ptr_vars ()) ctx) in
      let reachable_mem_from_struct_globals = D.join reachable_mem_from_struct_ptr_globals reachable_mem_from_struct_non_ptr_globals in
      let reachable_mem = D.join reachable_mem_from_non_struct_globals reachable_mem_from_struct_globals in
      (* Check and warn if there's unreachable allocated memory at program exit *)
      let allocated_and_unreachable_mem = D.diff allocated_mem reachable_mem in
      if not (D.is_empty allocated_and_unreachable_mem) then (
        set_mem_safety_flag InvalidMemTrack;
        M.warn ~category:(Behavior (Undefined MemoryLeak)) ~tags:[CWE 401] "There is unreachable allocated heap memory at program exit. A memory leak might occur for the alloc vars %a" (Pretty.d_list ", " CilType.Varinfo.pretty) (D.elements allocated_and_unreachable_mem)
      );
      (* Check and warn if some of the allocated memory is not deallocated at program exit *)
      match assert_exp_imprecise, exp with
      | true, Some exp ->
        set_mem_safety_flag InvalidMemcleanup;
        M.warn ~category:(Behavior (Undefined MemoryLeak)) ~tags:[CWE 401] "Assert expression %a is unknown. Memory leak might possibly occur for heap variables: %a" d_exp exp D.pretty allocated_mem
      | _ ->
        set_mem_safety_flag InvalidMemcleanup;
        M.warn ~category:(Behavior (Undefined MemoryLeak)) ~tags:[CWE 401] "Memory leak detected for heap variables"

  (* TRANSFER FUNCTIONS *)
  let return ctx (exp:exp option) (f:fundec) : D.t =
    (* Check for a valid-memcleanup and memtrack violation in a multi-threaded setting *)
    (* The check for multi-threadedness is to ensure that valid-memtrack and valid-memclenaup are treated separately for single-threaded programs *)
    if (ctx.ask (Queries.MayBeThreadReturn) &&  not (must_be_single_threaded ctx ~since_start:true)) then (
      warn_for_thread_return_or_exit ctx true
    );
    (* Returning from "main" is one possible program exit => need to check for memory leaks *)
    if f.svar.vname = "main" then (
      check_for_mem_leak ctx;
      if not (must_be_single_threaded ctx ~since_start:false) && was_malloc_called ctx then begin
        set_mem_safety_flag InvalidMemTrack;
        set_mem_safety_flag InvalidMemcleanup;
        M.warn ~category:(Behavior (Undefined MemoryLeak)) ~tags:[CWE 401] "Possible memory leak: Memory was allocated in a multithreaded program, but not all threads are joined."
      end
    );
    ctx.local

  let special ctx (lval:lval option) (f:varinfo) (arglist:exp list) : D.t =
    let state = ctx.local in
    let desc = LibraryFunctions.find f in
    match desc.special arglist with
    | Malloc _
    | Calloc _
    | Realloc _ ->
      ctx.sideg () true;
      begin match ctx.ask (Queries.AllocVar {on_stack = false}) with
        | `Lifted var ->
          ToppedVarInfoSet.add var state
        | _ -> state
      end
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
      begin match ctx.ask (Queries.EvalInt exp) with
        | a when Queries.ID.is_bot a -> M.warn ~category:Assert "assert expression %a is bottom" d_exp exp
        | a ->
          begin match Queries.ID.to_bool a with
            | Some true -> ()
            | Some false ->
              (* If we know for sure that the expression in "assert" is false => need to check for memory leaks *)
              warn_for_multi_threaded_due_to_abort ctx;
              check_for_mem_leak ctx
            | None ->
              warn_for_multi_threaded_due_to_abort ctx;
              check_for_mem_leak ctx ~assert_exp_imprecise:true ~exp:(Some exp)
          end
      end;
      state
    | ThreadExit _ ->
      begin match ctx.ask (Queries.CurrentThreadId) with
        | `Lifted tid ->
          warn_for_thread_return_or_exit ctx false
        | _ -> ()
      end;
      state
    | _ -> state

  let startcontext () = D.top ()
  let startstate v = D.bot ()
  let exitstate v = D.top ()

  let threadenter ctx ~multiple lval f args = [D.bot ()]
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
