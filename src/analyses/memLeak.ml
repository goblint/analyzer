(** An analysis for the detection of memory leaks ([memLeak]). *)

open GoblintCil
open Analyses
open MessageCategory
open AnalysisStateUtil

module ToppedVarInfoSet = SetDomain.ToppedSet(CilType.Varinfo)(struct let topname = "All Heap Variables" end)

module Spec : Analyses.MCPSpec =
struct
  include Analyses.IdentitySpec

  let name () = "memLeak"

  module D = ToppedVarInfoSet
  module C = D
  module P = IdentityP (D)

  let context _ d = d

  (* HELPER FUNCTIONS *)
  let get_global_vars () =
    List.filter_map (function GVar (v, _, _) | GVarDecl (v, _) -> Some v | _ -> None) !Cilfacade.current_file.globals

  let get_global_struct_ptr_vars () =
    get_global_vars ()
    |> List.filter (fun v ->
        match unrollType v.vtype with
        | TPtr (TComp _, _)
        | TPtr ((TNamed ({ttype = TComp _; _}, _)), _) -> true
        | TComp (_, _)
        | (TNamed ({ttype = TComp _; _}, _)) -> false
        | _ -> false)

  let get_global_struct_non_ptr_vars () =
    get_global_vars ()
    |> List.filter (fun v ->
        match unrollType v.vtype with
        | TComp (_, _)
        | (TNamed ({ttype = TComp _; _}, _)) -> true
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
        if ctx.ask (Queries.IsHeapVar var) then eval_value_of_heap_var var
        else if not (ctx.ask (Queries.IsAllocVar var)) && isPointerType var.vtype then get_pts_of_non_heap_ptr_var var
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
                  List.fold_left (fun acc_addr addr ->
                      match addr with
                      | Queries.AD.Addr.Addr (v, _) -> (v :: get_reachable_mem_from_str_ptr_globals [v] ctx) @ acc_addr
                      | _ -> acc_addr
                    ) [] (Queries.AD.elements a)
                in reachable_from_addr_set @ acc_field
              | _ -> acc_field
            ) [] fields
        in
        reachable_from_fields @ acc_struct
      ) []

  let warn_for_multi_threaded ctx =
    if not (ctx.ask (Queries.MustBeSingleThreaded { since_start = true })) then (
      set_mem_safety_flag InvalidMemTrack;
      set_mem_safety_flag InvalidMemcleanup;
      M.warn ~category:(Behavior (Undefined MemoryLeak)) ~tags:[CWE 401] "Program isn't running in single-threaded mode. A memory leak might occur due to multi-threading"
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
        M.warn ~category:(Behavior (Undefined MemoryLeak)) ~tags:[CWE 401] "There is unreachable allocated heap memory at program exit. A memory leak might occur for the alloc vars %a\n" (Pretty.d_list ", " CilType.Varinfo.pretty) (D.elements allocated_and_unreachable_mem)
      );
      (* Check and warn if some of the allocated memory is not deallocated at program exit *)
      match assert_exp_imprecise, exp with
      | true, Some exp ->
        set_mem_safety_flag InvalidMemcleanup;
        M.warn ~category:(Behavior (Undefined MemoryLeak)) ~tags:[CWE 401] "Assert expression %a is unknown. Memory leak might possibly occur for heap variables: %a" d_exp exp D.pretty allocated_mem
      | _ ->
        set_mem_safety_flag InvalidMemcleanup;
        M.warn ~category:(Behavior (Undefined MemoryLeak)) ~tags:[CWE 401] "Memory leak detected for heap variables: %a" D.pretty allocated_mem

  (* TRANSFER FUNCTIONS *)
  let return ctx (exp:exp option) (f:fundec) : D.t =
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
      (* Warn about multi-threaded programs as soon as we encounter a dynamic memory allocation function *)
      warn_for_multi_threaded ctx;
      begin match ctx.ask (Queries.AllocVar {on_stack = false}) with
        | `Lifted var -> D.add var state
        | _ -> state
      end
    | Free ptr ->
      begin match ctx.ask (Queries.MayPointTo ptr) with
        | ad when not (Queries.AD.is_top ad) && Queries.AD.cardinal ad = 1 ->
          (* Note: Need to always set "ana.malloc.unique_address_count" to a value > 0 *)
          begin match Queries.AD.choose ad with
            | Queries.AD.Addr.Addr (v,_) when ctx.ask (Queries.IsAllocVar v) && ctx.ask (Queries.IsHeapVar v) && not @@ ctx.ask (Queries.IsMultiple v) -> D.remove v state (* Unique pointed to heap vars *)
            | _ -> state
          end
        | _ -> state
      end
    | Abort ->
      (* An "Abort" special function indicates program exit => need to check for memory leaks *)
      check_for_mem_leak ctx;
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
    | _ -> state

  let startstate v = D.bot ()
  let exitstate v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
