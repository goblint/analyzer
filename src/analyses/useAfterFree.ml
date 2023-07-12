(** An analysis for the detection of use-after-free vulnerabilities ([useAfterFree]). *)

open GoblintCil
open Analyses
open MessageCategory

module ToppedVarInfoSet = SetDomain.ToppedSet(CilType.Varinfo)(struct let topname = "All Heap Variables" end)

module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let name () = "useAfterFree"

  module D = ToppedVarInfoSet
  module C = Lattice.Unit

  (** TODO: Try out later in benchmarks to see how we perform with and without context-sensititivty *)
  let context _ _ = ()


  (* HELPER FUNCTIONS *)

  let warn_for_multi_threaded ctx behavior cwe_number =
    if not (ctx.ask (Queries.MustBeSingleThreaded { since_start = true })) then
      M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Program isn't running in single-threaded mode. Use-After-Free might occur due to multi-threading"

  let rec warn_lval_might_contain_freed ?(is_double_free = false) (transfer_fn_name:string) ctx (lval:lval) =
    let state = ctx.local in
    let undefined_behavior = if is_double_free then Undefined DoubleFree else Undefined UseAfterFree in
    let cwe_number = if is_double_free then 415 else 416 in
    warn_for_multi_threaded ctx undefined_behavior cwe_number; (* Simple solution to warn when multi-threaded *)
    let rec offset_might_contain_freed offset =
      match offset with
      | NoOffset -> ()
      | Field (f, o) -> offset_might_contain_freed o
      | Index (e, o) -> warn_exp_might_contain_freed transfer_fn_name ctx e; offset_might_contain_freed o
    in
    let (lval_host, o) = lval in offset_might_contain_freed o; (* Check the lval's offset *)
    let lval_to_query =
      match lval_host with
      | Var _ -> Lval lval
      | Mem _ -> mkAddrOf lval (* Take the lval's address if its lhost is of the form *p, where p is a ptr *)
    in
    match ctx.ask (Queries.MayPointTo lval_to_query) with
    | a when not (Queries.LS.is_top a) && not (Queries.LS.mem (dummyFunDec.svar, `NoOffset) a) ->
      let warn_for_heap_var var =
        if D.mem var state then
          M.warn ~category:(Behavior undefined_behavior) ~tags:[CWE cwe_number] "lval (%s) in \"%s\" points to a maybe freed memory region" var.vname transfer_fn_name
      in
      let pointed_to_heap_vars =
        Queries.LS.elements a
        |> List.map fst
        |> List.filter (fun var -> ctx.ask (Queries.IsHeapVar var))
      in
      List.iter warn_for_heap_var pointed_to_heap_vars (* Warn for all heap vars that the lval possibly points to *)
    | _ -> ()

  and warn_exp_might_contain_freed ?(is_double_free = false) (transfer_fn_name:string) ctx (exp:exp) =
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
    | CastE (_, e) -> warn_exp_might_contain_freed ~is_double_free transfer_fn_name ctx e
    | BinOp (_, e1, e2, _) ->
      warn_exp_might_contain_freed ~is_double_free transfer_fn_name ctx e1;
      warn_exp_might_contain_freed ~is_double_free transfer_fn_name ctx e2
    | Question (e1, e2, e3, _) ->
      warn_exp_might_contain_freed ~is_double_free transfer_fn_name ctx e1;
      warn_exp_might_contain_freed ~is_double_free transfer_fn_name ctx e2;
      warn_exp_might_contain_freed ~is_double_free transfer_fn_name ctx e3
    (* Lval cases (need [warn_lval_might_contain_freed] for them) *)
    | Lval lval
    | StartOf lval
    | AddrOf lval -> warn_lval_might_contain_freed ~is_double_free transfer_fn_name ctx lval


  (* TRANSFER FUNCTIONS *)

  let assign ctx (lval:lval) (rval:exp) : D.t =
    warn_lval_might_contain_freed "assign" ctx lval;
    warn_exp_might_contain_freed "assign" ctx rval;
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    warn_exp_might_contain_freed "branch" ctx exp;
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    Option.iter (fun x -> warn_exp_might_contain_freed "return" ctx x) exp;
    ctx.local

  let enter ctx (lval:lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let caller_state = ctx.local in
    List.iter (fun arg -> warn_exp_might_contain_freed "enter" ctx arg) args;
    if D.is_empty caller_state then
      [caller_state, caller_state]
    else (
      let reachable_from_args = List.fold_left (fun acc arg -> Queries.LS.join acc (ctx.ask (ReachableFrom arg))) (Queries.LS.empty ()) args in
      if Queries.LS.is_top reachable_from_args || D.is_top caller_state then
        [caller_state, caller_state]
      else
        let reachable_vars = List.map fst (Queries.LS.elements reachable_from_args) in
        let callee_state = D.filter (fun var -> List.mem var reachable_vars) caller_state in
        [caller_state, callee_state]
    )

  let combine_env ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) (f_ask:Queries.ask) : D.t =
    let caller_state = ctx.local in
    D.join caller_state callee_local

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) (f_ask: Queries.ask): D.t =
    Option.iter (fun x -> warn_lval_might_contain_freed "enter" ctx x) lval;
    ctx.local

  let special ctx (lval:lval option) (f:varinfo) (arglist:exp list) : D.t =
    let state = ctx.local in
    Option.iter (fun x -> warn_lval_might_contain_freed ("special: " ^ f.vname) ctx x) lval;
    List.iter (fun arg -> warn_exp_might_contain_freed ~is_double_free:(f.vname = "free") ("special: " ^ f.vname) ctx arg) arglist;
    let desc = LibraryFunctions.find f in
    match desc.special arglist with
    | Free ptr ->
      begin match ctx.ask (Queries.MayPointTo ptr) with
        | a when not (Queries.LS.is_top a) && not (Queries.LS.mem (dummyFunDec.svar, `NoOffset) a) ->
          let pointed_to_heap_vars =
            Queries.LS.elements a
            |> List.map fst
            |> List.filter (fun var -> ctx.ask (Queries.IsHeapVar var))
          in
          D.join state (D.of_list pointed_to_heap_vars) (* Add all heap vars, which ptr points to, to the state *)
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