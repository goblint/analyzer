(** An analysis for tracking pointers that point to uninitialized memory. *)

open GoblintCil
open Analyses
open MessageCategory

module HeapVarInfoSet = SetDomain.Make(CilType.Varinfo)

module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let name () = "uninitMem"

  module D = HeapVarInfoSet
  module C = Lattice.Unit

  (* TODO: Handle later *)
  let context _ _ = ()

  (* HELPER FUNCTIONS *)

  let exp_points_to_bot_blob (exp:exp) ctx =
    match ctx.ask (Queries.EvalValue exp) with
    | a when not (Queries.VD.is_top a) ->
      begin match a with
        | `Blob (value, _, _) -> if ValueDomain.Compound.is_bot value then true else false
        | _ -> false
      end
    | _ -> false

  let rec warn_lval_might_contain_uninitd_mem (lval:lval) ctx =
    let state = ctx.local in
    let undefined_behavior = Undefined UninitializedMemoryAccess in
    let cwe_number = 824 in
    let rec offset_might_contain_uninitd_mem offset =
      match offset with
      | NoOffset -> ()
      | Field (f, o) -> offset_might_contain_uninitd_mem o
      | Index (e, o) -> warn_exp_might_contain_uninitd_mem e ctx; offset_might_contain_uninitd_mem o
    in
    let (lval_host, o) = lval in offset_might_contain_uninitd_mem o;
    let lval_to_query =
      match lval_host with
      | Var _ -> Lval lval
      | Mem _ -> mkAddrOf lval
    in
    match ctx.ask (Queries.MayPointTo lval_to_query) with
    | a when not (Queries.LS.is_top a) && not (Queries.LS.mem (dummyFunDec.svar, `NoOffset) a) ->
      begin try
          let v, o = Queries.LS.choose a in
          if ctx.ask (Queries.IsHeapVar v) && D.mem v state then
            M.warn ~category:(Behavior undefined_behavior) ~tags:[CWE cwe_number] "lval (%s) points to uninitialized memory" v.vname
        with Not_found -> ()
      end
    | _ -> ()

  and warn_exp_might_contain_uninitd_mem (exp:exp) ctx =
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
    | CastE (_, e) -> warn_exp_might_contain_uninitd_mem e ctx
    | BinOp (_, e1, e2, _) ->
      warn_exp_might_contain_uninitd_mem e1 ctx;
      warn_exp_might_contain_uninitd_mem e2 ctx
    | Question (e1, e2, e3, _) ->
      warn_exp_might_contain_uninitd_mem e1 ctx;
      warn_exp_might_contain_uninitd_mem e2 ctx;
      warn_exp_might_contain_uninitd_mem e3 ctx
    (* Lval cases (need [warn_lval_might_contain_uninitd_mem] for them) *)
    | Lval lval
    | StartOf lval
    | AddrOf lval -> warn_lval_might_contain_uninitd_mem lval ctx


  (* TRANSFER FUNCTIONS *)

  let assign ctx (lval:lval) (rval:exp) : D.t =
    let state = ctx.local in
    warn_lval_might_contain_uninitd_mem lval ctx;
    warn_exp_might_contain_uninitd_mem rval ctx;
    if not (exp_points_to_bot_blob rval ctx) then
      match ctx.ask (Queries.MayPointTo (mkAddrOf lval)) with
      | a when not (Queries.LS.is_top a) && not (Queries.LS.mem (dummyFunDec.svar, `NoOffset) a) ->
        let v, o = Queries.LS.choose a in
        if ctx.ask (Queries.IsHeapVar v) && D.mem v state then
          (*
            * Intuition:
             * If a rval which doesn't point to uninitd. mem. gets assigned to an lval that points to uninitd. mem.,
             * then remove the heap var that's pointed to by the lval
          *)
          D.remove v state
        else state
      | _ -> state
    else state

  let branch ctx (exp:exp) (tv:bool) : D.t =
    warn_exp_might_contain_uninitd_mem exp ctx;
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    Option.iter (fun x -> warn_exp_might_contain_uninitd_mem x ctx) exp;
    ctx.local

  let enter ctx (lval:lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let caller_state = ctx.local in
    Option.iter (fun x -> warn_lval_might_contain_uninitd_mem x ctx) lval;
    List.iter (fun arg -> warn_exp_might_contain_uninitd_mem arg ctx) args;
    if D.is_empty caller_state then
      [caller_state, caller_state]
    else (
      let reachable_from_args = List.fold_left (fun acc arg -> Queries.LS.join acc (ctx.ask (ReachableFrom arg))) (Queries.LS.empty ()) args in
      if Queries.LS.is_top reachable_from_args || D.is_top caller_state then
        [caller_state, caller_state]
      else
        let reachable_vars =
          Queries.LS.elements reachable_from_args
          |> List.map fst
          |> List.filter (fun var -> ctx.ask (Queries.IsHeapVar var))
          |> D.of_list
        in
        let callee_state = D.inter caller_state reachable_vars in
        [caller_state, callee_state]
    )

  let combine_env ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) (f_ask:Queries.ask) : D.t =
    let caller_state = ctx.local in
    D.join caller_state callee_local

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) (f_ask: Queries.ask): D.t =
    ctx.local

  let special ctx (lval:lval option) (f:varinfo) (arglist:exp list) : D.t =
    let state = ctx.local in
    Option.iter (fun x -> warn_lval_might_contain_uninitd_mem x ctx) lval;
    List.iter (fun arg -> warn_exp_might_contain_uninitd_mem arg ctx) arglist;
    let desc = LibraryFunctions.find f in
    match desc.special arglist, lval with
    | Malloc _, Some lval
    | Realloc _, Some lval ->
      (*
        * Intuition: heap vars created by malloc() and realloc() point to uninitd. mem. (cf. `man 3 malloc`)
        * Hence, need to add such heap vars to state
      *)
      begin match ctx.ask (Queries.HeapVar) with
        | `Lifted var -> D.add var state
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