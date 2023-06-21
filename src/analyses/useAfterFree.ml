(** An analysis for the detection of use-after-free vulnerabilities. *)

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

  let rec warn_lval_might_contain_freed ?(is_double_free = false) (transfer_fn_name:string) (lval:lval) ctx =
    let state = ctx.local in
    let undefined_behavior = if is_double_free then Undefined DoubleFree else Undefined UseAfterFree in
    let cwe_number = if is_double_free then 415 else 416 in
    let rec offset_might_contain_freed offset =
      match offset with
      | NoOffset -> ()
      | Field (f, o) -> offset_might_contain_freed o
      | Index (e, o) -> warn_exp_might_contain_freed transfer_fn_name e ctx; offset_might_contain_freed o
    in
    let (lval_host, o) = lval in offset_might_contain_freed o; (* Check the lval's offset *)
    let lval_to_query =
      match lval_host with
      | Var _ -> Lval lval
      | Mem _ -> mkAddrOf lval (* Take the lval's address if its lhost is of the form *p, where p is a ptr *)
    in
    match ctx.ask (Queries.MayPointTo lval_to_query) with
    | a when not (Queries.LS.is_top a) && not (Queries.LS.mem (dummyFunDec.svar, `NoOffset) a) ->
      begin try
          let v, o = Queries.LS.choose a in
          if ctx.ask (Queries.IsHeapVar v) && D.mem v state then
            M.warn ~category:(Behavior undefined_behavior) ~tags:[CWE cwe_number] "lval (%s) in \"%s\" points to a maybe freed memory region" v.vname transfer_fn_name
        with Not_found -> ()
      end
    | _ -> ()

  and warn_exp_might_contain_freed ?(is_double_free = false) (transfer_fn_name:string) (exp:exp) ctx =
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
    | CastE (_, e) -> warn_exp_might_contain_freed ~is_double_free transfer_fn_name e ctx
    | BinOp (_, e1, e2, _) ->
      warn_exp_might_contain_freed ~is_double_free transfer_fn_name e1 ctx;
      warn_exp_might_contain_freed ~is_double_free transfer_fn_name e2 ctx
    | Question (e1, e2, e3, _) ->
      warn_exp_might_contain_freed ~is_double_free transfer_fn_name e1 ctx;
      warn_exp_might_contain_freed ~is_double_free transfer_fn_name e2 ctx;
      warn_exp_might_contain_freed ~is_double_free transfer_fn_name e3 ctx
    (* Lval cases (need [warn_lval_might_contain_freed] for them) *)
    | Lval lval
    | StartOf lval
    | AddrOf lval -> warn_lval_might_contain_freed ~is_double_free transfer_fn_name lval ctx


  (* TRANSFER FUNCTIONS *)

  let assign ctx (lval:lval) (rval:exp) : D.t =
    warn_lval_might_contain_freed "assign" lval ctx;
    warn_exp_might_contain_freed "assign" rval ctx;
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    warn_exp_might_contain_freed "branch" exp ctx;
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    Option.iter (fun x -> warn_exp_might_contain_freed "return" x ctx) exp;
    ctx.local

  let enter ctx (lval:lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let caller_state = ctx.local in
    Option.iter (fun x -> warn_lval_might_contain_freed "enter" x ctx) lval;
    List.iter (fun arg -> warn_exp_might_contain_freed "enter" arg ctx) args;
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
    Option.iter (fun x -> warn_lval_might_contain_freed ("special: " ^ f.vname) x ctx) lval;
    List.iter (fun arg -> warn_exp_might_contain_freed ~is_double_free:(f.vname = "free") ("special: " ^ f.vname) arg ctx) arglist;
    let desc = LibraryFunctions.find f in
    match desc.special arglist with
    | Free ptr ->
      begin match ctx.ask (Queries.MayPointTo ptr) with
        | a when not (Queries.LS.is_top a) && not (Queries.LS.mem (dummyFunDec.svar, `NoOffset) a) ->
          begin try
              let (v, _) = Queries.LS.choose a in
              if ctx.ask (Queries.IsHeapVar v) then
                D.add v state
              else state
            with Not_found -> state
          end
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