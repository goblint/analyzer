open Prelude.Ana
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "taintPartialContexts"
  module D = SetDomain.ToppedSet (Basetype.Variables) (struct let topname = "All" end)
  module C = D

  let is_global (v:varinfo) : bool = (** Unused*)
    foldGlobals !Cilfacade.current_file (fun acc global -> 
      match global with
      | GVar (gv, _, _) when not (BaseUtil.is_static gv) -> acc || v == gv
      | _ -> acc
      ) false

  let taint_lval ctx (lval:lval) : D.t =
    let d = ctx.local in
    match lval with
    | (Var v, _) -> D.add v d
    | (Mem e, _) -> 
      let targets = Queries.LS.elements (ctx.ask (Queries.MayPointTo e)) in
      List.fold_left (fun accD targ -> 
        match (Lval.CilLval.to_lval targ) with
        | (Var v, _) -> D.add v accD
        | _ -> accD) d targets

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    taint_lval ctx lval

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    let locals = D.of_list (f.sformals @ f.slocals) in
    let locals_noweak = D.filter (fun v -> not (ctx.ask (Queries.IsMultiple v))) locals in
    if M.tracing then M.trace "taintPC" "returning from %s: tainted vars: %a\n - locals: %a\n - locals_noweak: %a\n" f.svar.vname D.pretty ctx.local D.pretty locals D.pretty locals_noweak;
    D.diff ctx.local locals_noweak

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [ctx.local, (D.bot ())] (** Entering a function, all globals count as untouched *)

  let combine ctx (lvalOpt:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    if M.tracing then M.trace "taintPC" "combine for %s in TaintPC: tainted: in function: %a before call: %a\n" f.svar.vname D.pretty au D.pretty ctx.local;
    let d =
      match lvalOpt with
      | Some lv -> taint_lval ctx lv
      | None -> ctx.local 
    in
    D.union d au

  let special ctx (lvalOpt: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let d =
      match lvalOpt with
      | Some lv -> taint_lval ctx lv
      | None -> ctx.local 
    in
    let desc = LibraryFunctions.find f in
    let shallow_addrs = LibraryDesc.Accesses.find desc.accs { kind = Write; deep = false } arglist in
    let deep_addrs = LibraryDesc.Accesses.find desc.accs { kind = Write; deep = true } arglist in
    let deep_addrs =
      if List.mem LibraryDesc.InvalidateGlobals desc.attrs then (
        foldGlobals !Cilfacade.current_file (fun acc global ->
            match global with
            | GVar (vi, _, _) when not (BaseUtil.is_static vi) ->
              mkAddrOf (Var vi, NoOffset) :: acc
            (* TODO: what about GVarDecl? (see "base.ml -> special_unknown_invalidate")*)
            | _ -> acc
          ) deep_addrs
      )
      else
        deep_addrs
    in
    let d = List.fold_left (fun accD addr -> 
      match addr with 
      | AddrOf x -> (
        match x with 
        | (Var v, _) -> D.add v accD
        | _ -> accD) (** Shallow; don't need to follow pointers *)
      | _ -> accD
    ) d shallow_addrs
    in
    let d = List.fold_left (fun accD addr -> 
      match addr with 
      | AddrOf x -> (
        match x with 
        | (Var v, _) -> D.add v accD
        | (Mem e, _) ->
          let reachables = Queries.LS.elements (ctx.ask (Queries.ReachableFrom e)) in
          List.fold_left (fun accD' targ -> 
            match (Lval.CilLval.to_lval targ) with
            | (Var v, _) -> D.add v accD'
            | _ -> accD') accD reachables)
      | _ -> accD
    ) d deep_addrs
    in
    d

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()

  let query ctx (type a) (q: a Queries.t) : a Queries.result =
    match q with
    | Tainted -> (ctx.local : Queries.TaintS.t)
    | _ -> Queries.Result.top q

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
