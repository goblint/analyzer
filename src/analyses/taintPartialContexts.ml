open Prelude.Ana
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "taintPartialContexts"
  module D = SetDomain.ToppedSet (Lval.CilLval) (struct let topname = "All" end)
  module C = D

  let rec resolve (offs : offset) : (CilType.Fieldinfo.t, Basetype.CilExp.t) Lval.offs =
    match offs with
    | NoOffset -> `NoOffset
    | Field (f_info, f_offs) -> `Field (f_info, (resolve f_offs))
    | Index (i_exp, i_offs) -> `Index (i_exp, (resolve i_offs))

  let taint_lval ctx (lval:lval) : D.t =
    let d = ctx.local in
    (match lval with 
    | (Var v, offs) -> D.add (v, resolve offs) d
    | (Mem e, _) -> D.union (ctx.ask (Queries.MayPointTo e)) d
    )

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    taint_lval ctx lval

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    let d = ctx.local in
    let locals = (f.sformals @ f.slocals) in
    let locals_noweak = List.filter (fun v_info -> not (ctx.ask (Queries.IsMultiple v_info))) locals in
    let d_return = if D.is_top d then d else D.filter (fun (v, _) -> not (List.mem v locals_noweak)) d in
    if M.tracing then M.trace "taintPC" "returning from %s: tainted vars: %a\n without locals: %a\n" f.svar.vname D.pretty d D.pretty d_return;
    d_return


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
        | (Var v, offs) -> D.add (v, resolve offs) accD
        | _ -> accD) (** Shallow; don't need to follow pointers *)
      | _ -> accD
    ) d shallow_addrs
    in
    let d = List.fold_left (fun accD addr -> 
      match addr with 
      | AddrOf x -> (
        match x with 
        | (Var v, offs) -> D.add (v, resolve offs) accD
        | (Mem e, _) -> D.union (ctx.ask (Queries.ReachableFrom e)) accD)
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
    | MayBeTainted -> (ctx.local : Queries.LS.t)
    | _ -> Queries.Result.top q

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)

module VS = SetDomain.ToppedSet(Basetype.Variables) (struct let topname = "All" end)

let conv_varset (lval_set : Spec.D.t) : VS.t = 
  if Spec.D.is_top lval_set then VS.top () else VS.of_list (List.map (fun (v, _) -> v) (Spec.D.elements lval_set))
