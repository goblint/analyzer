(** Taint analysis of variables modified in a function ([taintPartialContexts]). *)

(* TaintPartialContexts: Set of Lvalues, which are tainted at a specific Node. *)
(* An Lvalue is tainted, if its Rvalue might have been altered in the context of the current function,
   implying that the Rvalue of any Lvalue not in the set has definitely not been changed within the current context. *)
open GoblintCil
open Analyses

module AD = ValueDomain.AD

module Spec =
struct
  include Analyses.IdentityUnitContextsSpec

  let name () = "taintPartialContexts"
  module D = AD

  (* Add Lval or any Lval which it may point to to the set *)
  let taint_lval man (lval:lval) : D.t =
    D.union (man.ask (Queries.MayPointTo (AddrOf lval))) man.local

  (* transfer functions *)
  let assign man (lval:lval) (rval:exp) : D.t =
    taint_lval man lval

  let return man (exp:exp option) (f:fundec) : D.t =
    (* remove locals, except ones which need to be weakly updated*)
    let d = man.local in
    let d_return =
      if D.is_top d then
        d
      else
        let locals = f.sformals @ f.slocals in
        D.filter (function
            | AD.Addr.Addr (v,_) -> not (List.exists (fun local -> CilType.Varinfo.equal v local && not (man.ask (Queries.IsMultiple local))) locals)
            | _ -> false
          ) d
    in
    if M.tracing then M.trace "taintPC" "returning from %s: tainted vars: %a\n without locals: %a" f.svar.vname D.pretty d D.pretty d_return;
    d_return


  let enter man (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    (* Entering a function, all globals count as untainted *)
    [man.local, (D.bot ())]

  let combine_env man lval fexp f args fc au f_ask =
    if M.tracing then M.trace "taintPC" "combine for %s in TaintPC: tainted: in function: %a before call: %a" f.svar.vname D.pretty au D.pretty man.local;
    D.union man.local au

  let combine_assign man (lvalOpt:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    match lvalOpt with
    | Some lv -> taint_lval man lv
    | None -> man.local

  let special man (lvalOpt: lval option) (f:varinfo) (arglist:exp list) : D.t =
    (* perform shallow and deep invalidate according to Library descriptors *)
    let d =
      match lvalOpt with
      | Some lv -> taint_lval man lv
      | None -> man.local
    in
    let desc = LibraryFunctions.find f in
    let shallow_addrs = LibraryDesc.Accesses.find desc.accs { kind = Write; deep = false } arglist in
    let deep_addrs = LibraryDesc.Accesses.find desc.accs { kind = Write; deep = true } arglist in
    let deep_addrs =
      if List.mem LibraryDesc.InvalidateGlobals desc.attrs then (
        foldGlobals !Cilfacade.current_file (fun acc global ->
            match global with
            | GVar (vi, _, _) when not (BaseUtil.is_static vi) ->
              mkAddrOf (Cil.var vi) :: acc
            (* TODO: what about GVarDecl? (see "base.ml -> special_unknown_invalidate")*)
            | _ -> acc
          ) deep_addrs
      )
      else
        deep_addrs
    in
    (* TODO: should one handle ad with unknown pointers separately like in (all) other analyses? *)
    let d = List.fold_left (fun accD addr -> D.union accD (man.ask (Queries.MayPointTo addr))) d shallow_addrs
    in
    let d = List.fold_left (fun accD addr -> D.union accD (man.ask (Queries.ReachableFrom addr))) d deep_addrs
    in
    d

  let startstate v = D.bot ()
  let threadenter man ~multiple lval f args =
    [D.bot ()]
  let threadspawn man ~multiple lval f args fman =
    match lval with
    | Some lv -> taint_lval man lv
    | None -> man.local
  let exitstate  v = D.top ()

  let query man (type a) (q: a Queries.t) : a Queries.result =
    match q with
    | MayBeTainted -> (man.local : Queries.AD.t)
    | _ -> Queries.Result.top q

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)

module VS = SetDomain.ToppedSet(Basetype.Variables) (struct let topname = "All" end)

(* Convert Lval set to (less precise) Varinfo set. *)
let conv_varset (addr_set : Spec.D.t) : VS.t =
  if Spec.D.is_top addr_set then
    VS.top ()
  else
    VS.of_list (Spec.D.to_var_may addr_set)
