(** Analysis that tracks which variables hold the results of calls to math library functions ([tmpSpecial]). *)

(** For each equivalence a set of expressions is tracked, that contains the arguments of the corresponding call as well as the Lval it is assigned to, so an equivalence can be removed if one of these expressions may be changed. *)

module VarEq = VarEq.Spec

open GoblintCil
open Analyses

module Spec =
struct
  include Analyses.IdentityUnitContextsSpec

  let name () = "tmpSpecial"
  module ML = LibraryDesc.MathLifted
  module Deps = SetDomain.Reverse (SetDomain.ToppedSet (CilType.Exp) (struct let topname = "All" end))
  module MLDeps = Lattice.Prod (ML) (Deps)
  module D = MapDomain.MapBot (Mval.Exp) (MLDeps)

  let invalidate ask exp_w st =
    D.filter (fun _ (ml, deps) -> (Deps.for_all (fun arg -> not (VarEq.may_change ask exp_w arg)) deps)) st

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    if M.tracing then M.tracel "tmpSpecial" "assignment of %a" d_lval lval;
    (* Invalidate all entrys from the map that are possibly written by the assignment *)
    invalidate (Analyses.ask_of_ctx ctx) (mkAddrOf lval) ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    (* For now we only track relationships intraprocedurally. *)
    [ctx.local, D.bot ()]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) f_ask : D.t =
    (* For now we only track relationships intraprocedurally. *)
    D.bot ()

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let d = ctx.local in
    let ask = Analyses.ask_of_ctx ctx in

    (* Just dbg prints *)
    (if M.tracing then
       match lval with
       | Some lv -> if M.tracing then M.tracel "tmpSpecial" "Special: %s with lval %a" f.vname d_lval lv
       | _ -> if M.tracing then M.tracel "tmpSpecial" "Special: %s" f.vname);


    let desc = LibraryFunctions.find f in

    (* remove entrys, dependent on lvals that were possibly written by the special function *)
    let write_args = LibraryDesc.Accesses.find_kind desc.accs Write arglist in
    (* TODO similar to symbLocks->Spec->special: why doesn't invalidate involve any reachable for deep write? *)
    let d = List.fold_left (fun d e -> invalidate ask e d) d write_args in

    (* same for lval assignment of the call*)
    let d =
      match lval with
      | Some lv -> invalidate ask (mkAddrOf lv) ctx.local
      | None -> d
    in

    (* add new math fun desc*)
    let d =
      match lval, desc.special arglist with
      | Some ((Var v, offs) as lv), (Math { fun_args; }) ->
        (* only add descriptor, if none of the args is written by the assignment, invalidating the equivalence *)
        (* actually it would be necessary to check here, if one of the arguments is written by the call. However this is not the case for any of the math functions and no other functions are covered so far *)
        if List.exists (fun arg -> VarEq.may_change ask (mkAddrOf lv) arg) arglist then
          d
        else
          D.add (v, Offset.Exp.of_cil offs) ((ML.lift fun_args, Deps.of_list ((Lval lv)::arglist))) d
      | _ -> d

    in

    if M.tracing then M.tracel "tmpSpecial" "Result: %a" D.pretty d;
    d


  let query ctx (type a) (q: a Queries.t) : a Queries.result =
    match q with
    | TmpSpecial lv -> let ml = fst (D.find lv ctx.local) in
      if ML.is_bot ml then Queries.Result.top q
      else ml
    | _ -> Queries.Result.top q

  let startstate v = D.bot ()
  let threadenter ctx ~multiple lval f args = [D.bot ()]
  let threadspawn ctx ~multiple lval f args fctx = ctx.local
  let exitstate  v = D.bot ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
