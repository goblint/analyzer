(** Analysis that tracks which variables hold the results of calls to math library functions ([tmpSpecial]). *)

(** For each equivalence a set of expressions is tracked, that contains the arguments of the corresponding call as well as the Lval it is assigned to, so an equivalence can be removed if one of these expressions may be changed. *)

module VarEq = VarEq.Spec

open GoblintCil
open Analyses

module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "tmpSpecial"
  module ML = LibraryDesc.MathLifted
  module Deps = SetDomain.ToppedSet (CilType.Exp) (struct let topname = "All" end)
  module MLDeps = Lattice.Prod (ML) (Deps)

  (* x maps to (mathf(arg), deps) arg if x = mathf(arg) *)
  module MM = MapDomain.MapBot_LiftTop (Mval.Exp) (MLDeps)

  (* arg maps to (x, mathf(arg), deps) if x = mathf(arg) *)
  module ExpFlat = Lattice.Flat (CilType.Exp)
  module MInvEntry = Lattice.Prod3 (ExpFlat) (ML) (Deps)
  module MInvEntrySet = SetDomain.Reverse (SetDomain.ToppedSet (MInvEntry) (struct let topname = "All" end))
  module MInv = MapDomain.MapBot_LiftTop (Mval.Exp) (MInvEntrySet)

  module D = Lattice.Prod (MM) (MInv)
  module C = Lattice.Unit

  let invalidate ask exp_w st =
    let depNotChanged = Deps.for_all (fun arg -> not (VarEq.may_change ask exp_w arg)) in
    let invalidate1 m = MM.filter (fun _ (ml, deps) -> depNotChanged deps) m in
    let invalidate2 m = MInv.map (fun es ->
        match es with
        | es when MInvEntrySet.is_bot es -> es
        | es -> MInvEntrySet.filter (fun (_,_,deps) -> depNotChanged deps) es) m in
    invalidate1 (fst st), invalidate2 (snd st)

  let context _ _ = ()

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    if M.tracing then M.tracel "tmpSpecial" "assignment of %a\n" d_lval lval;
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
       | Some lv -> if M.tracing then M.tracel "tmpSpecial" "Special: %s with lval %a\n" f.vname d_lval lv
       | _ -> if M.tracing then M.tracel "tmpSpecial" "Special: %s\n" f.vname);


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
        else (
          if M.tracing then M.trace "tmpSpecialInv" "build tmpSpecial Entry for Lval %s\n" v.vname;
          let d1 = MM.add (v, Offset.Exp.of_cil offs) ((ML.lift fun_args, Deps.of_list ((Lval lv)::arglist))) (fst d) in
          let castNeglectable t x = match fun_args with
            | LibraryDesc.Isinf _
            | LibraryDesc.Isnan _ -> BaseDomain.VD.is_safe_cast t (Cilfacade.typeOfLval x)
            | _ -> false
          in
          let build_inv_entry varg offsarg =
            if M.tracing then M.trace "tmpSpecialInv" "build tmpSpecialInv Entry for Argument %s\n" varg.vname;
            let key = (varg, Offset.Exp.of_cil offsarg) in
            let newE = (`Lifted (Lval lv), ML.lift fun_args, Deps.of_list ((Lval lv)::arglist)) in
            if M.tracing then M.trace "tmpSpecialInv" "newE (lv: %s, ml: %s)\n" v.vname (LibraryDesc.MathPrintable.show fun_args);
            let combined = match MInv.find key (snd d) with
              | `Top -> MInvEntrySet.singleton newE
              | s -> MInvEntrySet.add newE s in
            Some (key, combined) in
          let pick_lval = function
            | Lval (Var varg, offsarg) -> build_inv_entry varg offsarg
            (* we store the relationship neglecting the cast only in case of upcasting and math functions being IsNan and IsInf *)
            | CastE(t, Lval (Var varg, offsarg)) when castNeglectable t (Var varg, offsarg) -> build_inv_entry varg offsarg
            | _ -> None in
          let d2 = MInv.add_list (List.filter_map pick_lval arglist) (snd d) in
          d1, d2)
      | _ -> d

    in

    if M.tracing then M.tracel "tmpSpecial" "Result: %a\n\n" D.pretty d;
    d


  let query ctx (type a) (q: a Queries.t) : a Queries.result =
    match q with
    | TmpSpecial lv -> let ml = fst (MM.find lv (fst ctx.local)) in
      if ML.is_bot ml then Queries.Result.top q
      else ml
    | TmpSpecialInv lv -> (
        if M.tracing then M.trace "tmpSpecialInv" "MInv.find lv: %s\n" (fst lv).vname;
        match MInv.find lv (snd ctx.local) with
        | `Top -> Queries.Result.top q
        | es ->
          let mlv = MInvEntrySet.fold (fun (v, ml, deps) acc -> Queries.MLInv.add (v,ml) acc) es (Queries.MLInv.empty ())
                    |> Queries.MLInv.filter (fun (v,ml) ->
                        match ml, v with
                        | `Bot, _ -> false
                        | _, `Lifted exp -> true
                        | _ -> false) in
          if Queries.MLInv.is_bot mlv then Queries.Result.top q
          else mlv)
    | _ -> Queries.Result.top q

  let startstate v = D.bot ()
  let threadenter ctx ~multiple lval f args = [D.bot ()]
  let threadspawn ctx ~multiple lval f args fctx = ctx.local
  let exitstate  v = D.bot ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
