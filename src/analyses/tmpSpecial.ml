(* Analysis that tracks which variables hold the results of calls to math library functions.
  For each equivalence a set of lvals is tracked, that contains all lvals on which the arguments of the corresponding call depend, so an equivalence can be removed if one of the lvals is written.*)

open GoblintCil
open Analyses

module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "tmpSpecial"
  module ML = LibraryDesc.MathLifted
  module LS = SetDomain.ToppedSet(Lval.CilLval) (struct let topname = "All" end)
  module MlLsProd = Lattice.Prod (ML) (LS)
  module D = MapDomain.MapBot (Lval.CilLval) (MlLsProd)
  module C = Lattice.Unit

  let rec resolve (offs : offset) : (CilType.Fieldinfo.t, Basetype.CilExp.t) Lval.offs =
    match offs with
    | NoOffset -> `NoOffset
    | Field (f_info, f_offs) -> `Field (f_info, (resolve f_offs))
    | Index (i_exp, i_offs) -> `Index (i_exp, (resolve i_offs))

  let ls_of_lv ctx (lval:lval) : LS.t =
    match lval with
    | (Var v, offs) -> LS.of_list [(v, resolve offs)]
    | (Mem e, _) -> (ctx.ask (Queries.MayPointTo e))

  let rec ls_of_exp ctx (exp:exp) : LS.t =
  match exp with
  | Const _ -> LS.empty ()
  | Lval lv -> ls_of_lv ctx lv
  | SizeOf _ -> LS.empty ()
  | Real e -> ls_of_exp ctx e
  | Imag e -> ls_of_exp ctx e
  | SizeOfE e -> ls_of_exp ctx e
  | SizeOfStr _ -> LS.empty ()
  | AlignOf _ -> LS.empty ()
  | AlignOfE e -> ls_of_exp ctx e
  | UnOp (_,e,_) -> ls_of_exp ctx e
  | BinOp (_,e1,e2,_) -> LS.union (ls_of_exp ctx e1) (ls_of_exp ctx e2)
  | Question (q,e1,e2,_) -> LS.union (ls_of_exp ctx q) (LS.union (ls_of_exp ctx e1) (ls_of_exp ctx e2))
  | CastE (_,e) -> ls_of_exp ctx e
  | AddrOf _ -> ctx.ask (Queries.MayPointTo exp)
  | AddrOfLabel _ -> LS.empty ()
  | StartOf _ -> ctx.ask (Queries.MayPointTo exp)


  let context _ _ = ()

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    (* get the set of all lvals written by the assign. Then remove all entrys from the map where the dependencies overlap with the set of written lvals *)
    let lvalsWritten = ls_of_lv ctx lval in
    if M.tracing then M.trace "tmpSpecial" "lvalsWritten %a\n" LS.pretty lvalsWritten;
    D.filter (fun _ (ml, ls) -> LS.is_bot (LS.meet lvalsWritten ls) ) ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    (* For now we only track relationships intraprocedurally. TODO: handle interprocedural tracking *)
    [ctx.local, D.bot ()]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) f_ask : D.t =
    (* For now we only track relationships intraprocedurally. TODO: handle interprocedural tracking *)
    D.bot ()

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let d = ctx.local in

    (* Just dbg prints *)
    (if M.tracing then
      match lval with
      | Some lv -> if M.tracing then M.tracel "tmpSpecial" "Special: %s with lval %a\n" f.vname d_lval lv
      | _ -> if M.tracing then M.tracel "tmpSpecial" "Special: %s\n" f.vname);

  
    let desc = LibraryFunctions.find f in
    (* remove entrys, dependent on lvals that were possibly written by the special function *)
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
      let lvalsWritten = ctx.ask (Queries.MayPointTo addr) in
      D.filter (fun _ (_, ls) -> LS.is_bot (LS.meet lvalsWritten ls) ) accD) d shallow_addrs
    in
    let d = List.fold_left (fun accD addr ->
      let lvalsWritten = ctx.ask (Queries.ReachableFrom addr) in
      D.filter (fun _ (_, ls) -> LS.is_bot (LS.meet lvalsWritten ls) ) accD) d deep_addrs
    in

    (* same for lval assignment of the call*)
    let d =
    match lval with
    | Some lv -> (
      (* get the set of all lvals written by the assign. Then remove all entrys from the map where the dependencies overlap with the set of written lvals *)
      let lvalsWritten = ls_of_lv ctx lv in
      D.filter (fun _ (ml, ls) -> LS.is_bot (LS.meet lvalsWritten ls) ) d
      )
    | None -> d
    in

    (* add new math fun desc*)
    let d =
    match lval, desc.special arglist with
      | Some (Var v, offs), (Math { fun_args; }) ->
        let argsDep = List.fold_left LS.union (LS.empty ()) (List.map (ls_of_exp ctx) arglist) in
        let lvalsWritten = ls_of_lv ctx (Var v, offs) in
        (* only add descriptor, if the set of lvals contained in the args is known and none is written by the assignment *)
        (* actually it would be necessary to check here, if one of the arguments is written by the call. However this is not the case for any of the math functions and no other functions are covered so far *)
        if LS.is_top argsDep || not (LS.is_empty (LS.meet argsDep lvalsWritten)) then
          d
        else
          D.add (v, resolve offs) ((ML.lift fun_args, LS.union argsDep lvalsWritten)) d
      | _ -> d
    in

    if M.tracing then M.tracel "tmpSpecial" "Result: %a\n\n" D.pretty d;
    d


    let query ctx (type a) (q: a Queries.t) : a Queries.result =
      match q with
      | TmpSpecial lv -> let ml = fst (D.find lv ctx.local) in
        if ML.is_bot ml then Queries.Result.top q
        else ml
      | _ -> Queries.Result.top q

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.bot ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.bot ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
