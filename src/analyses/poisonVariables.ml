open Prelude.Ana
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec
  module VS = SetDomain.ToppedSet(CilType.Varinfo) (struct let topname = "All vars" end)

  let name () = "poisonVariables"
  module D = VS
  module C = Lattice.Unit

  let context _ _ = ()

  (* TODO: use Access events instead of reimplementing logic? *)

  let check_lval tainted ((v, offset): Queries.LS.elt) =
    if not v.vglob && VS.mem v tainted then
      M.warn "accessing poisonous variable %a" d_varinfo v

  let rec rem_lval ask tainted lval = match lval with
    | (Var v, NoOffset) -> VS.remove v tainted (* TODO: If there is an offset, it is a bit harder to remove, as we don't know where the indeterminate value is *)
    | (Mem e, NoOffset) ->
      (try
         let r = Queries.LS.elements (ask (Queries.MayPointTo e)) in
         match r with
         | [x] -> rem_lval ask tainted @@ Lval.CilLval.to_lval x
         | _ -> tainted
       with
         SetDomain.Unsupported _ -> tainted)
    | _ -> tainted (* If there is an offset, it is a bit harder to remove, as we don't know where the indeterminate value is *)


  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    rem_lval ctx.ask ctx.local lval

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    (* remove locals, except ones which need to be weakly updated*)
    if D.is_top ctx.local then
      ctx.local
    else
      let locals = (f.sformals @ f.slocals) in
      let locals_noweak = List.filter (fun v_info -> not (ctx.ask (Queries.IsMultiple v_info))) locals in
      D.filter (fun v -> not (List.mem v locals_noweak)) ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    if VS.is_empty ctx.local then
      [ctx.local,ctx.local]
    else (
       let reachable_from_args = List.fold (fun ls e -> Queries.LS.join ls (ctx.ask (ReachableFrom e))) (Queries.LS.empty ()) args in
       if Queries.LS.is_top reachable_from_args || VS.is_top ctx.local then
         [ctx.local, ctx.local]
       else
         let reachable_vars = Queries.LS.elements reachable_from_args |> List.map fst |> VS.of_list in
         [VS.diff ctx.local reachable_vars, VS.inter reachable_vars ctx.local])

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    (* Actually, this ask would have to be on the post state?! *)
    Option.map_default (rem_lval ctx.ask au) (VS.join au ctx.local) lval

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    Option.map_default (rem_lval ctx.ask ctx.local) ctx.local lval

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.bot ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()

  let event ctx e octx =
    match e with
    | Events.Longjmped {lval} ->
      let modified_locals = ctx.ask (MayBeModifiedSinceSetjmp (ctx.prev_node, ctx.control_context ())) in
      let modified_locals = match lval with
        | Some (Var v, NoOffset) -> Queries.VS.remove v modified_locals
        | _ -> modified_locals (* Does usually not really occur, if it does, this is sound *)
      in
      let (_, longjmp_nodes) = ctx.ask ActiveJumpBuf in
      JmpBufDomain.NodeSet.iter (fun longjmp_node ->
          if Queries.VS.is_top modified_locals then
            M.warn ~loc:(Node longjmp_node) "Information: Since setjmp at %s, potentially all locals were modified! Acessing them will yield Undefined Behavior." (Node.show ctx.prev_node)
          else if not (Queries.VS.is_empty modified_locals) then
            M.warn ~loc:(Node longjmp_node) "Information: Since setjmp at %s, locals %s were modified! Acessing them will yield Undefined Behavior." (Node.show ctx.prev_node) (Queries.VS.show modified_locals)
          else
            ()
        ) longjmp_nodes;
      D.join modified_locals ctx.local
    | Access {lvals; kind = Read; _} ->
      Queries.LS.iter (fun lv ->
          (* Use original access state instead of current with removed written vars. *)
          check_lval octx.local lv
        ) lvals;
      ctx.local
    | _ -> ctx.local

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
