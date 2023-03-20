open Prelude.Ana
open Analyses

module Spec =
struct
  include Analyses.IdentitySpec
  module VS = SetDomain.ToppedSet(CilType.Varinfo) (struct let topname = "All vars" end)

  let name () = "poisonVariables"
  module D = VS
  module C = Lattice.Unit

  let context _ _ = ()

  let check_lval tainted ((v, offset): Queries.LS.elt) =
    if not v.vglob && VS.mem v tainted then
      M.warn ~category:(Behavior (Undefined Other)) "Reading poisonous variable %a" d_varinfo v

  let rem_lval tainted ((v, offset): Queries.LS.elt) = match offset with
    | `NoOffset -> VS.remove v tainted
    | _ -> tainted (* If there is an offset, it is a bit harder to remove, as we don't know where the indeterminate value is *)


  (* transfer functions *)
  let return ctx (exp:exp option) (f:fundec) : D.t =
    (* remove locals, except ones which need to be weakly updated*)
    if D.is_top ctx.local then
      ctx.local
    else (
      let locals = f.sformals @ f.slocals in
      D.filter (fun v ->
          not (List.exists (fun local ->
              CilType.Varinfo.equal v local && not (ctx.ask (Queries.IsMultiple local))
            ) locals)
        ) ctx.local
    )

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    if VS.is_empty ctx.local then
      [ctx.local,ctx.local]
    else (
      let reachable_from_args = List.fold (fun ls e -> Queries.LS.join ls (ctx.ask (ReachableFrom e))) (Queries.LS.empty ()) args in
      if Queries.LS.is_top reachable_from_args || VS.is_top ctx.local then
        [ctx.local, ctx.local]
      else
        let reachable_vars =
          Queries.LS.elements reachable_from_args
          |> List.map fst
          |> VS.of_list
        in
        [VS.diff ctx.local reachable_vars, VS.inter reachable_vars ctx.local]
    )

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    VS.join au ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.bot ()]
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
            M.info ~category:(Behavior (Undefined Other)) ~loc:(Node longjmp_node) "Since setjmp at %s, potentially all locals were modified! Reading them will yield Undefined Behavior." (Node.show ctx.prev_node)
          else if not (Queries.VS.is_empty modified_locals) then
            M.info ~category:(Behavior (Undefined Other)) ~loc:(Node longjmp_node) "Since setjmp at %s, locals %s were modified! Reading them will yield Undefined Behavior." (Node.show ctx.prev_node) (Queries.VS.show modified_locals)
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
    | Access {lvals; kind = Write; _} ->
      Queries.LS.fold (fun lv acc ->
          rem_lval acc lv
        ) lvals ctx.local
    | _ -> ctx.local

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
