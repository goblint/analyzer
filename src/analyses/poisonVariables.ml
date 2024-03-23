(** Taint analysis of variables that were modified between [setjmp] and [longjmp] and not yet overwritten. ([poisonVariables]). *)

open Batteries
open GoblintCil
open Analyses

module Spec =
struct
  include Analyses.IdentitySpec
  module VS = SetDomain.ToppedSet(CilType.Varinfo) (struct let topname = "All vars" end)

  let name () = "poisonVariables"
  module D = VS
  module C = Printable.Unit

  let context _ _ = ()

  let check_mval tainted (addr: Queries.AD.elt) =
    match addr with
    | Queries.AD.Addr.Addr (v,_) ->
      if not v.vglob && VS.mem v tainted then
        M.warn ~category:(Behavior (Undefined Other)) "Reading poisonous variable %a" CilType.Varinfo.pretty v
    | _ -> ()

  let rem_mval tainted (addr: Queries.AD.elt) =
    match addr with
    | Queries.AD.Addr.Addr (v,`NoOffset) -> VS.remove v tainted
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

  let enter ctx (_:lval option) (_:fundec) (args:exp list) : (D.t * D.t) list =
    if VS.is_empty ctx.local then
      [ctx.local,ctx.local]
    else (
      let reachable_from_args = List.fold (fun ad e -> Queries.AD.join ad (ctx.ask (ReachableFrom e))) (Queries.AD.empty ()) args in
      if Queries.AD.is_top reachable_from_args || VS.is_top ctx.local then
        [ctx.local, ctx.local]
      else
        let reachable_vars =
          let get_vars addr vs =
            match addr with
            | Queries.AD.Addr.Addr (v,_) -> VS.add v vs
            | _ -> vs
          in
          Queries.AD.fold get_vars reachable_from_args (VS.empty ())
        in
        [VS.diff ctx.local reachable_vars, VS.inter reachable_vars ctx.local]
    )

  let combine_env ctx lval fexp f args fc au f_ask =
    VS.join au ctx.local

  let startstate v = D.bot ()
  let threadenter ctx ~multiple lval f args = [D.bot ()]
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
            M.info ~category:(Behavior (Undefined Other)) ~loc:(Node longjmp_node) "Since setjmp at %a, potentially all locals were modified! Reading them will yield Undefined Behavior." Node.pretty ctx.prev_node
          else if not (Queries.VS.is_empty modified_locals) then
            M.info ~category:(Behavior (Undefined Other)) ~loc:(Node longjmp_node) "Since setjmp at %a, locals %a were modified! Reading them will yield Undefined Behavior." Node.pretty ctx.prev_node Queries.VS.pretty modified_locals
        ) longjmp_nodes;
      D.join modified_locals ctx.local
    | Access {ad; kind = Read; _} ->
      (* TODO: what about AD with both known and unknown pointers? *)
      begin match ad with
        | ad when Queries.AD.is_top ad && not (VS.is_empty octx.local) ->
          M.warn ~category:(Behavior (Undefined Other)) "reading unknown memory location, may be tainted!"
        | ad ->
          (* Use original access state instead of current with removed written vars. *)
          Queries.AD.iter (check_mval octx.local) ad
      end;
      ctx.local
    | Access {ad; kind = Write; _} ->
      (* TODO: what about AD with both known and unknown pointers? *)
      begin match ad with
        | ad when Queries.AD.is_top ad ->
          ctx.local
        | ad ->
          Queries.AD.fold (fun addr vs ->
              rem_mval vs addr
            ) ad ctx.local
      end
    | _ -> ctx.local

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
