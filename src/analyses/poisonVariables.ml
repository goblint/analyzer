(** Taint analysis of variables that were modified between [setjmp] and [longjmp] and not yet overwritten. ([poisonVariables]). *)

open Batteries
open GoblintCil
open Analyses

module Spec =
struct
  include Analyses.IdentityUnitContextsSpec
  module VS = SetDomain.ToppedSet(CilType.Varinfo) (struct let topname = "All vars" end)

  let name () = "poisonVariables"
  module D = VS

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
  let return man (exp:exp option) (f:fundec) : D.t =
    (* remove locals, except ones which need to be weakly updated*)
    if D.is_top man.local then
      man.local
    else (
      let locals = f.sformals @ f.slocals in
      D.filter (fun v ->
          not (List.exists (fun local ->
              CilType.Varinfo.equal v local && not (man.ask (Queries.IsMultiple local))
            ) locals)
        ) man.local
    )

  let enter man (_:lval option) (_:fundec) (args:exp list) : (D.t * D.t) list =
    if VS.is_empty man.local then
      [man.local,man.local]
    else (
      let reachable_from_args = List.fold (fun ad e -> Queries.AD.join ad (man.ask (ReachableFrom e))) (Queries.AD.empty ()) args in
      if Queries.AD.is_top reachable_from_args || VS.is_top man.local then
        [man.local, man.local]
      else
        let reachable_vars =
          let get_vars addr vs =
            match addr with
            | Queries.AD.Addr.Addr (v,_) -> VS.add v vs
            | _ -> vs
          in
          Queries.AD.fold get_vars reachable_from_args (VS.empty ())
        in
        [VS.diff man.local reachable_vars, VS.inter reachable_vars man.local]
    )

  let combine_env man lval fexp f args fc au f_ask =
    VS.join au man.local

  let startstate v = D.bot ()
  let threadenter man ~multiple lval f args = [D.bot ()]
  let exitstate  v = D.top ()

  let event man e oman =
    match e with
    | Events.Longjmped {lval} ->
      let modified_locals = man.ask (MayBeModifiedSinceSetjmp (man.prev_node, man.control_context ())) in
      let modified_locals = match lval with
        | Some (Var v, NoOffset) -> Queries.VS.remove v modified_locals
        | _ -> modified_locals (* Does usually not really occur, if it does, this is sound *)
      in
      let (_, longjmp_nodes) = man.ask ActiveJumpBuf in
      JmpBufDomain.NodeSet.iter (fun longjmp_node ->
          if Queries.VS.is_top modified_locals then
            M.info ~category:(Behavior (Undefined Other)) ~loc:(Node longjmp_node) "Since setjmp at %a, potentially all locals were modified! Reading them will yield Undefined Behavior." Node.pretty man.prev_node
          else if not (Queries.VS.is_empty modified_locals) then
            M.info ~category:(Behavior (Undefined Other)) ~loc:(Node longjmp_node) "Since setjmp at %a, locals %a were modified! Reading them will yield Undefined Behavior." Node.pretty man.prev_node Queries.VS.pretty modified_locals
        ) longjmp_nodes;
      D.join modified_locals man.local
    | Access {ad; kind = Read; _} ->
      (* TODO: what about AD with both known and unknown pointers? *)
      begin match ad with
        | ad when Queries.AD.is_top ad && not (VS.is_empty oman.local) ->
          M.warn ~category:(Behavior (Undefined Other)) "reading unknown memory location, may be tainted!"
        | ad ->
          (* Use original access state instead of current with removed written vars. *)
          Queries.AD.iter (check_mval oman.local) ad
      end;
      man.local
    | Access {ad; kind = Write; _} ->
      (* TODO: what about AD with both known and unknown pointers? *)
      begin match ad with
        | ad when Queries.AD.is_top ad ->
          man.local
        | ad ->
          Queries.AD.fold (fun addr vs ->
              rem_mval vs addr
            ) ad man.local
      end
    | _ -> man.local

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
