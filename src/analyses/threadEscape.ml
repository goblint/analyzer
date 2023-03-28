(** Variables that escape threads using the last argument from pthread_create. *)

open Prelude.Ana
open Analyses

module M = Messages

let has_escaped (ask: Queries.ask) (v: varinfo): bool =
  assert (not v.vglob);
  if not v.vaddrof then
    false (* Cannot have escaped without taking address. Override provides extra precision for degenerate ask in base eval_exp used for partitioned arrays. *)
  else
    ask.f (Queries.MayEscape v)

module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "escape"
  module D = EscapeDomain.EscapedVars
  module C = EscapeDomain.EscapedVars
  module V = VarinfoV
  module G = EscapeDomain.EscapedVars

  let rec cut_offset x =
    match x with
    | `NoOffset    -> `NoOffset
    | `Index (_,o) -> `NoOffset
    | `Field (f,o) -> `Field (f, cut_offset o)

  let reachable (ask: Queries.ask) e: D.t =
    match ask.f (Queries.ReachableFrom e) with
    | a when not (Queries.LS.is_top a) ->
      (* let to_extra (v,o) set = D.add (Addr.from_var_offset (v, cut_offset o)) set in *)
      let to_extra (v,o) set = D.add v set in
      Queries.LS.fold to_extra (Queries.LS.remove (dummyFunDec.svar, `NoOffset) a) (D.empty ())
    (* Ignore soundness warnings, as invalidation proper will raise them. *)
    | a ->
      if M.tracing then M.tracel "escape" "reachable %a: %a\n" d_exp e Queries.LS.pretty a;
      D.empty ()

  let mpt (ask: Queries.ask) e: D.t =
    match ask.f (Queries.MayPointTo e) with
    | a when not (Queries.LS.is_top a) ->
      (* let to_extra (v,o) set = D.add (Addr.from_var_offset (v, cut_offset o)) set in *)
      let to_extra (v,o) set = D.add v set in
      Queries.LS.fold to_extra (Queries.LS.remove (dummyFunDec.svar, `NoOffset) a) (D.empty ())
    (* Ignore soundness warnings, as invalidation proper will raise them. *)
    | a ->
      if M.tracing then M.tracel "escape" "mpt %a: %a\n" d_exp e Queries.LS.pretty a;
      D.empty ()

  (* queries *)
  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MayEscape v -> D.mem v ctx.local
    | _ -> Queries.Result.top q

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    let ask = Analyses.ask_of_ctx ctx in
    let vs = mpt ask (AddrOf lval) in
    if M.tracing then M.tracel "escape" "assign vs: %a\n" D.pretty vs;
    if D.exists (fun v -> v.vglob || has_escaped ask v) vs then (
      let escaped = reachable ask rval in
      let escaped = D.filter (fun v -> not v.vglob) escaped in
      if M.tracing then M.tracel "escape" "assign vs: %a | %a\n" D.pretty vs D.pretty escaped;
      if not (D.is_empty escaped) && ThreadFlag.is_multi ask then (* avoid emitting unnecessary event *)
        ctx.emit (Events.Escape escaped);
      D.iter (fun v ->
          ctx.sideg v escaped;
        ) vs;
      D.join ctx.local escaped
    )
    else
      ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [ctx.local,ctx.local]

  let combine_env ctx lval fexp f args fc au f_ask =
    ctx.local

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    au

  let special ctx (lval: lval option) (f:varinfo) (args:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special args, f.vname, args with
    | _, "pthread_setspecific" , [key; pt_value] ->
      let escaped = reachable (Analyses.ask_of_ctx ctx) pt_value in
      let escaped = D.filter (fun v -> not v.vglob) escaped in
      if not (D.is_empty escaped) then (* avoid emitting unnecessary event *)
        ctx.emit (Events.Escape escaped);
      let extra = D.fold (fun v acc -> D.join acc (ctx.global v)) escaped (D.empty ()) in (* TODO: must transitively join escapes of every ctx.global v as well? *)
      D.join ctx.local (D.join escaped extra)
    | _ -> ctx.local

  let startstate v = D.bot ()
  let exitstate  v = D.bot ()

  let threadenter ctx lval f args =
    match args with
    | [ptc_arg] ->
      let escaped = reachable (Analyses.ask_of_ctx ctx) ptc_arg in
      let escaped = D.filter (fun v -> not v.vglob) escaped in
      if not (D.is_empty escaped) then (* avoid emitting unnecessary event *)
        ctx.emit (Events.Escape escaped);
      let extra = D.fold (fun v acc -> D.join acc (ctx.global v)) escaped (D.empty ()) in (* TODO: must transitively join escapes of every ctx.global v as well? *)
      [D.join ctx.local (D.join escaped extra)]
    | _ -> [ctx.local]

  let threadspawn ctx lval f args fctx =
    D.join ctx.local @@
      match args with
      | [ptc_arg] ->
        (* not reusing fctx.local to avoid unnecessarily early join of extra *)
        let escaped = reachable (Analyses.ask_of_ctx ctx) ptc_arg in
        let escaped = D.filter (fun v -> not v.vglob) escaped in
        if M.tracing then M.tracel "escape" "%a: %a\n" d_exp ptc_arg D.pretty escaped;
        if not (D.is_empty escaped) then (* avoid emitting unnecessary event *)
          ctx.emit (Events.Escape escaped);
        escaped
      | _ -> D.bot ()

  let event ctx e octx =
    match e with
    | Events.EnterMultiThreaded ->
      let escaped = ctx.local in
      if not (D.is_empty escaped) then (* avoid emitting unnecessary event *)
        ctx.emit (Events.Escape escaped);
      ctx.local
    | _ -> ctx.local
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
