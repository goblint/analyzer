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
    (* | Top ->
      M.warn @@ "Variable " ^ v.vname ^ " considered escaped since its address is taken somewhere and the thread escape analysis is not active!";
      true *)


module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "escape"
  module D = EscapeDomain.EscapedVars
  module C = EscapeDomain.EscapedVars
  module G = Lattice.Unit

  (* queries *)
  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MayEscape v -> D.mem v ctx.local
    | _ -> Queries.Result.top q

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [ctx.local,ctx.local]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) : D.t =
    au

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
      Queries.LS.fold to_extra a (D.empty ())
    (* Ignore soundness warnings, as invalidation proper will raise them. *)
    | _ -> D.empty ()

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.bot ()
  let exitstate  v = D.bot ()

  let threadenter ctx lval f args =
    match args with
    | [ptc_arg] ->
      let escaped = reachable (Analyses.ask_of_ctx ctx) ptc_arg in
      if not (D.is_empty escaped) then (* avoid emitting unnecessary event *)
        ctx.emit (Events.Escape escaped);
      [escaped]
    | _ -> [D.bot ()]

  let threadspawn ctx lval f args fctx =
    D.join ctx.local @@
      match args with
      | [ptc_arg] ->
        let escaped = fctx.local in (* reuse reachable computation from threadenter *)
        if not (D.is_empty escaped) then (* avoid emitting unnecessary event *)
          ctx.emit (Events.Escape escaped);
        escaped
      | _ -> D.bot ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
