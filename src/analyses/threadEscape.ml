(** Variables that escape threads using the last argument from pthread_create. *)

open Prelude.Ana
open Analyses

module M = Messages

module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "escape"
  module D = EscapeDomain.EscapedVars
  module C = EscapeDomain.EscapedVars
  module G = Lattice.Unit

  (* queries *)
  let query ctx (q:Queries.t) : Queries.Result.t =
    match q with
    | Queries.MayEscape v -> `MayBool (D.mem v ctx.local)
    | _ -> Queries.Result.top ()

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local,ctx.local]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t =
    au

  let rec cut_offset x =
    match x with
    | `NoOffset    -> `NoOffset
    | `Index (_,o) -> `NoOffset
    | `Field (f,o) -> `Field (f, cut_offset o)

  let reachable ask e: D.t =
    match ask (Queries.ReachableFrom e) with
    | `LvalSet a when not (Queries.LS.is_top a) ->
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
      let escaped = reachable ctx.ask ptc_arg in
      ctx.emit (Events.Escape escaped);
      escaped
    | _ -> D.bot ()

  let threadspawn ctx lval f args fctx =
    match args with
    | [ptc_arg] ->
      let escaped = reachable ctx.ask ptc_arg in (* TODO: just use fd? *)
      ctx.emit (Events.Escape escaped);
      escaped
    | _ -> D.bot ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
