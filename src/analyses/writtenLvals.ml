
open Prelude.Ana
open Analyses

module Q = Queries

module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let name () = "writtenLvals"
  module D = Lattice.Unit
  module G = Q.LS
  module C = Lattice.Unit

  let val_of _ = ()

  let side_to_f ctx side =
    let get_current_fun () = Option.map MyCFG.getFun !MyCFG.current_node in
    let f = get_current_fun () in
    (match f with
      | Some f -> ctx.sideg f.svar side
      | None -> ()
    );
    side

  let is_heap_var ctx (v: varinfo) =
    match ctx.ask (Q.IsHeapVar v) with
      | `MustBool true -> true
      | _ -> false

  (* transfer functions *)
  let add_written_lval ctx (lval:lval): G.t =
    let query e = ctx.ask (Q.MayPointTo e) in
    let filter (s: Q.LS.t) = Q.LS.filter (fun (v,offset) -> is_heap_var ctx v) s in
    let side = match lval with
      | Mem e, NoOffset
      | Mem e, Index _ ->
        (match query e with
          | `LvalSet s -> filter s
          | _ -> G.bot ()
        )
      | Mem e, Field (finfo, offs) ->
        (match query e with
          | `LvalSet s -> filter s |> Q.LS.map (fun (v, offset) -> (v, `Field (finfo, offset)))
          | _ -> G.bot ()
        )
      | _, _ -> G.bot ()
    in
    side_to_f ctx side

  let add_written_option_lval ctx (lval: lval option): G.t =
    match lval with
    | Some lval -> add_written_lval ctx lval
    | None -> G.bot ()

  let assign ctx (lval:lval) (rval:exp) : D.t =
    ignore @@ add_written_lval ctx lval

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local, D.bot ()]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t =
    let side = G.union (add_written_option_lval ctx lval) (ctx.global f) in
    ignore @@ side_to_f ctx side

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let newst = add_written_option_lval ctx lval in
    ignore @@ side_to_f ctx newst

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = D.bot ()
  let exitstate  v = D.top ()

  let query ctx (q:Q.t) = match q with
    | Q.WrittenLvals f ->
      `LvalSet (ctx.global f)
    | _ -> `Top

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
