open Prelude.Ana
open Analyses

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  let name () = "observer"

  module ChainParams =
  struct
    let n = 3
    let names x = "state " ^ string_of_int x
  end
  module D = Lattice.Flat (Printable.Chain (ChainParams)) (Printable.DefaultNames)
  module G = Lattice.Unit
  module C = D

  let should_join x y = D.equal x y (* fully path-sensitive *)

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    match lval with
    | (Var v, NoOffset) ->
      begin match ctx.local with
      | `Lifted 2 -> raise Deadcode
      | `Lifted x -> `Lifted (x + 1)
      | _ -> ctx.local
      end
    | _ ->
      ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local, ctx.local]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = `Lifted 0
  let otherstate v = D.top ()
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : Spec)
