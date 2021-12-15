open Prelude.Ana
open Analyses

module Spec : Analyses.MCPSpec =
struct
  let name () = "expsplit"

  module D = MapDomain.MapBot (Basetype.CilExp) (Lattice.Unit)
  module C = D

  let startstate v = D.bot ()
  let exitstate = startstate

  include Analyses.IdentitySpec (* TODO: implement others correctly instead of identity *)

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) =
    match f.vname with
    | "__goblint_split_begin" ->
      let exp = List.hd arglist in
      D.add exp () ctx.local
    | "__goblint_split_end" ->
      let exp = List.hd arglist in
      D.remove exp ctx.local
    | _ ->
      ctx.local (* TODO: not identity *)
end

let () =
  MCP.register_analysis (module Spec : MCPSpec)
