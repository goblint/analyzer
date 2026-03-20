open Analyses

module Spec =
struct
  include UnitAnalysis.Spec

  let name () = "threadAccess"

  module V =
  struct
    include CilType.Varinfo
    let is_write_only _ = false
  end

  module G = ConcDomain.ThreadSet

  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | MayBePublic {global; _} ->
      if G.cardinal (man.global global) = 1 then
        false
      else
        Queries.Result.top q
    | _ -> Queries.Result.top q

  let event man e oman =
    match e with
    | Events.Access {ad; _} ->
      Queries.AD.iter (function
          | Queries.AD.Addr.Addr (v, _) when v.vglob ->
            begin match man.ask CurrentThreadId with
              | `Lifted tid -> man.sideg v (G.singleton tid)
              | _ -> () (* TODO: what here? *)
            end
          | _ -> ()
        ) ad
    | _ ->
      man.local
end

let _ =
  MCP.register_analysis ~dep:["access"] (module Spec : MCPSpec)
