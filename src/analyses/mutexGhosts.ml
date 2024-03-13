(** ([mutexGhosts]). *)

open Analyses


module Spec =
struct
  include UnitAnalysis.Spec
  let name () = "mutexGhosts"

  module V =
  struct
    include Node
    let is_write_only _ = true
  end

  module Locked =
  struct
    include LockDomain.Mutexes
    let name () = "locked"
  end
  module Unlocked =
  struct
    include LockDomain.Mutexes
    let name () = "unlocked"
  end
  module G = Lattice.Prod (Locked) (Unlocked)

  let event ctx e octx =
    begin match e with
      | Events.Lock (l, _) ->
        ctx.sideg ctx.prev_node (Locked.singleton l, Unlocked.bot ())
      | Events.Unlock l ->
        ctx.sideg ctx.prev_node (Locked.bot (), Unlocked.singleton l)
      | _ -> ()
    end;
    ctx.local
end

let _ =
  MCP.register_analysis ~dep:["mutexEvents"] (module Spec : MCPSpec)
