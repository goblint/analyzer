(** thread descendants analysis [threadDescendants]
    flow-insensitive construction of descendants may-set for every thread
*)

open Analyses
module TID = ThreadIdDomain.Thread

module Spec = struct
  include IdentityUnitContextsSpec
  module D = Lattice.Unit

  module V = struct
    include TID
    include StdV
  end

  module G = ConcDomain.ThreadSet

  let name () = "threadDescendants"
  let startstate _ = D.bot ()
  let exitstate _ = D.bot ()

  let query man (type a) (x : a Queries.t) : a Queries.result =
    match x with
    | Queries.DescendantThreads t -> (man.global t : G.t)
    | _ -> Queries.Result.top x

  let threadspawn man ~multiple lval f args fman =
    let ask = ask_of_man man in
    let tid_lifted = ask.f Queries.CurrentThreadId in
    let child_ask = ask_of_man fman in
    let child_tid_lifted = child_ask.f Queries.CurrentThreadId in
    match tid_lifted, child_tid_lifted with
    | `Lifted tid, `Lifted child_tid ->
      let to_contribute = G.add child_tid (man.global child_tid) in
      man.sideg tid to_contribute
    | _ -> ()
end

let _ = MCP.register_analysis ~dep:[ "threadid" ] (module Spec : MCPSpec)
