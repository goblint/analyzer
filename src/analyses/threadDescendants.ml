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
    | Queries.DescendantThreads t ->
      let children = man.global t in
      let ask = ask_of_man man in
      (G.fold
         (fun e acc -> G.union (ask.f @@ Queries.DescendantThreads e) acc)
         children
         children
       : G.t)
    | _ -> Queries.Result.top x

  let threadspawn man ~multiple lval f args fman =
    let ask = ask_of_man man in
    let tid_lifted = ask.f Queries.CurrentThreadId in
    let child_ask = ask_of_man fman in
    let child_tid_lifted = child_ask.f Queries.CurrentThreadId in
    match tid_lifted, child_tid_lifted with
    | `Lifted tid, `Lifted child_tid -> man.sideg tid (G.singleton child_tid)
    | _ -> ()
end

let _ = MCP.register_analysis ~dep:[ "threadid" ] (module Spec : MCPSpec)
