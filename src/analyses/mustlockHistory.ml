open Analyses
module TIDs = SetDomain.Reverse (ConcDomain.ThreadSet)
module Lock = LockDomain.MustLock

module Spec = struct
  include IdentityUnitContextsSpec
  module D = Queries.LH

  let name () = "mustlockHistory"
  let startstate _ = D.empty ()
  let exitstate _ = D.empty ()

  let lock man tid lock =
    let old_threadset = D.find lock man.local in
    let new_threadset = TIDs.add tid old_threadset in
    D.add lock new_threadset man.local

  let event man e _ =
    match e with
    (* we only handle exclusive locks here *)
    | Events.Lock (addr, true) ->
      let tid_lifted = man.ask Queries.CurrentThreadId in
      let lock_opt = Lock.of_addr addr in
      (match tid_lifted, lock_opt with
       | `Lifted tid, Some l -> lock man tid l
       | _ -> man.local)
    | _ -> man.local

  let query man (type a) (x : a Queries.t) : a Queries.result =
    match x with
    | Queries.MustlockHistory -> (man.local : D.t)
    | _ -> Queries.Result.top x
end

let _ =
  MCP.register_analysis
    ~dep:[ "threadid"; "threadJoins"; "threadDescendants" ]
    (module Spec : MCPSpec)
