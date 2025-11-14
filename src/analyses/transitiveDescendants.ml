open Analyses
module TID = ThreadIdDomain.Thread

(** flow-insensitive analysis mapping threads to may-sets of descendants *)
module TransitiveDescendants = struct
  include IdentityUnitContextsSpec
  module D = Lattice.Unit

  module V = struct
    include TID
    include StdV
  end

  module G = ConcDomain.ThreadSet

  let name () = "transitiveDescendants"
  let startstate _ = D.bot ()
  let exitstate _ = D.bot ()

  let query man (type a) (x : a Queries.t) : a Queries.result =
    match x with
    | Queries.DescendantThreads t -> (man.global : TID.t -> ConcDomain.ThreadSet.t)
    | _ -> Queries.Result.top x
  ;;

  let threadspawn man ~multiple lval f args fman =
    let ask = Analyses.ask_of_man man in
    let tid_lifted = ask.f Queries.CurrentThreadId in
    match tid_lifted with
    | `Top | `Bot -> ()
    | `Lifted tid ->
      let child_ask = Analyses.ask_of_man fman in
      let child_tid_lifted = child_ask.f Queries.CurrentThreadId in
      (match child_tid_lifted with
       | `Top | `Bot -> ()
       | `Lifted child_tid ->
         (* contribute new child *)
         let _ = man.sideg tid (G.singleton child_tid) in
         (* transitive hull *)
         let child_descendants = man.global child_tid in
         man.sideg tid child_descendants)
  ;;
end

let _ = MCP.register_analysis ~dep:[ "threadid" ] (module TransitiveDescendants : MCPSpec)
