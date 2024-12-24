(** Must have waited barriers for Pthread barriers ([pthreadBarriers]). *)

open GoblintCil
open Analyses
module LF = LibraryFunctions

module Spec =
struct
  module Barriers = struct
    include SetDomain.ToppedSet (ValueDomain.Addr) (struct let topname = "All barriers" end)
    let name () = "mayBarriers"
  end

  module MustBarriers = struct
    include Lattice.Reverse (Barriers)
    let name () = "mustBarriers" 
  end

  module Capacity = Queries.ID

  include Analyses.IdentitySpec
  module V = VarinfoV

  module TID = ThreadIdDomain.Thread
  module Waiters = SetDomain.ToppedSet (TID) (struct let topname = "All MHP" end)
  module G = Lattice.Prod (Capacity) (Waiters)

  let name () = "pthreadBarriers"
  module D = Lattice.Prod (Barriers) (MustBarriers)

  include Analyses.ValueContexts(D)

  let possible_vinfos (a: Queries.ask) barrier =
    Queries.AD.to_var_may (a.f (Queries.MayPointTo barrier))

  let special man (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LF.find f in
    match desc.special arglist with
    | BarrierWait barrier ->
      let ask = Analyses.ask_of_man man in
      let may, must = man.local in
      let barriers = possible_vinfos ask barrier in
      let tid = match ThreadId.get_current ask with
        | `Lifted tid -> Waiters.singleton tid
        | _ -> Waiters.top ()
      in
      let handle_one b =
        man.sideg b (Capacity.bot (), tid);
        let addr = ValueDomain.Addr.of_var b in
        let (capacity, waiters) = man.global b in
        let relevant_waiters = Waiters.filter (fun tid -> true) waiters in
        let may_run =
          if Waiters.exists (fun tid -> not @@ TID.is_unique tid) relevant_waiters then
            true
          else
            let count = Waiters.cardinal relevant_waiters in
            match capacity with
            | `Lifted c -> 
              Z.geq (Z.of_int count) (BatOption.default Z.zero (Capacity.I.minimal c)) 
            | _ -> true
        in
        if may_run then
          (Barriers.add addr may, Barriers.add addr must)
        else
          D.bot ()
      in
      let (may, must) = List.fold_left (fun acc b-> D.join acc (handle_one b)) (D.bot ()) barriers in
      if Barriers.is_empty may then raise Analyses.Deadcode;
      (may, must)
    | BarrierInit { barrier; count } ->
      let count = man.ask (Queries.EvalInt count) in
      let publish_one b = man.sideg b (count, Waiters.bot ()) in 
      let barriers = possible_vinfos (Analyses.ask_of_man man) barrier in
      List.iter publish_one barriers;
      man.local
    | _ -> man.local

  let startstate v = (Barriers.empty (), Barriers.empty ())
  let threadenter man ~multiple lval f args = [man.local]
  let exitstate  v = (Barriers.empty (), Barriers.empty ())
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
