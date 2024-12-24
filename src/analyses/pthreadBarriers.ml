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

  include Analyses.IdentitySpec
  module V = VarinfoV

  module Waiters = SetDomain.ToppedSet (MHP) (struct let topname = "All MHP" end)
  module G = Lattice.Prod (Queries.ID) (Waiters)

  let name () = "pthreadBarriers"
  module D = Lattice.Prod (Barriers) (MustBarriers)

  include Analyses.ValueContexts(D)

  let possible_vinfos (a: Queries.ask) barrier =
    Queries.AD.to_var_may (a.f (Queries.MayPointTo barrier))

  let special man (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LF.find f in
    match desc.special arglist with
    | BarrierWait barrier ->
      let may, must = man.local in
      let barriers = possible_vinfos (Analyses.ask_of_man man) barrier in
      let may = List.fold_left (fun may a -> Barriers.add (ValueDomain.Addr.of_var a) may) may barriers in
      let must = match barriers with
      | [a] -> Barriers.add (ValueDomain.Addr.of_var a) must
      | _ -> must
      in
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
