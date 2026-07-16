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

  module TID = ThreadIdDomain.ThreadLifted

  (* Tracking TID separately, as there is no type-safe way to get it from MCPATuple *)
  module MCPAPlusTID = struct
    include Printable.Prod (MCPAccess.A) (TID)
    let name () = "MCPAPlusTID"
  end

  let part_access man: MCPAccess.A.t =
    Obj.obj (man.ask (PartAccess Point))

  module Waiters = SetDomain.ToppedSet (MCPAPlusTID) (struct let topname = "All MHP" end)
  module Multiprocess = BoolDomain.MayBool
  module G = Lattice.Prod3 (Multiprocess) (Capacity) (Waiters)

  let name () = "pthreadBarriers"

  module MustObserved = MapDomain.MapTop_LiftBot (TID) (MustBarriers)
  module D = Lattice.Prod (Barriers) (MustObserved)

  include Analyses.ValueContexts(D)

  let possible_vinfos (a: Queries.ask) barrier =
    Queries.AD.to_var_may (a.f (Queries.MayPointTo barrier))

  let special man (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let exists_k pred k waiters =
      let k_product elems =
        let rec doit k =
          if k = 1 then
            Seq.map (Seq.return) elems
          else
            let arg = doit (k-1) in
            Seq.map_product (Seq.cons) elems arg
        in
        doit k
      in
      Seq.exists pred (k_product (Waiters.to_seq waiters))
    in
    let desc = LF.find f in
    match desc.special arglist with
    | BarrierWait barrier ->
      let ask = Analyses.ask_of_man man in
      let may, must = man.local in
      let mcpa = part_access man in
      let tid = ThreadId.get_current ask in
      let barriers = possible_vinfos ask barrier in
      let handle_one b =
        try
          man.sideg b (Multiprocess.bot (), Capacity.bot (), Waiters.singleton (mcpa, tid));
          let addr = ValueDomain.Addr.of_var b in
          let (multiprocess, capacity, waiters) = man.global b in
          let may = Barriers.add addr may in
          if multiprocess then
            (may, must)
          else
            let relevant_waiters = Waiters.filter (fun (othermcpa, _) -> MCPAccess.A.may_race mcpa othermcpa) waiters in
            if Waiters.exists (fun (t,_) -> MCPAccess.A.may_race t t) relevant_waiters then
              (may, must)
            else
              match capacity with
              | `Top | `Bot -> (may, must)
              | `Lifted c ->
                let count = Waiters.cardinal relevant_waiters in
                (* Add 1 as the thread calling wait at the moment will not be MHP with itself *)
                let min_cap = (BatOption.default Z.zero (Capacity.I.minimal c)) in
                if Z.leq min_cap Z.one then
                  (may, must)
                else if Z.geq (Z.of_int (count + 1)) min_cap then
                  (* This is quite a cute problem: Do (min_cap-1) elements exist in the set such that
                      MHP is pairwise true? This solution is a sledgehammer, there should be something much
                      better algorithmically (beyond just laziness)
                      Simmo: This sounds like looking for a (min_cap - 1)-clique in the MHP graph with the set of nodes.
                        That seems to be exponential in min_cap though: https://en.wikipedia.org/wiki/Clique_problem#Cliques_of_fixed_size.
                  *)
                  let must =
                    let waiters = Waiters.elements relevant_waiters in
                    let min_cap = Z.to_int min_cap in
                    let can_proceed_pred seq =
                      let rec doit seq = match Seq.uncons seq with
                        | None -> true
                        | Some ((h,_), t) -> Seq.for_all (fun (x,_) -> MCPAccess.A.may_race h x) t && (doit [@tailcall]) t
                      in
                      doit seq
                    in
                    let can_proceed = exists_k can_proceed_pred (min_cap - 1) relevant_waiters in
                    if not can_proceed then raise Analyses.Deadcode;
                    (* limit to this case to avoid having to construct all permutations above *)
                    if BatList.compare_length_with waiters (min_cap - 1) = 0 then
                      List.fold_left (fun acc (_,tid) ->
                          let curr = MustObserved.find tid acc in
                          let must' = MustObserved.add tid (Barriers.add addr curr) acc in
                          must'
                        ) must waiters
                    else
                      must
                  in
                  (may, must)
                else
                  raise Analyses.Deadcode;
        with Analyses.Deadcode -> D.bot ()
      in
      let (may, must) = List.fold_left (fun acc b-> D.join acc (handle_one b)) (D.bot ()) barriers in
      if Barriers.is_empty may then raise Analyses.Deadcode;
      (may, must)
    | BarrierInit { barrier; attr; count } ->
      let multiprocess = not @@ Queries.AD.is_null @@ man.ask (Queries.MayPointTo attr) in
      if multiprocess then M.warn "Barrier initialized with a non-NULL attr argument. Handled as if PTHREAD_PROCESS_SHARED potentially set.";
      let count = man.ask (Queries.EvalInt count) in
      let publish_one b = man.sideg b (multiprocess, count, Waiters.bot ()) in
      let barriers = possible_vinfos (Analyses.ask_of_man man) barrier in
      List.iter publish_one barriers;
      man.local
    | _ -> man.local

  let startstate v = (Barriers.empty (), MustObserved.empty ())
  let threadenter man ~multiple lval f args = [man.local]
  let exitstate  v = (Barriers.empty (), MustObserved.empty ())

  module A =
  struct
    include Lattice.Prod3 (Barriers) (MustObserved) (TID)
    let name () = "barriers"
    let may_race (may_await_t1, must_observed_by_t1, t1) (may_await_t2, must_observed_by_t2, t2) =
      let observed_from_t2 = MustObserved.find t2 must_observed_by_t1 in
      if not (Barriers.subset observed_from_t2 may_await_t2) then
        false
      else
        let observed_from_t1 = MustObserved.find t1 must_observed_by_t2 in
        Barriers.subset observed_from_t1 may_await_t1
    let should_print f = true
  end

  let access man (a: Queries.access) =
    let (may,must) = man.local in
    let mhp = MHP.current (Analyses.ask_of_man man) in
    (may, must, mhp.tid)
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
