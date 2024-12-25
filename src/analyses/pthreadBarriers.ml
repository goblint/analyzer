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

  module MHPplusLock = struct
    module Locks = LockDomain.MustLockset

    include Printable.Prod (MHP) (Locks)
    let name () = "MHPplusLock"

    let mhp (mhp1, l1) (mhp2, l2) = 
      MHP.may_happen_in_parallel mhp1 mhp2 && Locks.is_empty (Locks.inter l1 l2)

    let tid ((mhp:MHP.t), _) = mhp.tid

    let may_be_non_unique_thread (mhp, _) = MHP.may_be_non_unique_thread mhp
  end

  module Waiters = SetDomain.ToppedSet (MHPplusLock) (struct let topname = "All MHP" end)
  module Multiprocess = BoolDomain.MayBool
  module G = Lattice.Prod3 (Multiprocess) (Capacity) (Waiters)

  let name () = "pthreadBarriers"

  module MustObserved = MapDomain.MapTop_LiftBot (TID) (MustBarriers)
  module D = Lattice.Prod (Barriers) (MustObserved)

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
      let mhp = MHP.current ask in
      let handle_one b =
        let locks = man.ask (Queries.MustLockset) in
        man.sideg b (Multiprocess.bot (), Capacity.bot (), Waiters.singleton (mhp, locks));
        let addr = ValueDomain.Addr.of_var b in
        let (multiprocess,capacity, waiters) = man.global b in
        let may_run, must =
          if multiprocess then
            true, must
          else
            let relevant_waiters = Waiters.filter (fun other -> MHPplusLock.mhp (mhp, locks) other) waiters in    
            if Waiters.exists MHPplusLock.may_be_non_unique_thread relevant_waiters then
              true, must
            else
              let count = Waiters.cardinal relevant_waiters in
              match capacity with
              | `Lifted c -> 
                (* Add 1 as the thread calling wait at the moment will not be MHP with itself *)
                let min_cap = (BatOption.default Z.zero (Capacity.I.minimal c)) in
                if Z.leq min_cap Z.one then
                  true, must
                else if min_cap = Z.of_int 2 && count = 1 then
                  let elem = Waiters.choose relevant_waiters in
                  let curr = MustObserved.find (MHPplusLock.tid elem) must in
                  let must' = MustObserved.add (MHPplusLock.tid elem) (Barriers.add addr curr) must in
                  true, must'
                else if min_cap = Z.of_int 2 && count >= 1 then
                  true, must
                else if Z.geq (Z.of_int (count + 1)) min_cap then
                  (* This is quite a cute problem: Do (min_cap-1) elements exist in the set such that 
                     MHP is pairwise true? This solution is a sledgehammer, there should be something much
                     better algorithmically (beyond just laziness) *)
                  (
                    let waiters = Waiters.elements relevant_waiters in
                    let min_cap = Z.to_int min_cap in
                    let lists = List.init (min_cap - 1) (fun _ -> waiters) in
                    let candidates = BatList.n_cartesian_product lists in
                    let pred = List.exists (fun candidate ->
                        let rec do_it = function
                          | [] -> true
                          | x::xs -> List.for_all (fun y -> MHPplusLock.mhp x y) xs  && do_it xs
                        in
                        do_it candidate
                      ) candidates
                    in
                    if pred then
                      (* limit to this case to avoid having to construct all permutations above *)
                      let must = if (List.length waiters) = min_cap-1 then
                          List.fold_left (fun acc elem -> 
                              let tid = MHPplusLock.tid elem in
                              let curr = MustObserved.find tid acc in
                              let must' = MustObserved.add tid (Barriers.add addr curr) acc in
                              must'
                            ) must waiters
                        else 
                          must
                      in
                      true, must
                    else
                      false, must
                  )
                else
                  false, must
              | _ -> true, must
        in
        if may_run then
          (Barriers.add addr may, must)
        else
          D.bot ()
      in
      let (may, must) = List.fold_left (fun acc b-> D.join acc (handle_one b)) (D.bot ()) barriers in
      if Barriers.is_empty may then raise Analyses.Deadcode;
      (may, must)
    | BarrierInit { barrier; attr; count } ->
      let multitprocess = not @@ Queries.AD.is_null @@ man.ask (Queries.MayPointTo attr) in
      if multitprocess then M.warn "Barrier initialized with a non-NULL attr argument. Handled as if PTHREAD_PROCESS_SHARED potentially set.";
      let count = man.ask (Queries.EvalInt count) in
      let publish_one b = man.sideg b (multitprocess, count, Waiters.bot ()) in 
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
