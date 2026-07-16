(** Must received signals analysis for Pthread condition variables ([pthreadSignals]). *)

open GoblintCil
open Analyses
module LF = LibraryFunctions

module Spec : Analyses.MCPSpec =
struct
  module Signals = SetDomain.ToppedSet (ValueDomain.Addr) (struct let topname = "All signals" end)
  module MustSignals = Lattice.Reverse (Signals)

  include Analyses.IdentitySpec
  module V = VarinfoV

  let name () = "pthreadSignals"
  module D = MustSignals
  include Analyses.ValueContexts(D)
  module G = SetDomain.ToppedSet (MHP) (struct let topname = "All Threads" end)

  let possible_vinfos (a: Queries.ask) cv_arg =
    Queries.AD.to_var_may (a.f (Queries.MayPointTo cv_arg))

  (* transfer functions *)

  let special man (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LF.find f in
    match desc.special arglist with
    | Signal cond
    | Broadcast cond ->
      let mhp = G.singleton @@ MHP.current (Analyses.ask_of_man man) in
      let publish_one a = man.sideg a mhp in
      let possible_vars = possible_vinfos (Analyses.ask_of_man man) cond in
      List.iter publish_one possible_vars;
      man.local
    | Wait {cond = cond; _} ->
      let current_mhp = MHP.current (Analyses.ask_of_man man) in
      let module Signalled = struct
        type signalled = Never | NotConcurrently | PossiblySignalled

        let (|||) a b = match a,b with
          | PossiblySignalled, _
          | _, PossiblySignalled -> PossiblySignalled
          | NotConcurrently , _
          | _, NotConcurrently -> NotConcurrently
          | Never, Never -> Never

        let can_be_signalled a =
          let signalling_tids = man.global a in
          if G.is_top signalling_tids then
            PossiblySignalled
          else if G.is_empty signalling_tids then
            Never
          else if not @@ G.exists (MHP.may_happen_in_parallel current_mhp) signalling_tids then
            NotConcurrently
          else
            PossiblySignalled
      end
      in
      let open Signalled in
      let add_if_singleton conds = match conds with | [a] -> Signals.add (ValueDomain.Addr.of_var a) man.local | _ -> man.local in
      let conds = possible_vinfos (Analyses.ask_of_man man) cond in
      (match List.fold_left (fun acc cond -> can_be_signalled cond ||| acc) Never conds with
       | PossiblySignalled -> add_if_singleton conds
       | NotConcurrently ->
         (M.warn ~category:Deadcode "The condition variable(s) pointed to by %a are never signalled concurrently, succeeding code is live due to spurious wakeups only!" Basetype.CilExp.pretty cond; man.local)
       | Never ->
         (M.warn ~category:Deadcode "The condition variable(s) pointed to by %a are never signalled, succeeding code is live due to spurious wakeups only!" Basetype.CilExp.pretty cond; man.local)
      )

    | TimedWait _ ->
      (* Time could simply have elapsed *)
      man.local
    | _ -> man.local

  let startstate v = Signals.empty ()
  let threadenter man ~multiple lval f args = [man.local]
  let exitstate  v = Signals.empty ()
end

let _ =
  MCP.register_analysis ~dep:["mutex"] (module Spec : MCPSpec)
