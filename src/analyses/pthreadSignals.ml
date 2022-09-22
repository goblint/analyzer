(** Analysis of must-received pthread_signals. *)

open Prelude.Ana
open Analyses
module LF = LibraryFunctions

module Spec : Analyses.MCPSpec =
struct
  module Signals = SetDomain.ToppedSet (ValueDomain.Addr) (struct let topname = "All signals" end)
  module MustSignals = Lattice.Reverse (Signals)

  include Analyses.DefaultSpec
  module V = VarinfoV

  let name () = "pthreadSignals"
  module D = MustSignals
  module C = MustSignals
  module G = SetDomain.ToppedSet (MHP) (struct let topname = "All Threads" end)

  let rec conv_offset x =
    match x with
    | `NoOffset    -> `NoOffset
    | `Index (Const (CInt (i,_,s)),o) -> `Index (IntDomain.of_const (i,Cilfacade.ptrdiff_ikind (),s), conv_offset o)
    | `Index (_,o) -> `Index (ValueDomain.IndexDomain.top (), conv_offset o)
    | `Field (f,o) -> `Field (f, conv_offset o)

  let eval_exp_addr (a: Queries.ask) exp =
    let gather_addr (v,o) b = ValueDomain.Addr.from_var_offset (v,conv_offset o) :: b in
    match a.f (Queries.MayPointTo exp) with
    | a when not (Queries.LS.is_top a) && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) a) ->
      Queries.LS.fold gather_addr (Queries.LS.remove (dummyFunDec.svar, `NoOffset) a) []
    | _ -> []

  let possible_vinfos a cv_arg =
    List.filter_map ValueDomain.Addr.to_var_may (eval_exp_addr a cv_arg)

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [ctx.local, ctx.local]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) : D.t =
    au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LF.find f in
    match desc.special arglist with
    | Signal cond
    | Broadcast cond ->
      let mhp = G.singleton @@ MHP.current (Analyses.ask_of_ctx ctx) in
      let publish_one a = ctx.sideg a mhp in
      let possible_vars = possible_vinfos (Analyses.ask_of_ctx ctx) cond in
      List.iter publish_one possible_vars;
      ctx.local
    | Wait {cond = cond; _} ->
      let current_mhp = MHP.current (Analyses.ask_of_ctx ctx) in
      let module Signalled = struct
        type signalled = Never | NotConcurrently | PossiblySignalled

        let (|||) a b = match a,b with
          | PossiblySignalled, _
          | _, PossiblySignalled -> PossiblySignalled
          | NotConcurrently , _
          | _, NotConcurrently -> NotConcurrently
          | Never, Never -> Never

        let can_be_signalled a =
          let signalling_tids = ctx.global a in
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
      let add_if_singleton conds = match conds with | [a] -> Signals.add (ValueDomain.Addr.from_var a) ctx.local | _ -> ctx.local in
      let conds = possible_vinfos (Analyses.ask_of_ctx ctx) cond in
      (match List.fold_left (fun acc cond -> can_be_signalled cond ||| acc) Never conds with
       | PossiblySignalled -> add_if_singleton conds
       | NotConcurrently ->
         (M.warn ~category:Deadcode "The condition variable(s) pointed to by %s are never signalled concurrently, succeeding code is live due to spurious wakeups only!" (Basetype.CilExp.show cond); ctx.local)
       | Never ->
         (M.warn ~category:Deadcode "The condition variable(s) pointed to by %s are never signalled, succeeding code is live due to spurious wakeups only!" (Basetype.CilExp.show cond); ctx.local)
      )

    | TimedWait _ ->
      (* Time could simply have elapsed *)
      ctx.local
    | _ -> ctx.local

  let startstate v = Signals.empty ()
  let threadenter ctx lval f args = [ctx.local]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = Signals.empty ()
end

let _ =
  MCP.register_analysis ~dep:["mutex"] (module Spec : MCPSpec)
