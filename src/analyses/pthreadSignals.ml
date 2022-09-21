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
  module G = ConcDomain.ThreadSet

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
      let tid = match ctx.ask CurrentThreadId with
        | `Lifted tid -> G.singleton (tid)
        | _ -> G.top ()
      in
      let publish_one a = ctx.sideg a tid in
      let possible_vars = possible_vinfos (Analyses.ask_of_ctx ctx) cond in
      List.iter publish_one possible_vars;
      ctx.local
    | Wait {cond = cond; _} ->
      let may_be_signaller tid other =
        let module TID = ThreadIdDomain.FlagConfiguredTID in
        let not_self_signal = (not (TID.is_unique tid)) || (not (TID.equal tid other)) in
        let may_be_running () =
          not @@ MHP.definitely_not_started (tid, ctx.ask Queries.CreatedThreads) other
        in
        let may_not_be_joined () =
          not @@ MHP.must_be_joined other (ctx.ask Queries.MustJoinedThreads)
        in
        not_self_signal && may_be_running () && may_not_be_joined ()
      in
      (match ctx.ask CurrentThreadId with
       | `Lifted tid ->
         (match possible_vinfos (Analyses.ask_of_ctx ctx) cond with
          | [a] ->
            (* Only report if there is one definite condition variable for which wait is called *)
            let signalling_tids = ctx.global a in
            if G.is_top signalling_tids then
              ctx.local
            else if G.is_empty signalling_tids then
              (M.warn ~category:Deadcode "The condition variable %s is never signalled, succeeding code is live due to spurious wakeups only!" a.vname; ctx.local)
            else if G.exists (may_be_signaller tid) signalling_tids then
              Signals.add (ValueDomain.Addr.from_var a) ctx.local
            else
              (M.warn ~category:Deadcode "The condition variable %s is never signalled concurrently, succeeding code is live due to spurious wakeups only!" a.vname; ctx.local)
          | _ -> ctx.local)
       | _ -> ctx.local)
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
