(** Thread creation and uniqueness analyses. *)

open Prelude.Ana
open Analyses

module T  = ThreadIdDomain.Thread
module TS = ConcDomain.ThreadSet

module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "thread"
  module D = ConcDomain.CreatedThreadSet
  module C = D
  module G = ConcDomain.ThreadCreation
  module V = T

  let should_join = D.equal
  module PS =
  struct
    include DefaultSpec.PS
    let cong = should_join
  end

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t = ctx.local
  let branch ctx (exp:exp) (tv:bool) : D.t =  ctx.local
  let body ctx (f:fundec) : D.t =  ctx.local
  let return ctx (exp:exp option) (f:fundec) : D.t =
    let tid = ThreadId.get_current (Analyses.ask_of_ctx ctx) in
    begin match tid with
      | `Lifted tid -> ctx.sideg tid (false, TS.bot (), not (D.is_empty ctx.local))
      | _ -> ()
    end;
    ctx.local
  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list = [ctx.local,ctx.local]
  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) : D.t = au

  let rec is_not_unique ctx tid =
    let (rep, parents, _) = ctx.global tid in
    let n = TS.cardinal parents in
    (* A thread is not unique if it is
      * a) repeatedly created,
      * b) created in multiple threads, or
      * c) created by a thread that is itself multiply created.
      * Note that starting threads have empty ancestor sets! *)
    rep || n > 1 || n > 0 && is_not_unique ctx (TS.choose parents)

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special arglist with
    | ThreadJoin { thread = id; ret_var } ->
      (* TODO: generalize ThreadJoin like ThreadCreate *)
      (* TODO: elements might throw an exception *)
      let threads = TS.elements (ctx.ask (Queries.EvalThread id)) in
      let has_clean_exit tid = not (BatTuple.Tuple3.third (ctx.global tid)) in
      let join_thread s tid =
        if has_clean_exit tid && not (is_not_unique ctx tid) then
          D.remove tid s
        else
          s
      in
      List.fold_left join_thread ctx.local threads
    | _ -> ctx.local

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MustBeUniqueThread -> begin
        let tid = ThreadId.get_current (Analyses.ask_of_ctx ctx) in
        match tid with
        | `Lifted tid -> not (is_not_unique ctx tid)
        | _ -> false
      end
    | Queries.MustBeSingleThreaded -> begin
        let tid = ThreadId.get_current (Analyses.ask_of_ctx ctx) in
        match tid with
        | `Lifted tid when T.is_main tid -> D.is_empty ctx.local
        | _ -> false
      end
    | _ -> Queries.Result.top q

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.bot ()]
  let threadspawn ctx lval f args fctx =
    let creator = ThreadId.get_current (Analyses.ask_of_ctx ctx) in
    let tid = ThreadId.get_current_unlift (Analyses.ask_of_ctx fctx) in
    let repeated = D.mem tid ctx.local in
    let eff =
      match creator with
      | `Lifted ctid -> (repeated, TS.singleton ctid, false)
      | `Top         -> (true,     TS.bot (),         false)
      | `Bot         -> (false,    TS.bot (),         false)
    in
    ctx.sideg tid eff;
    D.join ctx.local (D.singleton tid)
  let exitstate  v = D.bot ()
end

let _ = MCP.register_analysis ~dep:["threadid"] (module Spec : MCPSpec)
