(** Thread creation and uniqueness analyses. *)

open Prelude.Ana
open Analyses

module T  = ConcDomain.Thread
module TS = ConcDomain.ThreadSet

module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "thread"
  module D = ConcDomain.CreatedThreadSet
  module C = D
  module G = ConcDomain.ThreadCreation

  let should_join = D.equal

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
  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list = [ctx.local,ctx.local]
  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t = au

  (* Helper function to convert query-offsets to valuedomain-offsets *)
  let rec conv_offset x =
    match x with
    | `NoOffset    -> `NoOffset
    | `Index (Const (CInt64 (i,ikind,s)),o) -> `Index (IntDomain.of_const (i,ikind,s), conv_offset o)
    | `Index (_,o) -> `Index (ValueDomain.IndexDomain.top (), conv_offset o)
    | `Field (f,o) -> `Field (f, conv_offset o)

  let eval_exp_addr (a: Queries.ask) exp =
    let gather_addr (v,o) b = ValueDomain.Addr.from_var_offset (v,conv_offset o) :: b in
    match a.f (Queries.MayPointTo exp) with
    | LvalSet a when not (Queries.LS.is_top a)
                   && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) a) ->
      Queries.LS.fold gather_addr (Queries.LS.remove (dummyFunDec.svar, `NoOffset) a) []
    | _ -> []

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
    match LibraryFunctions.classify f.vname arglist with
    | `ThreadJoin (id, ret_var) ->
      (* TODO: generalize ThreadJoin like ThreadCreate *)
      let ids = eval_exp_addr (Analyses.ask_of_ctx ctx) id in
      let threads = List.concat (List.map ValueDomain.Addr.to_var_may ids) in
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
        | `Lifted tid -> MustBool (not (is_not_unique ctx tid))
        | _ -> MustBool false
      end
    | Queries.MustBeSingleThreaded -> begin
        let tid = ThreadId.get_current (Analyses.ask_of_ctx ctx) in
        match tid with
        | `Lifted {vname="main"; _} -> MustBool (D.is_empty ctx.local)
        | _ -> MustBool false
      end
    | _ -> Queries.Result.top ()

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

(* really stupid thread-ids *)
module StartLocIDs =
struct
  include Analyses.DefaultSpec

  let name () = "thread-id-location"
  module D = ConcDomain.ThreadStringSet
  module C = D
  module G = Lattice.Unit

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t = ctx.local
  let branch ctx (exp:exp) (tv:bool) : D.t =  ctx.local
  let body ctx (f:fundec) : D.t =  ctx.local
  let return ctx (exp:exp option) (f:fundec) : D.t = ctx.local
  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list = [ctx.local,ctx.local]
  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t = ctx.local
  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t = ctx.local

  let main = D.singleton "main"
  let startstate v = main
  let exitstate  v = D.top ()

  let threadenter ctx lval f args =
    let location x = let l = !Tracing.current_loc in l.file ^ ":" ^ string_of_int l.line ^ ":" ^ x.vname in
    [D.singleton (location f)]

  let threadspawn ctx lval f args fctx = ctx.local
end

let _ = MCP.register_analysis (module StartLocIDs : MCPSpec)
let _ = MCP.register_analysis ~dep:["threadid"] (module Spec : MCPSpec)
