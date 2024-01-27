(** Created threads and their uniqueness analysis ([thread]). *)

open GoblintCil
open Analyses

module T  = ThreadIdDomain.Thread
module TS = ConcDomain.ThreadSet

module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "thread"
  module D = ConcDomain.CreatedThreadSet
  module C = D
  module G = ConcDomain.ThreadCreation
  module V =
  struct
    include T
    include StdV
  end
  module P = IdentityP (D)

  (* transfer functions *)
  let handle_thread_return ctx (exp: exp option) =
    let tid = ThreadId.get_current (Analyses.ask_of_ctx ctx) in
    match tid with
      | `Lifted tid -> ctx.sideg tid (false, TS.bot (), not (D.is_empty ctx.local))
      | _ -> ()

  let return ctx (exp:exp option) _ : D.t =
    if ctx.ask Queries.MayBeThreadReturn then
      handle_thread_return ctx exp;
    ctx.local

  let rec is_not_unique ctx tid =
    let (rep, parents, _) = ctx.global tid in
    if rep then
      true (* repeatedly created *)
    else (
      let n = TS.cardinal parents in
      if n > 1 then
        true (* created in multiple threads *)
      else if n > 0 then (
        (* created by single thread *)
        let parent = TS.choose parents in
        (* created by itself thread-recursively or by a thread that is itself multiply created *)
        T.equal tid parent || is_not_unique ctx parent (* equal check needed to avoid infinte self-recursion *)
      )
      else
        false (* no ancestors, starting thread *)
    )

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special arglist with
    | ThreadJoin { thread = id; ret_var } ->
      (* TODO: generalize ThreadJoin like ThreadCreate *)
      (let has_clean_exit tid = not (BatTuple.Tuple3.third (ctx.global tid)) in
       let tids = ctx.ask (Queries.EvalThread id) in
       let join_thread s tid =
         if has_clean_exit tid && not (is_not_unique ctx tid) then
           D.remove tid s
         else
           s
       in
       if TS.is_top tids
       then ctx.local
       else match TS.elements tids with
         | [t] -> join_thread ctx.local t (* single thread *)
         | _ -> ctx.local (* if several possible threads are may-joined, none are must-joined *))
    | ThreadExit { ret_val } ->
      handle_thread_return ctx (Some ret_val);
      ctx.local
    | _ -> ctx.local

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MustBeUniqueThread -> begin
        let tid = ThreadId.get_current (Analyses.ask_of_ctx ctx) in
        match tid with
        | `Lifted tid -> not (is_not_unique ctx tid)
        | _ -> false
      end
    | Queries.MustBeSingleThreaded {since_start = false} -> begin
        let tid = ThreadId.get_current (Analyses.ask_of_ctx ctx) in
        match tid with
        | `Lifted tid when T.is_main tid ->
          (* This analysis cannot tell if we are back in single-threaded mode or never left it. *)
          D.is_empty ctx.local
        | _ -> false
      end
    | _ -> Queries.Result.top q

  let startstate v = D.bot ()

  let threadenter ctx ~multiple lval f args =
    if multiple then
      (let tid = ThreadId.get_current_unlift (Analyses.ask_of_ctx ctx) in
       ctx.sideg tid (true, TS.bot (), false));
    [D.bot ()]

  let threadspawn ctx ~multiple lval f args fctx =
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
