(** Joined threads analysis ([threadJoins]). *)

open GoblintCil
open Analyses

module TID  = ThreadIdDomain.Thread
module TIDs = ConcDomain.ThreadSet
module MustTIDs = ConcDomain.MustThreadSet

module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "threadJoins"
  module D = MustTIDs
  module C = D
  module G = MustTIDs
  module V =
  struct
    include TID
    include StdV
  end

  (* transfer functions *)
  let return ctx (exp:exp option) (f:fundec) : D.t =
    (
      match ctx.ask CurrentThreadId with
      | `Lifted tid when ThreadReturn.is_current (Analyses.ask_of_ctx ctx) -> ctx.sideg tid ctx.local
      | _ -> () (* correct? *)
    );
    ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special arglist, f.vname with
    | ThreadExit _, _ -> (match ctx.ask CurrentThreadId with
        | `Lifted tid -> ctx.sideg tid ctx.local
        | _ -> () (* correct? *)
      );
      ctx.local
    | ThreadJoin { thread = id; ret_var }, _ ->
      let threads = ctx.ask (Queries.EvalThread id) in
      if TIDs.is_top threads then
        ctx.local
      else (
        (* elements throws if the thread set is top *)
        let threads = TIDs.elements threads in
        match threads with
        | [tid] when TID.is_unique tid->
          let joined = ctx.global tid in
          D.union (D.add tid ctx.local) joined
        | _ -> ctx.local (* if multiple possible thread ids are joined, none of them is must joined*)
        (* Possible improvement: Do the intersection first, things that are must joined in all possibly joined threads are must-joined *)
      )
    | Unknown, "__goblint_assume_join" ->
      let id = List.hd arglist in
      let threads = ctx.ask (Queries.EvalThread id) in
      if TIDs.is_top threads then (
        M.info ~category:Unsound "Unknown thread ID assume-joined, assuming ALL threads must-joined.";
        D.bot () (* consider everything joined, D is reversed so bot is All threads *)
      )
      else (
        (* elements throws if the thread set is top *)
        let threads = TIDs.elements threads in
        if List.compare_length_with threads 1 > 0 then
          M.info ~category:Unsound "Ambiguous thread ID assume-joined, assuming all of those threads must-joined.";
        List.fold_left (fun acc tid ->
            let joined = ctx.global tid in
            D.union (D.add tid acc) joined
          ) ctx.local threads
      )
    | _, _ -> ctx.local

  let threadspawn ctx lval f args fctx =
    if D.is_bot ctx.local then ( (* bot is All threads *)
      M.info ~category:Imprecise "Thread created while ALL threads must-joined, continuing with no threads joined.";
      D.top () (* top is no threads *)
    )
    else
      match ThreadId.get_current (Analyses.ask_of_ctx fctx) with
      | `Lifted tid ->
        D.remove tid ctx.local
      | _ ->
        ctx.local

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MustJoinedThreads -> (ctx.local:ConcDomain.MustThreadSet.t) (* type annotation needed to avoid "would escape the scope of its equation" *)
    | _ ->  Queries.Result.top q

  let combine_env ctx lval fexp f args fc au f_ask =
    D.union ctx.local au

  let startstate v = D.top ()
  let exitstate  v = D.top ()
end

let _ = MCP.register_analysis ~dep:["threadid"] (module Spec : MCPSpec)
