(** Joined threads analysis ([threadJoins]).

    @see <https://arxiv.org/abs/2301.06439> Schwarz, M., Saan, S., Seidl, H., Erhard, J., Vojdani, V. Clustered Relational Thread-Modular Abstract Interpretation with Local Traces. Appendix F. *)

open GoblintCil
open Analyses

module TID  = ThreadIdDomain.Thread
module TIDs = ConcDomain.ThreadSet
module MustTIDs = ConcDomain.MustThreadSet
module CleanExit = Queries.MustBool

module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "threadJoins"

  (* The first component is the set of must-joined TIDs, the second component tracks whether all TIDs recorded in MustTIDs have been exited cleanly, *)
  (* i.e., all created subthreads have also been joined. This is helpful as there is no set of all transitively created threads available. *)
  module D = Lattice.Prod(MustTIDs)(CleanExit)
  module C = D
  module G = D
  module V =
  struct
    include TID
    include StdV
  end

  (* transfer functions *)
  let threadreturn ctx =
    match ctx.ask CurrentThreadId with
    | `Lifted tid ->
      let (j,joined_clean) = ctx.local in
      (* the current thread has been exited cleanly if all joined threads where exited cleanly, and all created threads are joined *)
      let created = ctx.ask Queries.CreatedThreads in
      let clean = TIDs.subset created j in
      ctx.sideg tid (j, joined_clean && clean)
    | _ -> () (* correct? *)


  let return ctx (exp:exp option) (f:fundec) : D.t =
    if ThreadReturn.is_current (Analyses.ask_of_ctx ctx) then threadreturn ctx;
    ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special arglist, f.vname with
    | ThreadExit _, _ -> threadreturn ctx; ctx.local
    | ThreadJoin { thread = id; ret_var }, _ ->
      let threads = ctx.ask (Queries.EvalThread id) in
      if TIDs.is_top threads then
        ctx.local
      else (
        (* all elements are known *)
        let threads = TIDs.elements threads in
        match threads with
        | [tid] when TID.is_unique tid->
          let (local_joined, local_clean) = ctx.local in
          let (other_joined, other_clean) = ctx.global tid in
          (MustTIDs.union (MustTIDs.add tid local_joined) other_joined, local_clean && other_clean)
        | _ -> ctx.local (* if multiple possible thread ids are joined, none of them is must joined *)
        (* Possible improvement: Do the intersection first, things that are must joined in all possibly joined threads are must-joined *)
      )
    | Unknown, "__goblint_assume_join" ->
      let id = List.hd arglist in
      let threads = ctx.ask (Queries.EvalThread id) in
      if TIDs.is_top threads then (
        M.info ~category:Unsound "Unknown thread ID assume-joined, assuming ALL threads must-joined.";
        (MustTIDs.bot(), true) (* consider everything joined, MustTIDs is reversed so bot is All threads *)
      )
      else (
        (* all elements are known *)
        let threads = TIDs.elements threads in
        if List.compare_length_with threads 1 > 0 then
          M.info ~category:Unsound "Ambiguous thread ID assume-joined, assuming all of those threads must-joined.";
        List.fold_left (fun (joined, clean) tid ->
            let (other_joined, other_clean) = ctx.global tid in
            (MustTIDs.union (MustTIDs.add tid joined) other_joined, clean && other_clean)
          ) (ctx.local) threads
      )
    | _, _ -> ctx.local

  let threadspawn ctx ~multiple lval f args fctx =
    if D.is_bot ctx.local then ( (* bot is All threads *)
      M.info ~category:Imprecise "Thread created while ALL threads must-joined, continuing with no threads joined.";
      D.top () (* top is no threads *)
    )
    else
      match ThreadId.get_current (Analyses.ask_of_ctx fctx) with
      | `Lifted tid ->
        let (j, clean) = ctx.local in
        (MustTIDs.remove tid j, clean)
      | _ ->
        ctx.local

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MustJoinedThreads -> (fst ctx.local:ConcDomain.MustThreadSet.t) (* type annotation needed to avoid "would escape the scope of its equation" *)
    | Queries.ThreadsJoinedCleanly -> (snd ctx.local:bool)
    | _ ->  Queries.Result.top q

  let combine_env ctx lval fexp f args fc au f_ask =
    let (caller_joined, local_clean) = ctx.local in
    let (callee_joined, callee_clean) = au in
    (MustTIDs.union caller_joined callee_joined, local_clean && callee_clean)

  let startstate v = (MustTIDs.empty (), true)
  let exitstate  v = (MustTIDs.empty (), true)
end

let _ = MCP.register_analysis ~dep:["threadid"] (module Spec : MCPSpec)
