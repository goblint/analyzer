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
  include Analyses.ValueContexts(D)
  module G = ConcDomain.ThreadCreation
  module V =
  struct
    include T
    include StdV
    let is_category x c = match c with | Variables.Concurrency -> true | _ -> false
  end
  module P = IdentityP (D)

  (* transfer functions *)
  let handle_thread_return man (exp: exp option) =
    let tid = ThreadId.get_current (Analyses.ask_of_man man) in
    match tid with
    | `Lifted tid -> man.sideg tid (false, TS.bot (), not (D.is_empty man.local))
      | _ -> ()

  let return man (exp:exp option) _ : D.t =
    if man.ask Queries.MayBeThreadReturn then
      handle_thread_return man exp;
    man.local

  let rec is_not_unique man tid =
    let (rep, parents, _) = man.global tid in
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
        T.equal tid parent || is_not_unique man parent (* equal check needed to avoid infinte self-recursion *)
      )
      else
        false (* no ancestors, starting thread *)
    )

  let special man (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special arglist with
    | ThreadJoin { thread = id; ret_var } ->
      (* TODO: generalize ThreadJoin like ThreadCreate *)
      (let has_clean_exit tid = not (BatTuple.Tuple3.third (man.global tid)) in
       let tids = man.ask (Queries.EvalThread id) in
       let join_thread s tid =
         if has_clean_exit tid && not (is_not_unique man tid) then
           D.remove tid s
         else
           s
       in
       if TS.is_top tids
       then man.local
       else match TS.elements tids with
         | [t] -> join_thread man.local t (* single thread *)
         | _ -> man.local (* if several possible threads are may-joined, none are must-joined *))
    | ThreadExit { ret_val } ->
      handle_thread_return man (Some ret_val);
      man.local
    | _ -> man.local

  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MustBeUniqueThread -> begin
        let tid = ThreadId.get_current (Analyses.ask_of_man man) in
        match tid with
        | `Lifted tid -> not (is_not_unique man tid)
        | _ -> false
      end
    | Queries.MustBeSingleThreaded {since_start = false} -> begin
        let tid = ThreadId.get_current (Analyses.ask_of_man man) in
        match tid with
        | `Lifted tid when T.is_main tid ->
          (* This analysis cannot tell if we are back in single-threaded mode or never left it. *)
          D.is_empty man.local
        | _ -> false
      end
    | _ -> Queries.Result.top q

  let startstate v = D.bot ()

  let threadenter man ~multiple lval f args =
    (* ctx is of creator, side-effects to denote non-uniqueness are performed in threadspawn *)
    [D.bot ()]

  let threadspawn man ~multiple lval f args fman =
    let creator = ThreadId.get_current (Analyses.ask_of_man man) in
    let tid = ThreadId.get_current_unlift (Analyses.ask_of_man fman) in
    let repeated = D.mem tid man.local in
    let eff =
      match creator with
      | `Lifted ctid -> (repeated || multiple, TS.singleton ctid, false)
      | `Top         -> (true, TS.bot (), false)
      | `Bot         -> (multiple, TS.bot (), false)
    in
    man.sideg tid eff;
    D.join man.local (D.singleton tid)
  let exitstate  v = D.bot ()
end

let _ = MCP.register_analysis ~dep:["threadid"] (module Spec : MCPSpec)
