(** Escape analysis for thread-local variables ([escape]). *)

open GoblintCil
open Analyses

module M = Messages
module AD = Queries.AD

let has_escaped (ask: Queries.ask) (v: varinfo): bool =
  assert (not v.vglob);
  if not v.vaddrof then
    false (* Cannot have escaped without taking address. Override provides extra precision for degenerate ask in base eval_exp used for partitioned arrays. *)
  else
    ask.f (Queries.MayEscape v)

module Spec =
struct
  include Analyses.IdentitySpec

  module ThreadIdSet = SetDomain.Make (ThreadIdDomain.ThreadLifted)

  let name () = "escape"
  module D = EscapeDomain.EscapedVars
  module C = EscapeDomain.EscapedVars
  module V = VarinfoV
  module G = ThreadIdSet

  let reachable (ask: Queries.ask) e: D.t =
    match ask.f (Queries.ReachableFrom e) with
    | ad when not (Queries.AD.is_top ad) ->
      let to_extra addr set =
        match addr with
        | Queries.AD.Addr.Addr (v,_) -> D.add v set
        | _ -> set
      in
      Queries.AD.fold to_extra ad (D.empty ())
    (* Ignore soundness warnings, as invalidation proper will raise them. *)
    | ad ->
      if M.tracing then M.tracel "escape" "reachable %a: %a\n" d_exp e Queries.AD.pretty ad;
      D.empty ()

  let mpt (ask: Queries.ask) e: D.t =
    match ask.f (Queries.MayPointTo e) with
    | ad when not (AD.is_top ad) ->
      let to_extra addr set =
        match addr with
        | AD.Addr.Addr (v,_) -> D.add v set
        | _ -> set
      in
      AD.fold to_extra (AD.remove (UnknownPtr ()) ad) (D.empty ())
    (* Ignore soundness warnings, as invalidation proper will raise them. *)
    | ad ->
      if M.tracing then M.tracel "escape" "mpt %a: %a\n" d_exp e AD.pretty ad;
      D.empty ()

  let thread_id ctx =
    ctx.ask Queries.CurrentThreadId

  (** Emit an escape event:
      Only necessary when code has ever been multithreaded,
      or when about to go multithreaded. *)
  let emit_escape_event ctx escaped =
    (* avoid emitting unnecessary event *)
    if not (D.is_empty escaped) then
      ctx.emit (Events.Escape escaped)

  (** Side effect escapes: In contrast to the emitting the event, side-effecting is
      necessary in single threaded mode, since we rely on escape status in Base
      for passing locals reachable via globals *)
  let side_effect_escape ctx escaped threadid =
    let threadid = G.singleton threadid in
    D.iter (fun v ->
        ctx.sideg v threadid) escaped

  (* queries *)
  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MayEscape v ->
      let threads = ctx.global v in
      if ThreadIdSet.is_empty threads then
        false
      else begin
        let possibly_started current = function
          | `Lifted tid ->
            let threads = ctx.ask Queries.CreatedThreads in
            let not_started = MHP.definitely_not_started (current, threads) tid in
            let possibly_started = not not_started in
            possibly_started
          | `Top -> true
          | `Bot -> false
        in
        let equal_current current = function
          | `Lifted tid ->
            ThreadId.Thread.equal current tid
          | `Top -> true
          | `Bot -> false
        in
        match ctx.ask Queries.CurrentThreadId with
        | `Lifted current ->
          let possibly_started = ThreadIdSet.exists (possibly_started current) threads in
          if possibly_started then
            true
          else
            let current_is_unique = ThreadId.Thread.is_unique current in
            let any_equal_current threads = ThreadIdSet.exists (equal_current current) threads in
            if not current_is_unique && any_equal_current threads then
              (* Another instance of the non-unqiue current thread may have escaped the variable *)
              true
            else
              (* Check whether current unique thread has escaped the variable *)
              D.mem v ctx.local
        | `Top ->
          true
        | `Bot ->
          M.warn ~category:MessageCategory.Analyzer "CurrentThreadId is bottom.";
          false
      end
    | _ -> Queries.Result.top q

  let escape_rval ctx (rval:exp) =
    let ask = Analyses.ask_of_ctx ctx in
    let escaped = reachable ask rval in
    let escaped = D.filter (fun v -> not v.vglob) escaped in

    let thread_id = thread_id ctx in
    side_effect_escape ctx escaped thread_id;
    if ThreadFlag.has_ever_been_multi ask then (* avoid emitting unnecessary event *)
      emit_escape_event ctx escaped;
    escaped

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    let ask = Analyses.ask_of_ctx ctx in
    let vs = mpt ask (AddrOf lval) in
    if D.exists (fun v -> v.vglob || has_escaped ask v) vs then (
      let escaped = escape_rval ctx rval in
      D.join ctx.local escaped
    ) else begin
      ctx.local
    end

  let special ctx (lval: lval option) (f:varinfo) (args:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special args, f.vname, args with
    | _, "pthread_setspecific" , [key; pt_value] ->
      let escaped = escape_rval ctx pt_value in
      D.join ctx.local escaped
    | _ -> ctx.local

  let startstate v = D.bot ()
  let exitstate  v = D.bot ()

  let threadenter ctx lval f args =
    [D.bot ()]

  let threadspawn ctx lval f args fctx =
    D.join ctx.local @@
    match args with
    | [ptc_arg] ->
      (* not reusing fctx.local to avoid unnecessarily early join of extra *)
      let escaped = reachable (Analyses.ask_of_ctx ctx) ptc_arg in
      let escaped = D.filter (fun v -> not v.vglob) escaped in
      if M.tracing then M.tracel "escape" "%a: %a\n" d_exp ptc_arg D.pretty escaped;
      let thread_id = thread_id ctx in
      emit_escape_event ctx escaped;
      side_effect_escape ctx escaped thread_id;
      escaped
    | _ -> D.bot ()

  let event ctx e octx =
    match e with
    | Events.EnterMultiThreaded ->
      let escaped = ctx.local in
      let thread_id = thread_id ctx in
      emit_escape_event ctx escaped;
      side_effect_escape ctx escaped thread_id;
      ctx.local
    | _ -> ctx.local
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
