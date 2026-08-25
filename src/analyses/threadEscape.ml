(** Escape analysis for thread-local variables ([escape]). *)

open GoblintCil
open Analyses

module M = Messages

let has_escaped (ask: Queries.ask) (v: varinfo): bool =
  assert (not v.vglob);
  if not v.vaddrof then
    false (* Cannot have escaped without taking address. Override provides extra precision for degenerate ask in base to_value_domain_ask used for partitioned arrays. *)
  else if GobConfig.get_bool "exp.single-threaded" then
    false (* Cannot have escaped if no other threads are assumed to exist. Override provides extra precision for analyzing single-threaded programs without escape analysis. *)
  else
    ask.f (Queries.MayEscape v)

module Spec =
struct
  include Analyses.IdentitySpec

  module ThreadIdSet = SetDomain.Make (ThreadIdDomain.ThreadLifted)

  let name () = "escape"
  module D = EscapeDomain.EscapedVars
  include Analyses.ValueContexts(D)
  module V = VarinfoV
  module G = ThreadIdSet

  let thread_id man =
    man.ask Queries.CurrentThreadId

  (** Emit an escape event:
      Only necessary when code has ever been multithreaded,
      or when about to go multithreaded. *)
  let emit_escape_event man escaped =
    (* avoid emitting unnecessary event *)
    if not (D.is_empty escaped) then
      man.emit (Events.Escape escaped)

  (** Side effect escapes: In contrast to the emitting the event, side-effecting is
      necessary in single threaded mode, since we rely on escape status in Base
      for passing locals reachable via globals *)
  let side_effect_escape man escaped threadid =
    let threadid = G.singleton threadid in
    D.iter (fun v ->
        man.sideg v threadid) escaped

  (* queries *)
  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MayEscape v ->
      let threads = man.global v in
      if ThreadIdSet.is_empty threads then
        false
      else begin
        let other_possibly_started current = function
          | `Lifted tid when (ThreadId.Thread.equal current tid && ThreadId.Thread.is_unique current) ->
            (* if our own (unique) thread is started here, that is not a problem *)
            false
          | `Lifted tid ->
            let threads = man.ask Queries.CreatedThreads in
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
        match man.ask Queries.CurrentThreadId with
        | `Lifted current ->
          let possibly_started = ThreadIdSet.exists (other_possibly_started current) threads in
          if possibly_started then
            true
          else
            let current_is_unique = ThreadId.Thread.is_unique current in
            let any_equal_current threads = ThreadIdSet.exists (equal_current current) threads in
            if not current_is_unique && any_equal_current threads then
              (* Another instance of the non-unique current thread may have escaped the variable *)
              true
            else
              (* Check whether current unique thread has escaped the variable *)
              D.mem v man.local
        | `Top ->
          true
        | `Bot ->
          M.warn ~category:MessageCategory.Analyzer "CurrentThreadId is bottom.";
          false
      end
    | _ -> Queries.Result.top q

  let escape_rval man ask (rval:exp) =
    let escaped = EscapeUtil.escaping ask rval in

    let thread_id = thread_id man in
    side_effect_escape man escaped thread_id;
    if ThreadFlag.has_ever_been_multi ask then (* avoid emitting unnecessary event *)
      emit_escape_event man escaped;
    escaped

  (* transfer functions *)
  let assign man (lval:lval) (rval:exp) : D.t =
    let ask = Analyses.ask_of_man man in
    if EscapeUtil.stores_into_escaped ask ~escaped:(fun v -> v.vglob || has_escaped ask v) lval then (
      let escaped = escape_rval man ask rval in
      D.join man.local escaped
    ) else begin
      man.local
    end

  let combine_assign man (lval:lval option) (fexp:exp) f args fc au f_ask : D.t =
    let ask = Analyses.ask_of_man man in
    match lval with
    | Some lval when EscapeUtil.stores_into_escaped ask ~escaped:(fun v -> v.vglob || has_escaped ask v) lval ->
      let rval = Lval (ReturnUtil.return_lval ()) in
      let escaped = escape_rval man f_ask rval in (* Using f_ask because the return value is only accessible in the context of that function at this point *)
      D.join man.local escaped
    | _ -> man.local

  let special man (lval: lval option) (f:varinfo) (args:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special args, f.vname, args with
    | Globalize ptr, _, _ ->
      let escaped = escape_rval man (Analyses.ask_of_man man) ptr in
      D.join man.local escaped
    | _, "pthread_setspecific" , [key; pt_value] ->
      let escaped = escape_rval man (Analyses.ask_of_man man) pt_value in
      D.join man.local escaped
    | _ -> man.local

  let startstate v = D.bot ()
  let exitstate  v = D.bot ()

  let threadenter man ~multiple lval f args =
    [D.bot ()]

  let threadspawn man ~multiple lval f args fman =
    D.join man.local @@
    match args with
    | [ptc_arg] ->
      (* not reusing fman.local to avoid unnecessarily early join of extra *)
      let escaped = EscapeUtil.escaping (Analyses.ask_of_man man) ptc_arg in
      if M.tracing then M.tracel "escape" "%a: %a" d_exp ptc_arg D.pretty escaped;
      let thread_id = thread_id man in
      emit_escape_event man escaped;
      side_effect_escape man escaped thread_id;
      escaped
    | _ -> D.bot ()

  let event man e oman =
    match e with
    | Events.EnterMultiThreaded ->
      let escaped = man.local in
      let thread_id = thread_id man in
      emit_escape_event man escaped;
      side_effect_escape man escaped thread_id;
      man.local
    | _ -> man.local
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
