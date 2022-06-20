(** [setjmp]/[longjmp] analysis.

    This analysis doesn't really do anything by itself, it only makes it really easy for other analysis to support analyzing [setjmp] and [longjmp] calls.

    To do this heterogeneous maps are used to be able to collect and manage state from other analysis without putting restrictions on the format of that data.
    An analysis must use HM.token to register 3 tokens for tracking:
    1. The return value of the [setjmp] call. The provided default value for this must be 0.
    2. The values of volatile locals. This could also contain the value of non-volatile locals as changing them between a setjmp and longjmp is undefined behaviour, but it must not contain any privatized globals as they are tracked separately.
    3. The values of privatized globals (if any).

    The setjmp analysis then uses 2 queries to retrieve state at the appropriate points:
    1. [Queries.Longjmp] is used whenever a longjmp is performed.
       In this query a map should be returned containing token 1 with the value of the provided expresssion and 3 with the values of any privatized globals or otherwise any state that should be transfered from the [longjmp] to the [setjmp] call.
    2. [Queries.VolatileLocals] is used for every call in a function after [setjmp] was called.
       Here a map with token 2 containg the values of volatile locals or otherwise any intra-function state that needs to be preserved from [longjmp] to [setjmp].

    Finally the [Events.Setjmp] event is called for every [setjmp]. It should take the value of token 1 and write it to the provided lval and also apply the state saved in token 2 and 3.

*)

open Prelude.Ana
open Analyses
module LF = LibraryFunctions
module HM = MapDomainHeterogeneous.MapSetJmp

module Spec =
struct
  include IdentitySpec

  let name () = "setjmp"

  module D = Queries.LS (* if the local method has performed a setjmp *)
  module C = D
  module G = HM
  module V = Basetype.Variables

  let startstate v = D.bot ()
  let exitstate = startstate

  (** write [data] into all the variables in [vars] *)
  let sideg (ctx : (D.t, G.t, C.t, V.t) ctx) (vars : D.t) data =
    D.elements vars 
    |> List.map fst
    |> List.iter (fun var ->
        (** explicit join with the old value is required or 0 can be lost from the possible setjmp return values. *)
        let getg = (ctx.global var) in
        let g = (HM.join data getg) in
        if M.tracing then M.tracel "setjmp" "Longjmp: %a\n+ %a\n-> %a\n" HM.pretty data HM.pretty getg HM.pretty g;
        ctx.sideg var g)

  let collect_volatiles ctx =
    if D.is_empty ctx.local then begin
      if M.tracing then M.tracel "setjmp" "Enter - no setjump\n";
    end else begin
      let locals = ctx.ask Queries.VolatileLocals in
      if M.tracing then M.tracel "setjmp" "Enter: %a\n" HM.pretty locals;
      sideg ctx ctx.local locals
    end

  let special (ctx : (D.t, G.t, C.t, V.t) ctx) result f args =
    match (LF.find f).special args with
    | Setjmp { env } ->
      (* join the state from all the setjump buffers, env may refer to (generally just one global) *)
      let vars = ctx.ask (Queries.MayPointTo env) in
      if M.tracing then M.traceli "setjmp" "Setjmp vars: [%a]\n" D.pretty vars;
      let data = List.fold HM.join (HM.top ()) (List.map ctx.global (D.elements vars |> List.map fst)) in
      if M.tracing then M.tracel "setjmp" "Setjmp: %a\n" HM.pretty data;
      if M.tracing then M.traceOutdent();
      (* and use it to call the event *)
      ctx.emit (Events.Setjmp (result, data));
      (* track the buffers that will need to receive volatiles *)
      D.join ctx.local vars
    | Longjmp { env; value } ->
      let data = ctx.ask (Queries.Longjmp value) in
      (* collect the state for all setjmp in the dummy var in case we try to setjmp with an unknown buffer *)
      let vars = D.add (dummyFunDec.svar, `NoOffset) (ctx.ask (Queries.MayPointTo env)) in
      if M.tracing then M.traceli "setjmp" "Longjmp vars: [%a]\n" D.pretty vars;
      sideg ctx vars data;
      collect_volatiles ctx;
      if M.tracing then M.traceOutdent();
      (* longjmp never returns directly it will return from the setjmp instead *)
      raise Deadcode
    | _ -> ctx.local

  let enter ctx lval fn args =
    collect_volatiles ctx;
    [ctx.local, startstate()]
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
