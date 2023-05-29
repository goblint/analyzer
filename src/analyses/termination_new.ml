(** Work in progress *)

open Analyses
open GoblintCil
open TerminationPreprocessing

exception PreProcessing of string

let loopCounters : varinfo list ref = ref []

let is_loop_counter_var (x : varinfo) =
  false (* TODO: Actually detect loop counter variables *)

let is_loop_exit_indicator (x : varinfo) =
  false (* TODO: Actually detect loop exit indicators *)

(** Checks whether a variable can be bounded *)
let check_bounded ctx varinfo =
  let exp = Lval (Var varinfo, NoOffset) in
  match ctx.ask (EvalInt exp) with
    `Top -> false
  | `Bot -> raise (PreProcessing "Loop variable is Bot")
  |    _ -> true (* TODO: Is this sound? *)

module Spec : Analyses.MCPSpec =
struct

  let name () = "termination"

  module D = MapDomain.MapBot (Basetype.Variables) (BoolDomain.MustBool)
  module C = D

  let startstate _ = D.bot ()
  let exitstate = startstate (* TODO *)

  (** Provides some default implementations *)
  include Analyses.IdentitySpec

  let assign ctx (lval : lval) (rval : exp) =
    (* Detect loop counter variable assignment to 0 *)
    match lval, rval with
    (* Assume that the following loop does not terminate *)
      (Var x, NoOffset), zero when is_loop_counter_var x ->
      D.add x false ctx.local
    (* Loop exit: Check whether loop counter variable is bounded *)
    | (Var y, NoOffset), Lval (Var x, NoOffset) when is_loop_exit_indicator y ->
      let is_bounded = check_bounded ctx x in
      D.add x is_bounded ctx.local
    | _ -> ctx.local

  let branch ctx (exp : exp) (tv : bool) =
    ctx.local (* TODO: Do we actually need a branch transfer function? *)

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    Result.top q (* TODO *)

end

let () =
  (** Register the preprocessing *)
  Cilfacade.register_preprocess_cil (Spec.name ()) (new loopCounterVisitor loopCounters);
  (** Register this analysis within the master control program *)
  MCP.register_analysis (module Spec : MCPSpec)
