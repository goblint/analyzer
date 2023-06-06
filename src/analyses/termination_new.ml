(** Work in progress *)

open Analyses
open GoblintCil
open TerminationPreprocessing

exception PreProcessing of string

let loopCounters : varinfo list ref = ref []

let loopExit : varinfo ref = ref (makeVarinfo false "-error" Cil.intType)

let is_loop_counter_var (x : varinfo) =
  List.mem x !loopCounters

let is_loop_exit_indicator (x : varinfo) =
  x = !loopExit

(** Checks whether a variable can be bounded *)
let check_bounded ctx varinfo =
  let open IntDomain.IntDomTuple in (* TODO: Remove *)
  let open Cil in
  let exp = Lval (Var varinfo, NoOffset) in
  match ctx.ask (EvalInt exp) with
    `Top -> print_endline (varinfo.vname ^ " is TOP"); false
  | `Bot -> print_endline (varinfo.vname ^ " is BOT"); raise (PreProcessing "Loop variable is Bot")
  | `Lifted v -> print_endline (varinfo.vname ^ " is " ^ IntDomain.IntDomTuple.show v); not (is_top v) (* TODO: Is this sound? *)

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
      (Var x, NoOffset), _ when is_loop_counter_var x ->
      D.add x false ctx.local
    (* Loop exit: Check whether loop counter variable is bounded *)
    | (Var y, NoOffset), Lval (Var x, NoOffset) when is_loop_exit_indicator y ->
      let is_bounded = check_bounded ctx x in
      D.add x is_bounded ctx.local
    | _ -> ctx.local

  let branch ctx (exp : exp) (tv : bool) =
    ctx.local (* TODO: Do we actually need a branch transfer function? *)


  (* provides information to Goblint*)
  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    match q with
    | Queries.MustTermLoop v when check_bounded ctx v ->
      true (* TODO should we use the checl_bound function?*)
    | Queries.MustTermProg ->
      true (*TODO check if all values in the domain are true -> true*)
    | _ -> Result.top q

end

let () =
  (** Register the preprocessing *)
  Cilfacade.register_preprocess_cil (Spec.name ()) (new loopCounterVisitor loopCounters loopExit);
  (** Register this analysis within the master control program *)
  MCP.register_analysis (module Spec : MCPSpec)
