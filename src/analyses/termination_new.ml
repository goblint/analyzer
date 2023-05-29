(** Work in progress *)

open Analyses
open GoblintCil
open TerminationPreprocessing

exception PreProcessing of string

let loopCounters : varinfo list ref = ref []

(*
let _ = WitnessUtil.find_loop_heads

let check_loop_head ctx = false
   *)

let get_prepr_var () : varinfo =
  raise (PreProcessing "No loop variable") (* TODO *)

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

  module D = Lattice.Unit (* TODO *)
  module C = D (* TODO *)

  let startstate _ = D.bot () (* TODO *)
  let exitstate = startstate (* TODO *)

  (** Provides some default implementations *)
  include Analyses.IdentitySpec

  let assign ctx (lval : lval) (rval : exp) =
    (* Detect preprocessing variable assignment to 0 *)
    match lval, rval with
      (Var get_prepr_var, NoOffset), zero -> ctx.local (* TODO *)
    | _ -> ctx.local

  let branch ctx (exp : exp) (tv : bool) =
    (*
    let is_loop_head = check_loop_head ctx in
    if is_loop_head then
      enter_loop ctx;
       *)
    ctx.local (* TODO *)

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    Result.top q (* TODO *)

end

let () =
  (** Register the preprocessing *)
  Cilfacade.register_preprocess_cil (Spec.name ()) (new loopCounterVisitor loopCounters);
  (** Register this analysis within the master control program *)
  MCP.register_analysis (module Spec : MCPSpec)
