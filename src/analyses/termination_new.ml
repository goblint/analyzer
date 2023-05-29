(** Work in progress *)

open Analyses
open GoblintCil
open TerminationPreprocessing

exception PreProcessing of string

let visited = Stack.create () (* TODO: Is this allowed? *)

let is_loop_counter_var (x : varinfo) =
  false (* TODO: Actually detect loop counter variables *)

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

  let startstate _ = D.bot () (* TODO *)
  let exitstate = startstate (* TODO *)

  (** Provides some default implementations *)
  include Analyses.IdentitySpec

  let assign ctx (lval : lval) (rval : exp) =
    (* Detect loop counter variable assignment to 0 *)
    match lval, rval with
    (* Assume that the following loop does not terminate *)
      (Var x, NoOffset), zero when is_loop_counter_var x ->
      (* Remember the lcv *)
      (*
      let () = Stack.push x visited in
      let () = enter_loop in
         *)
      D.add x false ctx.local
    | _ -> ctx.local

  let branch ctx (exp : exp) (tv : bool) =
    ctx.local (* TODO *)

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    Result.top q (* TODO *)

end

let () =
  (** Register the preprocessing *)
  Cilfacade.register_preprocess_cil (Spec.name ()) (new loopCounterVisitor);
  (** Register this analysis within the master control program *)
  MCP.register_analysis (module Spec : MCPSpec)
