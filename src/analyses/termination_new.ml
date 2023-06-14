(** Work in progress *)

open Analyses
open GoblintCil
open TerminationPreprocessing

exception PreProcessing of string

let loopCounters : varinfo list ref = ref []

(* Contains the locations of the upjumping gotos *)
let upjumpingGotos : location list ref = ref []

let loopExit : varinfo ref = ref (makeVarinfo false "-error" Cil.intType)

let is_loop_counter_var (x : varinfo) =
  List.mem x !loopCounters

let is_loop_exit_indicator (x : varinfo) =
  x = !loopExit

(** Checks whether at the current location (=loc) of the analysis an
 * upjumping goto was already reached. Returns true if no upjumping goto was
 * reached until now *)
let currrently_no_upjumping_gotos (loc : location) =
  List.for_all (function (l) -> (l >= loc)) upjumpingGotos.contents

let no_upjumping_gotos () =
  (List.length upjumpingGotos.contents) <= 0

(** Checks whether a variable can be bounded *)
let check_bounded ctx varinfo =
  let open IntDomain.IntDomTuple in
  let exp = Lval (Var varinfo, NoOffset) in
  match ctx.ask (EvalInt exp) with
  | `Top -> false
  | `Lifted v -> not (is_top_of (ikind v) v)
  | `Bot -> raise (PreProcessing "Loop variable is Bot")

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
    (* Detect assignment to loop counter variable *)
    match lval, rval with
      (Var x, NoOffset), _ when is_loop_counter_var x ->
      (* Assume that the following loop does not terminate *)
      if not (no_upjumping_gotos ()) then printf "\n4 problem\n";
      D.add x false ctx.local
    | (Var y, NoOffset), Lval (Var x, NoOffset) when is_loop_exit_indicator y ->
      (* Loop exit: Check whether loop counter variable is bounded *)
      let is_bounded = check_bounded ctx x in
      if not (no_upjumping_gotos ()) then printf "\n5 problem\n";
      D.add x is_bounded ctx.local
    | _ -> ctx.local

  (** Provides information to Goblint *)
  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    match q with
    | Queries.MustTermLoop v when check_bounded ctx v ->
      true (* TODO should we use the check_bounded function? *)
    | Queries.MustTermProg ->
      true (*TODO check if all values in the domain are true -> true *)
    | _ -> Result.top q

end

let () =
  (* Register the preprocessing *)
  Cilfacade.register_preprocess_cil (Spec.name ()) (new loopCounterVisitor loopCounters upjumpingGotos loopExit);
  (* Register this analysis within the master control program *)
  MCP.register_analysis (module Spec : MCPSpec)
