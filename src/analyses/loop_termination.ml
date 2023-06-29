(** Work in progress *)

open Analyses
open GoblintCil
open TerminationPreprocessing

exception PreProcessing of string

(*
let loop_heads () =
  let module FileCfg =
  struct
    let file = !Cilfacade.current_file
    module Cfg = (val !MyCFG.current_cfg)
  end in
  let module WitnessInvariant = WitnessUtil.Invariant (FileCfg) in
  WitnessInvariant.loop_heads (* TODO: Unused *)
*)

(** Contains all loop counter variables (varinfo) and maps them to their corresponding loop statement. *)
let loop_counters : stmt VarToStmt.t ref = ref VarToStmt.empty

(** Contains the locations of the upjumping gotos *)
let upjumping_gotos : location list ref = ref []

(** Indicates the place in the code, right after a loop is exited. *)
let loop_exit : varinfo ref = ref (makeVarinfo false "-error" Cil.intType)

let is_loop_counter_var (x : varinfo) =
  VarToStmt.mem x !loop_counters

let is_loop_exit_indicator (x : varinfo) =
  x = !loop_exit

let no_upjumping_gotos () =
  upjumping_gotos.contents = []

(** Checks whether a variable can be bounded *)
let check_bounded ctx varinfo =
  let open IntDomain.IntDomTuple in
  let exp = Lval (Var varinfo, NoOffset) in
  match ctx.ask (EvalInt exp) with
  | `Top -> false
  | `Lifted v -> not (is_top_of (ikind v) v)
  | `Bot -> raise (PreProcessing "Loop variable is Bot")

module UnitV =
struct
  include Printable.Unit
  include StdV
end

(** We want to record termination information of loops and use the loop
 * statements for that. We use this lifting because we need to have a
 * lattice. *)
module Statements = Lattice.Flat (CilType.Stmt) (Printable.DefaultNames) (* TODO: Use Basetype.CilStmt instead? *)

(** The termination analysis considering loops and gotos *)
module Spec : Analyses.MCPSpec =
struct

  (** Provides some default implementations *)
  include Analyses.IdentitySpec

  let name () = "termination"

  module D = Lattice.Unit
  module C = D
  module V = UnitV
  module G = MapDomain.MapBot (Statements) (BoolDomain.MustBool)

  let startstate _ = ()
  let exitstate = startstate

  let assign ctx (lval : lval) (rval : exp) =
    (* Detect assignment to loop counter variable *)
    match lval, rval with
      (Var y, NoOffset), Lval (Var x, NoOffset) when is_loop_exit_indicator y ->
      (* Loop exit: Check whether loop counter variable is bounded *)
      (* TODO: Move to special *)
      let is_bounded = check_bounded ctx x in
      let loop_statement = VarToStmt.find x !loop_counters in
      let () = ctx.sideg () (G.add (`Lifted loop_statement) is_bounded (ctx.global ())) in
      ()
    | _ -> ()

  let special ctx (lval : lval option) (f : varinfo) (arglist : exp list) =
    (* TODO: Implement check for our special loop exit indicator function *)
    ()

  (** Checks whether a new thread was spawned some time. We want to discard
   * any knowledge about termination then (see query function) *)
  let must_be_single_threaded_since_start ctx =
    ctx.ask (Queries.MustBeSingleThreaded {since_start = true})

  (** Provides information to Goblint *)
  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MustTermLoop loop_statement ->
      (match G.find_opt (`Lifted loop_statement) (ctx.global ()) with
         Some b -> b
       | None -> false)
      && must_be_single_threaded_since_start ctx
    | Queries.MustTermProg ->
      G.for_all (fun _ term_info -> term_info) (ctx.global ())
      && no_upjumping_gotos ()
      && must_be_single_threaded_since_start ctx
    | _ -> Queries.Result.top q

end

let () =
  (* Register the preprocessing *)
  Cilfacade.register_preprocess_cil (Spec.name ()) (new loopCounterVisitor loop_counters upjumping_gotos loop_exit);
  (* Register this analysis within the master control program *)
  MCP.register_analysis (module Spec : MCPSpec)
