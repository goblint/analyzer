(** Termination analysis for loops and [goto] statements ([termination]). *)

open Analyses
open GoblintCil
open TerminationPreprocessing

exception PreProcessing of string

(** Stores the result of the query if the program is single threaded or not 
    since finalize does not has ctx as an argument*)
let single_thread : bool ref = ref false

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
  let is_write_only _ = true
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

  let finalize () = 
    (* warning for detected possible non-termination *)
    (*upjumping gotos *)
    if not (no_upjumping_gotos ()) then (
      List.iter 
        (fun x -> 
          let msgs =
            [(Pretty.dprintf "The program might not terminate! (Upjumping Goto)\n", Some (M.Location.CilLocation x));] in
          M.msg_group Warning ~category:NonTerminating "Possibly non terminating loops" msgs)
        (!upjumping_gotos) 
    );
    (* multithreaded *)
    if not (!single_thread) then (
      M.warn ~category:NonTerminating "The program might not terminate! (Multithreaded)\n"
    )


  let assign ctx (lval : lval) (rval : exp) =
    if !AnalysisState.postsolving then
      (* Detect assignment to loop counter variable *)
      match lval, rval with
        (Var y, NoOffset), Lval (Var x, NoOffset) when is_loop_exit_indicator y ->
        (* Loop exit: Check whether loop counter variable is bounded *)
        (* TODO: Move to special *)
        let is_bounded = check_bounded ctx x in
        let loop_statement = VarToStmt.find x !loop_counters in
        ctx.sideg () (G.add (`Lifted loop_statement) is_bounded (ctx.global ()));
        (* In case the loop is not bounded, a warning is created*)
        if not (is_bounded) then (
          let msgs =
            [(Pretty.dprintf "The program might not terminate! (Loop analysis)\n", Some (M.Location.CilLocation (Cilfacade.get_stmtLoc loop_statement)));] in
          M.msg_group Warning ~category:NonTerminating "Possibly non terminating loops" msgs);
        ()
      | _ -> ()
    else ()

    (*
  let special ctx (lval : lval option) (f : varinfo) (arglist : exp list) =
      (* TODO: Implement check for our special loop exit indicator function *)
    if !AnalysisState.postsolving then
      match f.vname, arglist with
        "__goblint_bounded", [Lval (Var x, NoOffset)] ->
        let () = print_endline "schpecial" in
        let is_bounded = check_bounded ctx x in
        let loop_statement = VarToStmt.find x !loop_counters in
        ctx.sideg () (G.add (`Lifted loop_statement) is_bounded (ctx.global ()));
        ()
      | _ -> ()
    else ()
    *)

  (** Checks whether a new thread was spawned some time. We want to discard
   * any knowledge about termination then (see query function) *)
  let must_be_single_threaded_since_start ctx =
    (*
    not (ctx.ask Queries.IsEverMultiThreaded)
    *)
    let single_threaded = ctx.ask (Queries.MustBeSingleThreaded {since_start = true}) in 
    single_thread := single_threaded;
    single_threaded

  (** Provides information to Goblint *)
  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MustTermLoop loop_statement ->
      must_be_single_threaded_since_start ctx 
      && (match G.find_opt (`Lifted loop_statement) (ctx.global ()) with
         Some b -> b
       | None -> false)
    | Queries.MustTermAllLoops ->
      must_be_single_threaded_since_start ctx (* must be the first to be evaluated! *)
      (*Reason: must_be_single_threaded_since_start has the side effect that the single_Thread variable is set
      In case of another order and due to lazy evaluation the correct value of single_Thread can otherwise not be guaranteed! *)
      && no_upjumping_gotos ()
      && G.for_all (fun _ term_info -> term_info) (ctx.global ())
    | _ -> Queries.Result.top q

end

let () =
  (* Register the preprocessing *)
  Cilfacade.register_preprocess_cil (Spec.name ()) (new loopCounterVisitor loop_counters upjumping_gotos loop_exit);
  (* Register this analysis within the master control program *)
  MCP.register_analysis (module Spec : MCPSpec)
