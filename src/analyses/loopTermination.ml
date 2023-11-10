(** Termination analysis for loops and [goto] statements ([termination]). *)

open Analyses
open GoblintCil
open TerminationPreprocessing

(** Stores the result of the query whether the program is single threaded or not
    since finalize does not have access to ctx. *)
let single_thread : bool ref = ref false

(** Contains all loop counter variables (varinfo) and maps them to their corresponding loop statement. *)
let loop_counters : stmt VarToStmt.t ref = ref VarToStmt.empty

(** Checks whether a variable can be bounded. *)
let check_bounded ctx varinfo =
  let open IntDomain.IntDomTuple in
  let exp = Lval (Var varinfo, NoOffset) in
  match ctx.ask (EvalInt exp) with
  | `Top -> false
  | `Lifted v -> not (is_top_of (ikind v) v)
  | `Bot -> failwith "Loop counter variable is Bot."

(** We want to record termination information of loops and use the loop
 * statements for that. We use this lifting because we need to have a
 * lattice. *)
module Statements = Lattice.Flat (CilType.Stmt) (Printable.DefaultNames)

(** The termination analysis considering loops and gotos *)
module Spec : Analyses.MCPSpec =
struct

  include Analyses.IdentitySpec

  let name () = "termination"

  module D = Lattice.Unit
  module C = D
  module V =
  struct
    include Printable.Unit
    let is_write_only _ = true
  end
  module G = MapDomain.MapBot (Statements) (BoolDomain.MustBool)

  let startstate _ = ()
  let exitstate = startstate

  (** Warnings for detected possible non-termination *)
  let finalize () =
    (* Multithreaded *)
    if not (!single_thread) then (
      M.warn ~category:Termination "The program might not terminate! (Multithreaded)\n"
    )

  (** Recognizes a call of [__goblint_bounded] to check the EvalInt of the
   * respective loop counter variable at that position. *)
  let special ctx (lval : lval option) (f : varinfo) (arglist : exp list) =
    if !AnalysisState.postsolving then
      match f.vname, arglist with
        "__goblint_bounded", [Lval (Var x, NoOffset)] ->
        (try
           let loop_statement = VarToStmt.find x !loop_counters in
           let is_bounded = check_bounded ctx x in
           ctx.sideg () (G.add (`Lifted loop_statement) is_bounded (ctx.global ()));
           (* In case the loop is not bounded, a warning is created. *)
           if not (is_bounded) then (
             M.warn ~loc:(M.Location.CilLocation (Cilfacade.get_stmtLoc loop_statement)) ~category:Termination "The program might not terminate! (Loop analysis)"
           );
           ()
         with Not_found ->
           failwith "Encountered a call to __goblint_bounded with an unknown loop counter variable.")
      | _ -> ()
    else ()

  (* Checks whether the program always remains single-threaded.
     If the program does not remain single-threaded, we assume non-termination (see query function). *)
  let must_always_be_single_threaded ctx =
    let single_threaded = not (ctx.ask Queries.IsEverMultiThreaded) in
    single_thread := single_threaded;
    single_threaded

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MustTermLoop loop_statement ->
      must_always_be_single_threaded ctx
      && (match G.find_opt (`Lifted loop_statement) (ctx.global ()) with
            Some b -> b
          | None -> false)
    | Queries.MustTermAllLoops ->
      let always_single_threaded = must_always_be_single_threaded ctx in
      (* Must be the first to be evaluated! This has the side effect that
       * single_thread is set. In case of another order and due to lazy
       * evaluation, the correct value of single_thread can not be guaranteed!
       * Therefore, we use a let-in clause here. *)
      always_single_threaded
      && G.for_all (fun _ term_info -> term_info) (ctx.global ())
    | _ -> Queries.Result.top q

end

let () =
  Cilfacade.register_preprocess (Spec.name ()) (new loopCounterVisitor loop_counters);
  MCP.register_analysis (module Spec : MCPSpec)
