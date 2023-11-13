(** Termination analysis for loops and [goto] statements ([termination]). *)

open Analyses
open GoblintCil
open TerminationPreprocessing

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
  module V = struct
    include UnitV
    let is_write_only _ = true
  end
  module G = MapDomain.MapBot (Statements) (BoolDomain.MustBool)

  let startstate _ = ()
  let exitstate = startstate

  let find_loop ~loop_counter =
    VarToStmt.find loop_counter !loop_counters

  (** Recognizes a call of [__goblint_bounded] to check the EvalInt of the
   * respective loop counter variable at that position. *)
  let special ctx (lval : lval option) (f : varinfo) (arglist : exp list) =
    if !AnalysisState.postsolving then
      match f.vname, arglist with
        "__goblint_bounded", [Lval (Var x, NoOffset)] ->
        (try
           let loop_statement = find_loop ~loop_counter:x in
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

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MustTermLoop loop_statement ->
      let multithreaded = ctx.ask Queries.IsEverMultiThreaded in
      (not multithreaded)
      && (match G.find_opt (`Lifted loop_statement) (ctx.global ()) with
            Some b -> b
          | None -> false)
    | Queries.MustTermAllLoops ->
      let multithreaded = ctx.ask Queries.IsEverMultiThreaded in
      if multithreaded then (
        M.warn ~category:Termination "The program might not terminate! (Multithreaded)\n";
        false)
      else
        G.for_all (fun _ term_info -> term_info) (ctx.global ())
    | _ -> Queries.Result.top q

end

let () =
  Cilfacade.register_preprocess (Spec.name ()) (new loopCounterVisitor loop_counters);
  MCP.register_analysis (module Spec : MCPSpec)
