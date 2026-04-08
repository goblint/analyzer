(** Termination analysis for loops and [goto] statements ([termination]).

    @see <https://theses.hal.science/tel-00288805> Halbwachs, N. Détermination automatique de relations linéaires vérifiées par les variables d’un programme. PhD thesis. Section 8.3. *)

open Analyses
open GoblintCil
open TerminationPreprocessing

(** Contains all loop counter variables (varinfo) and maps them to their corresponding loop statement. *)
let loop_counters : stmt VarToStmt.t ref = ref VarToStmt.empty

(** Checks whether a variable can be bounded. *)
let ask_bound man varinfo =
  let open IntDomain.IntDomTuple in
  let exp = Lval (Var varinfo, NoOffset) in
  match man.ask (EvalInt exp) with
  | `Top -> `Top
  | `Lifted v when is_top_of (ikind v) v -> `Top
  | `Lifted v -> `Lifted v
  | `Bot -> failwith "Loop counter variable is Bot."

(** The termination analysis considering loops and gotos *)
module Spec : Analyses.MCPSpec =
struct

  include Analyses.IdentitySpec

  let name () = "termination"

  module D = Lattice.Unit
  include Analyses.ValueContexts(D)

  module V = struct
    include UnitV
    let is_write_only _ = true
  end

  (** We want to record termination information of loops and use the loop statements for that. *)
  module G = MapDomain.MapBot (CilType.Stmt) (BoolDomain.MustBool)

  let startstate _ = ()
  let exitstate = startstate

  (** Recognizes a call of [__goblint_bounded] to check the EvalInt of the
   * respective loop counter variable at that position. *)
  let special man (lval : lval option) (f : varinfo) (arglist : exp list) =
    if !AnalysisState.postsolving then (
      match f.vname, arglist with
      | "__goblint_bounded", [Lval (Var loop_counter, NoOffset)] ->
        begin match VarToStmt.find_opt loop_counter !loop_counters with
          | Some loop_statement ->
            let bound = ask_bound man loop_counter in
            let is_bounded = bound <> `Top in
            man.sideg () (G.add loop_statement is_bounded (man.global ()));
            let loc = M.Location.CilLocation (Cilfacade.get_stmtLoc loop_statement) in
            begin match bound with
              | `Top ->
                M.warn ~category:Termination ~loc "The program might not terminate! (Loop analysis)"
              | `Lifted bound ->
                (* TODO: aggregate these per loop (if unrolled) and warn using WarnGlobal? *)
                if GobConfig.get_bool "dbg.termination-bounds" then
                  M.success ~category:Termination ~loc "Loop terminates: bounded by %a iteration(s)" IntDomain.IntDomTuple.pretty bound;
            end
          | None ->
            failwith "Encountered a call to __goblint_bounded with an unknown loop counter variable."
        end
      | "__goblint_bounded", _ ->
        failwith "__goblint_bounded call unexpected arguments"
      | _ -> ()
    )

  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MustTermLoop loop_statement ->
      let multithreaded = man.ask Queries.IsEverMultiThreaded in
      (not multithreaded)
      && (BatOption.default false (G.find_opt loop_statement (man.global ())))
    | Queries.MustTermAllLoops ->
      let multithreaded = man.ask Queries.IsEverMultiThreaded in
      if multithreaded then (
        M.warn ~category:Termination "The program might not terminate! (Multithreaded)";
        false)
      else
        G.for_all (fun _ term_info -> term_info) (man.global ())
    | _ -> Queries.Result.top q

end

let () =
  Cilfacade.register_preprocess (Spec.name ()) (new loopCounterVisitor loop_counters);
  MCP.register_analysis (module Spec : MCPSpec)
