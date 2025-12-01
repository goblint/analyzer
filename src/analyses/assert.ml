(** Analysis of [assert] results ([assert]). *)

open Batteries
open GoblintCil
open Analyses
open GobConfig

module Spec : Analyses.MCPSpec =
struct
  include UnitAnalysis.Spec

  let name () = "assert"

  (* transfer functions *)

  let assert_fn man e check refine =
    let expr = CilType.Exp.show e in
    let warn warn_fn msg =
      if check then
        warn_fn msg
    in
    (* TODO: use format instead of %s for the following messages *)
    match Queries.eval_bool (Analyses.ask_of_man man) e with
    | `Lifted false ->
      warn (M.error ~category:Assert "%s") ("Assertion \"" ^ expr ^ "\" will fail.");
      if refine then raise Analyses.Deadcode else man.local
    | `Lifted true ->
      warn (M.success ~category:Assert "%s") ("Assertion \"" ^ expr ^ "\" will succeed");
      man.local
    | `Bot ->
      M.error ~category:Assert "%s" ("Assertion \"" ^ expr ^ "\" produces a bottom. What does that mean? (currently uninitialized arrays' content is bottom)");
      man.local
    | `Top ->
      warn (M.warn ~category:Assert "%s") ("Assertion \"" ^ expr ^ "\" is unknown.");
      man.local

  let special man (lval: lval option) (f:varinfo) (args:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special args, f.vname with
    | Assert { exp; check; refine }, _ -> assert_fn man exp check refine
    | _, _ -> man.local
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
