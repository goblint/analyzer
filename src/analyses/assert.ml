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
    let assert_msg severity fmt =
      if check then
        M.msg severity ~category:Assert fmt
      else
        GobPretty.igprintf () fmt
    in
    match Queries.eval_bool (Analyses.ask_of_man man) e with
    | `Lifted false ->
      assert_msg Error "Assertion \"%a\" will fail." CilType.Exp.pretty e;
      Checks.error Checks.Category.AssertionFailure "Assertion \"%a\" will fail." CilType.Exp.pretty e;
      if refine then raise Analyses.Deadcode else man.local
    | `Lifted true ->
      assert_msg Success "Assertion \"%a\" will succeed" CilType.Exp.pretty e;
      Checks.safe_msg Checks.Category.AssertionFailure "Assertion \"%a\" will succeed" CilType.Exp.pretty e;
      man.local
    | `Bot ->
      M.error ~category:Assert "Assertion \"%a\" produces a bottom. What does that mean? (currently uninitialized arrays' content is bottom)" CilType.Exp.pretty e;
      Checks.error Checks.Category.AssertionFailure "Assertion \"%a\" produces a bottom. What does that mean? (currently uninitialized arrays' content is bottom)" CilType.Exp.pretty e;
      man.local
    | `Top ->
      assert_msg Warning "Assertion \"%a\" is unknown." CilType.Exp.pretty e;
      Checks.warn Checks.Category.AssertionFailure "Assertion \"%a\" is unknown." CilType.Exp.pretty e;
      man.local

  let special man (lval: lval option) (f:varinfo) (args:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special args, f.vname with
    | Assert { exp; check; refine }, _ -> assert_fn man exp check refine
    | _, _ -> man.local
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
