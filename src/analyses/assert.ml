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
    let warn warn_fn ?annot msg = if check then
        if get_bool "dbg.regression" then ( (* This only prints unexpected results (with the difference) as indicated by the comment behind the assert (same as used by the regression test script). *)
          let loc = !M.current_loc in
          let line = List.at (List.of_enum @@ File.lines_of loc.file) (loc.line-1) in (* nosemgrep: batenum-of_enum *)
          let open Str in
          let expected = if string_match (regexp ".+//.*\\(FAIL\\|UNKNOWN\\).*") line 0 then Some (matched_group 1 line) else None in
          if expected <> annot then (
            let result = if annot = None && (expected = Some ("NOWARN") || (expected = Some ("UNKNOWN") && not (String.exists line "UNKNOWN!"))) then "improved" else "failed" in
            (* Expressions with logical connectives like a && b are calculated in temporary variables by CIL. Instead of the original expression, we then see something like tmp___0. So we replace expr in msg by the original source if this is the case. *)
            let assert_expr = if string_match (regexp ".*assert(\\(.+\\));.*") line 0 then matched_group 1 line else expr in
            let msg = if expr <> assert_expr then String.nreplace ~str:msg ~sub:expr ~by:assert_expr else msg in
            warn_fn (msg ^ " Expected: " ^ (expected |? "SUCCESS") ^ " -> " ^ result)
          )
        ) else
          warn_fn msg
    in
    (* TODO: use format instead of %s for the following messages *)
    match Queries.eval_bool (Analyses.ask_of_man man) e with
    | `Lifted false ->
      warn (M.error ~category:Assert "%s") ~annot:"FAIL" ("Assertion \"" ^ expr ^ "\" will fail.");
      Checks.error Checks.Category.AssertionFailure "Assertion \"%s\" will fail." expr;
      if refine then raise Analyses.Deadcode else man.local
    | `Lifted true ->
      warn (M.success ~category:Assert "%s") ("Assertion \"" ^ expr ^ "\" will succeed");
      Checks.safe Checks.Category.AssertionFailure ~message:("Assertion \"" ^ expr ^ "\" will succeed");
      man.local
    | `Bot ->
      M.error ~category:Assert "%s" ("Assertion \"" ^ expr ^ "\" produces a bottom. What does that mean? (currently uninitialized arrays' content is bottom)");
      Checks.error Checks.Category.AssertionFailure "Assertion \"%s\" produces a bottom. What does that mean? (currently uninitialized arrays' content is bottom)" expr;
      man.local
    | `Top ->
      warn (M.warn ~category:Assert "%s") ~annot:"UNKNOWN" ("Assertion \"" ^ expr ^ "\" is unknown.");
      Checks.warn Checks.Category.AssertionFailure "Assertion \"%s\" is unknown." expr;
      man.local

  let special man (lval: lval option) (f:varinfo) (args:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special args, f.vname with
    | Assert { exp; check; refine }, _ -> assert_fn man exp check refine
    | _, _ -> man.local
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
