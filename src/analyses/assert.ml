open Prelude.Ana
open Analyses
open GobConfig

module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let name () = "assert"
  module D = Lattice.Unit
  module G = Lattice.Unit
  module C = Lattice.Unit

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (fd:fundec) (args:exp list) : (D.t * D.t) list =
    [ctx.local, ctx.local]

  let combine ctx (lval:lval option) fexp (fd:fundec) (args:exp list) fc (au:D.t) : D.t =
    au

  let assert_fn ctx e should_warn change =

    let check_assert e st =
      match ctx.ask (Queries.EvalInt e) with
      | v when Queries.ID.is_bool v ->
        begin match Queries.ID.to_bool v with
          | Some false ->  `Lifted false
          | Some true  ->  `Lifted true
          | _ -> `Top
        end
      | v when Queries.ID.is_bot v -> `Bot
      | _ -> `Top
    in
    let expr = sprint d_exp e in
    let warn warn_fn ?annot msg = if should_warn then
        if get_bool "dbg.regression" then ( (* This only prints unexpected results (with the difference) as indicated by the comment behind the assert (same as used by the regression test script). *)
          let loc = !M.current_loc in
          let line = List.at (List.of_enum @@ File.lines_of loc.file) (loc.line-1) in
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
    match check_assert e ctx.local with
    | `Lifted false ->
      warn (M.error ~category:Assert "%s") ~annot:"FAIL" ("Assertion \"" ^ expr ^ "\" will fail.");
      if change then raise Analyses.Deadcode else ctx.local
    | `Lifted true ->
      warn (M.success ~category:Assert "%s") ("Assertion \"" ^ expr ^ "\" will succeed");
      ctx.local
    | `Bot ->
      M.error ~category:Assert "%s" ("Assertion \"" ^ expr ^ "\" produces a bottom. What does that mean? (currently uninitialized arrays' content is bottom)");
      ctx.local
    | `Top ->
      warn (M.warn ~category:Assert "%s") ~annot:"UNKNOWN" ("Assertion \"" ^ expr ^ "\" is unknown.");
      ctx.local

  let special ctx (lval: lval option) (f:varinfo) (args:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special args, f.vname with
    | Assert { exp; should_warn; change }, _ -> assert_fn ctx exp should_warn change
    | _, _ -> ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
