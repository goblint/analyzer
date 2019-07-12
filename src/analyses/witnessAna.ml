(** An analysis specification for witnesses. *)

open Prelude.Ana
open Analyses

module PrintableVar =
struct
  include Var
  let to_yojson = MyCFG.node_to_yojson

  let isSimple _ = true
  let pretty_f _ = pretty
  let pretty_diff () (x,y) = dprintf "Unsupported"
  (* let short n x = Pretty.sprint n (pretty () x) *)
  (* let short _ x = var_id x *)
  let short _ x =
    let open MyCFG in
    match x with
    | Statement stmt  -> string_of_int stmt.sid
    | Function f      -> "return of " ^ f.vname ^ "()"
    | FunctionEntry f -> f.vname ^ "()"
  let toXML x =
    let text = short 100 x in
    Xml.Element ("value", [], [Xml.Element ("data", [], [Xml.PCData text])])
  let toXML_f _ = toXML
  let printXml f x =
    BatPrintf.fprintf f "%s" (Xml.to_string (toXML x))
  let name () = "var"
  let invariant _ _ = Invariant.none
end

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  let name = "witness"

  module V = PrintableVar
  module S = SetDomain.Make (V)

  module D = Lattice.Prod (S) (S)
  module G = Lattice.Unit
  module C = D

  let stuff ctx =
    let (_, prev) = ctx.local in
    (prev, S.singleton ctx.node)

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    stuff ctx

  let branch ctx (exp:exp) (tv:bool) : D.t =
    stuff ctx

  let body ctx (f:fundec) : D.t =
    stuff ctx

  let return ctx (exp:exp option) (f:fundec) : D.t =
    stuff ctx

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    let (_, prev) = ctx.local in
    let enter = (prev, S.singleton (FunctionEntry f)) in
    [ctx.local, enter]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    let (_, prev) = au in
    (prev, S.singleton ctx.node)

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    stuff ctx

  let startstate v = D.bot ()
  let otherstate v = D.bot ()
  let exitstate  v = D.bot ()
end

let _ =
  MCP.register_analysis (module Spec : Spec)
