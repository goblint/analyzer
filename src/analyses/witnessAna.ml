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

module FlatBot (Base: Printable.S) = Lattice.LiftBot (Lattice.Fake (Base))

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  let name = "witness"

  module V = PrintableVar
  module S = SetDomain.Make (V)
  module F = FlatBot (V)

  module D = Lattice.Prod (S) (F)
  module G = Lattice.Unit
  module C = D

  let set_of_flat (x:F.t): S.t = match x with
    | `Lifted x -> S.singleton x
    | `Bot -> S.bot ()

  let step (from:D.t) (to_node:V.t): D.t =
    let prev = set_of_flat (snd from) in
    (prev, F.lift to_node)

  let step_ctx ctx = step ctx.local ctx.node

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    step_ctx ctx

  let branch ctx (exp:exp) (tv:bool) : D.t =
    step_ctx ctx

  let body ctx (f:fundec) : D.t =
    step_ctx ctx

  let return ctx (exp:exp option) (f:fundec) : D.t =
    step_ctx ctx

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local, step ctx.local (FunctionEntry f)]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    step au ctx.node

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    step_ctx ctx

  let startstate v = D.bot ()
  let otherstate v = D.bot ()
  let exitstate  v = D.bot ()
end

let _ =
  MCP.register_analysis (module Spec : Spec)
