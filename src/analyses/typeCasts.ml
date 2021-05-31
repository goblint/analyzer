(** An analysis that collects for each type in a function, to which set of types it was cast to.
    Needed for the library analysis.
*)

open Prelude.Ana
open Analyses


module TypeSig : MapDomain.Groupable with type t = Cil.typsig =
struct
  type t = Cil.typsig

  let compare = Stdlib.compare
  let equal = (=)
  let show x = Pretty.sprint ~width:80 (Cil.d_typsig () x)
  let to_yojson (x :t) = `String (show x)
  let hash = Hashtbl.hash

  let pretty = Cil.d_typsig
  let pretty_diff = failwith "unimplemented"
  (* These two lets us reuse the short function, and allows some overriding
   * possibilities. *)
  let printXml = failwith "uimplemented"
  (* This is for debugging *)
  let name () = "typeCasts"

  let invariant _ _ = None
  let tag = failwith "unimplemented"

  let arbitrary = failwith "unimplemented arbitrary"

  let relift x = x


  type group = Trivial
  let show_group _ = "Trivial"
  let to_group x = Some Trivial
  let  trace_enabled = false
end

module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let name () = "typecasts"

  module TypeSigSet = SetDomain.Make (TypeSig)
  module D = Lattice.Unit
  module G = MapDomain.MapBot (TypeSig) (TypeSigSet)
  module C = Lattice.Unit

  (* returns a list of casts that have been performed, as list of mappinggs from typesigs to sets of typesigs *)
  let rec collect_casts (e: exp): G.t =
    match e with
    (* TODO: Add handling for lvalues!*)
    | Lval lv -> collect_casts_lv lv
    | CastE (t,e) ->
      let typeOrig = TypeSigSet.singleton (typeSig (typeOf e)) in
      G.singleton (typeSig t) typeOrig
    | UnOp (_,e,t) -> collect_casts e
    | BinOp (_,e1,e2,_) -> G.join (collect_casts e1) (collect_casts e2)
    | _ -> G.bot ()

  and collect_casts_lv (lv: lval): G.t = match lv with
    | Mem e,offs -> G.join (collect_casts e) (collect_casts_offs offs)
    | Var _,offs -> collect_casts_offs offs

  and collect_casts_offs (offs: offset) = match offs with
    | Field (_,offs) -> collect_casts_offs offs
    | Index (e,offs) -> G.join (collect_casts e) (collect_casts_offs offs)
    | NoOffset -> G.bot ()


  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    let side_effects = collect_casts rval in
    let current_fun = MyCFG.getFun ctx.node in
    (* Side effect to the function start node *)
    ctx.sideg current_fun.svar side_effects;
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local, ctx.local]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t =
    au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
