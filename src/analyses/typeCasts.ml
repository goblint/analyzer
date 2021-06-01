(** An analysis that collects for each type in a function, to which set of types it was cast to.
    Needed for the library analysis.
*)

open Prelude.Ana
open Analyses
open TypeCastDomain
module Q = Queries


module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let name () = "typecasts"

  module D = Lattice.Unit
  module G = TypeCastMap
  module C = Lattice.Unit

  (* returns a list of casts that have been performed, as list of mappinggs from typesigs to sets of typesigs *)
  let rec collect_casts (e: exp): G.t =
    match e with
    (* TODO: Add handling for lvalues!*)
    | Lval lv -> collect_casts_lv lv
    | CastE (t,e) ->
      let orig_type = typeSig (typeOf e) in
      let casted_to_type = TypeSigSet.singleton (typeSig t) in
      G.singleton orig_type casted_to_type
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

  let side_effect_casts ctx (g: G.t) =
    let current_fun = MyCFG.getFun ctx.node in
    (* Side effect to the function start node *)
    ctx.sideg current_fun.svar g

  let side_effect_casts_lv ctx (lv: lval) =
    side_effect_casts ctx (collect_casts_lv lv)

  let side_effect_casts_exp ctx (e: exp) =
    side_effect_casts ctx (collect_casts e)

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    side_effect_casts_exp ctx rval;
    side_effect_casts_lv ctx lval;
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    (* TODO: Side effect? *)
    [ctx.local, ctx.local]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t =
    List.iter (side_effect_casts_exp ctx) args;
    ignore @@ BatOption.map (side_effect_casts_lv ctx) lval;
    au

  let special ctx (lval: lval option) (f:varinfo) (args:exp list) : D.t =
    List.iter (side_effect_casts_exp ctx) args;
    ignore @@ BatOption.map (side_effect_casts_lv ctx) lval;
    ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()

  let query ctx (type a) (q: a Q.t): a Q.result =
    match q with
    | Q.TypeCasts v -> (ctx.global v: G.t)
    | _ -> Q.Result.top q

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
