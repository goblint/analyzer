(** An analysis of all writes execution of a function *)

open Prelude.Ana
open Analyses

module Q = Queries
module AD = ValueDomain.AD
module VD = ValueDomain.Compound

module Spec =
struct
  include Analyses.DefaultSpec



  let name () = "written"
  (* Value of entries not in mapping: bot, LiftTop such that there is a `Top map. *)
  module D = WrittenDomain.Written
  module C = Lattice.Unit

  let context _ _ = C.bot ()

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    let ask = Analyses.ask_of_ctx ctx in
    match ask.f (Queries.EvalLval lval) with
      | `Top ->
        M.warn "Written lvalue is top. Write is not recorded!";
        ctx.local
      | `Lifted lv ->
        let rv = ask.f (Queries.EvalValue rval) in
        let st = D.add lv rv ctx.local in
        st

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let callee_state = D.bot () in
    [ctx.local, callee_state]

  let combine_env ctx lval fexp f args fc au f_ask =
    (* TODO: For a function call, we need to adapt the values collected for the callee into the representation of the caller. *)
    (* I.e. this requires application of h^{-1}(., A), with A being the set of reachable addresses at the call. *)
    let ask = Analyses.ask_of_ctx ctx in
    let get_reachable_exp (exp: exp) =
      match ask.f (Q.ReachableAddressesFrom exp) with
      | `Top -> failwith @@ "Received `Top value for ReachableAddressesFrom " ^ (CilType.Exp.show exp) ^" query."
      | `Lifted rs -> rs
    in
    let reachable = List.map get_reachable_exp args in
    let reachable = List.fold AD.join (AD.bot ()) reachable in
    let translate_and_insert (k: AD.t) (v: VD.t) (map: D.t) =
      let k' = match ModularUtil.ValueDomainExtension.map_back (`Address k) ~reachable with
        | `Address a -> a
        | _ -> failwith "map_back yielded a non-address value for address input."
      in
      let v' = ModularUtil.ValueDomainExtension.map_back v ~reachable in
      D.add k' v' map
    in
    D.fold translate_and_insert au (ctx.local)

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    (* TODO: Record assignment of rval to lval*)
    ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()

  let query ctx (type a) (q: a Q.t): a Q.result =
    match q with
    | Written ->
      let written : D.t = ctx.local in
      written
    | _ -> Q.Result.top q
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
