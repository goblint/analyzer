(** An analysis of all writes execution of a function *)
open Analyses
open GoblintCil
open Batteries

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
  module V = struct
    include CilType.Fundec
    let is_write_only _ = false
  end
  module G = ValueDomain.ADGraph

  let context _ _ = C.bot ()

  let add_entry k v d =
    let value = match D.find_opt k d with
      | Some old_value -> VD.join old_value v
      | None -> v
    in
    D.add k value d

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    let ask = Analyses.ask_of_ctx ctx in
    let lv = ask.f (Queries.EvalLval lval) in
    let rv = ask.f (Queries.EvalValue rval) in
    add_entry lv rv ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let join_address_list (a : AD.t list) =
    List.fold AD.join (AD.bot ()) a

  let return ctx (exp:exp option) (f:fundec) : D.t =
    let ask = Analyses.ask_of_ctx ctx in
    let start_state = ask.f (Queries.StartCPA f) in
    let written = List.map Tuple2.first (D.bindings ctx.local) in
    let written = join_address_list written in

    (* TODO: Collect used globals in global invariant, as this may omit globals accessed in the return *)
    let callee_globals = match ask.f Queries.AccessedGlobals with
      | `Top -> []
      | `Lifted globals -> ModularUtil.VS.to_list globals
    in
    let effective_params = f.sformals @ callee_globals in
    let params = List.map (AD.of_var ~is_modular:true) effective_params in
    let params = join_address_list params in
    let graph = ask.f (CollectGraph (start_state, params, written)) in (* TODO: Adapt start and end sets *)
    M.tracel "startstate" "Looking for path from %a to %a in state %a\n" AD.pretty params AD.pretty written BaseDomain.CPA.pretty start_state;
    ctx.sideg f graph;
    ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let callee_state = D.bot () in
    [ctx.local, callee_state]


  let get_reachable ctx args f_ask  =
    let ask = Analyses.ask_of_ctx ctx in
    let used_globals = UsedGlobals.get_used_globals_exps f_ask in
    let get_reachable_exp (exp: exp) =
      ask.f (Q.ReachableAddressesFrom exp)
    in
    let effective_args = used_globals @ args in
    let reachable = List.map get_reachable_exp effective_args in
    List.fold AD.join (AD.bot ()) reachable

  let combine_env ctx lval fexp f args fc au f_ask =
    let reachable = get_reachable ctx args f_ask in
    let translate_and_insert (k: AD.t) (v: VD.t) (map: D.t) =
      let k' = match ModularUtil.ValueDomainExtension.map_back (Address k) ~reachable with
        | Address a -> a
        | _ -> failwith "map_back yielded a non-address value for address input."
      in
      let v' = ModularUtil.ValueDomainExtension.map_back v ~reachable in
      add_entry k' v' map
    in
    D.fold translate_and_insert au (ctx.local)

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    let assign_return_val lval =
      let reachable = get_reachable ctx args f_ask in
      let return_value = f_ask.f (Queries.EvalValue (Lval (Base0.return_lval ()))) in
      let return_value = ModularUtil.ValueDomainExtension.map_back return_value ~reachable in
      let ask = Analyses.ask_of_ctx ctx in
      let lv =  ask.f (Queries.EvalLval lval) in
      add_entry lv return_value ctx.local
    in
    Option.map_default assign_return_val ctx.local lval

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.bot ()
  let threadenter ctx ~multiple lval f args = [D.bot ()]
  let threadspawn ctx ~multiple lval f args fctx = ctx.local
  let exitstate  v = D.top ()

  let query ctx (type a) (q: a Q.t): a Q.result =
    match q with
    | Written ->
      let written : D.t = ctx.local in
      written
    | WriteGraph f ->
      let written : G.t = ctx.global f in
      written
    | _ -> Q.Result.top q

  let modular_support () = Modular
end

let _ =
  let dep = ["startstate"] in
  MCP.register_analysis ~dep (module Spec : MCPSpec)
