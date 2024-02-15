(** An analysis of all reads during the execution of a modularly analyzed function. Useful for race-detection *)
open Analyses
open GoblintCil
open Batteries

module Q = Queries
module AD = Queries.AD
module VD = ValueDomain.Compound

module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "read"
  module D = AD
  module C = Lattice.Unit
  module V = struct
    include CilType.Fundec
    let is_write_only _ = false
  end
  module G = ValueDomain.ADGraph

  let context _ _ = C.bot ()

  let add_entry k d =
    AD.join k d

  let eval_lval ctx (varinfo, offs) =
    let offset = Offset.Exp.to_cil offs in
    let lval = (Var varinfo, offset) in
    ctx.ask (Q.EvalLval lval)

  let reads_on_exp ctx exp =
    let reads = ref (AD.empty ()) in
    let collect_read exp =
      match exp with
      | AddrOf lval ->
        let addresses : AD.t = ctx.ask (Q.MayPointTo (AddrOf lval)) in
        reads := AD.join addresses !reads
      | _ ->
        (* Ignore cases without AddrOf*)
        ()
    in
    Access.distribute_access_exp collect_read exp;
    !reads

  let add_reads_on_exp ctx exp =
    let reads = reads_on_exp ctx exp in
    AD.join reads ctx.local

  let add_reads_on_optional_exp ctx exp =
    BatOption.map_default (reads_on_exp ctx) ctx.local exp

  let reads_on_exps ctx exps =
    let add_exp_reads acc exp =
      let exp_reads = reads_on_exp ctx exp in
      AD.join acc exp_reads
    in
    let reads = List.fold add_exp_reads (AD.bot ()) exps in
    reads

  let add_reads_on_exps ctx exps =
    let reads = reads_on_exps ctx exps in
    AD.join ctx.local reads

  let reads_on_lval ctx lval =
    let lval_exp = AddrOf lval in
    reads_on_exp ctx lval_exp

  let add_reads_on_lval ctx lval =
    let reads_on_lval = reads_on_lval ctx lval in
    AD.join ctx.local reads_on_lval

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    let reads_on_lval = reads_on_lval ctx lval in
    let reads = D.join ctx.local reads_on_lval in
    let reads_on_exp = reads_on_exp ctx rval in
    let reads = D.join reads reads_on_exp in
    reads

  let branch ctx (exp:exp) (tv:bool) : D.t =
    add_reads_on_exp ctx exp

  let body ctx (f:fundec) : D.t =
    ctx.local

  let join_address_list (a : AD.t list) =
    List.fold AD.join (AD.bot ()) a

  let return_side_effect ctx local exp f =
    let ask = Analyses.ask_of_ctx ctx in
    let start_state = ask.f (Queries.StartCPA f) in
    let read = local in
    (* TODO: Collect used globals in global invariant, as this may omit globals accessed in the return *)
    let callee_globals = match ask.f Queries.AccessedGlobals with
      | `Top -> []
      | `Lifted globals -> ModularUtil.VS.to_list globals
    in
    let effective_params = f.sformals @ callee_globals in
    let params = List.map (AD.of_var ~is_modular:true) effective_params in
    let params = join_address_list params in
    let graph = ask.f (CollectGraph (start_state, params, read)) in (* TODO: Adapt start and end sets *)
    M.tracel "written" "Looking for path from %a to %a in state %a\n" AD.pretty params AD.pretty read BaseDomain.CPA.pretty start_state;
    ctx.sideg f graph

  let return ctx (exp:exp option) (f:fundec) : D.t =
    let local = add_reads_on_optional_exp ctx exp in
    return_side_effect ctx local exp f;
    local

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
    add_reads_on_exps ctx args

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    match lval with
    | Some lval ->
      add_reads_on_lval ctx lval
    | None ->
      ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let lval_reads = BatOption.map_default (add_reads_on_lval ctx) ctx.local lval in
    let exp_reads = reads_on_exps ctx arglist in
    D.join lval_reads exp_reads

  let startstate v = D.bot ()
  let threadenter ctx ~multiple lval f args = [D.bot ()]
  let threadspawn ctx ~multiple lval f args fctx = ctx.local
  let exitstate  v = D.top ()

  let query ctx (type a) (q: a Q.t): a Q.result =
    match q with
    | Read ->
      let read : D.t = ctx.local in
      read
    | ReadGraph f ->
      let read : G.t = ctx.global f in
      read
    | _ -> Q.Result.top q

  let modular_support () = Modular
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
