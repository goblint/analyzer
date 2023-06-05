(** An analysis of all reads during the execution of a modularly analyzed function. Useful for race-detection *)
open Analyses
open GoblintCil
open Batteries

module Q = Queries
module AD = ValueDomain.AD
module VD = ValueDomain.Compound

module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "read"
  module D = ValueDomain.AD
  module C = Lattice.Unit

  let context _ _ = C.bot ()

  let add_entry k d =
    AD.join k d

  let eval_lval ctx (varinfo, offs) =
    let offset = Lval.CilLval.to_ciloffs offs in
    let lval = (Var varinfo, offset) in
    match ctx.ask (Q.EvalLval lval) with
    | `Lifted a -> a
    | _ -> failwith "EvalLval returned top"

  module LvalSet = BatSet.Make (Lval.CilLval)

  let eval_lvals ctx lvals =
    LvalSet.fold (fun lval acc -> AD.join acc (eval_lval ctx lval)) lvals (AD.bot ())

  let reads_on_exp ctx exp =
    let reads = ref (AD.empty ()) in
    let collect_read exp =
      match exp with
      | AddrOf lval ->
        let lvals = match ctx.ask (Q.MayPointTo (AddrOf lval)) with
          | `Lifted a -> a
          | _ -> failwith "MayPointTo returned top"
        in
        let address = eval_lvals ctx lvals in
        reads := AD.join address !reads
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

  let return ctx (exp:exp option) (f:fundec) : D.t =
    add_reads_on_optional_exp ctx exp

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let callee_state = D.bot () in
    [ctx.local, callee_state]


  let get_reachable ctx args f_ask  =
    let ask = Analyses.ask_of_ctx ctx in
    let used_globals = ModularUtil.get_callee_globals f_ask in
    let get_reachable_exp (exp: exp) =
      match ask.f (Q.ReachableAddressesFrom exp) with
      | `Top ->
        Messages.warn ~category:Messages.Category.Analyzer  ~tags:[Category Unsound] "Target address of expression %a could not be resolved (i.e. the address was top)" CilType.Exp.pretty exp;
        AD.top_ptr
      | `Lifted rs -> rs
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
  let threadenter ctx lval f args = [D.bot ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()

  let query ctx (type a) (q: a Q.t): a Q.result =
    match q with
    | Read ->
      let read : D.t = ctx.local in
      `Lifted read
    | _ -> Q.Result.top q

  let modular_support () = Modular
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
