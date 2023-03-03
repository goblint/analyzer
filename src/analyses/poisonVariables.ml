open Prelude.Ana
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec
  module VS = SetDomain.ToppedSet(CilType.Varinfo) (struct let topname = "All vars" end)

  let name () = "poisonVariables"
  module D = VS
  module C = Lattice.Unit

  let rec check_exp tainted e = match e with
    (* Recurse over the structure in the expression, returning true if any varinfo appearing in the expression is tainted *)
    | AddrOf v
    | StartOf v
    | Lval v -> check_lval tainted v
    | BinOp (_,e1,e2,_) -> check_exp tainted e1; check_exp tainted e2
    | Real e
    | Imag e
    | SizeOfE e
    | AlignOfE e
    | CastE (_,e)
    | UnOp (_,e,_) -> check_exp tainted e
    | SizeOf _ | SizeOfStr _ | Const _  | AlignOf _ | AddrOfLabel _ -> ()
    | Question (b, t, f, _) -> check_exp tainted b; check_exp tainted t; check_exp tainted f
  and check_lval ?(ignore_var = false) tainted lval = match lval with
    | (Var v, offset) ->
      if not ignore_var && not v.vglob && VS.mem v tainted then M.warn "accessing poisonous variable %a" d_varinfo v;
      check_offset tainted offset
    | _ -> () (* TODO: Consider mem *)
  and check_offset tainted offset = match offset with
    | NoOffset -> ()
    | Field (_, o) -> check_offset tainted o
    | Index (e, o) -> check_exp tainted e; check_offset tainted o

  let rem_lval ask tainted lval = match lval with
    | (Var v, NoOffset) -> VS.remove v tainted (* TODO: If there is an offset, it is a bit harder to remove, as we don't know where the indeterminate value is *)
    | _ -> tainted


  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    check_lval ~ignore_var:true ctx.local lval;
    check_exp ctx.local rval;
    rem_lval ctx.ask ctx.local lval

  let branch ctx (exp:exp) (tv:bool) : D.t =
    check_exp ctx.local exp;
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    Option.may (check_exp ctx.local) exp;
    (* remove locals, except ones which need to be weakly updated*)
    let d = ctx.local in
    let locals = (f.sformals @ f.slocals) in
    let locals_noweak = List.filter (fun v_info -> not (ctx.ask (Queries.IsMultiple v_info))) locals in
    D.filter (fun v -> not (List.mem v locals_noweak)) d

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    Option.may (check_lval  ~ignore_var:true ctx.local) lval;
    List.iter (check_exp ctx.local) args;
    [ctx.local, ctx.local]

  let combine ctx ?(longjmpthrough = false) (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    Option.map_default (rem_lval f_ask au) au lval

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    Option.may (check_lval  ~ignore_var:true ctx.local) lval;
    List.iter (check_exp ctx.local) arglist;
    Option.map_default (rem_lval ctx.ask ctx.local) ctx.local lval

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.bot ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()

  let context _ _ = ()

  let event ctx e octx =
    match e with
    | Events.Poison poisoned -> D.join poisoned ctx.local
    | _ -> ctx.local

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
