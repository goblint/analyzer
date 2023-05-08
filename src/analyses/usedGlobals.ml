(** An analysis that collects all global program variables syntactically appearing in each funciton and its callees. *)

open Batteries
open GoblintCil
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "used_globals"
  module D = SetDomain.Make(CilType.Varinfo)
  module C = Lattice.Unit

  let context _ _ = ()

  let collect_in_expression (exp: exp) (p : varinfo -> bool) : D.t =
    (* TODO: Implement as visitor instead?*)
    let collect_varinfo (varinfo: varinfo) (acc: D.t) : D.t =
      if p varinfo then
        D.add varinfo acc
      else
        acc
    in
    let rec collect_in_lval (lhost, offset: lval) (acc: D.t) : D.t =
      match lhost with
      | Var v -> collect_varinfo v acc
      | Mem exp -> collect_in_expression exp acc
    and collect_in_typ (typ: typ) (acc: D.t) : D.t =
      match typ with
      | TPtr (typ, _) -> collect_in_typ typ acc
      | TArray (typ, exp, _) ->
        let acc = collect_in_typ typ acc in
        Option.apply (Option.map collect_in_expression exp) acc
      | TFun _ ->
        (* TODO: Can globals appear in types of functions, and should they be considered?*)
        acc
      | TVoid _
      | TInt _
      | TFloat _
      | TNamed _
      | TComp _
      | TEnum _
      | TBuiltin_va_list _ -> acc
    and collect_in_expression (exp: exp) (acc: D.t) : D.t =
      match exp with
      | Const _  -> acc
      | Lval lval -> collect_in_lval lval acc
      | AlignOf t
      | SizeOf t -> collect_in_typ t acc
      | Real exp
      | Imag exp
      | SizeOfE exp
      | AlignOfE exp -> collect_in_expression exp acc
      | SizeOfStr _ -> acc
      | CastE (typ, exp)
      | UnOp (_, exp, typ) ->
        let acc = collect_in_expression exp acc in
        collect_in_typ typ acc
      | BinOp (_, exp1, exp2, typ) ->
        let acc = collect_in_expression exp1 acc in
        let acc = collect_in_expression exp2 acc in
        collect_in_typ typ acc
      | Question (exp1, exp2, exp3, typ) ->
        let acc = collect_in_expression exp1 acc in
        let acc = collect_in_expression exp2 acc in
        let acc = collect_in_expression exp3 acc in
        collect_in_typ typ acc
      | AddrOf lval ->
        collect_in_lval lval acc
      | AddrOfLabel _ -> acc
      | StartOf lval -> collect_in_lval lval acc
    in
    collect_in_expression exp (D.bot ())

  let collect_globals (exp: exp) : D.t =
    let is_global (v: varinfo) =
      v.vglob
    in
    collect_in_expression exp is_global

  let add_globals_from_exp_option (exp: exp option) (globals: D.t) : D.t =
    let new_globals = Option.map_default collect_globals (D.bot ()) exp in
    D.join globals new_globals

  let add_globals_from_list (args: exp list) (globals : D.t) : D.t =
    List.fold (fun acc exp -> D.join acc (collect_globals exp)) globals args

  let lval_opt_to_exp_opt (lval: lval option) : exp option =
    Option.map (fun lv -> Lval lv) lval

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    let lval_globals = collect_globals (Lval lval) in
    let rval_globals = collect_globals rval in
    D.join ctx.local (D.join lval_globals rval_globals)

  let branch ctx (exp:exp) (tv:bool) : D.t =
    let globals = collect_globals exp in
    D.join ctx.local globals

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp: exp option) (f:fundec) : D.t =
    add_globals_from_exp_option exp ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let callee_state = D.bot () in
    [ctx.local, callee_state]

  let combine_env ctx lval fexp f args fc callee_globals f_ask =
    let globals = D.join ctx.local callee_globals in
    add_globals_from_list args globals

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    add_globals_from_exp_option (lval_opt_to_exp_opt lval) ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    add_globals_from_exp_option (lval_opt_to_exp_opt lval) ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.bot ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate v = D.top ()

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | AccessedGlobals -> `Lifted ctx.local
    | _ -> Queries.Result.top q
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
