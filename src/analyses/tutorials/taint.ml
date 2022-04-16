(** An analysis specification for didactic purposes. *)
(* Helpful link on CIL: https://goblint.in.tum.de/assets/goblint-cil/ *)
(* Goblint documentation: https://goblint.readthedocs.io/en/latest/ *)
(* You may test your analysis on our toy examples by running `ruby scripts/update_suite.rb group tutorials` *)

open Prelude.Ana
open Analyses

(* Use to check if a specific function is a sink / source *)
let is_sink varinfo = Cil.hasAttribute "taint_sink" varinfo.vattr
let is_source varinfo = Cil.hasAttribute "taint_source" varinfo.vattr


(* Fake variable to handle returning from a function *)
let return_varinfo = dummyFunDec.svar

module Spec : Analyses.MCPSpec with module D = SetDomain.Make(CilType.Varinfo) and module C = Lattice.Unit =
struct
  include Analyses.DefaultSpec

  let name () = "taint"
  module D = SetDomain.Make(CilType.Varinfo) (* Change such that you have a fitting local domain, and update also in the signature above *)
  module C = Lattice.Unit

  (* We are context insensitive in this analysis *)
  let context _ _ = ()

  let rec is_exp_tainted (state:D.t) (e:Cil.exp) = match e with
    (* Recurse over the structure in the expression, retruning true if any varinfo appearing in the expression is tainted *)
    | AddrOf v
    | StartOf v
    | Lval v -> is_lval_tainted state v
    | BinOp (_,e1,e2,_) -> is_exp_tainted state e1 || is_exp_tainted state e2
    | Real e
    | Imag e
    | SizeOfE e
    | AlignOfE e
    | CastE (_,e)
    | UnOp (_,e,_) -> is_exp_tainted state e
    | SizeOf _ | SizeOfStr _ | Const _  | AlignOf _ | AddrOfLabel _ -> false
    | Question _ -> failwith "should be optimized away by CIL"
  and is_lval_tainted state = function
    | (Var v, _) -> D.mem v state
    | _ ->
      (* We assume using a tainted offset does not taint the expression, and that our language has no pointers *)
      false

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    match lval with
    | Var v,_ ->
      if is_exp_tainted ctx.local rval then
        D.add v ctx.local
      else
        D.remove v ctx.local
    | _ -> ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    (* Nothing needs to be done here, as the (non-formals) locals are initally untainted *)
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    (* We add a fake variable to indicate the return value is tainted *)
    match exp with
    | Some e when is_exp_tainted ctx.local e  -> D.add return_varinfo ctx.local
    | _ -> ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let caller_local = ctx.local in
    (* Create list of (formal, actual_exp)*)
    let zipped = List.combine f.sformals args in
    let callee_state = List.fold_left (fun ts (f,a) -> if is_exp_tainted caller_local a then D.add f ts else ts) (D.empty ()) zipped in
    (* first component is state of caller, second component is state of callee *)
    [caller_local, callee_state]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) : D.t =
    let caller_local = ctx.local in
    match lval with
    | Some (Var v,_) when D.mem return_varinfo callee_local -> D.add v caller_local
    | _ -> caller_local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    (* here you should check if f is a sink / source and handle it appropriately *)
    if is_source f then
      (* taint return value *)
      match lval with
      | Some (Var v,_) -> D.add v ctx.local
      | _ -> ctx.local
    else if is_sink f then
      if List.exists (is_exp_tainted ctx.local) arglist then
        (M.warn "Tainted variable reaches sink!"; ctx.local)
      else
        ctx.local
    else
      (* neither source nor sink *)
      match lval with
      | Some (Var v,_) ->
        if List.exists (is_exp_tainted ctx.local) arglist then
          D.add v ctx.local
        else
          D.remove v ctx.local
      | _ -> ctx.local

  (* You may leave these alone *)
  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
