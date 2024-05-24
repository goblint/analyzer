(** Remembers the abstract address value of each parameter at the beginning of each function by adding a ghost variable for each parameter.
    Used by the wrpointer anaylysis. *)

open GoblintCil
open Batteries
open Analyses


(*First all parameters (=formals) of the function are duplicated (by negating their ID),
    then we remember the value of each local variable at the beginning of the function
  in this new duplicated variable. *)
module Spec : Analyses.MCPSpec =
struct
  let name () = "startState"
  module VD = BaseDomain.VD
  module AD = ValueDomain.AD
  module Value = AD
  module D = MapDomain.MapBot (Basetype.Variables) (Value)
  module C = D

  include Analyses.IdentitySpec


  let duplicated_variable var = { var with vid = - var.vid - 4; vname = var.vname ^ "'" }
  let original_variable var = { var with vid = - (var.vid + 4); vname = String.rchop var.vname }
  let return_varinfo = {dummyFunDec.svar with vid=(-2);vname="@return"}

  let get_value (ask: Queries.ask) exp = ask.f (MayPointTo exp)

  (** If e is a known variable, then it returns the value for this variable.
      If e is &x' for a duplicated variable x' of x, then it returns MayPointTo of &x.
      If e is an unknown variable or an expression that is not simply a variable, then it returns top. *)
  let eval (ask: Queries.ask) (d: D.t) (exp: exp): Value.t = match exp with
    | Lval (Var x, NoOffset) -> begin match D.find_opt x d with
        | Some v -> if M.tracing then M.trace "wrpointer-tainted" "QUERY %a : res = %a\n" d_exp exp AD.pretty v;v
        | None -> Value.top()
      end
    | AddrOf (Var x, NoOffset) -> if x.vid < -1 then (let res = get_value ask (AddrOf (Var (original_variable x), NoOffset)) in if M.tracing then M.trace "wrpointer-tainted" "QUERY %a : res = %a\n" d_exp exp AD.pretty res;res) else Value.top()
    | _ -> Value.top ()

  let startstate v = D.bot ()
  let exitstate = startstate

  (* TODO: there should be a better way to do this, this should be removed here. *)
  let return ctx exp_opt f =
    (* remember all values of local vars *)
    let st = List.fold_left (fun st var -> let value = get_value (ask_of_ctx ctx) (Lval (Var var, NoOffset)) in
                              if M.tracing then M.trace "startState" "return: added value: var: %a; value: %a" d_lval (Var var, NoOffset) Value.pretty value;
                              D.add (var) value st) (D.empty()) (f.sformals @ f.slocals) in
    (* remember value of tainted vars in the return variable *)
    let tainted = ctx.ask (MayBeTainted) in
    let st = D.add return_varinfo tainted st
    in if M.tracing then M.tracel "wrpointer-tainted" "startState: %a; state: %a\n" AD.pretty tainted D.pretty st;st


  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    match q with
    | MayPointTo e -> eval (ask_of_ctx ctx) ctx.local e
    | EvalValue e -> Address (eval (ask_of_ctx ctx) ctx.local e)
    | _ -> Result.top q

  let enter ctx var_opt f args =
    [ctx.local, ctx.local]

  let body ctx (f:fundec) =
    (* assign function parameters *)
    List.fold_left (fun st var -> let value = get_value (ask_of_ctx ctx) (Lval (Var var, NoOffset)) in
                     if M.tracing then M.trace "startState" "added value: var: %a; value: %a" d_lval (Var (duplicated_variable var), NoOffset) Value.pretty value;
                     D.add (duplicated_variable var) value st) (D.empty()) f.sformals

  let combine_env ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc au (f_ask: Queries.ask) =
    ctx.local

  let combine_assign ctx var_opt expr f exprs t_context_opt t ask =
    (* remove duplicated vars and local vars *)
    ctx.local

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
