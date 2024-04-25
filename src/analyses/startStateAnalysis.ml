(** Remebers the Value of each parameter at the beginning of each function.
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


  let duplicated_variable var = { var with vid = - var.vid; vname = var.vname ^ "'" }
  let original_variable var = { var with vid = - var.vid; vname = String.rchop var.vname }

  let get_value (ask: Queries.ask) exp = ask.f (MayPointTo exp)

  (** If e is a known variable, then it returns the value for this variable.
      If e is an unknown variable, then it returns bot.
      If e is another expression that is not simply a ariable, then it returns top. *)
  let eval (ask: Queries.ask) (d: D.t) (exp: exp): Value.t = match exp with
    | Lval (Var x, NoOffset) -> begin match D.find_opt x d with
        | Some v -> v
        | None -> Value.top()
      end
    | AddrOf (Var x, NoOffset) -> if x.vid < 0 then get_value ask (AddrOf (Var (original_variable x), NoOffset)) else Value.top()
    | _ -> Value.top ()

  let startstate v = D.bot ()
  let exitstate = startstate

  let return ctx exp_opt f =
    (*remember all value of local vars*)
    List.fold_left (fun st var -> let value = get_value (ask_of_ctx ctx) (Lval (Var var, NoOffset)) in
                     if M.tracing then M.trace "startState" "return: added value: var: %a; value: %a" d_lval (Var var, NoOffset) Value.pretty value;
                     D.add (var) value st) (D.empty()) (f.sformals @ f.slocals)


  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    match q with
    | MayPointTo e -> eval (ask_of_ctx ctx) ctx.local e
    | EvalValue e -> Address (eval (ask_of_ctx ctx) ctx.local e)
    | _ -> Result.top q

  let enter ctx var_opt f args =
    (* assign function parameters *)
    [ctx.local, ctx.local]

  let body ctx (f:fundec) =
    List.fold_left (fun st var -> let value = get_value (ask_of_ctx ctx) (Lval (Var var, NoOffset)) in
                     if M.tracing then M.trace "startState" "added value: var: %a; value: %a" d_lval (Var (duplicated_variable var), NoOffset) Value.pretty value;
                     D.add (duplicated_variable var) value st) (D.empty()) f.sformals

  let combine_assign ctx var_opt expr f exprs t_context_opt t ask =
    (*remove duplicated vars and local vars *)
    List.fold_left (fun st var ->
        if M.tracing then M.trace "startState" "removing var: var: %a" d_lval (Var var, NoOffset);
        D.remove (var) st) (D.empty()) (f.sformals @ f.slocals @ (List.map duplicated_variable f.sformals))

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
