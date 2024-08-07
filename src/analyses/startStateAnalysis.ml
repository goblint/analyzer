(** Remembers the abstract address value of each parameter at the beginning of each function by adding a ghost variable for each parameter.
    Used by the wrpointer anaylysis. *)

open GoblintCil
open Batteries
open Analyses
open DuplicateVars.Var

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

  let ask_may_point_to (ask: Queries.ask) exp =
    match ask.f (MayPointTo exp) with
    | exception (IntDomain.ArithmeticOnIntegerBot _) -> AD.top()
    | res -> res

  let get_value (ask: Queries.ask) exp = ask_may_point_to ask exp

  (** If e is a known variable, then it returns the value for this variable.
      If e is &x' for a duplicated variable x' of x, then it returns MayPointTo of &x.
      If e is an unknown variable or an expression that is not simply a variable, then it returns top. *)
  let eval (ask: Queries.ask) (d: D.t) (exp: exp): Value.t = match exp with
    | Lval (Var x, NoOffset) -> begin match D.find_opt x d with
        | Some v -> if M.tracing then M.trace "wrpointer-tainted" "QUERY %a : res = %a\n" d_exp exp AD.pretty v;v
        | None -> Value.top()
      end
    | AddrOf (Var x, NoOffset) -> if is_wrpointer_ghost_variable x then (let res = get_value ask (AddrOf (Var (original_variable x), NoOffset)) in if M.tracing then M.trace "wrpointer-tainted" "QUERY %a, id: %d : res = %a\n" d_exp exp x.vid AD.pretty res;res) else Value.top()
    | _ -> Value.top ()

  let startcontext () = D.empty ()
  let startstate v = D.bot ()
  let exitstate = startstate

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    match q with
    | MayPointTo e -> eval (ask_of_ctx ctx) ctx.local e
    | EvalValue e -> Address (eval (ask_of_ctx ctx) ctx.local e)
    | _ -> Result.top q

  let body ctx (f:fundec) =
    (* assign function parameters *)
    List.fold_left (fun st var -> let value = get_value (ask_of_ctx ctx) (Lval (Var var, NoOffset)) in
                     if M.tracing then M.trace "startState" "added value: var: %a; value: %a" d_lval (Var (duplicated_variable var), NoOffset) Value.pretty value;
                     D.add (duplicated_variable var) value st) (D.empty()) f.sformals
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
