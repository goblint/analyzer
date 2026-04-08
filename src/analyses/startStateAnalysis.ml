(** Remembers the abstract address value of each parameter at the beginning of each function by adding a ghost variable for each parameter.
    Used by the c2po analysis. *)

open GoblintCil
open Batteries
open Analyses
open DuplicateVars.Var

(** First, all parameters (=formals) of the function are duplicated (by using DuplicateVars),
    then, we remember the value of each local variable at the beginning of the function a new duplicated variable. *)
module Spec : Analyses.MCPSpec =
struct
  let name () = "startState"
  module AD = ValueDomain.AD
  module D = MapDomain.MapBot (Basetype.Variables) (AD)
  module C = D

  include Analyses.IdentitySpec

  let get_value (ask: Queries.ask) exp =
    try
      ask.f (MayPointTo exp)
    with IntDomain.ArithmeticOnIntegerBot _ ->
      AD.top()

  (** If e is a known variable (=one of the duplicated variables), then it returns the value for this variable.
      If e is an unknown variable or an expression that is not simply a variable, then it returns top. *)
  let eval (ask: Queries.ask) (d: D.t) (exp: exp): AD.t =
    match exp with
    | Lval (Var x, NoOffset) ->
      let maybe_value = D.find_opt x d in
      Option.default (AD.top ()) maybe_value
    | _ ->
      AD.top ()

  let startcontext () =
    D.empty ()

  let startstate v =
    D.bot ()

  let exitstate =
    startstate

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    match q with
    | MayPointTo e ->
      eval (ask_of_man ctx) ctx.local e
    | _ ->
      Result.top q

  let body ctx (f: fundec) =
    let add_var_value st var =
      let lval = Lval (Var var, NoOffset) in
      let value = get_value (ask_of_man ctx) lval in
      let duplicated_var = to_varinfo (DuplicVar var) in
      if M.tracing then M.trace "startState" "added value: var: %a; value: %a" CilType.Varinfo.pretty duplicated_var AD.pretty value;
      D.add duplicated_var value st
    in
    (* assign function parameters *)
    List.fold_left add_var_value (D.empty ()) f.sformals
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
