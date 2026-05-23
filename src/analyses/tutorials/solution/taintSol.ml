(** Simple interprocedural taint analysis template ([taint]). *)

(** An analysis specification for didactic purposes. *)
(* Goblint documentation: https://goblint.readthedocs.io/en/latest/ *)
(* Helpful link on CIL: https://goblint.github.io/cil/ *)
(* You may test your analysis on our toy examples by running `ruby scripts/update_suite.rb group tutorials` *)
(* after removing the `SKIP` from the beginning of the tests in tests/regression/99-tutorials/{03-taint_simple.c,04-taint_inter.c} *)
(* A tutorial on this analysis is available at:  https://youtu.be/CoVFNbJJfZY *)
(* Caveat: The second half of it gives away solutions, might be best to attempt solving it yourself first.  *)

open GoblintCil
open Analyses
open SimplifiedAnalysis

module VarinfoSet = SetDomain.Make(CilType.Varinfo)

(* Use to check if a specific function is a sink / source *)
let is_sink varinfo = Cil.hasAttribute "taint_sink" varinfo.vattr
let is_source varinfo = Cil.hasAttribute "taint_source" varinfo.vattr


(** "Fake" variable to handle returning from a function *)
let return_varinfo = dummyFunDec.svar

module Spec : SimplifiedSpec =
struct
  let name = "taintSol"
  module V = Printable.Unit
  module G = Lattice.Unit
  module D = SetDomain.Make(CilType.Varinfo) (* TODO: Change such that you have a fitting local domain *)
  module C = Printable.Unit

  (* We are context insensitive in this analysis *)
  let context _ _ _ _ = ()
  let startcontext = ()


  (** Determines whether an expression [e] is tainted, given a [state]. *)
  let rec is_exp_tainted (state:D.t) (e:Cil.exp) = match e with
    (* Recurse over the structure in the expression, returning true if any varinfo appearing in the expression is tainted *)
    | AddrOf v
    | StartOf v
    | Lval v -> is_lval_tainted state v
    | BinOp (_,e1,e2,_) -> is_exp_tainted state e1 || is_exp_tainted state e2
    | Real e
    | Imag e
    | SizeOfE e
    | AlignOfE e
    | CastE (_,_,e)
    | UnOp (_,e,_) -> is_exp_tainted state e
    | SizeOf _ | SizeOfStr _ | Const _  | AlignOf _ | AddrOfLabel _ -> false
    | Question (b, t, f, _) -> is_exp_tainted state b || is_exp_tainted state t || is_exp_tainted state f
  and is_lval_tainted state = function
    | (Var v, _) ->
      (* TODO: Check whether variable v is tainted *)
      D.mem v state
    | _ ->
      (* We assume using a tainted offset does not taint the expression, and that our language has no pointers *)
      false

  (* transfer functions *)

  (** Handles assignment of [rval] to [lval]. *)
  let query _ _ (type a) (q: a Queries.t): a Queries.result =
    Queries.Result.top q

  let assign _ state (lval:lval) (rval:exp) : D.t =
    match lval with
    | Var v,_ ->
      (* TODO: Check whether rval is tainted, handle assignment to v accordingly *)
      if is_exp_tainted state rval then
        D.add v state
      else
        D.remove v state
    | _ -> state

  (** Handles conditional branching yielding truth value [tv]. *)
  let branch _ state (exp:exp) (tv:bool) : D.t =
    (* Nothing needs to be done *)
    ignore (exp, tv);
    state

  (** For a call to a _special_ function f "lval = f(args)" or "f(args)",
      computes the caller state after the function call.
      For this analysis, source and sink functions will be considered _special_ and have to be treated here. *)
  let special _ caller_state (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    (* TODO: Check if f is a sink / source and handle it appropriately *)
    (* To warn about a potential issue in the code, use M.warn. *)
    if is_source f then
      (* taint return value *)
      match lval with
      | Some (Var v,_) -> D.add v caller_state
      | _ -> caller_state
    else if is_sink f then
      if List.exists (is_exp_tainted caller_state) arglist then
        (M.warn "Tainted variable reaches sink!"; caller_state)
      else
        caller_state
    else
      (* neither source nor sink *)
      match lval with
      | Some (Var v,_) ->
        if List.exists (is_exp_tainted caller_state) arglist then
          D.add v caller_state
        else
          D.remove v caller_state
      | _ -> caller_state

  (** Handles going from start node of function [f] into the function body of [f].
      Meant to handle e.g. initialization of local variables. *)
  let body _ state (f:fundec) : D.t =
    (* Nothing needs to be done here, as the (non-formals) locals are initally untainted *)
    ignore f;
    state

  (** Handles the [return] statement, i.e. "return exp" or "return", in function [f]. *)
  let return _ state (exp:exp option) (f:fundec) : D.t =
    match exp with
    | Some e when is_exp_tainted state e ->
      (* TODO: Record whether a tainted value was returned. *)
      (* Hint: You may use return_varinfo in place of a variable. *)
      D.add return_varinfo state
    | _ -> state

  (** For a function call "lval = f(args)" or "f(args)",
      [enter] returns a caller state, and the initial state of the callee.
      In [enter], the caller state can usually be returned unchanged, as [combine_env] and [combine_assign] (below)
      will compute the caller state after the function call, given the return state of the callee. *)
  let enter _ caller_state (lval: lval option) (f:fundec) (args:exp list) : D.t =
    (* Create list of (formal, actual_exp)*)
    let zipped = List.combine f.sformals args in
    (* TODO: For the initial callee_state, collect formal parameters where the actual is tainted. *)
    let callee_state = List.fold_left (fun ts (f,a) ->
        if is_exp_tainted caller_state a
        then D.add f ts (* TODO: Change accumulator ts here? *)
        else ts)
        (D.bot ())
        zipped in
    ignore lval;
    callee_state

  (** For a function call "lval = f(args)" or "f(args)",
      computes the global environment state of the caller after the call.
      Argument [callee_local] is the state of [f] at its return node. *)
  let combine _ caller_state (callee_local:D.t) (lval:lval option) (_: fundec) (_: exp list): D.t =
    (* TODO: Record whether lval was tainted. *)
    match lval with
    | Some (Var v,_) ->
      if D.mem return_varinfo callee_local
      then D.add v caller_state
      else D.remove v caller_state
    | _ -> caller_state

  (* You may leave these alone *)
  let startstate = D.bot ()
  let threadenter _ _ _ _ = D.top ()
end

let _ =
  MCPRegistry.registered_simplified_analysis (module Spec : SimplifiedSpec)
