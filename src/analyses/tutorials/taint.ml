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

module VarinfoSet = SetDomain.Make(CilType.Varinfo)

(* Use to check if a specific function is a sink / source *)
let is_sink varinfo = Cil.hasAttribute "taint_sink" varinfo.vattr
let is_source varinfo = Cil.hasAttribute "taint_source" varinfo.vattr


(** "Fake" variable to handle returning from a function *)
let return_varinfo = dummyFunDec.svar

module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let name () = "taint"
  module D = Lattice.Unit (* TODO: Change such that you have a fitting local domain *)
  module C = Printable.Unit

  (* We are context insensitive in this analysis *)
  let context man _ _ = ()
  let startcontext () = ()


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
    | CastE (_,e)
    | UnOp (_,e,_) -> is_exp_tainted state e
    | SizeOf _ | SizeOfStr _ | Const _  | AlignOf _ | AddrOfLabel _ -> false
    | Question (b, t, f, _) -> is_exp_tainted state b || is_exp_tainted state t || is_exp_tainted state f
  and is_lval_tainted state = function
    | (Var v, _) ->
      (* TODO: Check whether variable v is tainted *)
      false
    | _ ->
      (* We assume using a tainted offset does not taint the expression, and that our language has no pointers *)
      false

  (* transfer functions *)

  (** Handles assignment of [rval] to [lval]. *)
  let assign man (lval:lval) (rval:exp) : D.t =
    let state = man.local in
    match lval with
    | Var v,_ ->
      (* TODO: Check whether rval is tainted, handle assignment to v accordingly *)
      state
    | _ -> state

  (** Handles conditional branching yielding truth value [tv]. *)
  let branch man (exp:exp) (tv:bool) : D.t =
    (* Nothing needs to be done *)
    man.local

  (** For a call to a _special_ function f "lval = f(args)" or "f(args)",
      computes the caller state after the function call.
      For this analysis, source and sink functions will be considered _special_ and have to be treated here. *)
  let special man (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let caller_state = man.local in
    (* TODO: Check if f is a sink / source and handle it appropriately *)
    (* To warn about a potential issue in the code, use M.warn. *)
    caller_state

  (** Handles going from start node of function [f] into the function body of [f].
      Meant to handle e.g. initialization of local variables. *)
  let body man (f:fundec) : D.t =
    (* Nothing needs to be done here, as the (non-formals) locals are initally untainted *)
    man.local

  (** Handles the [return] statement, i.e. "return exp" or "return", in function [f]. *)
  let return man (exp:exp option) (f:fundec) : D.t =
    let state = man.local in
    match exp with
    | Some e ->
      (* TODO: Record whether a tainted value was returned. *)
      (* Hint: You may use return_varinfo in place of a variable. *)
      state
    | None -> state

  (** For a function call "lval = f(args)" or "f(args)",
      [enter] returns a caller state, and the initial state of the callee.
      In [enter], the caller state can usually be returned unchanged, as [combine_env] and [combine_assign] (below)
      will compute the caller state after the function call, given the return state of the callee. *)
  let enter man (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let caller_state = man.local in
    (* Create list of (formal, actual_exp)*)
    let zipped = List.combine f.sformals args in
    (* TODO: For the initial callee_state, collect formal parameters where the actual is tainted. *)
    let callee_state = List.fold_left (fun ts (f,a) ->
        if is_exp_tainted caller_state a
        then ts (* TODO: Change accumulator ts here? *)
        else ts)
        (D.bot ())
        zipped in
    (* first component is state of caller, second component is state of callee *)
    [caller_state, callee_state]

  (** For a function call "lval = f(args)" or "f(args)",
      computes the global environment state of the caller after the call.
      Argument [callee_local] is the state of [f] at its return node. *)
  let combine_env man (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) (f_ask: Queries.ask): D.t =
    (* Nothing needs to be done *)
    man.local

  (** For a function call "lval = f(args)" or "f(args)",
      computes the state of the caller after assigning the return value from the call.
      Argument [callee_local] is the state of [f] at its return node. *)
  let combine_assign man (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) (f_ask: Queries.ask): D.t =
    let caller_state = man.local in
    (* TODO: Record whether lval was tainted. *)
    caller_state

  (* You may leave these alone *)
  let startstate v = D.bot ()
  let threadenter man ~multiple lval f args = [D.top ()]
  let threadspawn man ~multiple lval f args fman = man.local
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
