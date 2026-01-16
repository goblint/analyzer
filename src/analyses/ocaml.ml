(** Simple interprocedural analysis of OCaml C-stubs ([ocaml]). *)

(* Goblint documentation: https://goblint.readthedocs.io/en/latest/ *)
(* Helpful link on CIL: https://goblint.github.io/cil/ *)
(* TODO: Write tests and test them with `ruby scripts/update_suite.rb group ocaml` *)
(* after removing the `SKIP` from the beginning of the tests in tests/regression/90-ocaml/{01-bagnall.c,04-o_inter.c} *)

open GoblintCil
open Analyses

module VarinfoSet = SetDomain.Make(CilType.Varinfo)

(* Use to check if a specific function is a sink / source *)
(* Sources take value-typed arguments *)
(* Sinks may trigger garbage collection *)
let is_sink varinfo = Cil.hasAttribute "ocaml_sink" varinfo.vattr
let is_source varinfo = Cil.hasAttribute "ocaml_source" varinfo.vattr


(** "Fake" variable to handle returning from a function *)
let return_varinfo = dummyFunDec.svar

module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let name () = "ocaml"
  module D =
  struct
    (* The first set contains variables of type value that are definitely in order. The second contains definitely registered variables. *)
    module P = Lattice.Prod (VarinfoSet) (VarinfoSet)
    include P

    let empty () = (VarinfoSet.empty (), VarinfoSet.empty ())

    (* After garbage collection, the second set is written to the first set *)
    let after_gc (accounted, registered) = (registered, registered)

    let mem_a v (accounted, registered) =
      VarinfoSet.mem v accounted

    let mem_r v (accounted, registered) =
      VarinfoSet.mem v registered

    let add_a v (accounted, registered) =
      (VarinfoSet.add v accounted, registered)

    let add_r v (accounted, registered) =
      (accounted, VarinfoSet.add v registered)

    let remove_a v (accounted, registered) =
      (VarinfoSet.remove v accounted, registered)

    let remove_r v (accounted, registered) =
      (accounted, VarinfoSet.remove v registered)
  end
  module C = Printable.Unit

  (* We are context insensitive in this analysis *)
  let context ctx _ _ = ()
  let startcontext () = ()


  (** Determines whether an expression [e] is healthy, given a [state]. *)
  let rec exp_accounted_for (state:D.t) (e:Cil.exp) = match e with
    (* Recurse over the structure in the expression, returning true if all varinfo appearing in the expression is accounted for *)
    | AddrOf v
    | StartOf v
    | Lval v -> lval_accounted_for state v
    | BinOp (_,e1,e2,_) -> exp_accounted_for state e1 && exp_accounted_for state e2
    | Real e
    | Imag e
    | SizeOfE e
    | AlignOfE e
    | CastE (_,e)
    | UnOp (_,e,_) -> exp_accounted_for state e
    | SizeOf _ | SizeOfStr _ | Const _  | AlignOf _ | AddrOfLabel _ -> false
    | Question (b, t, f, _) -> exp_accounted_for state b && exp_accounted_for state t && exp_accounted_for state f
  and lval_accounted_for state = function
    | (Var v, _) ->
      (* Checks whether variable v is accounted for *) (*false*)
      if D.mem_a v state then true else let _ = M.warn "Value %a might be garbage collected" CilType.Varinfo.pretty v in false
    | _ ->
      (* The Gemara asks: is using an offset safe for the expression? The Gemara answers: by default, no. We assume our language has no pointers *)
      false

  (* transfer functions *)

  (** Handles assignment of [rval] to [lval]. *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    let state = ctx.local in
    match lval with
    | Var v,_ ->
      (* If lval is of type value, checks whether rval is accounted for, handles assignment to v accordingly *) (* state *)
      if exp_accounted_for state rval then D.add_a v state
      else D.remove_a v state
    | _ -> state

  (** Handles conditional branching yielding truth value [tv]. *)
  let branch ctx (exp:exp) (tv:bool) : D.t =
    (* Nothing needs to be done *)
    ctx.local

  (** Handles going from start node of function [f] into the function body of [f].
      Meant to handle e.g. initializiation of local variables. *)
  let body ctx (f:fundec) : D.t =
    (* The (non-formals) locals are initially accounted for *)
    let state = ctx.local in
    List.fold_left (fun st v -> D.add_a v st) state f.sformals

  (** Handles the [return] statement, i.e. "return exp" or "return", in function [f]. *)
  let return ctx (exp:exp option) (f:fundec) : D.t =
    let state = ctx.local in
    match exp with
    | Some e ->
      (* Checks that value returned is accounted for. *)
      (* Return_varinfo is used in place of a "real" variable. *)
      (* state *)
      if exp_accounted_for state e then D.add_a return_varinfo state
      else let _ = M.warn "Value returned might be garbage collected" in D.remove_a return_varinfo state
    | None -> state

  (** For a function call "lval = f(args)" or "f(args)",
      [enter] returns a caller state, and the initial state of the callee.
      In [enter], the caller state can usually be returned unchanged, as [combine_env] and [combine_assign] (below)
      will compute the caller state after the function call, given the return state of the callee. *)
  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let caller_state = ctx.local in
    (* Create list of (formal, actual_exp)*)
    (*
    let zipped = List.combine f.sformals args in
    (* TODO: For the initial callee_state, collect formal parameters where the actual is healthy. *)
    let callee_state = List.fold_left (fun ts (f,a) ->
        if exp_accounted_for caller_state a
        then D.add f ts (* TODO: Change accumulator ts here? *)
        else D.remove f ts)
        (D.bot ())
        zipped in
    *)
    (* TODO: Should this be checked with locals or formals, and how exactly? Likely with locals. *)
    let callee_state = caller_state in
    (* first component is state of caller, second component is state of callee *)
    [caller_state, callee_state]

  (** For a function call "lval = f(args)" or "f(args)",
      computes the global environment state of the caller after the call.
      Argument [callee_local] is the state of [f] at its return node. *)
  let combine_env ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) (f_ask: Queries.ask): D.t =
    (* Nothing needs to be done *)
    ctx.local

  (** For a function call "lval = f(args)" or "f(args)",
      computes the state of the caller after assigning the return value from the call.
      Argument [callee_local] is the state of [f] at its return node. *)
  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) (f_ask: Queries.ask): D.t =
    let caller_state = ctx.local in
    (* Records whether lval was accounted for. *) (* caller_state *)
    match lval with (* The variable returned is played by return_varinfo *)
    | Some (Var v, _) -> if D.mem_a return_varinfo callee_local then D.add_a v caller_state
      else let _ = M.warn "Returned value may be garbage-collected" in D.remove_a v caller_state
    | _ -> caller_state

  (** For a call to a _special_ function f "lval = f(args)" or "f(args)",
      computes the caller state after the function call.
      For this analysis, source and sink functions will be considered _special_ and have to be treated here. *)
  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let caller_state = ctx.local in
    (* TODO: Check if f is a sink / source and handle it appropriately *)
    (* To warn about a potential issue in the code, use M.warn. *)
    (* caller_state *)
    let desc = LibraryFunctions.find f in
    match desc.special arglist with
    | OCamlParam params ->
      (* Variables are registered with a Param macro. *)
      (* TODO: Which pattern below do the params in fact match? Does it vary? *)
      (* The function extract_varinfo is from AI - the code can be shortened when we know what the params are. *)
      let rec extract_varinfo (e: Cil.exp): varinfo option = match e with
        (* Direct variable lvalue *)
        | Lval (Var v, _) -> Some v
        (* Dereferenced pointer - try to extract from inner expression *)
        | Lval (Mem inner, _) -> extract_varinfo inner
        (* Address of variable *)
        | AddrOf (Var v, _) -> Some v (* TODO: Maybe this is the only thing a param can be. *)
        (* Address of dereferenced - try to extract from inner *)
        | AddrOf (Mem inner, _) -> extract_varinfo inner
        (* Array start of variable *)
        | StartOf (Var v, _) -> Some v
        (* Array start of dereferenced *)
        | StartOf (Mem inner, _) -> extract_varinfo inner
        (* Type cast - extract from inner expression *)
        | CastE (_, inner) -> extract_varinfo inner
        (* Unary operations - extract from operand *)
        | UnOp (_, inner, _) -> extract_varinfo inner
        (* Real/imaginary parts *)
        | Real inner | Imag inner -> extract_varinfo inner
        (* Binary operations - try both operands *)
        | BinOp (_, e1, e2, _) ->
          (match extract_varinfo e1 with
           | Some v -> Some v
           | None -> extract_varinfo e2)
        (* Size and alignment - may contain var info in the type *)
        | SizeOfE inner | AlignOfE inner -> extract_varinfo inner
        (* Constants, SizeOf, AlignOf, SizeOfStr, AddrOfLabel - no varinfo *)
        | Const _ | SizeOf _ | AlignOf _ | SizeOfStr _ | AddrOfLabel _ -> None
        (* Question conditional *)
        | Question (cond, e1, e2, _) ->
          (match extract_varinfo cond with
           | Some v -> Some v
           | None ->
             (match extract_varinfo e1 with
              | Some v -> Some v
              | None -> extract_varinfo e2))
      in
      List.fold_left (fun state param -> 
          match param with
          | Some e -> 
            (match extract_varinfo e with
             | Some v -> D.add_r v state
             | None -> state)
          | None -> state) caller_state [params.param1; params.param2; params.param3; params.param4; params.param5]
    | OCamlAlloc size_exp ->
      (* Garbage collection may trigger here and overwrite unregistered variables *)
      let _ = M.debug "Garbage collection triggers" in (match lval with
          | Some (Var v, _) -> D.add_a v (D.after_gc caller_state)
          | _ ->
            (* if not (List.for_all (exp_accounted_for caller_state) arglist) then let _ = M.warn "GC might delete value" in D.empty () else *) D.empty ()
        )
    | _ ->
      List.iter (fun e -> ignore (exp_accounted_for caller_state e)) arglist; (* Just to trigger warnings for arguments passed to sinks/sources *)
      if is_source f then match lval with (* Assigning the source's result makes the variable accounted for. *)
        | Some (Var v, _) -> D.add_a v caller_state
        | _ -> caller_state
      else
      if is_sink f then (* Warns if unaccounted variables reach the function. Empties the state of unregistered variables. *)
        let _ = M.debug "Garbage collection triggers" in match lval with
        | Some (Var v, _) -> D.add_a v (D.after_gc caller_state)
        | _ ->
          (* if not (List.for_all (exp_accounted_for caller_state) arglist) then let _ = M.warn "GC might delete value" in D.empty () else *)
          D.after_gc caller_state
      else caller_state

  (* You may leave these alone *)
  let startstate v = D.bot ()
  let threadenter ctx ~multiple lval f args = [D.top ()]
  let threadspawn ctx ~multiple lval f args fctx = ctx.local
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
