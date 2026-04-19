(** Simple interprocedural analysis of OCaml C-stubs ([ocaml]). *)

(* Goblint documentation: https://goblint.readthedocs.io/en/latest/ *)
(* Helpful link on CIL: https://goblint.github.io/cil/ *)
(* TODO: Write tests and test them with `ruby scripts/update_suite.rb group ocaml` *)
(* after removing the `SKIP` from the beginning of the tests in tests/regression/90-ocaml/{01-bagnall.c,04-o_inter.c} *)

open GoblintCil
open Analyses

module VarinfoSet = SetDomain.Make(CilType.Varinfo)

(** "Fake" variable to handle returning from a function *)
let return_varinfo = dummyFunDec.svar
(** Flag for first function entered *)
let first_function = (emptyFunction "@first").svar
(** Flag for deregistering at return *)
let to_deregister = (emptyFunction "@dereg").svar

module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let name () = "ocaml"
  module D =
  struct
    (* The first set contains variables of type value that are definitely accounted. The second contains definitely registered variables. *)
    module P = Lattice.Reverse (Lattice.Prod (VarinfoSet) (VarinfoSet))
    include P

    let empty () = (VarinfoSet.empty (), VarinfoSet.empty ())

    (* After garbage collection, the first set loses variables not in the second set. *)
    let after_gc (accounted, registered) = (VarinfoSet.inter accounted registered, registered)

    (* Untracked variables are always fine. *)
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
    | SizeOf _ | SizeOfStr _ | Const _  | AlignOf _ | AddrOfLabel _ -> true
    | Question (b, t, f, _) -> exp_accounted_for state b && exp_accounted_for state t && exp_accounted_for state f
  and lval_accounted_for state = function
    | (Var v, _) ->
      (* Checks whether variable v is accounted for *) (*false*)
      if D.mem_a v state then true else (M.warn "Value %a might be garbage collected" CilType.Varinfo.pretty v; false)
    | _ ->
      (* The Gemara asks: is using an offset safe for the expression? The Gemara answers: by default, no. We assume our language has no pointers *)
      false

  (** Determines whether an expression [e] is registered, given a [state]. *)
  let rec exp_registered (state:D.t) (e:Cil.exp) = match e with
    (* Recurse over the structure in the expression, returning true if all varinfo appearing in the expression is registered *)
    | AddrOf v
    | StartOf v
    | Lval v -> lval_registered state v
    | BinOp (_,e1,e2,_) -> exp_registered state e1 && exp_registered state e2
    | Real e
    | Imag e
    | SizeOfE e
    | AlignOfE e
    | CastE (_,e)
    | UnOp (_,e,_) -> exp_registered state e
    | SizeOf _ | SizeOfStr _ | Const _  | AlignOf _ | AddrOfLabel _ -> true
    | Question (b, t, f, _) -> exp_registered state b && exp_registered state t && exp_registered state f
  and lval_registered state = function
    | (Var v, _) ->
      (* Checks whether variable v is registered *) (*false*)
      D.mem_r v state
    | _ ->
      false

  let is_value_type (t:typ): bool = match t with
    | TNamed (info, attr) -> info.tname = "value"
    | _ -> false

  let assignment (v:varinfo) (rval:exp) (rval_type:typ) (state:D.t) (warning:string): D.t =
    (* If rval is a pointer, checks whether rval is accounted for, handles assignment to v accordingly *)    
    if Cil.isPointerType rval_type || is_value_type rval_type then
      if exp_accounted_for state rval then
        if exp_registered state rval then D.add_a v state
        else D.add_a v state
      else (M.info "%s" warning; D.remove_a v state)
    else D.add_a v (D.add_r v state)

  (* transfer functions *)

  (** Handles assignment of [rval] to [lval]. *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    let state = ctx.local in
    match lval with
    | Var v,_ -> assignment v rval (Cil.typeOf rval) state "The above is being assigned"
    | _ -> state

  (** Handles conditional branching yielding truth value [tv]. *)
  let branch ctx (exp:exp) (tv:bool) : D.t =
    (* The expression checked must be accounted for *)
    ignore (exp_accounted_for ctx.local exp);
    ctx.local

  (** Handles going from start node of function [f] into the function body of [f].
      Meant to handle e.g. initializiation of local variables. *)
  let body ctx (f:fundec) : D.t =
    let state = ctx.local in
    (* It is assumed that the startstate's values are not nptrs. This avoids warnings from other analyses. *)
    if D.mem_r first_function state then
      List.iter (fun v -> (if is_value_type v.vtype then
                             (ctx.emit (Events.SplitBranch (Cil.Lval (Cil.var v), true)))))
        f.sformals;
    (* TODO: Is there a way without a flag to only emit at the start? *)
    D.remove_r first_function state

  (** Handles the [return] statement, i.e. "return exp" or "return", in function [f]. *)
  let return ctx (exp:exp option) (f:fundec) : D.t =
    let state = ctx.local in
    (* Warn if variables are still registered *)
    List.iter (fun v -> if D.mem_r v state then M.warn "Value %a registered at return" CilType.Varinfo.pretty v) (f.sformals @ f.slocals);
    match exp with
    (* Checks that value returned is accounted for. *)
    (* Return_varinfo is used in place of a "real" variable. *)
    (* TODO: Warn if registered local variables remain. *)
    | Some e -> assignment return_varinfo e (Cil.typeOf e) state "The above is being returned"
    (* Checks that value returned is accounted for. *)
    (* Return_varinfo is used in place of a "real" variable. *)
    (* let return_state = assignment return_varinfo e (Cil.typeOf e) state "The above is being returned" in
       (* Remove this function's formals and locals if correctly returned *)
       D.remove_r to_deregister (if D.mem_r to_deregister return_state then
                                List.fold_left (fun st v -> D.remove_a v (D.remove_r v st)) return_state (f.sformals @ f.slocals)
                              else return_state) *)
    | None -> state

  (** For a function call "lval = f(args)" or "f(args)",
      [enter] returns a caller state, and the initial state of the callee.
      In [enter], the caller state can usually be returned unchanged, as [combine_env] and [combine_assign] (below)
      will compute the caller state after the function call, given the return state of the callee. *)
  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let caller_state = ctx.local in
    List.iter (fun e -> ignore (exp_accounted_for caller_state e)) args;
    (* Entering a function doesn't change the caller state *)
    let callee_state = List.fold_left2 (fun st v rval ->
        (* At the start, arguments are accounted for and not registered. The first_function flag is added.*)
        if rval == MyCFG.unknown_exp then
          if is_value_type v.vtype then D.add_r first_function (D.add_a v (D.remove_r v st))
          else D.add_a v (D.add_r v st)
          (* Arguments of inner functions inherit the caller's state. *)
          (* TODO: Assignment was changed and no longer copies registration. It must be passed using other means. *)
        else assignment v rval (Cil.typeOf rval) st "Entering function with possibly deleted argument")
        caller_state f.sformals args in
    (* first component is state of caller, second component is state of callee *)
    [caller_state, callee_state]

  (** For a function call "lval = f(args)" or "f(args)",
      computes the global environment state of the caller after the call.
      Argument [callee_local] is the state of [f] at its return node. *)
  let combine_env ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) (f_ask: Queries.ask): D.t =
    (* If GC could have triggered during the call, the caller state loses variables not registered in the callee. *)
    (* Since the callee state is basically copied from the caller, the caller state changes the same way through the callee's GCs. *)
    callee_local

  (** For a function call "lval = f(args)" or "f(args)",
      computes the state of the caller after assigning the return value from the call.
      Argument [callee_local] is the state of [f] at its return node. *)
  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) (f_ask: Queries.ask): D.t =
    let caller_state = ctx.local in
    (* Records whether lval was accounted for. Registration for v must already be handled. *)
    (* TODO: What happens if a pointer to a value is returned? *)
    match lval with (* The variable returned is played by return_varinfo *)
    | Some (Var v, _) -> let state =
                           (* Unlike other assignment-like functions, the type here is the return type of the function, not of return_varinfo. *)
                           (match f.svar.vtype with
                            | TFun (t, _, _, _) ->
                              assignment v (Cil.Lval (Cil.var return_varinfo)) t caller_state "The above is being combined"
                            | _ -> caller_state) in
      D.remove_a return_varinfo (D.remove_r return_varinfo state)
    | _ -> caller_state

  (** For a call to a _special_ function f "lval = f(args)" or "f(args)",
      computes the caller state after the function call. *)
  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let caller_state = ctx.local in
    (* To warn about a potential issue in the code, use M.warn. *)
    (* caller_state *)
    let desc = LibraryFunctions.find f in
    List.iter (fun e -> ignore (exp_accounted_for caller_state e)) arglist; (* Just to trigger warnings for arguments passed to special functions *)
    match desc.special arglist with
    | OCamlParam params ->
      (* Variables are registered with a Param macro. *)
      List.fold_left (fun state param -> match param with
          | AddrOf (Var v, _) -> (match ctx.ask (Queries.EvalInt (Cil.Lval (Cil.var v))) with
              | `Bot -> ()
              | `Lifted x -> if IntDomain.IntDomTuple.equal_to Z.zero x <> `Neq then M.warn "Null pointer registered"
              | `Top -> M.warn "Null pointer possibly registered"
            );
            D.add_r v state
          | _ -> state
        ) caller_state params
    | OCamlAlloc size_exp ->
      (* Garbage collection may trigger here and overwrite unregistered variables. *)
      M.debug "Garbage collection triggers";
      (match lval with
       | Some (Var v, _) -> D.add_a v (D.after_gc caller_state)
       | _ -> D.after_gc caller_state
      )
    | OCamlReturn ->
      (* Deregisters all formal and local variables. *)
      (* TODO: Write tests to check not too much is deregistered. *)
      let caller_fun = Node.find_fundec ctx.node in
      List.fold_left (fun st v -> D.remove_r v st) caller_state (caller_fun.sformals @ caller_fun.slocals)
    | _ -> caller_state

  (* You may leave these alone *)
  let startstate v = D.empty ()
  let threadenter ctx ~multiple lval f args = [D.top ()]
  let threadspawn ctx ~multiple lval f args fctx = ctx.local
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
