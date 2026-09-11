(** Interprocedural analysis of OCaml C-stubs ([ocaml]). *)

(* Goblint documentation: https://goblint.readthedocs.io/en/latest/ *)
(* Helpful link on CIL: https://goblint.github.io/cil/ *)
(* TODO: Write tests and test them with `ruby scripts/update_suite.rb group ocaml` *)
(* after removing the `SKIP` from the beginning of the tests in tests/regression/90-ocaml/{01-bagnall.c,04-o_inter.c} *)

open GoblintCil
open Analyses
open ReturnUtil

module VarinfoSet = SetDomain.Make(CilType.Varinfo)
module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let name () = "ocaml"
  module D =
  struct
    (* The first set contains variables of type value that are definitely accounted for. The second contains definitely registered variables. There is a flag for the first function. *)
    module P = Lattice.Prod3 (Lattice.Reverse (VarinfoSet)) (Lattice.Lift3 (Lattice.Reverse (VarinfoSet)) (Lattice.Liszt (Lattice.Reverse (VarinfoSet))) (Lattice.Reverse (VarinfoSet))) (BoolDomain.MayBool)
    include P

    let empty () = (VarinfoSet.empty (), `Bot, false)

    (* Puts all registered variables into a single set. *)
    let flatten_r (accounted, registered, first) =
      match registered with
      | `Lifted1 r -> r
      | `Lifted2 r -> List.fold_left (fun acc rr -> VarinfoSet.union acc rr) (VarinfoSet.empty ()) r
      | `Lifted3 r -> r
      | _ -> VarinfoSet.empty ()

    (* After garbage collection, the first set loses variables not in the registered stack. *)
    let after_gc (accounted, registered, first) =
      match registered with
      | `Bot -> (VarinfoSet.empty (), registered, first)
      | `Lifted1 r -> (VarinfoSet.inter accounted r, `Lifted1 r, first)
      | `Lifted2 r -> (VarinfoSet.inter accounted (flatten_r (accounted, registered, first)), `Lifted2 r, first)
      | `Lifted3 r -> (VarinfoSet.inter accounted r, `Lifted3 r, first)
      | `Top -> (VarinfoSet.empty (), registered, first) (* Top means that we do not know the format of registrations and therefore cannot retrieve info about registered variables. *)

    let mem_a v (accounted, registered, first) =
      VarinfoSet.mem v accounted

    let mem_r v (accounted, registered, first) =
      VarinfoSet.mem v (flatten_r (accounted, registered, first))

    let add_a v (accounted, registered, first) =
      (VarinfoSet.add v accounted, registered, first)

    (* Initialises the registration set in the right mode and/or opens a new block in the stack*)
    let add_r vs mode (accounted, registered, first) =
      match registered with
      | `Bot -> (match mode with
          | "CAMLparam0" -> (accounted, `Lifted1 (VarinfoSet.empty ()), first)
          | "CAMLparam" -> failwith "CAMLlocal used without CAMLparam"
          | "Begin_roots" -> (accounted, `Lifted2 [vs], first)
          | "" -> (accounted, `Lifted3 vs, first)
          | _ -> failwith "Undefined mode")
      | `Lifted1 r -> (match mode with
          | "CAMLparam0" -> (accounted, registered, first)
          | "CAMLparam" -> (accounted, `Lifted1 (VarinfoSet.union vs r), first)
          | "Begin_roots" -> M.warn "Begin_roots used with CAMLparam0"; (accounted, `Lifted2 [vs; r], first)
          | "" -> (accounted, `Lifted1 (VarinfoSet.union vs r), first)
          | _ -> failwith "Undefined mode")
      | `Lifted2 (r::rs) -> (match mode with
          | "CAMLparam0" -> failwith "CAMLparam used with Begin_roots"
          | "CAMLparam" -> failwith "CAMLlocal used with Begin_roots"
          | "Begin_roots" -> (accounted, `Lifted2 (vs::r::rs), first)
          | "" -> (accounted, `Lifted2 ((VarinfoSet.union vs r)::rs), first) (* TODO: When a untracked variable is added this way, the next End_roots will remove it together with what is needed. *)
          | _ -> failwith "Undefined mode")
      | `Lifted3 r -> (match mode with
          | "CAMLparam0" -> (accounted, `Lifted1 r, first)
          | "CAMLparam" -> failwith "CAMLlocal used without CAMLparam"
          | "Begin_roots" -> M.tracel "Ocaml" "%a" VarinfoSet.pretty vs; (accounted, `Lifted2 [vs; r], first)
          | "" -> (accounted, `Lifted3 (VarinfoSet.union vs r), first)
          | _ -> failwith "Undefined mode")
      | `Top -> (accounted, registered, first)
      | _ -> failwith "Registration stack in analysis is empty, but not in bottom state. This should not happen."

    let add_r_v v mode (accounted, registered, first) =
      add_r (VarinfoSet.singleton v) mode (accounted, registered, first)

    let remove_a v (accounted, registered, first) =
      (VarinfoSet.remove v accounted, registered, first)

    let rec remover v registered =
      match registered with
      | [] -> []
      | r::rs -> (VarinfoSet.remove v r)::(remover v rs)

    let remove_r v (accounted, registered, first) =
      match registered with
      | `Lifted1 r -> (accounted, `Lifted1 (VarinfoSet.remove v r), first)
      | `Lifted2 r -> (accounted, `Lifted2 (remover v r), first)
      | `Lifted3 r -> (accounted, `Lifted3 (VarinfoSet.remove v r), first)
      | _ -> (accounted, registered, first) (* TODO: Choose one representation when modifying the top value *)

    (* Simulates End_roots by removing one block. *)
    (* TODO: End_roots actually removes blocks until it has removed one named caml_roots_block. *)
    let pop_r (accounted, registered, first) =
      match registered with
      | `Lifted1 r -> failwith "Roots ended on a simple set"
      | `Lifted2 r ->
        (match r with
         | [] -> (accounted, `Lifted2 [], first)
         | _::rs -> (accounted, `Lifted2 rs, first))
      | `Lifted3 r -> failwith "Roots ended before beginning"
      | _ -> (accounted, registered, first) (* TODO: Choose one representation when modifying the top value *)

    (* Removes all blocks created in the current scope, like CAMLreturn. *)
    (* vs: current function's formals and locals *)
    let rec after_drop vs (accounted, registered, first) =
      match registered with
      | `Lifted1 r -> (accounted, `Lifted1 (VarinfoSet.diff r (VarinfoSet.of_list vs)), first)
      | `Lifted2 r ->
        (match r with
         | [] -> (accounted, `Lifted2 [], first)
         | regd::regds ->
           (* Checks whether any of the variables in vs is in the current block. *)
           (* TODO: If CAMLparam0 is not used, this will also delete the previous block. Could this be improved? *)
           if List.exists (fun v -> VarinfoSet.mem v regd) vs then
             after_drop vs (accounted, `Lifted2 regds, first)
           else (accounted, registered, first))
      | `Lifted3 r -> (accounted, `Lifted3 (VarinfoSet.diff r (VarinfoSet.of_list vs)), first)
      | _ -> (accounted, registered, first) (* TODO: Choose one representation when modifying the top value *)

    let set_first_function (accounted, registered, first) =
      (accounted, registered, true)

    let clear_first_function (accounted, registered, first) =
      (accounted, registered, false)

    let is_first_function (accounted, registered, first) =
      first
  end

  (* We are context sensitive in this analysis *)
  module C = Printable.Unit

  (* We are context insensitive in this analysis *)
  let context man _ _ = ()
  let startcontext () = ()

  module P =
  struct
    let name () = "p"
    type t = int
    [@@deriving eq, ord, show, hash]
    include Printable.SimpleShow(struct type nonrec t = t let show = show end)
    include Printable.StdLeaf
    (* To get path sensitivity, the name of this analysis must be put into options.schema.json. *)
    let of_elt (accounted, registered, first) = match registered with
      | `Bot -> -3
      | `Lifted1 _ -> -2
      | `Lifted2 r -> List.length r
      | `Lifted3 _ -> -1
      | _ -> -4
  end

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
        D.add_a v state
      else (M.info "%s" warning; D.remove_a v state)
      (* If an untracked variable is assigned to another, it is already in R, so R cannot be empty. *)
    else D.add_a v (D.add_r_v v "" state)

  (* transfer functions *)

  (** Handles assignment of [rval] to [lval]. *)
  let assign man (lval:lval) (rval:exp) : D.t =
    let state = man.local in
    match lval with
    | Var v,_ -> assignment v rval (Cil.typeOf rval) state "The above is being assigned"
    | _ -> state

  (** Handles conditional branching yielding truth value [tv]. *)
  let branch man (exp:exp) (tv:bool) : D.t =
    (* The expression checked must be accounted for *)
    ignore (exp_accounted_for man.local exp);
    man.local

  (** Handles going from start node of function [f] into the function body of [f].
      Meant to handle e.g. initializiation of local variables. *)
  let body man (f:fundec) : D.t =
    let state = man.local in
    (* It is assumed that the startstate's values are not nptrs. This avoids warnings from other analyses. *)
    if D.is_first_function state then
      List.iter (fun v -> (if is_value_type v.vtype then
                             (man.emit (Events.SplitBranch (Cil.Lval (Cil.var v), true)))))
        f.sformals;
    D.clear_first_function state

  (** Handles the [return] statement, i.e. "return exp" or "return", in function [f]. *)
  let return man (exp:exp option) (f:fundec) : D.t =
    let state = man.local in
    (* Warns if values or pointers are still registered. *)
    List.iter (fun v -> if D.mem_r v state &&
                           (Cil.isPointerType (Cil.typeOf (Cil.Lval (Cil.var v))) ||
                            is_value_type (Cil.typeOf (Cil.Lval (Cil.var v))))
                then M.warn "Value %a registered at return" CilType.Varinfo.pretty v) (f.sformals @ f.slocals);
    (* After the warning, they need to be counted as deregistered to not upset the stack structure of the outer function. *)
    let state = D.after_drop (f.sformals @ f.slocals) state in
    match exp with
    (* Checks that value returned is accounted for. *)
    (* Return_varinfo is used in place of a "real" variable. *)
    | Some e -> assignment (return_varinfo ()) e (Cil.typeOf e) state "The above is being returned"
    | None -> state

  (** For a function call "lval = f(args)" or "f(args)",
      [enter] returns a caller state, and the initial state of the callee.
      In [enter], the caller state can usually be returned unchanged, as [combine_env] and [combine_assign] (below)
      will compute the caller state after the function call, given the return state of the callee. *)
  let enter man (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let caller_state = man.local in
    List.iter (fun e -> ignore (exp_accounted_for caller_state e)) args;
    (* Entering a function doesn't change the caller state *)
    let callee_state = List.fold_left2 (fun st v rval ->
        (* At the start, arguments are accounted for and not registered. The first_function flag is set.*)
        if rval == MyCFG.unknown_exp then
          if is_value_type v.vtype then D.set_first_function (D.add_a v st)
          (* TODO: The mode should be allowed to change as a new function is entered. *)
          else D.add_a v (D.add_r_v v "" st)
          (* Arguments of inner functions inherit the caller's state. *)
          (* Every registration copied becomes its own set to avoid them going to a previous bigger set. TODO: This comment is false. Rethink. *)
        else (*assignment v rval (Cil.typeOf rval) st "Entering function with possibly deleted argument")*)
        if Cil.isPointerType (Cil.typeOf rval) || is_value_type (Cil.typeOf rval) then
          if exp_accounted_for st rval then
            if exp_registered st rval then D.add_a v (D.add_r_v v "" st)
            else D.add_a v (D.remove_r v st)
          else (M.info "Entering function with possibly deleted argument"; D.remove_a v st)
        else D.add_a v (D.add_r_v v "" st))
        caller_state f.sformals args in
    (* first component is state of caller, second component is state of callee *)
    [caller_state, callee_state]

  (** For a function call "lval = f(args)" or "f(args)",
      computes the global environment state of the caller after the call.
      Argument [callee_local] is the state of [f] at its return node. *)
  let combine_env man (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) (f_ask: Queries.ask): D.t =
    (* If GC could have triggered during the call, the caller state loses variables not registered in the callee. *)
    (* Since the callee state is basically copied from the caller, the caller state changes the same way through the callee's GCs. *)
    callee_local

  (** For a function call "lval = f(args)" or "f(args)",
      computes the state of the caller after assigning the return value from the call.
      Argument [callee_local] is the state of [f] at its return node. *)
  let combine_assign man (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) (f_ask: Queries.ask): D.t =
    let caller_state = man.local in
    (* Records whether lval was accounted for. Registration for v must already be handled. *)
    (* TODO: What happens if a pointer to a value is returned? *)
    match lval with (* The variable returned is played by return_varinfo *)
    | Some (Var v, _) -> let state =
                           (* Unlike other assignment-like functions, the type here is the return type of the function, not of return_varinfo. *)
                           (match f.svar.vtype with
                            | TFun (t, _, _, _) ->
                              assignment v (Cil.Lval (Cil.var (return_varinfo ()))) t caller_state "The above is being combined"
                            | _ -> caller_state) in
      D.remove_a (return_varinfo ()) (D.remove_r (return_varinfo ()) state)
    | _ -> caller_state

  (** For a call to a _special_ function f "lval = f(args)" or "f(args)",
      computes the caller state after the function call. *)
  let special man (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let caller_state = man.local in
    (* To warn about a potential issue in the code, use M.warn. *)
    let desc = LibraryFunctions.find f in
    List.iter (fun e -> ignore (exp_accounted_for caller_state e)) arglist; (* Just to trigger warnings for arguments passed to special functions *)
    match desc.special arglist with
    | OCamlParam0 -> D.add_r (VarinfoSet.empty ()) "CAMLparam0" caller_state
    | OCamlParam params ->
      (* Variables are registered. *)
      List.fold_left (fun state param -> match param with
          | AddrOf (Var v, _) -> (match man.ask (Queries.EvalInt (Cil.Lval (Cil.var v))) with
              | `Bot -> ()
              | `Lifted x -> if IntDomain.IntDomTuple.equal_to Z.zero x <> `Neq then M.warn "Null pointer registered"
              | `Top -> M.warn "Null pointer possibly registered"
            );
            D.add_r_v v "CAMLparam" state
          | _ -> state
        ) caller_state params
    | OCamlAlloc size_exp ->
      (* Garbage collection may trigger here and overwrite unregistered variables. *)
      M.debug "Garbage collection triggers";
      (match lval with
       | Some (Var v, _) -> D.add_a v (D.after_gc caller_state)
       | _ -> D.after_gc caller_state
      )
    | OCamlDrop ->
      (* Deregisters all formal and local variables. *)
      let caller_fun = Node.find_fundec man.node in
      D.after_drop (caller_fun.sformals @ caller_fun.slocals) caller_state
    | OCamlBeginRoots params ->
      D.add_r
        (* Variables are registered as a single block. *)
        (List.fold_left (fun block param -> match param with
             | AddrOf (Var v, _) -> (match man.ask (Queries.EvalInt (Cil.Lval (Cil.var v))) with
                 | `Bot -> ()
                 | `Lifted x -> if IntDomain.IntDomTuple.equal_to Z.zero x <> `Neq then M.warn "Null pointer registered"
                 | `Top -> M.warn "Null pointer possibly registered"
               ); VarinfoSet.add v block
             | _ -> block
           ) (VarinfoSet.empty ()) params)
        "Begin_roots" caller_state
    | OCamlEndRoots -> D.pop_r caller_state
    | _ -> caller_state

  (* You may leave these alone *)
  let startstate v = D.empty ()
  let threadenter man ~multiple lval f args = [D.top ()]
  let threadspawn man ~multiple lval f args fman = man.local
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)