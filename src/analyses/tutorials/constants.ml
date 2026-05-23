(** Simple intraprocedural integer constants analysis example ([constants]). *)

open GoblintCil
open Analyses
open SimplifiedAnalysis

(** An analysis specification for didactic purposes.
 It only considers definite values of local variables.
 We do not pass information interprocedurally. *)
module Spec : SimplifiedSpec =
struct
  let name = "constants"
  module V = Printable.Unit
  module G = Lattice.Unit

  module I = IntDomain.Flattened

  (* Map of (local int) variables to flat integers *)
  module D = MapDomain.MapBot (Basetype.Variables) (I)

  (* No contexts *)
  module C = Printable.Unit

  let get_local = function
    | Var v, NoOffset when isIntegralType v.vtype && not (v.vglob || v.vaddrof) -> Some v (* local integer variable whose address is never taken *)
    | _, _ -> None

  (** Evaluates expressions *)
  let rec eval (state : D.t) (e: exp) =
    match e with
    | Const c -> (match c with
        | CInt (i,_,_) ->
          (try I.of_int (Z.to_int64 i) with Z.Overflow -> I.top ())
        (* Our underlying int domain here can not deal with values that do not fit into int64 *)
        (* Use Z.to_int64 instead of Cilint.int64_of_cilint to get exception instead of silent wrap-around *)
        | _ -> I.top ()
      )
    | Lval lv -> (match get_local lv with
      | Some v -> D.find v state
      | _ -> I.top ()
      )
    | BinOp (PlusA, e1, e2, t) -> (
      let v1 = eval state e1 in
      let v2 = eval state e2 in
      I.add v1 v2
    )
    | _ -> I.top ()

  (* transfer functions *)
  let query _ _ (type a) (q: a Queries.t): a Queries.result =
    Queries.Result.top q

  let assign _ state (lval:lval) (rval:exp) : D.t =
    match get_local lval with
    | Some loc -> D.add loc (eval state rval) state
    | None -> state

  let branch _ state (exp:exp) (tv:bool) : D.t =
    let v = eval state exp in
    match I.to_bool v with
      | Some b when b <> tv -> raise Deadcode (* if the expression evaluates to not tv, the tv branch is not reachable *)
      | _ -> state

  let body _ state (f:fundec) : D.t =
    (* Initialize locals to top *)
    List.fold_left (fun m l -> D.add l (I.top ()) m) state f.slocals

  let return _ state (exp:exp option) (f:fundec) : D.t =
    (* Do nothing, as we are not interested in return values for now. *)
    state

  let enter _ (_: D.t) (_: lval option) (f:fundec) (_: exp list) : D.t =
    (* Set the formal int arguments to top *)
    List.fold_left (fun m l -> D.add l (I.top ()) m) (D.bot ()) f.sformals

  let set_local_int_lval_top (state: D.t) (lval: lval option) =
    match lval with
      | Some lv ->
        (match get_local lv with
          | Some local -> D.add local (I.top ()) state
          | _ -> state
        )
      |_ -> state

  let combine _ state (_: D.t) (lval:lval option) (_: fundec) (_: exp list): D.t =
    (* If we have a function call with assignment
        x = f (e1, ... , ek)
      with a local int variable x on the left, we set it to top *)
    set_local_int_lval_top state lval

  let special _ state (lval: lval option) (_:varinfo) (_:exp list) : D.t =
    (* When calling a special function, and assign the result to some local int variable, we also set it to top. *)
    set_local_int_lval_top state lval

  let startstate = D.bot ()
  let startcontext = ()
  let context _ (_, c) _ _ = c
  let threadenter _ _ _ _ = D.top ()
end

let _ =
  MCPRegistry.registered_simplified_analysis (module Spec : SimplifiedSpec)
