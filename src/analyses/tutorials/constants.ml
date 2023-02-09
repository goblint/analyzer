
open Prelude.Ana
open Analyses

(** An analysis specification for didactic purposes.
 It only considers definite values of local variables.
 We do not pass information interprocedurally. *)
module Spec : Analyses.MCPSpec =
struct
  let name () = "constants"

  module I = IntDomain.Flattened

  (* Map of (local int) variables to flat integers *)
  module D = MapDomain.MapBot (Basetype.Variables) (I)
  (* No contexts*)
  module C = Lattice.Unit

  include Analyses.IdentitySpec
  let context _ _ = ()

  let is_integer_var (v: varinfo) =
    match v.vtype with
      | TInt _ -> true
      | _ -> false

  let get_local = function
    | Var v, NoOffset when is_integer_var v && not (v.vglob || v.vaddrof) -> Some v (* local integer variable whose address is never taken *)
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
  let assign ctx (lval:lval) (rval:exp) : D.t =
    match get_local lval with
        | Some loc -> D.add loc (eval ctx.local rval) ctx.local
        | None -> ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    let v = eval ctx.local exp in
    match I.to_bool v with
      | Some b when b <> tv -> raise Deadcode (* if the expression evaluates to not tv, the tv branch is not reachable *)
      | _ -> ctx.local

  let body ctx (f:fundec) : D.t =
    (* Initialize locals to top *)
    List.fold (fun m l -> D.add l (I.top ()) m) ctx.local f.slocals

  let return ctx (exp:exp option) (f:fundec) : D.t =
    (* Do nothing, as we are not interested in return values for now. *)
    ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    (* Set the formal int arguments to top *)
    let callee_state = List.fold (fun m l -> D.add l (I.top ()) m) (D.bot ()) f.sformals in
    [(ctx.local, callee_state)]

  let set_local_int_lval_top (state: D.t) (lval: lval option) =
    match lval with
      | Some lv ->
        (match get_local lv with
          | Some local -> D.add local (I.top ()) state
          | _ -> state
        )
      |_ -> state

  let combine ctx ?(longjmpthrough = false) (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) : D.t =
    (* If we have a function call with assignment
        x = f (e1, ... , ek)
      with a local int variable x on the left, we set it to top *)
    set_local_int_lval_top ctx.local lval

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    (* When calling a special function, and assign the result to some local int variable, we also set it to top. *)
    set_local_int_lval_top ctx.local lval

  let startstate v = D.bot ()
  let exitstate v = D.top () (* TODO: why is this different from startstate? *)
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
