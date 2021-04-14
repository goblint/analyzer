(** An analysis specification for didactic purposes. *)

open Prelude.Ana
open Analyses

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  let name () = "constants"

  module I = IntDomain.Flattened
  module D = MapDomain.MapBot (Basetype.Variables) (I)
  module G = Lattice.Unit
  module C = Lattice.Unit

  let val_of () = D.bot ()
  let context _ = ()

  let is_integer_var (v: varinfo) =
    match v.vtype with
      | TInt _ -> true
      | _ -> false

  let get_local = function
    | Var v, NoOffset when is_integer_var v && not (v.vglob || v.vaddrof) -> Some v (* local integer variable whose address is never taken *)
    | _, _ -> None

  let rec eval (state : D.t) (e: exp) =
    match e with
    | Const c -> (match c with
      | CInt64 (i,_,_) -> I.of_int i
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
    (* testen ob tv erfüllbar *)
    let v = eval ctx.local exp in
    match I.to_bool v with
      | Some b when b <> tv -> D.bot () (* if the expression evalautes to not tv, the tv branch is not reachable *)
      | _ -> ctx.local

  let body ctx (f:fundec) : D.t =
    (* Initialize locals to top *)
    List.fold (fun m l -> D.add l (I.top ()) m) ctx.local f.slocals

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local, D.bot ()]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t =
    ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = D.top ()
  let threadspawn ctx lval f args fctx = D.bot ()
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : Spec)
