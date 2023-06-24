(** Analysis of variables modified since [setjmp] ([modifiedSinceLongjmp]). *)

(* TODO: this name is wrong *)

open GoblintCil
open Analyses

module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "modifiedSinceLongjmp"
  module D = JmpBufDomain.LocallyModifiedMap
  module VS = D.VarSet
  module C = Lattice.Unit

  let context _ _ = ()

  let add_to_all_defined vs d =
    D.map (fun vs' -> VS.union vs vs') d

  let is_relevant v =
    (* Only checks for v.vglob on purpose, acessing espaced locals after longjmp is UB like for any local *)
    not v.vglob (* *) && not (BaseUtil.is_volatile v) && v.vstorage <> Static

  let relevants_from_ls ls =
    if Queries.LS.is_top ls then
      VS.top ()
    else
      Queries.LS.fold (fun (v, _) acc -> if is_relevant v then VS.add v acc else acc) ls (VS.empty ())

  (* transfer functions *)
  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [ctx.local, D.bot ()] (* enter with bot as opposed to IdentitySpec *)

  let combine_env ctx lval fexp f args fc au (f_ask: Queries.ask) =
    let taintedcallee = relevants_from_ls (f_ask.f Queries.MayBeTainted) in
    add_to_all_defined taintedcallee ctx.local

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask:Queries.ask) : D.t =
    ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special arglist with
    | Setjmp _ ->
      let entry = (ctx.prev_node, ctx.control_context ()) in
      let v = D.find entry ctx.local in (* Will make bot binding explicit here *)
      (* LHS of setjmp not marked as tainted on purpose *)
      D.add entry v ctx.local
    | _ ->
      ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.bot ()]
  let exitstate  v = D.top ()

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MayBeModifiedSinceSetjmp entry -> D.find entry ctx.local
    | _ -> Queries.Result.top q

  let event ctx (e: Events.t) octx =
    match e with
    | Access {lvals; kind = Write; _} ->
      add_to_all_defined (relevants_from_ls lvals) ctx.local
    | _ ->
      ctx.local
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
