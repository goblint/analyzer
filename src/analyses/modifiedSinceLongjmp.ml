(** Locally track the variables that may have been written since the corresponding jumpbuffer was set *)

open Prelude.Ana
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

  (* TODO: use Access events instead of reimplementing logic? *)

  let is_relevant v =
    (* Only checks for v.vglob on purpose, acessing espaced locals after longjmp is UB like for any local *)
    not v.vglob (* *) && not (BaseUtil.is_volatile v) && v.vstorage <> Static

  let relevants_from_ls ls =
    if Queries.LS.is_top ls then
      VS.top ()
    else
      Queries.LS.fold (fun (v, _) acc -> if is_relevant v then VS.add v acc else acc) ls (VS.empty ())

  let relevants_from_lval_opt ctx lval = match lval with
    | Some (Var v, _) -> if is_relevant v then VS.singleton v else VS.empty ()
    | Some (Mem e, _) -> relevants_from_ls (ctx.ask (Queries.MayPointTo e))
    | None -> VS.empty ()

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    add_to_all_defined (relevants_from_lval_opt ctx (Some lval)) ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [ctx.local, D.bot ()] (* enter with bot as opposed to IdentitySpec *)

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask:Queries.ask) : D.t =
    let taintedcallee = relevants_from_ls (f_ask.f Queries.MayBeTainted) in
    add_to_all_defined taintedcallee ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special arglist with
    | Setjmp _ ->
      let entry = (ctx.prev_node, ctx.control_context ()) in
      let v = D.find entry ctx.local in (* Will make bot binding explicit here *)
      (* LHS of setjmp not marked as tainted on purpose *)
      D.add entry v ctx.local
    | _ ->
      (* perform shallow and deep invalidate according to Library descriptors *)
      let vs = relevants_from_lval_opt ctx lval in
      let desc = LibraryFunctions.find f in
      let shallow_addrs = LibraryDesc.Accesses.find desc.accs { kind = Write; deep = false } arglist in
      let deep_addrs = LibraryDesc.Accesses.find desc.accs { kind = Write; deep = true } arglist in
      let vs = List.fold_left (fun acc addr -> VS.union acc (relevants_from_ls (ctx.ask (Queries.MayPointTo addr)))) vs shallow_addrs in
      let vs = List.fold_left (fun acc addr -> VS.union acc (relevants_from_ls (ctx.ask (Queries.ReachableFrom addr)))) vs deep_addrs in
      add_to_all_defined vs ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.bot ()]
  let threadspawn ctx lval f args fctx =
    add_to_all_defined (relevants_from_lval_opt ctx lval) ctx.local

  let exitstate  v = D.top ()

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MayBeModifiedSinceSetjmp entry -> D.find entry ctx.local
    | _ -> Queries.Result.top q
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
