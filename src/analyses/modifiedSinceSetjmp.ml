(** Analysis of variables modified since [setjmp] ([modifiedSinceSetjmp]). *)

open GoblintCil
open Analyses

module Spec =
struct
  include Analyses.IdentityUnitContextsSpec

  let name () = "modifiedSinceSetjmp"
  module D = JmpBufDomain.LocallyModifiedMap
  module VS = D.VarSet

  let add_to_all_defined vs d =
    D.map (fun vs' -> VS.union vs vs') d

  let is_relevant v =
    (* Only checks for v.vglob on purpose, acessing espaced locals after longjmp is UB like for any local *)
    not v.vglob (* *) && not (BaseUtil.is_volatile v) && v.vstorage <> Static

  let relevants_from_ad ls =
    (* TODO: what about AD with both known and unknown pointers? *)
    if Queries.AD.is_top ls then
      VS.top ()
    else
      Queries.AD.fold (fun addr acc ->
          match addr with
          | Queries.AD.Addr.Addr (v, _) when is_relevant v -> VS.add v acc
          | _ -> acc
        ) ls (VS.empty ())

  (* transfer functions *)
  let enter man (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [man.local, D.bot ()] (* enter with bot as opposed to IdentitySpec *)

  let combine_env man lval fexp f args fc au (f_ask: Queries.ask) =
    let taintedcallee = relevants_from_ad (f_ask.f Queries.MayBeTainted) in
    add_to_all_defined taintedcallee man.local

  let combine_assign man (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask:Queries.ask) : D.t =
    man.local

  let special man (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special arglist with
    | Setjmp _ ->
      let entry = (man.prev_node, man.control_context ()) in
      let v = D.find entry man.local in (* Will make bot binding explicit here *)
      (* LHS of setjmp not marked as tainted on purpose *)
      D.add entry v man.local
    | _ ->
      man.local

  let startstate v = D.bot ()
  let threadenter man ~multiple lval f args = [D.bot ()]
  let exitstate  v = D.top ()

  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MayBeModifiedSinceSetjmp entry -> D.find entry man.local
    | _ -> Queries.Result.top q

  let event man (e: Events.t) oman =
    match e with
    | Access {ad; kind = Write; _} ->
      add_to_all_defined (relevants_from_ad ad) man.local
    | _ ->
      man.local
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
