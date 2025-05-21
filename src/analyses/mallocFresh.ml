(** Analysis of unescaped (i.e. thread-local) heap locations ([mallocFresh]). *)

open GoblintCil
open Analyses


module Spec =
struct
  include Analyses.IdentitySpec

  (* must fresh variables *)
  module D = SetDomain.Reverse (SetDomain.ToppedSet (CilType.Varinfo) (struct let topname = "All variables" end)) (* need bot (top) for hoare widen *)
  include Analyses.ValueContexts(D)

  let name () = "mallocFresh"

  let startstate _ = D.empty ()
  let exitstate _ = D.empty ()

  let assign_lval (ask: Queries.ask) lval local =
    match ask.f (MayPointTo (AddrOf lval)) with
    | ad when Queries.AD.is_top ad -> D.empty ()
    | ad when Queries.AD.exists (function
        | Queries.AD.Addr.Addr (v,_) -> not (D.mem v local) && (v.vglob || ThreadEscape.has_escaped ask v)
        | _ -> false
      ) ad -> D.empty ()
    | _ -> local

  let assign man lval rval =
    assign_lval (Analyses.ask_of_man man) lval man.local

  let combine_env man lval fexp f args fc au f_ask =
    man.local (* keep local as opposed to IdentitySpec *)

  let combine_assign man lval f fd args context f_local (f_ask: Queries.ask) =
    match lval with
    | None -> f_local
    | Some lval -> assign_lval (Analyses.ask_of_man man) lval f_local

  let special man lval f args =
    let desc = LibraryFunctions.find f in
    let alloc_var on_stack =
      match man.ask (AllocVar {on_stack = on_stack}) with
      | `Lifted var -> D.add var man.local
      | _ -> man.local
    in
    match desc.special args with
    | Malloc _
    | Calloc _
    | Realloc _ -> alloc_var false
    | Alloca _ -> alloc_var true
    | _ ->
      match lval with
      | None -> man.local
      | Some lval -> assign_lval (Analyses.ask_of_man man) lval man.local

  let threadenter man ~multiple lval f args =
    [D.empty ()]

  let threadspawn man ~multiple lval f args fman =
    D.empty ()

  module A =
  struct
    include BoolDomain.Bool
    let name () = "fresh"
    let may_race f1 f2 = not (f1 || f2)
    let should_print f = f
  end
  let access man (a: Queries.access) =
    match a with
    | Memory {var_opt = Some v; _} ->
      D.mem v man.local
    | _ ->
      false
end

let _ =
  MCP.register_analysis ~dep:["mallocWrapper"] (module Spec : MCPSpec)
