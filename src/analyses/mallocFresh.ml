open Prelude.Ana
open Analyses


module Spec =
struct
  include Analyses.IdentitySpec

  (* must fresh variables *)
  module D = SetDomain.Reverse (SetDomain.Make (CilType.Varinfo))
  module C = D

  let name () = "mallocFresh"

  let startstate _ = D.empty ()
  let exitstate _ = D.empty ()

  let assign_lval (ask: Queries.ask) lval local =
    match ask.f (MayPointTo (AddrOf lval)) with
    | ls when Queries.LS.is_top ls || Queries.LS.mem (dummyFunDec.svar, `NoOffset) ls ->
      D.empty ()
    | ls when Queries.LS.exists (fun (v, _) -> not (D.mem v local) && (v.vglob || ThreadEscape.has_escaped ask v)) ls ->
      D.empty ()
    | _ ->
      local

  let assign ctx lval rval =
    assign_lval (Analyses.ask_of_ctx ctx) lval ctx.local

  let combine ctx lval f fd args context f_local =
    match lval with
    | None -> f_local
    | Some lval -> assign_lval (Analyses.ask_of_ctx ctx) lval f_local

  let special ctx lval f args =
    match LibraryFunctions.classify f.vname args with
    | `Malloc _
    | `Calloc _
    | `Realloc _ ->
      begin match ctx.ask HeapVar with
        | `Lifted var -> D.add var ctx.local
        | _ -> ctx.local
      end
    | _ ->
      match lval with
      | None -> ctx.local
      | Some lval -> assign_lval (Analyses.ask_of_ctx ctx) lval ctx.local

  let threadenter ctx lval f args =
    [D.empty ()]

  let threadspawn ctx lval f args fctx =
    D.empty ()

  module A =
  struct
    include BoolDomain.Bool
    let name () = "fresh"
    let may_race f1 f2 = not (f1 || f2)
    let should_print f = f
  end
  let access ctx (a: Queries.access) =
    match a with
    | Memory {var_opt = Some v; _} ->
      D.mem v ctx.local
    | _ ->
      false
end

let _ =
  MCP.register_analysis ~dep:["mallocWrapper"] (module Spec : MCPSpec)
