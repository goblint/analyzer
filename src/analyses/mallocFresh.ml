open GoblintCil
open Analyses


module Spec =
struct
  include Analyses.IdentitySpec

  (* must fresh (or may not fresh) variables *)
  module DF = SetDomain.ToppedSet (CilType.Varinfo) (struct let topname = "All variables" end)
  module D =
  struct 
    include Lattice.Prod (DF) (DF)

    let join (df1, dnf1) (df2, dnf2) =
      let df_both = DF.meet df1 df2 in
      let dnf_any = DF.union dnf1 dnf2 in
      let df_one = DF.diff (DF.union df1 df2) dnf_any in
      (DF.union df_both df_one, dnf_any)

    let leq x y = equal (join x y) y (* TODO: what should happen here? *)

    let meet _ _ = assert false

    let widen = join
    let narrow a b = a
  end
  module C = D

  let name () = "mallocFresh"

  let startstate _ = (DF.empty (), DF.empty ())
  let exitstate _ = (DF.empty (), DF.empty ())

  let assign ctx lval rval =
    ctx.local

  let combine_env ctx lval fexp f args fc au f_ask =
    ctx.local (* keep local as opposed to IdentitySpec *)

  let combine_assign ctx lval f fd args context f_local (f_ask: Queries.ask) =
    f_local (* TODO: no combine_assign in threadEscape *)

  let special ctx lval f args =
    let desc = LibraryFunctions.find f in
    match desc.special args with
    | Malloc _
    | Calloc _
    | Realloc _ ->
      begin match ctx.ask HeapVar with
        | `Lifted var -> (DF.add var (fst ctx.local), (snd ctx.local))
        | _ -> ctx.local
      end
    | _ -> ctx.local

  let threadenter ctx lval f args =
    [(DF.empty (), DF.empty ())]

  let threadspawn ctx lval f args fctx =
    ctx.local

  let event ctx e octx =
    let local = ctx.local in
    match e, local with
    | Events.Escape {escaped; escaped_to}, (df, dnf) when DF.is_top escaped ->
      (DF.empty (), DF.union df dnf)
    | Events.Escape {escaped; escaped_to}, (df, dnf) when not (DF.subset escaped_to df) ->
      (DF.diff df escaped, DF.union dnf (DF.inter df escaped))
    | _ ->
      local

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
      DF.mem v (fst ctx.local)
    | _ ->
      false
end

let _ =
  MCP.register_analysis ~dep:["mallocWrapper"] (module Spec : MCPSpec)
