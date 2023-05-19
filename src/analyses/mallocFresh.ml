open GoblintCil
open Analyses


module Spec =
struct
  include Analyses.IdentitySpec

  (* must fresh (or may not fresh) variables *)
  module DF = SetDomain.ToppedSet (CilType.Varinfo) (struct let topname = "All variables" end) (* need bot (top) for hoare widen *)
  module D =
  struct 
    include Lattice.Prod (DF) (DF)

    let join (df1, dnf1) (df2, dnf2) =
      let dnf = DF.union dnf1 dnf2 in
      let df = DF.diff (DF.union df1 df2) dnf in
      (df, dnf)

    let leq x y = equal (join x y) y

    let widen = join
    let narrow a b = a
  end
  module C = D

  let name () = "mallocFresh"

  let startstate _ = (DF.empty (), DF.empty ())
  let exitstate _ = (DF.empty (), DF.empty ())

  let assign_lval (ask: Queries.ask) lval local =
    match ask.f (MayPointTo (AddrOf lval)), local with
    | ls, (df, dnf) when Queries.LS.is_top ls || Queries.LS.mem (dummyFunDec.svar, `NoOffset) ls ->
      (DF.empty (), DF.union df dnf)
    | ls, (df, dnf) when Queries.LS.exists (fun (v, _) -> not (DF.mem v (fst local)) && (v.vglob || ThreadEscape.has_escaped ask v)) ls ->
      (DF.empty (), DF.union df dnf)
    | _ ->
      local

  let assign ctx lval rval =
    assign_lval (Analyses.ask_of_ctx ctx) lval ctx.local

  let combine_env ctx lval fexp f args fc au f_ask =
    ctx.local (* keep local as opposed to IdentitySpec *)

  let combine_assign ctx lval f fd args context f_local (f_ask: Queries.ask) =
    match lval with
    | None -> f_local
    | Some lval -> assign_lval (Analyses.ask_of_ctx ctx) lval f_local

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
    | _ ->
      match lval with
      | None -> ctx.local
      | Some lval -> assign_lval (Analyses.ask_of_ctx ctx) lval ctx.local

  let threadenter ctx lval f args =
    [(DF.empty (), DF.empty ())]

  let threadspawn ctx lval f args fctx =
    (DF.empty (), DF.union (fst ctx.local) (snd ctx.local))

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
