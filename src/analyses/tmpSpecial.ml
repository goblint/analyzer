(* Analysis that tracks which variables hold the results of calls to math library functions. *)

open Prelude.Ana
open Analyses

module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "tmpSpecial"
  module ML = LibraryDesc.MathLifted
  module D = MapDomain.MapBot (Basetype.Variables) (ML)
  module C = Lattice.Unit

  let context _ _ = ()

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    match lval with
    | (Var v, _) -> D.remove v ctx.local 
    (* TODO: Handle mem -> may point to*)
    | _ -> ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [ctx.local, D.bot ()]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) f_ask : D.t =
    D.bot ()

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    (* Just dbg prints *)
    (match lval with
    | Some (Var v, offs) -> if M.tracing then M.tracel "tmpSpecial" "Special: %s with\n lval %a; vinfo %a; attr %a \n" f.vname d_lval (Var v, offs) d_varinfo v d_attrlist v.vattr
    | _ -> if M.tracing then M.tracel "tmpSpecial" "Special: %s\n" f.vname);

    let desc = LibraryFunctions.find f in
    let res =
    match lval, desc.special arglist with
      | (Some (Var v, _)), (Math { fun_args; }) -> D.add v (ML.lift fun_args) ctx.local 
      | _ -> ctx.local 
    in
    if M.tracing then M.tracel "tmpSpecial" "Result: %a\n\n" D.pretty res;
    res

    let query ctx (type a) (q: a Queries.t): a Queries.result =
      Queries.Result.top q

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.bot ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.bot ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
