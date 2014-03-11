open Cil
open Pretty
open Analyses

module LV = Lval.CilLval
module LS = Queries.LS
module LM = MapDomain.MapBot_LiftTop (LV) (LS)

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  let name = "impvar"
  module D = Lattice.Unit
  module G = LS
  module C = Lattice.Unit
  
  let get_deps ctx (v,os) = 
    match ctx.ask (Queries.VariableDeps (Var v, os)) with
      | `Bot -> LS.bot ()
      | `LvalSet ls -> ls
      | _ -> LS.top ()

  let add_var ctx (v,os) = 
    (* Printf.printf "%s is important too!\n" v.vname; *)
    let ls = LS.add (v,LV.of_ciloffs os) (get_deps ctx (v,os)) in
    LS.iter (fun (v,os) -> ctx.sideg v (LS.singleton (v,os))) ls

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t = ()
  let branch ctx (exp:exp) (tv:bool) : D.t = ()
  let body ctx (f:fundec) : D.t = ()
  let return ctx (exp:exp option) (f:fundec) : D.t = ()
  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list = [ctx.local, ctx.local]
  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t = ()
  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    match f.vname, List.map stripCasts arglist with
      | "important_var", [Lval (Var v, os)] -> add_var ctx (v,os)
      | _ -> ()

  let startstate v = D.bot ()
  let otherstate v = D.bot ()
  let exitstate  v = D.bot ()
end

let _ = 
  MCP.register_analysis (module Spec : Spec)
