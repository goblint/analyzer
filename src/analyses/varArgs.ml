(** This analysis collects the types into which variadic arguments are extracted to within functions . *)

open Prelude.Ana
open Analyses
open TypeDomain
module Q = Queries

module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let name () = "varArgs"
  module D = Lattice.Unit
  module G = TypeSet (* Set of types that are extracted from varargs within a function *)
  module C = Lattice.Unit

  let builtin_va_arg_str =  "__builtin_va_arg"

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local, ctx.local]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t =
    au

  let special ctx (lval: lval option) (f:varinfo) (args:exp list) : D.t =
    let current_fun = (MyCFG.getFun ctx.node).svar in
    (if f.vname = builtin_va_arg_str then
      if List.length args <> 3 then
        M.warn @@ "Unexpected number of arguments to " ^ builtin_va_arg_str ^ ". Length was:" ^ (string_of_int (List.length args))
      else begin
        match List.nth args 1 with
        | SizeOf t -> ctx.sideg current_fun (TypeSet.singleton t)
        | _ ->  M.warn @@ "Unexpected argument to " ^ builtin_va_arg_str ^ ".";
      end);
    ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()

  let query ctx (type a) (q: a Q.t): a Q.result =
    match q with
    | VarArgSet v -> `Lifted (ctx.global v)
    | _ -> Queries.Result.top q

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
