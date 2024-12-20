(** Simplest possible analysis with unit domain ([unit]). *)

open GoblintCil
open Analyses

(* module Spec : Analyses.MCPSpec with module D = Lattice.Unit and module C = Printable.Unit and type marshal = unit = *)
(* No signature so others can override module G *)
module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "unit"
  module D = Lattice.Unit
  module C = Printable.Unit

  (* transfer functions *)
  let assign man (lval:lval) (rval:exp) : D.t =
    man.local

  let branch man (exp:exp) (tv:bool) : D.t =
    man.local

  let body man (f:fundec) : D.t =
    man.local

  let return man (exp:exp option) (f:fundec) : D.t =
    man.local

  let enter man (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [man.local, man.local]

  let combine_env man lval fexp f args fc au f_ask =
    au

  let combine_assign man (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    man.local

  let special man (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    man.local

  let startcontext () = ()
  let startstate v = D.bot ()
  let threadenter man ~multiple lval f args = [D.top ()]
  let threadspawn man ~multiple lval f args fman = man.local
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
