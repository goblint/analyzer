(** Termination of loops. *)

open Prelude.Ana
open Analyses

module M = Messages
let (%?) = Option.bind
let (||?) a b = match a,b with Some x,_ | _, Some x -> Some x | _ -> None

module TermDomain = struct
  include SetDomain.ToppedSet (Basetype.Variables) (struct let topname = "All Variables" end)
end

module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "loopTerm"
  module D = TermDomain
  module C = TermDomain
  module G = Lattice.Unit

  (* queries *)
  (*let query ctx (q:Queries.t) : Queries.Result.t =*)
  (*match q with*)
  (*| Queries.MustTerm loc -> `Bool (D.mem v ctx.local)*)
  (*| _ -> Queries.Result.top ()*)

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
    [ctx.local,ctx.local]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t =
    au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = D.bot ()
  let threadspawn ctx lval f args fctx = D.bot ()
  let exitstate  v = D.bot ()
end

let _ =
  (* Cilfacade.register_preprocess Spec.name (new loopCounterVisitor); *)
 (*Cilfacade.register_preprocess (Spec.name ()) (new loopBreaksVisitor);
  Cilfacade.register_preprocess (Spec.name ()) (new loopVarsVisitor);
  Cilfacade.register_preprocess (Spec.name ()) (new loopInstrVisitor);
  Cilfacade.register_preprocess (Spec.name ()) (new recomputeVisitor);
  Hashtbl.clear loopBreaks; (* because the sids are now different *)
  Cilfacade.register_preprocess (Spec.name ()) (new loopBreaksVisitor);*)
  MCP.register_analysis (module Spec : Spec)
