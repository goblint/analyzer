(** Thread returning analysis. *)

open Prelude.Ana
open Analyses

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  let name () = "threadreturn"
  module D = IntDomain.Booleans
  module G = Lattice.Unit
  module C = D

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    if ctx.local then (
      match ThreadId.get_current ctx.ask with
      | `Lifted tid ->
        (* can't use exp because it might contain variables, which are out of scope after base return *)
        ctx.assign ~name:"base" (var tid) (Lval (Base.Main.return_lval ()))
      | _ -> ()
    );
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local, false]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t =
    ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    begin match LibraryFunctions.classify f.vname arglist with
      | `ThreadJoin (id, ret_val) ->
        ctx.assign ~name:"base" (Mem ret_val, NoOffset) (Lval (Mem id, NoOffset))
        (* TODO: crashes because id is actually int and cannot be dereferenced *)
      | _ -> ()
    end;
    ctx.local

  let startstate v = true
  let threadenter ctx lval f args = true
  let threadspawn ctx lval f args fctx = D.bot ()
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : Spec)
