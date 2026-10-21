(** Analysis of [assume_abort_if_not]-style functions ([abortUnless]).

    Such a function only returns if its only argument has a non-zero value. *)

open GoblintCil
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "abortUnless"
  module D = BoolDomain.MustBool
  module C = Printable.Unit

  let context man _ _ = ()
  let startcontext () = ()

  (* transfer functions *)
  let assign man (lval:lval) (rval:exp) : D.t =
    false

  let branch man (exp:exp) (tv:bool) : D.t =
    man.local

  let body man (f:fundec) : D.t =
    man.local

  let return man (exp:exp option) (f:fundec) : D.t =
    if man.local then
      match f.sformals with
      | [arg] when isIntegralType arg.vtype ->
        (match Queries.eval_bool (Analyses.ask_of_man man) (Lval (Var arg, NoOffset)) with
         | `Bot -> false
         | `Lifted b -> b
         | `Top -> false)
      | _ ->
        (* should not happen, man.local should always be false in this case *)
        false
    else
      false

  let enter man (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let candidate = match f.sformals with
      | [arg] when isIntegralType arg.vtype -> true
      | _ -> false
    in
    [false, candidate]

  let combine_env man lval fexp f args fc au f_ask =
    if au then (
      (* Assert before combine_assign, so if variables in `arg` are assigned to, asserting doesn't unsoundly yield bot *)
      (* See test 62/03 *)
      match args with
      | [arg] -> man.emit (Events.Assert arg)
      | _ -> ()
    );
    false

  let combine_assign man (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    false

  let special man (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    false

  let startstate v = false
  let threadenter man ~multiple lval f args = [false]
  let threadspawn man ~multiple lval f args fman = false
  let exitstate v = false
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
