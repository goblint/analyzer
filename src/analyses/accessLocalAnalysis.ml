(** Access and data race analysis. *)

module M = Messages
module LF = LibraryFunctions
open Prelude.Ana
open Analyses
open GobConfig


(** Access and data race analyzer without base --- this is the new standard *)
module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "accessLocal"

  module D = AccessDomain.EventSet
  module C = Lattice.Unit

  (** We just lift start state, global and dependency functions: *)
  let startstate v = D.empty ()
  let threadenter ctx lval f args = [D.empty ()]
  let exitstate  v = D.empty ()
  let context fd d = ()

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | MayAccessed ->
      (ctx.local: D.t)
    | _ -> Queries.Result.top q

  let sync reason ctx: D.t =
    D.empty ()

  let event ctx e octx =
    match e with
    | Events.Access {exp; lvals; kind; reach} ->
      begin match lvals with
        | ls when Queries.LS.is_top ls ->
          let access: AccessDomain.Event.t = {var_opt = None; offs_opt = None; kind} in
          D.add access ctx.local
        | ls ->
          Queries.LS.fold (fun (var, offs) acc ->
              let coffs = Lval.CilLval.to_ciloffs offs in
              let access: AccessDomain.Event.t =
                if CilType.Varinfo.equal var dummyFunDec.svar then
                  {var_opt = None; offs_opt = (Some coffs); kind}
                else
                  {var_opt = (Some var); offs_opt = (Some coffs); kind}
              in
              D.add access acc
            ) ls ctx.local
      end
    | _ ->
      ctx.local
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
