(** A Weakly-Relational Pointer Analysis..([wrpointer])*)

(** TODO description *)

(* open Batteries
   open GoblintCil
   open Pretty *)
open Analyses
open GoblintCil
open WeaklyRelationalPointerDomain

module Operations =
struct
  module D = D
  let assign_lval (t:D.domain) lval expr =
    match t with
    | None -> (* The domain is bottom *)None
    | Some t ->
      match D.T.from_lval lval, D.T.from_cil expr with
      (* Indefinite assignment *)
      | (Some lterm, Some loffset), (None, _) -> Some (D.remove_may_equal_terms t lterm)
      (* Definite assignment *)
      | (Some (Addr x), Some loffset), (Some term, Some offset) when Z.compare loffset Z.zero = 0 ->
        (* This is not even possible *)
        D.meet_conjs_opt (D.insert_set (D.remove_terms_containing_variable t (Addr x)) (D.SSet.TSet.of_list [Addr x; term])) [Equal (Addr x, term, offset)]
      | (Some lterm, Some loffset), (Some term, Some offset) when Z.compare loffset Z.zero = 0 ->
        D.meet_conjs_opt (D.insert_set (D.remove_may_equal_terms t lterm) (D.SSet.TSet.of_list [lterm; term])) [Equal (lterm, term, offset)]
      (* invertibe assignment *)
      | _ -> Some t (* TODO what if lhs is None? Just ignore? -> Not a good idea *)

end

(* module M = Messages
   module VS = SetDomain.Make (CilType.Varinfo) *)
module Spec : MCPSpec =
struct
  include DefaultSpec
  include Analyses.IdentitySpec
  include Operations
  module C = D

  let name () = "wrpointer"
  let startstate v = D.empty()
  let exitstate v = D.empty()

  let assign ctx var expr =
    assign_lval ctx.local var expr

  let branch ctx expr neg = ctx.local

  let body ctx f = ctx.local (*DONE*)

  let return ctx exp_opt f = ctx.local

  let special ctx var_opt v exprs  = D.top()

  let enter ctx var_opt f args =
    let state = ctx.local in
    let arg_assigns =
      GobList.combine_short f.sformals args
    in
    let new_state = List.fold_left (fun st (var, exp) -> assign_lval st (Var var, NoOffset) exp) state arg_assigns in
    [ctx.local, new_state] (*TODO remove callee vars?*)
  let combine_env ctx var_opt expr f exprs t_context_opt t ask = t

  let combine_assign ctx var_opt expr f exprs t_context_opt t ask = ctx.local

  let threadenter ctx ~multiple var_opt v exprs = [ctx.local]
  let threadspawn ctx ~multiple var_opt v exprs ctx2 = ctx.local

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
