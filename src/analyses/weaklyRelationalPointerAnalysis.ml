(** A Weakly-Relational Pointer Analysis..([wrpointer])*)

(** TODO description *)

(* open Batteries
   open GoblintCil
   open Pretty *)
open Analyses
open GoblintCil
open WeaklyRelationalPointerDomain
module CC = CongruenceClosure
open CC.CongruenceClosure(Var)

module Operations =
struct
  let from_cil a = let res = T.from_cil a in
    let string_res = match res with
      | None,None -> "None, None"
      | Some res,None -> (T.show res) ^", None"
      | None, Some z -> "None, " ^Z.to_string z
      | Some res, Some z -> (T.show res) ^", "^Z.to_string z in
    if M.tracing then M.trace "wrpointer" "Converting rhs: <%a>. Plain expression: <%a>. Result: <%s>\n" d_exp a d_plainexp a string_res;res


  let from_lval a = let res = T.from_lval a in
    let string_res = match res with
      | None,None -> "None, None"
      | Some res,None -> (T.show res) ^", None"
      | None, Some z -> "None, " ^Z.to_string z
      | Some res, Some z -> (T.show res) ^", "^Z.to_string z in
    if M.tracing then M.trace "wrpointer" "Converting lhs: <%a>. Plain lval: <%a>. Result: <%s>\n" d_lval a d_plainlval a string_res;res

  let assign_lval (t:D.domain) ask lval expr =
    match t with
    | None -> (* The domain is bottom *)None
    | Some t ->
      match from_lval lval, from_cil expr with
      (* Indefinite assignment *)
      | (Some lterm, Some loffset), (None, _) -> Some (D.remove_may_equal_terms t ask lterm)
      (* Definite assignment *)
      | (Some (Addr x), Some loffset), (Some term, Some offset) when Z.compare loffset Z.zero = 0 ->
        (* This is not even possible *)
        meet_conjs_opt (insert_set (D.remove_terms_containing_variable t (Addr x)) (SSet.TSet.of_list [Addr x; term])) [Equal (Addr x, term, offset)]
      | (Some lterm, Some loffset), (Some term, Some offset) when Z.compare loffset Z.zero = 0 ->
        meet_conjs_opt (insert_set (D.remove_may_equal_terms t ask lterm) (SSet.TSet.of_list [lterm; term])) [Equal (lterm, term, offset)]
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
  module D = D
  module C = D

  let name () = "wrpointer"
  let startstate v = D.empty()
  let exitstate v = D.empty()

  let query _ (type a) (q: a Queries.t) = Queries.Result.top q

  let assign ctx var expr =
    let res = assign_lval ctx.local (ask_of_ctx ctx) var expr in
    if M.tracing then M.trace "wrpointer" "ASSIGN: var: %a; expr: %a; result: %s. UF: %s\n" d_lval var d_exp expr (D.show res) (Option.fold ~none:"" ~some:(fun r -> TUF.show_uf r.part) res); res

  let branch ctx expr neg = ctx.local

  let body ctx f = ctx.local (*DONE*)

  let return ctx exp_opt f = ctx.local

  let special ctx var_opt v exprs  = ctx.local

  let enter ctx var_opt f args =
    let state = ctx.local in
    let arg_assigns =
      GobList.combine_short f.sformals args
    in
    let new_state = List.fold_left (fun st (var, exp) -> assign_lval st (ask_of_ctx ctx) (Var var, NoOffset) exp) state arg_assigns in
    if M.tracing then M.trace "wrpointer" "ENTER: result: %s\n" (D.show new_state);
    [ctx.local, new_state] (*TODO remove callee vars?*)
  let combine_env ctx var_opt expr f exprs t_context_opt t ask = t

  let combine_assign ctx var_opt expr f exprs t_context_opt t ask = ctx.local

  let threadenter ctx ~multiple var_opt v exprs = [ctx.local]
  let threadspawn ctx ~multiple var_opt v exprs ctx2 = ctx.local

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
