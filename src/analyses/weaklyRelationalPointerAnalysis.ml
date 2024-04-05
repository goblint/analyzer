(** A Weakly-Relational Pointer Analysis..([wrpointer])*)

(** TODO description *)

(* open Batteries
   open GoblintCil
   open Pretty *)
open Analyses
open WeaklyRelationalPointerDomain

module Operations =
struct
  include CongruenceClosure
  module D = D
  let assign (t:D.domain) lval expr =
   match t with
   | None -> None
   | Some t ->
    match D.T.from_lval lval, D.T.from_cil expr with
    | (Some lterm, Some loffset), (Some term, Some offset) when Z.compare loffset Z.zero = 0 ->
      D.meet_conjs_opt (D.insert_set (D.remove_terms_containing_variable t lterm) (D.SSet.TSet.of_list [lterm; term])) [Equal (lterm, term, offset)]
    | _ -> Some t

end

(* module M = Messages
   module VS = SetDomain.Make (CilType.Varinfo) *)
module Spec : MCPSpec =
struct
  include DefaultSpec
  include Operations
  module C = D

  let name () = "wrpointer"
  let startstate v = D.top()
  let exitstate v = D.top()

  let assign ctx var expr =
    assign ctx.local var expr

  let branch ctx expr neg = D.top()

  let body ctx f = D.top()
  let return ctx exp_opt f = D.top()

  let special ctx var_opt v exprs  = D.top()

  let enter ctx var_opt f exprs =  []
  let combine_env ctx var_opt expr f exprs t_context_opt t ask = t

  let combine_assign ctx var_opt expr f exprs t_context_opt t ask = t

  let threadenter ctx ~multiple var_opt v exprs = []
  let threadspawn ctx ~multiple var_opt v exprs ctx2 = C.top()

end
