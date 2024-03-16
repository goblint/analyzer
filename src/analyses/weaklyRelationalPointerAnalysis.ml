(** A Weakly-Relational Pointer Analysis.. *)

(** TODO description *)

(* open Batteries
   open GoblintCil
   open Pretty *)
open Analyses
open WeaklyRelationalPointerDomain

(* module M = Messages
   module VS = SetDomain.Make (CilType.Varinfo) *)
module Spec : Spec =
struct
  include DefaultSpec
  module D = D
  module C = D

  let name () = "weakly rlational pointer analysis"

  let startstate v = D.top()

  let exitstate v = D.top()
  let assign ctx var expr = D.top()
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
