(** This approach is inspired by 
    @see <https://arxiv.org/abs/2301.06439> Schwarz, M., Saan, S., Seidl, H., Erhard, J., Vojdani, V. Clustered Relational Thread-Modular Abstract Interpretation with Local Traces. Appendix F. 
    The idea is to improve the call string appraoch, by representing all detected loops of the call stack in a set
*)

open Analyses

module Spec : MCPSpec =
struct
  include Analyses.IdentitySpec

  let name () = "loopfree_callstring"

  module FundecSet = SetDomain.Make (CilType.Fundec)
  module Either =  Printable.Either (CilType.Fundec) (FundecSet) 

  module D = Lattice.Flat (Printable.Liszt (Either)) (* should be a list of Fundecs and Sets of Fundecs. Lattice.Flat is used to fulfill the type *) 
  module C = D
  module V = EmptyV
  module G = Lattice.Unit
  let startstate v = `Lifted([])
  let exitstate  v = `Lifted([])

  let get_list list = match list with
    | `Lifted e -> e
    | _ -> failwith "Error loopfreeCallstring (get_list): D should represent a call stack not `Top or `Bottom!"

  let loop_detected f = function
    (* a call stack contains each function at most once *)
    | `Left ele -> CilType.Fundec.equal f ele
    | `Right set -> FundecSet.mem f set 

  let add_to_set old = function
    | `Left ele -> FundecSet.add ele old
    | `Right set -> FundecSet.join old set

  let rec callee_state f prev_set prev_list = function
    | [] -> (`Left f)::(List.rev prev_list) (* f is not yet contained in the call stack*)
    | e::rem_list -> 
      let new_set = add_to_set prev_set e in
      if loop_detected f e (* f is already present in the call stack *)
      then (`Right new_set)::rem_list (* combine all elements of the loop in a set *)
      else callee_state f new_set (e::prev_list) rem_list

  let callee_state f ctx = `Lifted(callee_state f (FundecSet.empty ()) [] (get_list ctx.local))

  let enter ctx r f args = [ctx.local, callee_state f ctx]

  let threadenter ctx ~multiple lval v args = [callee_state (Cilfacade.find_varinfo_fundec v) ctx]

  let combine_env ctx lval fexp f args fc au f_ask = ctx.local
end

let _ = MCP.register_analysis (module Spec : MCPSpec)
