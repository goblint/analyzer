(** This approach is inspired by 
    @see <https://arxiv.org/abs/2301.06439> Schwarz, M., Saan, S., Seidl, H., Erhard, J., Vojdani, V. Clustered Relational Thread-Modular Abstract Interpretation with Local Traces. Appendix F. *)

open Analyses

module Spec : MCPSpec =
struct
  include Analyses.IdentitySpec

  let name () = "loopfree_callstring"

  module FundecSet = SetDomain.Make (CilType.Fundec)
  module Either = struct 
    include Lattice.Flat (Printable.Either (CilType.Fundec) (FundecSet)) (* should be a list. Since a Lattice is required, Lattice.Flat is used to fulfill the type *) 

    let printXml f = function
      | `Lifted x -> 
        begin
          match x with 
          | `Left x -> BatPrintf.fprintf f "<value>%a</value>" CilType.Fundec.printXml x
          | `Right x -> BatPrintf.fprintf f "<value>set:\n%a</value>" FundecSet.printXml x
        end;
      | _ -> failwith "Error loopfreeCallstring (printXml): the Flat Lattice containing Either shouldn't be `Top or `Bottom!"
  end

  module D = Lattice.Liszt (Either)
  module C = D
  module V = EmptyV
  module G = Lattice.Unit
  let startstate v = []
  let exitstate  v = []

  let get_either either = match either with
    | `Lifted e -> e
    | _ -> failwith "Error loopfreeCallstring (get_either): the Flat Lattice containing Either shouldn't be `Top or `Bottom!"

  let rec callee_state f prev_set prev_list cur_list = 
    match cur_list with 
    | [] -> (`Lifted(`Left f))::(List.rev prev_list)
    | e::rem_list -> 
      begin
        match get_either e with
        | `Left ele -> 
          if CilType.Fundec.equal f ele
            then (let new_set = FundecSet.add f prev_set in
                (`Lifted(`Right new_set))::rem_list)
          else
            (let new_prev_set = FundecSet.add ele prev_set in
             let new_prev_list = `Lifted(`Left ele)::prev_list in
             callee_state f new_prev_set new_prev_list rem_list)
        | `Right set -> 
          let new_set = FundecSet.join prev_set set in
          if FundecSet.mem f set 
          then (`Lifted(`Right new_set))::rem_list
          else (let new_prev_list = `Lifted(`Right set)::prev_list in
                callee_state f new_set new_prev_list rem_list)
      end

  let callee_state f ctx = callee_state f (FundecSet.empty ()) [] ctx.local

  let enter ctx r f args = [ctx.local, callee_state f ctx]

  let threadenter ctx ~multiple lval v args = [callee_state (Cilfacade.find_varinfo_fundec v) ctx]

  let combine_env ctx lval fexp f args fc au f_ask = ctx.local
end

let _ = MCP.register_analysis (module Spec : MCPSpec)
