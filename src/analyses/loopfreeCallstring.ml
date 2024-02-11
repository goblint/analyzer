(** This approach is inspired by 
    @see <https://arxiv.org/abs/2301.06439> Schwarz, M., Saan, S., Seidl, H., Erhard, J., Vojdani, V. Clustered Relational Thread-Modular Abstract Interpretation with Local Traces. Appendix F. *)

open Analyses

module Spec : MCPSpec =
struct
  include Analyses.IdentitySpec

  let name () = "loopfree_callstring"

  module FundecSet = SetDomain.Make (CilType.Fundec)
  module FundecList = Printable.Liszt (CilType.Fundec) 
  module Either = struct 
    include Lattice.Flat (Printable.Either (FundecList) (FundecSet)) (* should be a list. Since a Lattice is required, Lattice.Flat is used to fulfill the type *) 

    let printXml f = function
      | `Lifted x -> 
        begin
          match x with 
          | `Left x -> BatPrintf.fprintf f "<value>stack:\n%a</value>" FundecList.printXml x
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

  let list_after_split f stack rem_list =
    let rec stack_split set (hd::rest) = 
      if CilType.Fundec.equal hd f 
      then (FundecSet.add hd set), (rest)
      else stack_split (FundecSet.add hd set) rest
    in
    let set, new_stack = stack_split (FundecSet.empty ()) stack in
    set, `Lifted(`Left new_stack)::rem_list 

  let append_elem_at_end new_list f = 
    match new_list with
    | [] -> [(`Lifted(`Left [f]))]
    | x::xs -> 
      begin
        match get_either x with 
        | `Left stack -> (`Lifted(`Left (f::stack)))::xs 
        | `Right set -> (`Lifted(`Left [f]))::new_list
      end

  let rec callee_state f prev_set prev_list cur_list = 
    match cur_list with 
    | [] -> append_elem_at_end (List.rev prev_list) f
    | e::rem_list -> 
      begin
        match get_either e with
        | `Left stack -> 
          if List.mem f stack
          then (let set, new_stack = list_after_split f stack rem_list in
                let new_set = FundecSet.join prev_set set in
                (`Lifted(`Right new_set))::new_stack)
          else
            (let new_prev_set = FundecSet.join prev_set (FundecSet.of_list stack) in
             let new_prev_list = `Lifted(`Left stack)::prev_list in
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
