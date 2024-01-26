open Analyses

module Spec : MCPSpec =
struct
  include Analyses.IdentitySpec

  let name () = "callstringTracking"

  module FundecSet = SetDomain.Make (CilType.Fundec)
  module FundecList = Lattice.Flat(Printable.Liszt(CilType.Fundec))
  module D = Lattice.Prod (FundecList) (FundecSet)
  module C = D
  module G = D
  module V = EmptyV
  let startstate v = D.bot ()
  let exitstate  v = D.bot ()

  let get_list l = match l with
    | `Lifted l -> l
    | `Bot -> []
    | `Top -> failwith "callstringTracking Error: the value of the Flat List shouldn't be top!"

  let callee_state f ctx = 
    let (list, set) = ctx.local in
    if (not(FundecSet.is_empty set)) || List.mem f (get_list list)
    then (list, FundecSet.add f set)
    else (`Lifted (f::(get_list list)), set)

  let enter ctx r f args = [ctx.local, callee_state f ctx]

  let threadenter ctx ~multiple lval v args = [callee_state (Cilfacade.find_varinfo_fundec v) ctx]

  let combine_env ctx lval fexp f args fc au f_ask = ctx.local
end

let _ = MCP.register_analysis (module Spec : MCPSpec)
