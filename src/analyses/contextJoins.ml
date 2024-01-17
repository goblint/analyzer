open Analyses


module Spec : MCPSpec =
struct
  include Analyses.IdentitySpec

  let name () = "contextJoins"

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
  | `Top -> failwith "contextJoins Error: the value of the Flat List shouldn't be top!"

  let enter ctx r f args = 
    let (l, set) = ctx.local in
    let list = get_list l in 
    let empty_set = FundecSet.is_empty set in
    let ele_in_list = List.mem f list in
    match empty_set, ele_in_list with
    | false, _ -> [ctx.local, (l, FundecSet.add f set)]
    | true, true -> [ctx.local, (l, FundecSet.add f set)]
    | true, false -> [ctx.local, (`Lifted (f::list), set)]

  let combine_env ctx lval fexp f args fc au f_ask = 
    ctx.local
    
end

let _ = MCP.register_analysis (module Spec : MCPSpec)
