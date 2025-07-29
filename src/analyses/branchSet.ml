(** Helper analysis to be path-sensitive in set of taken branches. *)

open GoblintCil
open Analyses

module Spec =
struct
  include Analyses.IdentitySpec

  module Branch = Printable.ProdSimple(BoolDomain.Bool)(CilType.Location)
  module BranchSet = SetDomain.Make(Branch)

  module D = BranchSet
  include Analyses.ValueContexts(D)
  module P = IdentityP (D)


  let enter man (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [man.local,man.local]

  let combine_env man lval fexp f args fc au f_ask =
    au

  let combine_assign man (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    man.local


  let branch man (exp:exp) (tv:bool) : D.t =
    let loc = Node.location man.node in
    BranchSet.add (tv, loc) man.local


  let name () = "branchSet"

  let startstate v = D.empty ()
  let threadenter man ~multiple lval f args = [D.empty ()]
  let exitstate  v = D.empty ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
