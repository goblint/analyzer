(** Helper analyses to be path-sensitive in set of taken branches ([branchSet], [branchSetFull], [branchSetLocal]).

    The different analysis variants have different interprocedural behavior. *)

open GoblintCil
open Analyses

(** Common parts of all variants. *)
module CommonSpec =
struct
  include Analyses.IdentitySpec

  module Branch = Printable.Prod(BoolDomain.Bool)(Node)
  module BranchSet = SetDomain.Make(Branch)

  module D = BranchSet
  include Analyses.ValueContexts(D)
  module P = IdentityP (D)

  let branch man (exp:exp) (tv:bool) : D.t =
    BranchSet.add (tv, man.node) man.local

  let startstate v = D.empty ()
  let threadenter man ~multiple lval f args = [D.empty ()]
  let exitstate  v = D.empty ()
end

(** Fully interprocedural branch path-sensitivity ([branchSetFull]).

    + Caller passes branches to callee.
    + Callee passes branches to caller. *)
module FullSpec =
struct
  include CommonSpec
  let name () = "branchSetFull"

  let enter man (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [man.local,man.local]

  let combine_env man lval fexp f args fc au f_ask =
    au
end

(** Semi-interprocedural branch path-sensitivity ([branchSet]).

    + Caller {e doesn't} pass branches to callee.
    + Callee passes branches to caller. *)
module Spec =
struct
  include CommonSpec
  let name () = "branchSet"

  let enter man (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [man.local, D.empty ()]

  let combine_env man lval fexp f args fc au f_ask =
    D.join man.local au
end

(** Fully intraprocedural branch path-sensitivity ([branchSetLocal]).

    + Caller {e doesn't} pass branches to callee.
    + Callee {e doesn't} pass branches to caller. *)
module LocalSpec =
struct
  include CommonSpec
  let name () = "branchSetLocal"

  let enter man (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [man.local, D.empty ()]

  let combine_env man lval fexp f args fc au f_ask =
    man.local
end

let _ =
  MCP.register_analysis (module FullSpec : MCPSpec);
  MCP.register_analysis (module Spec : MCPSpec);
  MCP.register_analysis (module LocalSpec : MCPSpec)
