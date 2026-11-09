(** Path-sensitivity in the last [ana.branchSeq.length] branch decisions.
    Histories pass from caller to callee and back; new threads start empty. *)

open Analyses

module Spec =
struct
  include Analyses.IdentitySpec
  let name () = "branchSeq"

  module Branch = Printable.Prod (BoolDomain.Bool) (Node)
  module D = Lattice.Flat (Printable.Liszt (Branch))
  include Analyses.ValueContexts (D)
  module P = IdentityP (D)

  let k = ref 0

  let init _ =
    k := GobConfig.get_int "ana.branchSeq.length";
    if !k <= 0 then
      failwith "ana.branchSeq.length must be positive if analysis is active"

  (* Most recent decision first. Consecutive visits to the same node preserve
     the existing history, including its recorded branch value. *)
  let branch man _ tv =
    match man.local with
    | `Lifted ((otv, node) :: _) when otv = tv && Node.equal node man.node -> man.local
    | `Lifted history -> `Lifted (Batteries.List.take !k ((tv, man.node) :: history))
    | (`Bot | `Top) as x -> x

  let startstate _ = `Lifted []
  let exitstate = startstate
  let threadenter man ~multiple lval f args = [`Lifted []]
end

let () = MCP.register_analysis (module Spec : MCPSpec)
