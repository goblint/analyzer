(**
   Loopfree Callstring analysis [loopfree_callstring] that reduces the call string length of the classical Call String approach for recursions
   The idea is to improve the Call String analysis by representing all detected call cycle of the call string in a set
   In case no call cycles appears, the call string is identical to the call string of the Call String approach
   For example:
   - call string [main, a, b, c, a] is represented as [main, {a, b, c}]
   - call string [main, a, a, b, b, b] is represented as [main, {a}, {b}]

   This approach is inspired by
   @see <https://arxiv.org/abs/2301.06439> Schwarz, M., Saan, S., Seidl, H., Erhard, J., Vojdani, V. Clustered Relational Thread-Modular Abstract Interpretation with Local Traces. Appendix F.
*)
open Analyses

module Spec : MCPSpec =
struct
  include UnitAnalysis.Spec

  let name () = "loopfree_callstring"

  module FundecSet = SetDomain.Make (CilType.Fundec)
  module Either =  Printable.Either (CilType.Fundec) (FundecSet)

  module C = Printable.Liszt (Either)

  let append fd current =
    let loop_detected f = function
      (* note: a call string contains each Fundec at most once *)
      | `Left ele -> CilType.Fundec.equal f ele
      | `Right set -> FundecSet.mem f set
    in
    let add_to_set old = function
      | `Left ele -> FundecSet.add ele old
      | `Right set -> FundecSet.join old set
    in
    let rec append f prev_set prev_list = function
      | [] -> (`Left f)::(List.rev prev_list) (* f is not yet contained in the call string *)
      | e::rem_list ->
        let new_set = add_to_set prev_set e in
        if loop_detected f e (* f is already present in the call string *)
        then (`Right new_set)::rem_list (* combine all elements of the call cycle in a set *)
        else append f new_set (e::prev_list) rem_list
    in
    append fd (FundecSet.empty ()) [] current

  let startcontext () = []
  let context ctx fd x = append fd (ctx.context ())
end

let _ = MCP.register_analysis (module Spec : MCPSpec)
