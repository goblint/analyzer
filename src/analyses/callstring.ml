(**
   Call String analysis [call_string] and/or Call Site analysis [call_site].
   The call string limitation for both approaches can be adjusted with the "callString_length" option.
   By adding new implementations of the CallstringType, additional analyses can be added.
*)

open Analyses
open GoblintCil
open GobConfig

(* Specifies the type of the call string elements *)
module type CallstringType =
sig
  include CilType.S
  val ana_name: string
  val new_ele: fundec -> ('d,'g,'c,'v) ctx -> t option (* returns an element that should be pushed to the call string *)
end

(** Analysis with infinite call string or with limited call string (k-CFA, tracks the last k call stack elements).
    With the CT argument it is possible to specify the type of the call string elements *)
module Spec (CT:CallstringType) : MCPSpec =
struct
  include UnitAnalysis.Spec

  (* simulates a call string (with or without limitation)*)
  module CallString = struct
    include Printable.PQueue (CT)

    let (empty:t) = BatDeque.empty

    (* pushes "elem" to the call string, guarantees a depth of k if limitation is specified with "ana.context.callString_length" *)
    let push callstr elem =
      match elem with
      | None -> callstr
      | Some e ->
        let new_callstr = BatDeque.cons e callstr in (* pushes new element to callstr *)
        if get_int "ana.context.callString_length" < 0
        then new_callstr (* infinite call string *)
        else (* maximum of k elements *)
          match BatDeque.size new_callstr - (get_int "ana.context.callString_length") with
          | 1 -> fst @@ Option.get (BatDeque.rear new_callstr)
          | x when x <= 0 -> new_callstr
          | _ -> failwith "CallString Error: It shouldn't happen that more than one element must be deleted to maintain the correct height!"
  end

  module C = CallString

  let name () = "call_"^ CT.ana_name

  let startcontext () = CallString.empty

  let context ctx fd _ =
    let elem = CT.new_ele fd ctx in (* receive element that should be added to call string *)
    CallString.push (ctx.context ()) elem
end

(* implementations of CallstringTypes*)
module Callstring: CallstringType = struct
  include CilType.Fundec
  let ana_name = "string"
  let new_ele f ctx =
    let f' = Node.find_fundec ctx.node in
    if CilType.Fundec.equal f' dummyFunDec
    then None
    else Some f'
end

module Callsite: CallstringType = struct
  include CilType.Stmt
  let ana_name = "site"
  let new_ele f ctx =
    match ctx.prev_node with
    | Statement stmt -> Some stmt
    | _ -> None (* first statement is filtered *)
end

let _ =
  (* call string approach *)
  MCP.register_analysis (module Spec (Callstring) : MCPSpec); (* [call_string] *)

  (* call site approach *)
  MCP.register_analysis (module Spec (Callsite) : MCPSpec); (* [call_site] *)
