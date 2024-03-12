open Analyses
open GoblintCil
open GobConfig

(* Specifies the type of the call stack elements for the call string analysis *)
module type Callstack_Type =
sig
  include CilType.S
  val stackTypeName: string
  val stackEle: fundec -> ('d,'g,'c,'v) ctx -> t option (* returns an element that should be pushed to the call stack *)
end

(** Analysis with the k-call string approach, which uses the last k call stack elements as context.
    With the CT argument it is possible to specify the type of the call stack elements *)
module Spec (CT:Callstack_Type) : MCPSpec = 
struct
  include Analyses.IdentitySpec

  (* simulates a call stack of depth k *)
  module CallStack = struct
    include Printable.PQueue (CT)

    let push stack elem = (* pushes elem to the call stack, guarantees a depth of k if it is specified with "ana.context.callStack_height" *)
      match elem with
      | None -> stack
      | Some e -> 
        let new_stack = BatDeque.cons e stack in (* pushes new element to stack *)     
        if get_int "ana.context.callStack_height" < 0
        then new_stack (* infinite call stack *)
        else 
          (* removes element from stack, if stack was filled with k elements *)
          match (BatDeque.size new_stack - (get_int "ana.context.callStack_height")) with
          | x when x <= 0 -> new_stack
          | 1 -> fst @@ Option.get (BatDeque.rear new_stack)
          | _ -> failwith "Callstack Error: It shouldn't happen that more than one element must be deleted to maintain the correct height!"
  end

  module D = Lattice.Flat (CallStack) (* should be the CallStack (C=D). Since a Lattice is required, Lattice.Flat is used to fulfill the type *) 
  module C = CallStack
  module V = EmptyV
  module G = Lattice.Unit

  let name () = "call_"^ CT.stackTypeName
  let startstate v = `Lifted (BatDeque.empty)
  let exitstate v =  `Lifted (BatDeque.empty)

  let context fd x = match x with 
    | `Lifted x -> x
    | _ -> failwith "Callstring: Context error! The context cannot be derived from Top or Bottom!"

  let callee_state ctx f = 
    let elem = CT.stackEle f ctx in
    let new_stack = CallStack.push (context f ctx.local) elem in
    `Lifted new_stack

  let enter ctx r f args = [ctx.local, callee_state ctx f]

  let combine_env ctx lval fexp f args fc au f_ask = ctx.local

  let threadenter ctx ~multiple lval v args = [callee_state ctx (Cilfacade.find_varinfo_fundec v)]
end

module Callstring:Callstack_Type = struct
  include CilType.Fundec
  let stackTypeName = "string"
  let stackEle f ctx = 
    let f' = Node.find_fundec ctx.node in 
    if CilType.Fundec.equal f' dummyFunDec 
    then None
    else Some (f')
end

module Callstring_Callee:Callstack_Type = struct
  include CilType.Fundec
  let stackTypeName = "string_withCallee"
  let stackEle f ctx = Some f
end

module Callsite:Callstack_Type = struct
  include CilType.Stmt
  let stackTypeName = "site"
  let stackEle f ctx = 
    match ctx.prev_node with
    | Statement stmt -> Some stmt
    | _ -> None (* first statement is filtered *)
end

(* Lifters for the call string approach with different call stack element types *)
let _ =
  (* call string approach *)
  MCP.register_analysis (module Spec (Callstring) : MCPSpec); (* [call_string_og] *)

  (* call string approach: additionally tracks the callee function in the call stack *)
  MCP.register_analysis (module Spec (Callstring_Callee) : MCPSpec); (* [call_string_incl_callee] *)

  (* call site approach *)
  MCP.register_analysis (module Spec (Callsite) : MCPSpec); (* [call_site] *)