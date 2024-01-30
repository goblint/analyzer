open Analyses
open GoblintCil
open GobConfig

(* Specifies the type of the callstack elements for the callstring analysis*)
module type Callstack_Type =
sig
  include CilType.S
  val stackTypeName: string
  val pushElem: fundec -> ('d,'g,'c,'v) ctx -> t option (* returns an elements that should be pushed to the Callstack *)
end

(** Analysis with the k-callsting approach, which uses the last k callstack elements as context.
    With the CT argument it is possible to specify the type of the callstack elements*)
module Spec (CT:Callstack_Type) : MCPSpec = 
struct
  include Analyses.IdentitySpec

  (* simulates a call stack of depth k*)
  module CallStack = struct
    include Printable.PQueue (CT)
    let depth = get_int "ana.context.callStack_height" (* must be >= 0 *)

    let push stack elem = (* pushes elem to the stack, guarantees stack depth of k*)
      match elem with
      | None -> stack
      | Some e -> 
        let new_stack = BatDeque.cons e stack in (* pushes new element to stack*)         
        (* removes element from stack, if stack was filled with k elements*)
        match (BatDeque.size new_stack - depth) with
        | x when x <= 0 -> new_stack
        | 1 -> fst @@ Option.get (BatDeque.rear new_stack)
        | _ -> failwith "Callstack Error: It shouldn't happen that more than one element must be deleted to maintain the correct height!"
  end

  module D = Lattice.Flat (CallStack)
  module C = CallStack
  module V = EmptyV
  module G = Lattice.Unit

  let name () = "callstring_"^ CT.stackTypeName
  let startstate v = `Lifted (BatDeque.empty)
  let exitstate v =  `Lifted (BatDeque.empty) (*TODO: should I use startstate here? Does this make a difference*)

  let context fd x = match x with 
    | `Lifted x -> x
    | _ -> failwith "Callstring: Context error! The context cannot be derived from Top or Bottom!" (* TODO: is there a possibility???*)

  let callee_state ctx f = 
    let elem = CT.pushElem f ctx in
    let new_stack = CallStack.push (context f ctx.local) elem in (*TODO: is it ok to use context here??? *)
    `Lifted new_stack

  let enter ctx r f args = [ctx.local, callee_state ctx f]

  let combine_env ctx lval fexp f args fc au f_ask = ctx.local

  let threadenter ctx ~multiple lval v args = [callee_state ctx (Cilfacade.find_varinfo_fundec v)]
end

module Fundec:Callstack_Type = struct
  include CilType.Fundec
  let stackTypeName = "fundec"
  let pushElem f ctx = Some f
end

module Stmt:Callstack_Type = struct
  include CilType.Stmt
  let stackTypeName = "stmt"
  let pushElem f ctx = 
    match ctx.prev_node with
    | Statement stmt -> Some stmt
    | _ -> None (* first statement is filtered*)
end

module Location:Callstack_Type = struct
  include CilType.Location
  let stackTypeName = "loc"
  let pushElem f ctx =
    Some !Goblint_tracing.current_loc
end

(* Lifters for the Callstring approach with different Callstack element types*)
let _ =
  MCP.register_analysis (module Spec (Fundec) : MCPSpec); (* name: callstring_fundec*)
  MCP.register_analysis (module Spec (Stmt) : MCPSpec); (* name: callstring_stmt*)
  MCP.register_analysis (module Spec (Location) : MCPSpec) (* name: callstring_loc*)