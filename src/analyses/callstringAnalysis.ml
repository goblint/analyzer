open Analyses
open Printf
open GoblintCil
open GobConfig

(* Specifies the type of the callstack elements for the CallstringLifter*)
module type Callstack_Type =
sig
  include CilType.S
  val stackTypeName: string
  val pushElem: fundec -> exp list -> ('d,'g,'c,'v) ctx -> t option (* returns a list of elements that should be pushed to the Callstack *) (*pushElem could be currently also a single element*)
  val printStack: fundec -> exp list -> t Queue2L.t -> t Queue2L.t -> unit (* a helper function to print the callstack *)
end

(** Lifts a [Spec] to analyse with the k-callsting approach. For this the last k callstack elements are used as context
    With the CT argument it is possible to specify the type of the Callstack elements
*)
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
        let new_stack = Queue2L.push e stack in (* pushes new element to stack*)         
        (* remove elements from stack, till the depth k is guaranteed*)
        Queue2L.del_n_elem (Queue2L.length new_stack - depth) new_stack
  end

  module D = Lattice.Fake(CallStack)
  module C = CallStack
  module V = EmptyV
  module G = Lattice.Unit

  let name () = "callstring_"^ CT.stackTypeName
  let startstate v = Queue2L.create ()
  let exitstate v =  Queue2L.create ()

  let enter ctx r f args = 
    let elem = CT.pushElem f args ctx in (* a list of elements that should be pushed onto the stack*)
    let new_stack = CallStack.push ctx.local elem in
    if not !AnalysisState.postsolving then CT.printStack f args ctx.local new_stack; (* just for debugging purpose*)
    [ctx.local, new_stack]

  let combine_env ctx lval fexp f args fc au f_ask = 
    ctx.local
end


module Fundec:Callstack_Type = struct
  include CilType.Fundec
  let stackTypeName = "fundec"
  let pushElem f args ctx = Some f

  let printStack f expL listA listB = 
    printf "fundec: %s\n" (CilType.Fundec.show f);
    printf "List alt: ";
    Queue2L.iter (fun x -> Printf.printf "%s; " (CilType.Fundec.show x)) listA;
    printf "\nList neu: ";
    Queue2L.iter (fun x -> Printf.printf "%s; " (CilType.Fundec.show x)) listB;
    printf "\n\n"
end


module Stmt:Callstack_Type = struct
  include CilType.Stmt
  let stackTypeName = "stmt"
  let pushElem f args ctx = 
    match ctx.prev_node with (* TODO: Why do I need to use prev_node???*)
    | Statement stmt -> Some stmt
    | _ -> None (* first statement is filtered*)

  let printStack f expL listA listB = 
    printf "fundec: %s\n" (CilType.Fundec.show f);
    printf "List alt: ";
    Queue2L.iter (fun x -> Printf.printf "%s; " (CilType.Stmt.show x)) listA;
    printf "\nList neu: ";
    Queue2L.iter (fun x -> Printf.printf "%s; " (CilType.Stmt.show x)) listB;
    printf "\n\n"
end


module Location:Callstack_Type = struct
  include CilType.Location
  let stackTypeName = "loc"
  let pushElem f args ctx =
    Some !Goblint_tracing.current_loc

  let printStack f expL listA listB = 
    printf "fundec: %s\n" (CilType.Fundec.show f);
    printf "List alt: ";
    Queue2L.iter (fun x -> Printf.printf "%s;\n " (CilType.Location.show x)) listA;
    printf "\nList neu: ";
    Queue2L.iter (fun x -> Printf.printf "%s;\n " (CilType.Location.show x)) listB;
    printf "\n\n"
end

(* Lifters for the Callstring approach with different Callstack element types*)
let _ =
  MCP.register_analysis (module Spec (Fundec) : MCPSpec); (* name: callstring_fundec*)
  MCP.register_analysis (module Spec (Stmt) : MCPSpec); (* name: callstring_stmt*)
  MCP.register_analysis (module Spec (Location) : MCPSpec) (* name: callstring_loc*)