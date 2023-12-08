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
  val printStack: fundec -> exp list -> (t list * t list) -> (t list * t list) -> unit (* a helper function to print the callstack *)
end

(** Lifts a [Spec] to analyse with the k-callsting approach. For this the last k callstack elements are used as context
    With the CT argument it is possible to specify the type of the Callstack elements
*)
module Spec (CT:Callstack_Type) : MCPSpec = 
struct
  include Analyses.IdentitySpec

  (* simulates a call stack of depth k*)
  module CallStack = struct
    include Printable.Prod (Printable.Liszt (CT)) (Printable.Liszt (CT))
    let depth = get_int "ana.context.callStack_height" (* must be >= 0 *)
    let length (stack_first, stack_last) = List.length stack_first + List.length stack_last

    let push stack elem = (* pushes elem to the stack, guarantees stack depth of k*)
      match elem with
      | None -> stack
      | Some e -> 
        match (length stack >= depth), stack with
        | _, ([], []) -> [e], []
        | false, ([], last) -> List.rev last, [e]
        | false, (stack_first, stack_last) -> stack_first @ List.rev stack_last, [e]  
        | true, ([], last) -> List.tl(List.rev(last)), [e]
        | true, (s::stack_first, stack_last) -> stack_first @ List.rev stack_last, [e]
  end

  module D = Lattice.Fake(CallStack)
  module C = CallStack
  module V = EmptyV
  module G = Lattice.Unit

  let name () = "callstring_"^ CT.stackTypeName
  let startstate v = ([],[])
  let exitstate v = ([],[])

  let enter ctx r f args = 
    let elem: CT.t option = CT.pushElem f args ctx in (* a list of elements that should be pushed onto the stack*)
    let new_stack: CallStack.t = CallStack.push ctx.local elem in
    if not !AnalysisState.postsolving then CT.printStack f args (ctx.local) new_stack; (* just for debugging purpose*)
    [ctx.local, new_stack]

  let combine_env ctx lval fexp f args fc au f_ask = 
    ctx.local
end


module Fundec:Callstack_Type = struct
  include CilType.Fundec
  let stackTypeName = "fundec"
  let pushElem f args ctx = Some f

  let printStack f expL (listA1, listA2) (listB1, listB2) = 
    printf "fundec: %s\n" (CilType.Fundec.show f);
    printf "List alt: ";
    List.iter (fun x -> Printf.printf "%s; " (CilType.Fundec.show x)) listA1;
    List.iter (fun x -> Printf.printf "%s; " (CilType.Fundec.show x)) (List.rev listA2);
    printf "\nList neu: ";
    List.iter (fun x -> Printf.printf "%s; " (CilType.Fundec.show x)) listB1;
    List.iter (fun x -> Printf.printf "%s; " (CilType.Fundec.show x)) (List.rev listB2);
    printf "\n\n"
end

module Stmt:Callstack_Type = struct
  include CilType.Stmt
  let stackTypeName = "stmt"
  let pushElem f args ctx = 
    match ctx.prev_node with (* TODO: Why do I need to use prev_node???*)
    | Statement stmt -> Some stmt
    | _ -> None (* first statement is filtered*)


  let printStack f expL (listA1, listA2) (listB1, listB2) = 
    printf "fundec: %s\n" (CilType.Fundec.show f);
    printf "List alt: ";
    List.iter (fun x -> Printf.printf "%s; " (CilType.Stmt.show x)) listA1;
    List.iter (fun x -> Printf.printf "%s; " (CilType.Stmt.show x)) (List.rev listA2);
    printf "\nList neu: ";
    List.iter (fun x -> Printf.printf "%s; " (CilType.Stmt.show x)) listB1;
    List.iter (fun x -> Printf.printf "%s; " (CilType.Stmt.show x)) (List.rev listB2);
    printf "\n\n"
end


module Location:Callstack_Type = struct
  include CilType.Location
  let stackTypeName = "loc"
  let pushElem f args ctx = 
    let q = Queue.create () in
    Queue.push 1 q; 
    Some !Tracing.current_loc

  let printStack f expL (listA1, listA2) (listB1, listB2) = 
    printf "fundec: %s\n" (CilType.Fundec.show f);
    printf "List alt: ";
    List.iter (fun x -> Printf.printf "%s; " (CilType.Location.show x)) listA1;
    List.iter (fun x -> Printf.printf "%s; " (CilType.Location.show x)) (List.rev listA2);
    printf "\nList neu: ";
    List.iter (fun x -> Printf.printf "%s; " (CilType.Location.show x)) listB1;
    List.iter (fun x -> Printf.printf "%s; " (CilType.Location.show x)) (List.rev listB2);
    printf "\n\n"
end

(* Lifters for the Callstring approach with different Callstack element types*)
let _ =
  MCP.register_analysis (module Spec (Fundec) : MCPSpec); (* name: callstring_fundec*)
  MCP.register_analysis (module Spec (Stmt) : MCPSpec); (* name: callstring_stmt*)
  MCP.register_analysis (module Spec (Location) : MCPSpec) (* name: callstring_loc*)


