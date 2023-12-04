open GoblintCil
open Analyses
open Printf

(* Specifies the type of the callstack elements for the CallstringLifter*)
module type Callstack_Type =
sig
  include CilType.S
  val stackTypeName: string
  val pushElem: fundec -> exp list -> ('d,'g,'c,'v) ctx -> t list (* returns a list of elements that should be pushed to the Callstack *) (*pushElem could be currently also a single element*)
  val printStack: fundec -> exp list -> t list -> t list -> unit (* a helper function to print the callstack *)
end

(** Lifts a [Spec] to analyse with the k-callsting approach. For this the last k callstack elements are used as context
    With the CT argument it is possible to specify the type of the Callstack elements
*)
module Spec (CT:Callstack_Type) : MCPSpec= 
struct
  include Analyses.IdentitySpec

  (* simulates a call stack of depth k*)
  module CallStack = struct
    include Printable.Liszt (CT) 
    let dummy = []
    let depth = 10 (* must be >= 0 *)

    let push stack elem = (* pushes elem to the stack, guarantees stack depth of k*)
      let rec take n = function
        | [] -> []
        | x :: xs when n > 0 -> x :: take (n - 1) xs
        | _ -> []
      in
      let remaining_space = depth - List.length elem in 
      if remaining_space >= 0 
      then
        let remaining_stack = take remaining_space (List.rev stack) in
        List.rev_append remaining_stack elem
      else
        List.rev(take depth (List.rev elem))
  end

  module D = Lattice.FakeSingleton(CallStack)
  module C = CallStack
  module V = EmptyV
  module G = Lattice.Unit

  let name () = "k-callstring approach ("^ CT.stackTypeName ^", k = " ^ (string_of_int CallStack.depth)^")"
  let startstate v = []
  let exitstate v = []

  let context fd d = d

  let enter ctx r f args = 
    let elem = CT.pushElem f args ctx in (* a list of elements that should be pushed onto the stack*)
    let new_stack = CallStack.push (ctx.local) elem in
    let ctx' = {ctx with context = (fun () -> new_stack) (* in case conv ctx makes the context to fail, this is not necessary*)
                       ; local = new_stack} in
    if not !AnalysisState.postsolving then CT.printStack f args (ctx.local) (ctx'.local); (* just for debugging purpose*)
    [ctx.local, new_stack]
end

module Fundec:Callstack_Type = struct
  include CilType.Fundec
  let stackTypeName = "Fundec"
  let pushElem f args ctx = [f]

  let printStack f expL listA listB = 
    printf "fundec: %s\n" (CilType.Fundec.show f);
    printf "List alt: ";
    List.iter (fun x -> Printf.printf "%s; " (CilType.Fundec.show x)) listA;
    printf "\nList neu: ";
    List.iter (fun x -> Printf.printf "%s; " (CilType.Fundec.show x)) listB;
    printf "\n\n"
end

module Stmt:Callstack_Type = struct
  include CilType.Stmt
  let stackTypeName = "Stmt"
  let pushElem f args ctx = match ctx.prev_node with (* TODO: Why do I need to use prev_node???*)
    | Statement stmt -> [stmt]
    | _ -> printf "not a stmt\n"; []

  let printStack f expL listA listB = 
    printf "fundec: %s\n" (CilType.Fundec.show f);
    printf "List alt: ";
    List.iter (fun x -> Printf.printf "%s; " (CilType.Stmt.show x)) listA;
    printf "\nList neu: ";
    List.iter (fun x -> Printf.printf "%s; " (CilType.Stmt.show x)) listB;
    printf "\n\n"
end

(* Lifters for the Callstring approach with different Callstack element types*)
module CallstringLifter_Fundec = Spec (Fundec)
module CallstringLifter_Stmt   = Spec (Stmt)

let _ =
  MCP.register_analysis (module CallstringLifter_Fundec : MCPSpec);
  MCP.register_analysis (module CallstringLifter_Stmt : MCPSpec)


