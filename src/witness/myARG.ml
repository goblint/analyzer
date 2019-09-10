open WitnessUtil
open MyCFG

module type Node =
sig
  include Hashtbl.HashedType

  val cfgnode: t -> MyCFG.node
  val to_string: t -> string
end

(* Abstract Reachability Graph *)
module type S =
sig
  module Node: Node

  val main_entry: Node.t
  val next: Node.t -> (MyCFG.edge * Node.t) list
end

module StackNode (Node: Node):
  Node with type t = Node.t list =
struct
  include HashedList (Node)

  let cfgnode nl = Node.cfgnode (List.hd nl)
  let to_string nl =
    nl
    |> List.map Node.to_string
    |> String.concat "@"
end

module Stack (Cfg:CfgForward) (Arg: S):
  S with module Node = StackNode (Arg.Node) =
struct
  module Node = StackNode (Arg.Node)

  let main_entry = [Arg.main_entry]

  let next = function
    | [] -> failwith "StackArg.next: empty"
    | n :: stack ->
      let cfgnode = Arg.Node.cfgnode n in
      match cfgnode with
      | Function _ -> (* TODO: can this be done without Cfg? *)
        begin match stack with
          (* | [] -> failwith "StackArg.next: return stack empty" *)
          | [] -> [] (* main return *)
          | call_n :: call_stack ->
            let call_cfgnode = Arg.Node.cfgnode call_n in
            let call_next =
              Cfg.next call_cfgnode
              (* filter because infinite loops starting with function call
                 will have another Neg(1) edge from the head *)
              |> List.filter (fun (locedges, to_node) ->
                  List.exists (function
                      | (_, Proc _) -> true
                      | (_, _) -> false
                    ) locedges
                )
            in
            begin match call_next with
              | [] -> failwith "StackArg.next: call next empty"
              | [(_, return_node)] ->
                Arg.next n
                |> List.filter (fun (edge, to_n) ->
                    let to_cfgnode = Arg.Node.cfgnode to_n in
                    MyCFG.Node.equal to_cfgnode return_node
                  )
                |> List.map (fun (edge, to_n) ->
                    let to_n' = to_n :: call_stack in
                    (edge, to_n')
                  )
              | _ :: _ :: _ -> failwith "StackArg.next: call next ambiguous"
            end
        end
      | _ ->
        Arg.next n
        |> List.map (fun (edge, to_n) ->
            let to_cfgnode = Arg.Node.cfgnode to_n in
            let to_n' = match to_cfgnode with
              | FunctionEntry _ -> to_n :: n :: stack
              | _ -> to_n :: stack
            in
            (edge, to_n')
          )
end

module type IsInteresting =
sig
  type node
  val is_interesting: node -> MyCFG.edge -> node -> bool
end

module InterestingArg (Arg: S) (IsInteresting: IsInteresting with type node := Arg.Node.t):
  S with module Node = Arg.Node =
struct
  include Arg

  (* too aggressive, duplicates some interesting edges *)
  (* let rec next node =
       Arg.next node
       |> List.map (fun (edge, to_node) ->
           if IsInteresting.is_interesting node edge to_node then
             [(edge, to_node)]
           else
             next to_node
         )
       |> List.flatten *)

  let rec next node =
    Arg.next node
    |> List.map (fun (edge, to_node) ->
        if IsInteresting.is_interesting node edge to_node then
          [(edge, to_node)]
        else begin
          let to_node_next = next to_node in
          if List.exists (fun (edge, to_node) ->
              IsInteresting.is_interesting node edge to_node
            ) to_node_next then
            [(edge, to_node)] (* don't shortcut if node has outdoing interesting edges, e.g. control *)
          else
            to_node_next
        end
      )
    |> List.flatten
end
